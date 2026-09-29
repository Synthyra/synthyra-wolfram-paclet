(* Recordings: every HTTP exchange the paclet makes, saved so a session can be replayed offline.

   SynthyraRecord wraps the transports and keeps each exchange; SynthyraUseRecording answers every
   request from a recording instead of the network, so a demo runs without a connection or a key.
   A request is identified by its method, URL, and body, never its headers, so a recording holds no
   credential. A request made more than once, such as a job status poll, replays its responses in
   the order they were recorded and then repeats the last. *)

$recordedResponseHeaders = {"content-type", "retry-after", "content-disposition"};

requestBodyText[body_ByteArray] := ByteArrayToString[body];
requestBodyText[body_String] := body;
requestBodyText[_] := "";

requestKey[request_HTTPRequest] := {
    ToUpperCase @ ToString @ request["Method"],
    ToString @ request["URL"],
    requestBodyText @ request["Body"]
};
requestKey[url_String] := {"GET", url, ""};
requestKey[other_] := {"GET", ToString[other], ""};

(* The body is kept once, as bytes; the replay transport restores its text. *)
recordedResponse[response_Association] := Association[
    KeyTake[response, {"StatusCode", "ContentType"}],
    "BodyByteArray" -> Replace[
        Lookup[response, "BodyByteArray", Missing["NotAvailable"]],
        Except[_ByteArray] :> StringToByteArray[ToString @ Lookup[response, "Body", ""], "UTF8"]
    ],
    "Headers" -> Select[
        Replace[Lookup[response, "Headers", {}], headers_Association :> Normal[headers]],
        MatchQ[#, _String -> _] && MemberQ[$recordedResponseHeaders, ToLowerCase[First[#]]] &
    ]
];

$activeRecording = None;
$replayRecording = None;
$replayPositions = <||>;
$liveTransports = None;
$replayClient = None;

appendExchange[kind_String, key_List, response_Association] := If[
    AssociationQ[$activeRecording],
    $activeRecording[kind] = Merge[{$activeRecording[kind], <|key -> {response}|>}, Apply[Join]]
];

recordingTransports[{http_, download_}] := {
    Function[{request, timeout}, Module[{response = http[request, timeout]},
        If[AssociationQ[response], appendExchange["HTTP", requestKey[request], recordedResponse[response]]];
        response
    ]],
    Function[{request, path, timeout}, Module[{response = download[request, path, timeout], file},
        file = Lookup[If[AssociationQ[response], response, <||>], "File", None];
        If[AssociationQ[response] && MatchQ[file, File[_String]] && FileExistsQ[file],
            appendExchange["Download", requestKey[request],
                Append[KeyDrop[recordedResponse[response], "BodyByteArray"], "Bytes" -> ReadByteArray[file]]]
        ];
        response
    ]]
};

nextReplayed[kind_String, key_List] := Module[{responses, position},
    responses = Lookup[Lookup[$replayRecording, kind, <||>], Key[key], Missing["NotRecorded"]];
    If[MissingQ[responses], Return[responses, Module]];
    position = Lookup[$replayPositions, Key[{kind, key}], 0] + 1;
    $replayPositions[{kind, key}] = position;
    responses[[Min[position, Length[responses]]]]
];

notRecorded[key_List] := Failure["NotRecorded", <|
    "MessageTemplate" -> "The recording in use has no response to `Method` `URL`; SynthyraUseRecording[None] returns to the network.",
    "MessageParameters" -> <|"Method" -> key[[1]], "URL" -> key[[2]]|>
|>];

replayTransports[] := {
    Function[{request, timeout}, Module[{key = requestKey[request], response},
        response = nextReplayed["HTTP", key];
        If[MissingQ[response],
            notRecorded[key],
            Append[response, "Body" -> ByteArrayToString[response["BodyByteArray"]]]
        ]
    ]],
    Function[{request, path, timeout}, Module[{key = requestKey[request], response, target},
        response = nextReplayed["Download", key];
        If[MissingQ[response], Return[notRecorded[key], Module]];
        target = ExpandFileName[path];
        If[BinaryWrite[target, response["Bytes"]] === $Failed, Return[$Failed, Module]];
        Close[target];
        Append[KeyDrop[response, "Bytes"], "File" -> File[target]]
    ]]
};

SetAttributes[SynthyraRecord, HoldFirst];

SynthyraRecord[expr_, file : (_String | File[_String])] := Module[
    {http = $SynthyraHTTPTransport, download = $SynthyraDownloadTransport, path = file /. File[name_] :> name, result, saved},
    If[AssociationQ[$activeRecording],
        Return @ Failure["RecordingInProgress", <|"MessageTemplate" -> "SynthyraRecord cannot be nested."|>]
    ];
    WithCleanup[
        $activeRecording = <|"Version" -> 1, "HTTP" -> <||>, "Download" -> <||>|>,
        result = Block[{$SynthyraHTTPTransport, $SynthyraDownloadTransport},
            {$SynthyraHTTPTransport, $SynthyraDownloadTransport} = recordingTransports[{http, download}];
            expr
        ];
        saved = Export[path, $activeRecording, "WXF", PerformanceGoal -> "Size"],
        $activeRecording = None
    ];
    If[saved === $Failed,
        Failure["RecordingNotSaved", <|
            "MessageTemplate" -> "The recording could not be written to `File`.",
            "MessageParameters" -> <|"File" -> path|>
        |>],
        result
    ]
];

SynthyraUseRecording[None] := (
    If[ListQ[$liveTransports],
        {$SynthyraHTTPTransport, $SynthyraDownloadTransport, $SynthyraPauseFunction} = $liveTransports
    ];
    If[$replayClient =!= None && $SynthyraClient === $replayClient, $SynthyraClient = None];
    $replayClient = None;
    $liveTransports = None;
    $replayRecording = None;
    $replayPositions = <||>;
    None
);

(* Recordings that ship with the paclet, used by name: SynthyraUseRecording["TP53Demo"]. *)
$synthyraRecordingDirectory = FileNameJoin[{ParentDirectory[$SynthyraKernelDirectory], "Examples", "Data"}];

recordingPath[File[path_String]] := path;
recordingPath[name_String] /; FileExtension[name] === "" && ! StringContainsQ[name, {"/", "\\"}] :=
    FileNameJoin[{$synthyraRecordingDirectory, name <> ".wxf"}];
recordingPath[path_String] := path;

SynthyraUseRecording[file : (_String | File[_String])] := Module[{recording},
    recording = Quiet @ Check[Import[recordingPath[file], "WXF"], $Failed];
    If[! AssociationQ[recording] || ! KeyExistsQ[recording, "HTTP"],
        Return @ Failure["InvalidRecording", <|
            "MessageTemplate" -> "`File` is not a Synthyra recording.",
            "MessageParameters" -> <|"File" -> file|>
        |>]
    ];
    If[! ListQ[$liveTransports],
        $liveTransports = {$SynthyraHTTPTransport, $SynthyraDownloadTransport, $SynthyraPauseFunction}
    ];
    $replayRecording = recording;
    $replayPositions = <||>;
    {$SynthyraHTTPTransport, $SynthyraDownloadTransport} = replayTransports[];
    $SynthyraPauseFunction = Function[Null];
    (* A replay needs no key. A session without a client gets one whose placeholder key never
       leaves the kernel, since every request is answered from the recording, and loses it when
       the replay ends. *)
    If[! MatchQ[$SynthyraClient, _SynthyraClientObject],
        $replayClient = SynthyraConnect[Authentication -> "offline-replay"]
    ];
    file
];

(* The input cells of a notebook file, in order, without those carrying any of the excluded cell
   tags. Tools/RecordDemo.wls records a demo notebook from these, and the tests replay it. *)
notebookInputs[path_String, excludedTags_List] := Cases[
    Get[path],
    Cell[code_String, "Input", options___] /; ! IntersectingQ[Flatten[{Lookup[{options}, CellTags, {}]}], excludedTags] :> code,
    Infinity
];
