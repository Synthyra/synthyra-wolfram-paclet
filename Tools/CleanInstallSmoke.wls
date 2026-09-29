pacletName = "Synthyra/SynthyraLink";
archivePath = Environment["SYNTHYRALINK_PACLET_ARCHIVE"];
installed = None;

cleanup[] := Quiet @ Check[
    If[installed =!= None && installed =!= $Failed,
        PacletUninstall[installed],
        PacletUninstall[pacletName]
    ],
    Null
];

fail[stage_String, value_] := (
    cleanup[];
    Print["::error title=", stage, "::", ToString[value, InputForm]];
    Exit[1]
);

If[! StringQ[archivePath] || StringLength[archivePath] == 0,
    fail["Clean install input", "SYNTHYRALINK_PACLET_ARCHIVE is not set."]
];
archivePath = ExpandFileName[archivePath];
If[! FileExistsQ[archivePath] || ! StringEndsQ[ToLowerCase[archivePath], ".paclet"],
    fail["Clean install input", archivePath]
];

(* This script must run in a fresh wolframscript process after PacletCI.wls. *)
Quiet @ Check[PacletUninstall[pacletName], Null];
If[PacletFind[pacletName] =!= {},
    fail["Clean install precondition", PacletFind[pacletName]]
];

installed = Quiet @ Check[
    PacletInstall[File[archivePath], ForceVersionInstall -> True],
    $Failed
];
If[installed === $Failed || FailureQ[installed],
    fail["Clean install", installed]
];
If[PacletFind[pacletName] === {},
    fail["Clean install discovery", installed]
];

loadResult = Quiet @ Check[Needs["Synthyra`SynthyraLink`"], $Failed];
If[loadResult === $Failed,
    fail["Installed paclet load", loadResult]
];

fixtureCredential = "fixture-only-not-a-live-key";
smokeResult = Quiet @ Check[
    client = SynthyraConnect[Authentication -> fixtureCredential];
    <|
        "Client" -> Normal[client],
        "Operations" -> client["Operations"]
    |>,
    $Failed
];
If[smokeResult === $Failed || ! AssociationQ[smokeResult["Client"]],
    fail["Installed paclet smoke test", smokeResult]
];
If[! FreeQ[smokeResult, fixtureCredential],
    fail["Credential redaction smoke test", smokeResult]
];
(* The operations the protein functions call must be generated. *)
If[! VectorQ[smokeResult["Operations"], StringQ] ||
    ! SubsetQ[smokeResult["Operations"], {"ScoreAtlasPPI", "ScoreAtlasPLI", "RunOracles", "SubmitFoldBatch", "SubmitPrediction", "GetJobPartial", "GetBatchResultsJSONL"}],
    fail["Generated operation smoke test", smokeResult["Operations"]]
];

(* The demo recording ships in the archive, and the protein layer replays it offline. *)
$SynthyraClient = None;
replayResult = Quiet @ Check[
    SynthyraUseRecording["TP53Demo"];
    {StringLength[SynthyraProteinSequence["TP53"]["SequenceString"]], VertexCount[SynthyraInteractome["TP53"]]},
    $Failed
];
If[replayResult =!= {393, 50},
    fail["Installed demo replay", replayResult]
];
SynthyraUseRecording[None];

cleanup[];
Print["Fresh-kernel archive installation and smoke test completed successfully."];
Exit[0];
