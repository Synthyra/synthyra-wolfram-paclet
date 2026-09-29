(* Check, test, and build the paclet with PacletCICD, as the repository's CI does. The
   documentation is built by Documentation/Source/BuildDocumentation.wl and committed, since
   building it needs a front end; this script checks it. The archive goes to
   SYNTHYRALINK_BUILD_DIRECTORY, or build/ under the paclet root.

       wolframscript -file Tools/PacletCI.wls
*)

pacletRoot = ParentDirectory[DirectoryName[ExpandFileName[$InputFileName]]];
buildDirectory = Replace[Environment["SYNTHYRALINK_BUILD_DIRECTORY"], Except[_String?(StringLength[#] > 0 &)] :> None];
(* BuildPaclet writes beside the paclet, so a build directory outside the source tree gets a copy
   of the source to build from. *)
If[StringQ[buildDirectory],
    Quiet @ DeleteDirectory[FileNameJoin[{buildDirectory, "source"}], DeleteContents -> True];
    pacletRoot = CopyDirectory[pacletRoot, FileNameJoin[{buildDirectory, "source"}]],
    buildDirectory = FileNameJoin[{pacletRoot, "build"}]
];
SetDirectory[pacletRoot];

fail[stage_String, value_] := (
    Print["::error title=", stage, "::", ToString[value, InputForm]];
    Exit[1]
);

Print["Wolfram Language ", $Version, " on ", $SystemID];

pacletCICDVersion = "0.36.2";
pacletCICDURL = "https://github.com/WolframResearch/PacletCICD/releases/download/v0.36.2/Wolfram__PacletCICD-0.36.2.paclet";
pacletCICDSHA256 = "b69d07cadeeef020c9fa2512d5138ce79d298b4119d10497088ccfae6d567038";
If[! AnyTrue[PacletFind["Wolfram/PacletCICD"], #["Version"] === pacletCICDVersion &],
    pacletCICDDownload = FileNameJoin[{
        $TemporaryDirectory,
        "Wolfram__PacletCICD-" <> CreateUUID[] <> ".paclet"
    }];
    downloadResult = Quiet @ Check[URLDownload[pacletCICDURL, pacletCICDDownload], $Failed];
    If[downloadResult === $Failed || ! FileExistsQ[pacletCICDDownload],
        fail["PacletCICD download", downloadResult]
    ];
    downloadedSHA256 = ToLowerCase @ IntegerString[
        FileHash[pacletCICDDownload, "SHA256"],
        16,
        64
    ];
    If[downloadedSHA256 =!= pacletCICDSHA256,
        Quiet @ DeleteFile[pacletCICDDownload];
        fail["PacletCICD digest", "Downloaded release asset did not match the pinned SHA-256."]
    ];
    installResult = Quiet @ Check[
        PacletInstall[File[pacletCICDDownload], ForceVersionInstall -> True],
        $Failed
    ];
    Quiet @ DeleteFile[pacletCICDDownload];
    If[installResult === $Failed || FailureQ[installResult],
        fail["PacletCICD install", installResult]
    ]
];
Needs["Wolfram`PacletCICD`"];

checkResult = Quiet @ Check[CheckPaclet[File[pacletRoot]], $Failed];
If[checkResult === $Failed || ! FreeQ[checkResult, _Failure], fail["CheckPaclet", checkResult]];
Print["CheckPaclet completed."];

(* TestPaclet accepts a directory path string, unlike CheckPaclet and BuildPaclet,
   whose directory overloads accept File[...]. *)
(* TestPaclet can emit harmless General::cdir messages while PacletCICD skips
   transient hidden cache directories. Judge its structured return value rather
   than converting every message into $Failed with Check. *)
testResult = Quiet @ TestPaclet[pacletRoot, "AnnotateTestIDs" -> False];
testSucceededQ[value_] :=
    MatchQ[value, Success["AllTestsSucceeded", _Association]] ||
    (MatchQ[value, _TestReportObject] && TrueQ[value["ReportSucceeded"]]);
If[testResult === $Failed || FailureQ[testResult] || ! TrueQ[testSucceededQ[testResult]],
    fail["TestPaclet", testResult]
];
Print["TestPaclet completed successfully."];

(* As with TestPaclet, transient directory/documentation messages do not make
   a structured successful build fail. Judge BuildPaclet's return value. *)
archive = Quiet @ BuildPaclet[File[pacletRoot]];
If[archive === $Failed || FailureQ[archive], fail["BuildPaclet", archive]];
archiveValue = Replace[
    archive,
    Success["PacletBuild", data_Association] :> Lookup[data, "PacletArchive", $Failed]
];
archivePath = Which[
    MatchQ[archiveValue, File[_String]], First[archiveValue],
    StringQ[archiveValue] && FileExistsQ[archiveValue], archiveValue,
    StringQ[archiveValue] && FileExistsQ[archiveValue <> ".paclet"], archiveValue <> ".paclet",
    True, $Failed
];
If[archivePath === $Failed || ! FileExistsQ[archivePath], fail["BuildPaclet archive", archive]];
If[! DirectoryQ[buildDirectory], CreateDirectory[buildDirectory, CreateIntermediateDirectories -> True]];
buildArchivePath = FileNameJoin[{buildDirectory, FileNameTake[archivePath]}];
If[ExpandFileName[archivePath] =!= ExpandFileName[buildArchivePath],
    CopyFile[archivePath, buildArchivePath, OverwriteTarget -> True]
];
archive = File[buildArchivePath];
Print["Built archive: ", archive];

Print["Paclet check, tests, and archive build completed successfully."];
Exit[0];
