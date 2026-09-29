#!/usr/bin/env wolframscript

packageRoot = DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{packageRoot, "Kernel", "SynthyraLink.wl"}]];
testFiles = Sort @ FileNames[
    "*.wlt",
    FileNameJoin[{packageRoot, "Tests", "Wolfram"}]
];
report = TestReport[testFiles];
failedTests = report["TestsFailed"];
summary = <|
    "test_files" -> FileNameTake /@ testFiles,
    "succeeded" -> report["TestsSucceededCount"],
    "failed" -> report["TestsFailedCount"],
    "total" -> report["TestsSucceededCount"] + report["TestsFailedCount"],
    "failed_ids" -> If[
        ListQ[failedTests],
        Replace[#["TestID"], _Missing -> "unknown"] & /@ failedTests,
        {}
    ],
    "failed_debug" -> If[
        TrueQ[report["TestsFailedCount"] > 0],
        ToString[failedTests, InputForm],
        Null
    ]
|>;
If[
    Length[$ScriptCommandLine] >= 2,
    Export[$ScriptCommandLine[[-1]], summary /. Missing[__] -> Null, "RawJSON"]
];
Print[ExportString[summary /. Missing[__] -> Null, "RawJSON"]];
If[TrueQ[report["TestsFailedCount"] > 0], Exit[1]];
