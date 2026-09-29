#!/usr/bin/env wolframscript
(* Record Examples/TP53.nb against production into Examples/Data/TP53Demo.wxf, the recording
   SynthyraUseRecording["TP53Demo"] replays. Needs SYNTHYRA_API_KEY; bills the demo's calls. *)

packageRoot = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[packageRoot];
Needs["Synthyra`SynthyraLink`"];

inputs = Synthyra`SynthyraLink`Private`notebookInputs[
    FileNameJoin[{packageRoot, "Examples", "TP53.nb"}],
    {"Setup", "Offline", "LLM"}
];
target = FileNameJoin[{packageRoot, "Examples", "Data", "TP53Demo.wxf"}];

failed = SynthyraRecord[
    Catch @ Scan[
        Function[code, Module[{value = ToExpression[code], started = AbsoluteTime[]},
            Print[StringTake[StringReplace[code, "\n" -> " "], UpTo[70]], "  ->  ", Head[value]];
            If[FailureQ[value], Throw[value]]
        ]],
        inputs
    ],
    target
];
If[FailureQ[failed], Print["FAILED: ", failed["Message"]]; Exit[1]];
Print["recorded ", Length[inputs], " cells into ", target, ", ", FileByteCount[target], " bytes"];
