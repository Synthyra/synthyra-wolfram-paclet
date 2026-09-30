Needs["Synthyra`SynthyraLink`"];

(* The protein layer, offline: its input handling against mocked transports, and each function
   end to end against the TP53 demo recording, which production answered on 2026-09-29. *)

ClearAll[mockResponse, withTransport];
mockResponse[status_Integer, body_String : "", contentType_String : "application/json"] := <|
    "StatusCode" -> status,
    "Headers" -> {},
    "ContentType" -> contentType,
    "Body" -> body,
    "BodyByteArray" -> StringToByteArray[body, "UTF8"]
|>;

SetAttributes[withTransport, HoldRest];
withTransport[transport_, expr_] := Block[
    {
        Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = transport,
        Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[Null],
        Synthyra`SynthyraLink`Private`$synthyraSequenceCache = <||>,
        $SynthyraClient = None
    },
    SynthyraConnect[Authentication -> "test-key"];
    expr
];

$tp53Fasta = ">sp|P04637|P53_HUMAN Cellular tumor antigen p53\nMEEPQSDPSV\nEPPLSQETFS\n";

(* Input handling *)

VerificationTest[
    withTransport[
        Function[{request, timeout}, mockResponse[200, $tp53Fasta, "text/plain"]],
        SynthyraProteinSequence["P04637"]
    ],
    BioSequence["Peptide", "MEEPQSDPSVEPPLSQETFS"],
    SameTest -> (#1["SequenceString"] === #2["SequenceString"] &),
    TestID -> "Protein-AccessionResolvesThroughUniProt"
];

VerificationTest[
    Module[{urls = {}},
        withTransport[
            Function[{request, timeout}, AppendTo[urls, request["URL"]]; mockResponse[200, "Entry\tSequence\nP04637\tMEEPQSDPSV\n", "text/plain"]],
            SynthyraProteinSequence["TP53", "Organism" -> "Mouse"]
        ];
        {Length[urls], StringContainsQ[First[urls], "gene_exact%3ATP53"], StringContainsQ[First[urls], "organism_id%3A10090"]}
    ],
    {1, True, True},
    TestID -> "Protein-GeneSymbolQueriesTheNamedOrganism"
];

VerificationTest[
    withTransport[
        Function[{request, timeout}, Throw["no request expected"]],
        {
            SynthyraProteinSequence["mkvlaagivalllaa"]["SequenceString"],
            SynthyraProteinSequence[BioSequence["Peptide", "MKV"]]["SequenceString"]
        }
    ],
    {"MKVLAAGIVALLLAA", "MKV"},
    TestID -> "Protein-SequencesNeedNoLookup"
];

VerificationTest[
    withTransport[
        Function[{request, timeout}, Throw["no request expected"]],
        {
            Keys @ SynthyraProteinSequence[{"MKVLAAGIVALLLAA", "p" -> "MKVLAAGIVALLLAA", "MKVLAAGIVALLLAA"}],
            Keys @ SynthyraProteinSequence[{"p" -> "MKVLAAGIVALLLAA", "p" -> "MSTNPKPQRKTKRNT"}]
        }
    ],
    {{"protein 1", "p", "protein 3"}, {"p (1)", "p (2)"}},
    TestID -> "Protein-LabelsAreUnique"
];

VerificationTest[
    withTransport[
        Function[{request, timeout}, Throw["no request expected"]],
        {
            FailureQ[SynthyraProteinSequence["not a protein!"]],
            SynthyraProteinSequence["not a protein!"][[1]],
            SynthyraProteinSequence["TP53", "Organism" -> "Martian"][[1]],
            SynthyraProteinSequence[BioSequence["DNA", "ACGT"]][[1]]
        }
    ],
    {True, "UnrecognizedProtein", "UnknownOrganism", "NotAProtein"},
    {SynthyraProteinSequence::failed, SynthyraProteinSequence::failed, SynthyraProteinSequence::failed, General::stop},
    TestID -> "Protein-InvalidInputsFailByName"
];

VerificationTest[
    withTransport[
        Function[{request, timeout}, Throw["no request expected"]],
        Synthyra`SynthyraLink`Private`resolveLigand["CC(=O)Oc1ccccc1C(=O)O"]
    ],
    <|"ID" -> Automatic, "SMILES" -> "CC(=O)Oc1ccccc1C(=O)O"|>,
    TestID -> "Protein-SMILESNeedsNoLookup"
];

VerificationTest[
    Module[{records, chunks},
        records = Table[<|"Key" -> ToString[i]|>, {i, 450}];
        chunks = Synthyra`SynthyraLink`Private`scoreChunks[Table[{i, i}, {i, 450}], records, records];
        {Length /@ chunks, Max[Length @ Union[Flatten[{"a" <> ToString[#[[1]]], "b" <> ToString[#[[2]]]} & /@ #]] & /@ chunks]}
    ],
    {{200, 200, 50}, 400},
    TestID -> "Protein-ScoreChunksKeepFourHundredSequences"
];

VerificationTest[
    Length @ Synthyra`SynthyraLink`Private`scoreChunks[
        Tuples[{Range[40], Range[30]}],
        Table[<|"Key" -> "a" <> ToString[i]|>, {i, 40}],
        Table[<|"Key" -> "b" <> ToString[i]|>, {i, 30}]
    ],
    2,
    TestID -> "Protein-ScoreChunksKeepAThousandPairs"
];

VerificationTest[
    Module[{bodies = {}, result},
        result = withTransport[
            Function[{request, timeout},
                AppendTo[bodies, ImportString[request["Body"], "RawJSON"]];
                mockResponse[200, "{\"scores\": [0.1, 0.2, 0.3, 0.4]}"]
            ],
            SynthyraProteinInteractionScore[{"a" -> "MKVLAAGIVALLLAA", "b" -> "MSTNPKPQRKTKRNT"}, {"c" -> "MGSSHHHHHHSSGLV", "d" -> "MAHHHHHHVDDDDKM"}]
        ];
        {Length[bodies], bodies[[1, "ids_a"]], bodies[[1, "ids_b"]], Normal[result]}
    ],
    {1, {"a", "a", "b", "b"}, {"c", "d", "c", "d"}, <|"a" -> <|"c" -> 0.1, "d" -> 0.2|>, "b" -> <|"c" -> 0.3, "d" -> 0.4|>|>},
    TestID -> "Protein-InteractionScoresEveryPair"
];

VerificationTest[
    withTransport[
        Function[{request, timeout}, mockResponse[422, "{\"error\": \"request_error\", \"message\": \"inputs_a and inputs_b must have the same length\"}"]],
        StringContainsQ[
            ToString @ SynthyraProteinInteractionScore["MKVLAAGIVALLLAA", "MSTNPKPQRKTKRNT"]["Message"],
            "same length"
        ]
    ],
    True,
    {SynthyraProteinInteractionScore::failed},
    TestID -> "Protein-ServerMessageReachesTheFailure"
];

VerificationTest[
    Synthyra`SynthyraLink`Private`associationLookup[<|"error" -> "request_error", "message" -> "why"|>, {"message", "error"}],
    "why",
    TestID -> "Runtime-AssociationLookupHonorsNameOrder"
];

(* End to end, from the TP53 demo recording *)

VerificationTest[
    SynthyraUseRecording["TP53Demo"];
    {MatchQ[$SynthyraClient, _SynthyraClientObject], StringLength[SynthyraProteinSequence["TP53"]["SequenceString"]]},
    {True, 393},
    TestID -> "Recording-SequenceReplays"
];

VerificationTest[
    Module[{fold = SynthyraFoldProtein["TP53", "Output" -> "Association"]},
        {Head[fold["Structure"]], Round[fold["MeanPLDDT"]], Length[fold["ResiduePLDDT"]["A"]], fold["Model"]}
    ],
    {BioMolecule, 80, 393, "Synthyra/ESMFold2-Fast"},
    TestID -> "Recording-FoldGivesABioMolecule"
];

VerificationTest[
    Head @ SynthyraStructurePlot[SynthyraFoldProtein["TP53"]],
    Legended,
    TestID -> "Recording-StructurePlotHasAConfidenceLegend"
];

VerificationTest[
    FailureQ @ SynthyraStructurePlot[SynthyraFoldProtein["TP53"], "ColorBy" -> "Rainbow"],
    True,
    {SynthyraStructurePlot::failed},
    TestID -> "Recording-StructurePlotRejectsUnknownColoring"
];

VerificationTest[
    Module[{properties = Normal @ SynthyraProteinProperties["TP53"]},
        {Length[properties] >= 10, MemberQ[properties[[All, "Property"]], "solubility"]}
    ],
    {True, True},
    TestID -> "Recording-PropertiesAreADataset"
];

VerificationTest[
    Module[{graph = SynthyraInteractome["TP53"]},
        {
            VertexCount[graph], EdgeCount[graph], MemberQ[VertexList[graph], "TP53"],
            MemberQ[VertexList[graph], "DNMT3B"], AnnotationValue[{graph, "DNMT3B"}, "UniProtID"],
            Head @ AnnotationValue[graph, "Enrichment"]
        }
    ],
    {50, 1222, True, True, "Q9UBC3", Dataset},
    TestID -> "Recording-InteractomeUsesGeneSymbols"
];

VerificationTest[
    Module[{graph = SynthyraInteractome["TP53"]},
        Sort @ Select[AdjacencyList[graph, "TP53"], ! TrueQ[AnnotationValue[{graph, First @ EdgeList[graph, "TP53" \[UndirectedEdge] #]}, "Novel"]] &] // Length
    ],
    19,
    TestID -> "Recording-InteractomeQueryIsCrossReferenced"
];

VerificationTest[
    Module[{ligands = Normal @ SynthyraLigandBindingScore[{"MDM2", "TP53"}, {"nutlin-3a", "idasanutlin", "eprenetapopt", "aspirin"}]},
        {ligands["MDM2", "nutlin-3a"] > 0.8, ligands["MDM2", "aspirin"] < 0.2}
    ],
    {True, True},
    TestID -> "Recording-LigandScoresReplay"
];

VerificationTest[
    Module[{complex = SynthyraFoldComplex[
            {"MDM2 N-terminal domain" -> BioSequence["Peptide", StringTake[SynthyraProteinSequence["MDM2"]["SequenceString"], {17, 125}]],
             "p53 transactivation helix" -> BioSequence["Peptide", StringTake[SynthyraProteinSequence["TP53"]["SequenceString"], {15, 29}]]},
            "Output" -> "Association"]},
        {Sort @ Keys[complex["ResiduePLDDT"]], complex["IPTM"] > 0.5}
    ],
    {{"A", "B"}, True},
    TestID -> "Recording-ComplexFoldsTwoChains"
];

VerificationTest[
    Module[{failure = None},
        Scan[
            Function[code, With[{value = ToExpression[code]}, If[FailureQ[value], failure = {code, value}]]],
            Synthyra`SynthyraLink`Private`notebookInputs[
                FileNameJoin[{DirectoryName[DirectoryName[DirectoryName[$TestFileName]]], "Examples", "TP53.nb"}],
                {"Source", "Setup", "Live", "Offline", "LLM"}
            ]
        ];
        failure
    ],
    None,
    TestID -> "Recording-DemoNotebookReplaysOffline"
];

VerificationTest[
    SynthyraProteinInteractionScore["TP53", "MKVLAAGIVALLLAA"][[1]],
    "NotRecorded",
    {SynthyraProteinInteractionScore::failed},
    TestID -> "Recording-UnrecordedRequestFails"
];

VerificationTest[
    SynthyraUseRecording[None];
    $SynthyraClient,
    None,
    TestID -> "Recording-EndingReplayDropsItsClient"
];

VerificationTest[
    Length @ SynthyraLLMTools[],
    5,
    TestID -> "Protein-LLMTools"
];
