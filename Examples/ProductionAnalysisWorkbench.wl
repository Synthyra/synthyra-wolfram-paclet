(* ::Title:: *)
(* Synthyra coordinated-analysis workbench *)

(*
This example is credit-free by default. It never submits compute.

To inspect an already-completed production job, set $UseCompletedSynthyraJob = True
and $CompletedSynthyraJobID before evaluating the file. The live branch performs
authenticated status and result reads only. It never submits compute. Large completed
results transparently use the API-provided HTTPS artifact when the result route fails.
*)

exampleDirectory = DirectoryName[$InputFileName];
pacletRoot = DirectoryName[exampleDirectory];

packageLoad = Quiet @ Check[Needs["Synthyra`SynthyraLink`"], $Failed];
If[packageLoad === $Failed,
    packageLoad = Quiet @ Check[
        Get[FileNameJoin[{pacletRoot, "Kernel", "SynthyraLink.wl"}]],
        $Failed
    ]
];
If[packageLoad === $Failed,
    Print @ Failure[
        "PackageLoadFailed",
        <|"MessageTemplate" -> "Install SynthyraLink or evaluate this file from its source checkout."|>
    ];
    Abort[]
];

If[! ValueQ[$UseCompletedSynthyraJob], $UseCompletedSynthyraJob = False];
If[! ValueQ[$CompletedSynthyraJobID], $CompletedSynthyraJobID = "REPLACE_WITH_COMPLETED_JOB_ID"];

fixtureActome = Import[
    FileNameJoin[{exampleDirectory, "Fixtures", "intra-actome-subgraph.json"}],
    "RawJSON"
];

fixtureNodes = Lookup[fixtureActome, "nodes", {}];
fixtureCoordinates = MapIndexed[
    Function[{node, index},
        Join[
            node,
            <|
                "x" -> Cos[2 Pi (First[index] - 1)/Max[1, Length[fixtureNodes]]],
                "y" -> Sin[2 Pi (First[index] - 1)/Max[1, Length[fixtureNodes]]]
            |>
        ]
    ],
    fixtureNodes
];
fixtureTmapEdges = Map[
    Function[edge,
        <|
            "source" -> Lookup[fixtureNodes[[Lookup[edge, "i"] + 1]], "id"],
            "target" -> Lookup[fixtureNodes[[Lookup[edge, "j"] + 1]], "id"],
            "score" -> Lookup[edge, "s"],
            "distance" -> 1. - Lookup[edge, "s"]/100.
        |>
    ],
    Take[Lookup[fixtureActome, "edges", {}], UpTo[Length[fixtureNodes] - 1]]
];

(*
An intentionally small synthetic peptide. The PDB B-factor column stores
per-residue pLDDT percentages so both native ribbons and confidence-colored
backbone views are available without downloading structure data.
*)
fixturePDB = StringRiffle[{
    "HEADER    SYNTHETIC PROTEIN SCIENCE FIXTURE       14-JUL-26   SYTH",
    "TITLE     SYNTHYRA OFFLINE EIGHT-RESIDUE PEPTIDE",
    "REMARK   1 SYNTHETIC COORDINATES; NOT AN EXPERIMENTAL STRUCTURE",
    "SEQRES   1 A    8  ALA CYS ASP GLU PHE GLY HIS ILE",
    "ATOM      1  N   ALA A   1       0.000   0.000   0.000  1.00 45.00           N",
    "ATOM      2  CA  ALA A   1       1.458   0.000   0.000  1.00 45.00           C",
    "ATOM      3  C   ALA A   1       2.028   1.410   0.000  1.00 45.00           C",
    "ATOM      4  O   ALA A   1       1.360   2.420   0.000  1.00 45.00           O",
    "ATOM      5  CB  ALA A   1       1.960  -0.780   1.210  1.00 45.00           C",
    "ATOM      6  N   CYS A   2       3.280   1.490   0.000  1.00 58.00           N",
    "ATOM      7  CA  CYS A   2       3.950   2.790   0.100  1.00 58.00           C",
    "ATOM      8  C   CYS A   2       5.460   2.650   0.100  1.00 58.00           C",
    "ATOM      9  O   CYS A   2       6.010   1.550   0.000  1.00 58.00           O",
    "ATOM     10  CB  CYS A   2       3.430   3.610   1.290  1.00 58.00           C",
    "ATOM     11  SG  CYS A   2       1.650   3.950   1.400  1.00 58.00           S",
    "ATOM     12  N   ASP A   3       6.120   3.750   0.200  1.00 68.00           N",
    "ATOM     13  CA  ASP A   3       7.560   3.720   0.200  1.00 68.00           C",
    "ATOM     14  C   ASP A   3       8.080   5.160   0.100  1.00 68.00           C",
    "ATOM     15  O   ASP A   3       7.350   6.130   0.100  1.00 68.00           O",
    "ATOM     16  CB  ASP A   3       8.090   2.920   1.400  1.00 68.00           C",
    "ATOM     17  CG  ASP A   3       9.600   2.780   1.420  1.00 68.00           C",
    "ATOM     18  N   GLU A   4       9.350   5.300   0.100  1.00 76.00           N",
    "ATOM     19  CA  GLU A   4       9.980   6.620   0.000  1.00 76.00           C",
    "ATOM     20  C   GLU A   4      11.500   6.480   0.000  1.00 76.00           C",
    "ATOM     21  O   GLU A   4      12.040   5.380   0.100  1.00 76.00           O",
    "ATOM     22  CB  GLU A   4       9.450   7.450   1.200  1.00 76.00           C",
    "ATOM     23  CG  GLU A   4       7.930   7.570   1.240  1.00 76.00           C",
    "ATOM     24  N   PHE A   5      12.170   7.580   0.000  1.00 85.00           N",
    "ATOM     25  CA  PHE A   5      13.610   7.550   0.000  1.00 85.00           C",
    "ATOM     26  C   PHE A   5      14.130   8.990   0.000  1.00 85.00           C",
    "ATOM     27  O   PHE A   5      13.400   9.960   0.100  1.00 85.00           O",
    "ATOM     28  CB  PHE A   5      14.130   6.750   1.200  1.00 85.00           C",
    "ATOM     29  CG  PHE A   5      15.620   6.600   1.220  1.00 85.00           C",
    "ATOM     30  N   GLY A   6      15.400   9.130   0.000  1.00 91.00           N",
    "ATOM     31  CA  GLY A   6      16.030  10.450   0.000  1.00 91.00           C",
    "ATOM     32  C   GLY A   6      17.550  10.310   0.000  1.00 91.00           C",
    "ATOM     33  O   GLY A   6      18.090   9.210   0.100  1.00 91.00           O",
    "ATOM     34  N   HIS A   7      18.220  11.410   0.000  1.00 88.00           N",
    "ATOM     35  CA  HIS A   7      19.660  11.380   0.000  1.00 88.00           C",
    "ATOM     36  C   HIS A   7      20.180  12.820   0.000  1.00 88.00           C",
    "ATOM     37  O   HIS A   7      19.450  13.790   0.100  1.00 88.00           O",
    "ATOM     38  CB  HIS A   7      20.180  10.580   1.200  1.00 88.00           C",
    "ATOM     39  CG  HIS A   7      21.670  10.430   1.220  1.00 88.00           C",
    "ATOM     40  N   ILE A   8      21.450  12.960   0.000  1.00 72.00           N",
    "ATOM     41  CA  ILE A   8      22.080  14.280   0.000  1.00 72.00           C",
    "ATOM     42  C   ILE A   8      23.600  14.140   0.000  1.00 72.00           C",
    "ATOM     43  O   ILE A   8      24.140  13.040   0.100  1.00 72.00           O",
    "ATOM     44  CB  ILE A   8      21.550  15.110   1.200  1.00 72.00           C",
    "TER      45      ILE A   8",
    "END"
}, "\n"];

fixtureCandidateIDs = {"QUERY_A", "QUERY_B", "P_01", "P_02", "P_03", "P_04"};
fixtureCandidateMatrix = {
    {100, 72, 96, 92, 85, 61},
    {72, 100, 68, 89, 59, 83},
    {96, 68, 100, 81, 77, 64},
    {92, 89, 81, 100, 74, 88},
    {85, 59, 77, 74, 100, 70},
    {61, 83, 64, 88, 70, 100}
};
fixtureProteomeIDs = Lookup[fixtureNodes, "id", {}];
fixtureProteomeScores = {100, 100, 96, 92, 89, 88, 85, 83, 81, 80, 78, 75, 73};
fixtureExpansionEnvelope = <|
    "candidate_ids" -> fixtureCandidateIDs,
    "candidate_nodes" -> Select[fixtureNodes, MemberQ[fixtureCandidateIDs, Lookup[#, "id", ""]] &],
    "candidate_matrix_shape" -> Dimensions[fixtureCandidateMatrix],
    "candidate_matrix_b64" -> BaseEncode[ByteArray[Flatten[fixtureCandidateMatrix]]],
    "proteome_ids" -> fixtureProteomeIDs,
    "proteome_scores_b64" -> BaseEncode[ByteArray[fixtureProteomeScores]],
    "dtype" -> "uint8",
    "encoding" -> "base64",
    "order" -> "row-major",
    "metadata" -> <|"fixture_only" -> True, "description" -> "Synthetic expansion-envelope scores."|>
|>;

fixtureResult = <|
    "dfa" -> <|
        "protein_id" -> "QUERY_A",
        "sequence" -> "ACDEFGHI",
        "pdb_string" -> fixturePDB,
        "plddt" -> .729,
        "foldseek_3di" -> "DVQVKDLP",
        "oracle_predictions" -> {
            <|
                "oracle_name" -> "solubility", "score_mode" -> "binary_prob", "score" -> .83,
                "attribution_labels" -> {"solubility"},
                "attributions" -> {-.18, -.08, .06, .22, .31, .17, -.04, .11}
            |>,
            <|"oracle_name" -> "thermostability", "score_mode" -> "regression", "score" -> 52.7|>,
            <|
                "oracle_name" -> "localization", "score_mode" -> "multiclass_prob",
                "label_names" -> {"cytoplasm", "nucleus", "secreted"},
                "score" -> {.12, .79, .09},
                "attribution_labels" -> {"cytoplasm", "nucleus", "secreted"},
                "attributions" -> {
                    {.08, -.03, -.01}, {.04, .10, -.02}, {-.07, .18, .01},
                    {-.03, .27, -.04}, {.02, .21, -.03}, {.05, .14, -.02},
                    {-.02, .19, .03}, {.01, .11, .04}
                }
            |>
        },
        "camp_annotations" -> {
            <|"annotation_id" -> "CAMP:fixture", "name" -> "Fixture catalytic region", "score" -> .88|>
        },
        "translator_annotations" -> {
            <|"annotation_id" -> "GO:fixture", "name" -> "Fixture DNA binding", "confidence" -> .76|>
        }
    |>,
    "network" -> <|
        "envelope" -> fixtureExpansionEnvelope,
        "actome_edges" -> Join[
            fixtureActome,
            <|
                "subnetwork_heatmap" -> <|
                    "ids" -> {"QUERY_A", "QUERY_B", "P_01", "P_02"},
                    "scores" -> {
                        {100, 72, 96, 92},
                        {72, 100, 68, 89},
                        {96, 68, 100, 81},
                        {92, 89, 81, 100}
                    },
                    "cluster_order" -> {0, 1, 2, 3},
                    "query_indices" -> {0, 1}
                |>
            |>
        ],
        "tmap_tree" -> <|
            "title" -> "Synthetic TMAP layout fixture",
            "nodes" -> fixtureCoordinates,
            "edges" -> fixtureTmapEdges,
            "focus" -> <|"query_ids" -> {"QUERY_A", "QUERY_B"}|>,
            "metadata" -> <|"fixture_only" -> True|>
        |>
    |>,
    "enrichment" -> <|
        "terms" -> {
            <|
                "term" -> "Fixture protein binding", "library" -> "GO_Molecular_Function",
                "p_value" -> 1.2*^-7, "adjusted_p_value" -> 3.6*^-6,
                "genes" -> {"QUERY_A", "P_01", "P_02"}, "gene_count" -> 3
            |>,
            <|
                "term" -> "Fixture signaling pathway", "library" -> "Reactome",
                "p_value" -> 4.1*^-5, "adjusted_p_value" -> 8.2*^-4,
                "genes" -> {"QUERY_B", "P_03"}, "gene_count" -> 2
            |>
        }
    |>
|>;

coordinatedResult = If[
    TrueQ[$UseCompletedSynthyraJob],
    If[
        ! StringQ[$CompletedSynthyraJobID] || StringStartsQ[$CompletedSynthyraJobID, "REPLACE_"],
        Failure[
            "ExampleConfiguration",
            <|"MessageTemplate" -> "Set $CompletedSynthyraJobID before enabling the completed-job reader."|>
        ],
        completedJobClient = SynthyraConnect[];
        If[
            FailureQ[completedJobClient],
            completedJobClient,
            completedSubmissionInformation = completedJobClient[
                "OperationInformation",
                "SubmitCoordinatedPrediction"
            ];
            If[
                FailureQ[completedSubmissionInformation],
                completedSubmissionInformation,
                completedJob = SynthyraJobObject[
                    <|
                        "Client" -> completedJobClient,
                        "SubmissionOperation" -> "SubmitCoordinatedPrediction",
                        "JobID" -> $CompletedSynthyraJobID,
                        "Job" -> Lookup[completedSubmissionInformation, "Async", <||>],
                        "Submission" -> <||>
                    |>
                ];
                SynthyraJobResult[
                    completedJob,
                    WaitForCompletion -> True,
                    PollInterval -> 1.,
                    Timeout -> 3600.,
                    MaxRetries -> 2
                ]
            ]
        ]
    ],
    fixtureResult
];

If[FailureQ[coordinatedResult], Print[coordinatedResult]; Abort[]];

synthyraAnalysis = SynthyraAnalysis[coordinatedResult];
If[FailureQ[synthyraAnalysis], Print[synthyraAnalysis]; Abort[]];

synthyraWorkbench = SynthyraWorkbench[synthyraAnalysis];
synthyraWorkbench
