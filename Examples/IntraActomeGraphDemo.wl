(* ::Title:: *)
(* SynthyraLink intra-actome neighborhood explorer *)

(*
Evaluate this file with Get[".../Examples/IntraActomeGraphDemo.wl"].
The default fixture mode is synthetic and consumes no credits. Set
$UseLiveSynthyra = True and provide a completed Synthyra job ID to read its
result_actome_edges payload from production. This demo does not submit compute.
*)

demoDirectory = DirectoryName[$InputFileName];
pacletRoot = DirectoryName[demoDirectory];

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

If[! ValueQ[$UseLiveSynthyra], $UseLiveSynthyra = False];
If[! ValueQ[$LiveJobID], $LiveJobID = "REPLACE_WITH_COMPLETED_JOB_ID"];
If[! ValueQ[$LiveQueryIDs], $LiveQueryIDs = Automatic];

intraActomePayload = If[
    TrueQ[$UseLiveSynthyra],
    If[! StringQ[$LiveJobID] || StringStartsQ[$LiveJobID, "REPLACE_"],
        Failure[
            "DemoConfiguration",
            <|"MessageTemplate" -> "Set $LiveJobID to a completed intra-actome job before enabling the live demo."|>
        ],
        synthyraClient = SynthyraConnect[];
        If[
            FailureQ[synthyraClient],
            synthyraClient,
            synthyraClient[
                "GetJobPartial",
                <|
                    "Parameters" -> <|
                        "job_id" -> $LiveJobID,
                        "field_name" -> "result_actome_edges"
                    |>
                |>,
                MaxRetries -> 2
            ]
        ]
    ],
    Import[
        FileNameJoin[{demoDirectory, "Fixtures", "intra-actome-subgraph.json"}],
        "RawJSON"
    ]
];

If[FailureQ[intraActomePayload], Print[intraActomePayload]; Abort[]];

effectiveQueryIDs = Replace[
    $LiveQueryIDs,
    Automatic :> Lookup[intraActomePayload, "query_ids", {}]
];
If[
    ! ListQ[effectiveQueryIDs] ||
        ! AllTrue[effectiveQueryIDs, StringQ[#] && StringTrim[#] =!= "" &],
    Print @ Failure[
        "DemoConfiguration",
        <|"MessageTemplate" -> "Set $LiveQueryIDs to the query protein identifiers represented in the network."|>
    ];
    Abort[]
];

intraActomeGraph = SynthyraGraph[intraActomePayload];
If[FailureQ[intraActomeGraph], Print[intraActomeGraph]; Abort[]];

intraActomeVertexID[Protein[id_]] := ToString[id];
intraActomeVertexID[Ligand[id_]] := ToString[id];
intraActomeVertexID[vertex_] := ToString[vertex, InputForm];

intraActomeConfidence[graph_Graph, edge_] := Module[{value},
    value = Quiet @ Check[AnnotationValue[{graph, edge}, "Confidence"], $Failed];
    If[
        NumericQ[value],
        Clip[If[N[value] <= 1., N[value], N[value]/100.], {0., 1.}],
        0.5
    ]
];

intraActomeNeighborhood[graph_Graph, seeds_List, radius_Integer] := Module[
    {vertices},
    vertices = VertexList @ NeighborhoodGraph[graph, seeds, radius];
    Subgraph[graph, vertices, AnnotationRules -> Inherited]
];

intraActomeRender[graph_Graph, seeds_List, layout_String] := Module[
    {
        vertices, edges, communities, communityRules, seedVertices,
        seedRules, edgeRules, labelRules, sizeRules
    },
    vertices = VertexList[graph];
    edges = EdgeList[graph];
    seedVertices = Intersection[vertices, Protein /@ seeds];
    communities = FindGraphCommunities[graph];
    communityRules = Flatten @ MapIndexed[
        Function[{community, index},
            With[
                {color = ColorData[97][1 + Mod[First[index] - 1, 15]]},
                (# -> color &) /@ Select[community, ! MemberQ[seedVertices, #] &]
            ]
        ],
        communities
    ];
    seedRules = (# -> Directive[Red, EdgeForm[Directive[Black, Thick]]] &) /@ seedVertices;
    edgeRules = Map[
        Function[edge,
            With[
                {confidence = intraActomeConfidence[graph, edge]},
                edge -> Directive[
                    GrayLevel[0.35],
                    Opacity[0.15 + 0.8 confidence],
                    AbsoluteThickness[0.5 + 2.0 confidence]
                ]
            ]
        ],
        edges
    ];
    labelRules = (# -> Placed[intraActomeVertexID[#], Tooltip] &) /@ vertices;
    sizeRules = Join[
        (# -> 0.34 &) /@ seedVertices,
        (# -> 0.22 &) /@ Select[vertices, ! MemberQ[seedVertices, #] &]
    ];
    Graph[
        vertices,
        edges,
        GraphLayout -> layout,
        VertexStyle -> Join[communityRules, seedRules],
        VertexSize -> sizeRules,
        VertexLabels -> labelRules,
        EdgeStyle -> edgeRules,
        ImageSize -> 900,
        PerformanceGoal -> "Quality"
    ]
];

intraActomeTopCentrality[graph_Graph, count_Integer : 10] := Module[
    {vertices, ranked},
    vertices = VertexList[graph];
    ranked = Take[
        Reverse @ SortBy[
            Thread[vertices -> BetweennessCentrality[graph]],
            Last
        ],
        UpTo[count]
    ];
    Dataset @ Map[
        <|
            "Protein" -> intraActomeVertexID[First[#]],
            "BetweennessCentrality" -> Last[#]
        |> &,
        ranked
    ]
];

presentSeeds = Intersection[VertexList[intraActomeGraph], Protein /@ effectiveQueryIDs];
If[presentSeeds === {},
    Print @ Failure[
        "MissingQueryVertices",
        <|
            "MessageTemplate" -> "None of the requested query identifiers appeared in the returned network.",
            "QueryIDs" -> effectiveQueryIDs
        |>
    ];
    Abort[]
];

twoHopSubgraph = intraActomeNeighborhood[intraActomeGraph, presentSeeds, 2];
topCentrality = intraActomeTopCentrality[twoHopSubgraph];

intraActomeExplorer = Manipulate[
    Module[{subgraph, communities},
        subgraph = intraActomeNeighborhood[intraActomeGraph, presentSeeds, radius];
        communities = FindGraphCommunities[subgraph];
        Column[
            {
                intraActomeRender[subgraph, effectiveQueryIDs, layout],
                Grid[
                    {{
                        "Proteins", VertexCount[subgraph],
                        "Interactions", EdgeCount[subgraph],
                        "Communities", Length[communities]
                    }},
                    Dividers -> {False, {False, True}}
                ]
            },
            Spacings -> 1
        ]
    ],
    {{radius, 1, "Local hop radius"}, 1, 3, 1, Appearance -> "Labeled"},
    {{layout, "SpringElectricalEmbedding", "Graph layout"},
        {"SpringElectricalEmbedding", "SpectralEmbedding", "CircularEmbedding"}},
    TrackedSymbols :> {radius, layout},
    SaveDefinitions -> True
];

intraActomeExplorer
