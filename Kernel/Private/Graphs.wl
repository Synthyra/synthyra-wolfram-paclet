(* Native Graph and lossless hypergraph conversion. *)

Protein /: MakeBoxes[Protein[id_], form_] := InterpretationBox[
    RowBox[{"Protein", "[", ToBoxes[id, form], "]"}],
    Protein[id]
];

Ligand /: MakeBoxes[Ligand[id_], form_] := InterpretationBox[
    RowBox[{"Ligand", "[", ToBoxes[id, form], "]"}],
    Ligand[id]
];

graphFailure[tag_String, message_String, data_: <||>] := Failure[
    tag,
    Join[<|"MessageTemplate" -> message|>, data]
];

networkPayloadShapeQ[payload_Association] := Module[
    {nodes, edges, proteins, ligands, ppiEdges, pliEdges},
    nodes = associationLookup[payload, {"nodes", "vertices"}, Missing["NotAvailable"]];
    edges = associationLookup[payload, {"edges", "links"}, Missing["NotAvailable"]];
    If[! MissingQ[nodes] && ! MissingQ[edges], Return[True]];
    proteins = associationLookup[payload, {"proteins"}, Missing["NotAvailable"]];
    ligands = associationLookup[payload, {"ligands"}, Missing["NotAvailable"]];
    ppiEdges = associationLookup[payload, {"ppiEdges", "ppi_edges"}, Missing["NotAvailable"]];
    pliEdges = associationLookup[payload, {"pliEdges", "pli_edges"}, Missing["NotAvailable"]];
    And @@ (! MissingQ[#] & /@ {proteins, ligands, ppiEdges, pliEdges})
];

tmapTreePayloadQ[payload_Association] := Module[
    {focus, htmlFormat, metadata, layoutSource, edges},
    focus = associationLookup[payload, {"focus"}, Missing["NotAvailable"]];
    htmlFormat = canonicalName @ associationLookup[payload, {"html_format", "htmlFormat"}, ""];
    metadata = associationLookup[payload, {"metadata"}, <||>];
    layoutSource = associationLookup[metadata, {"layout_source", "layoutSource"}, Missing["NotAvailable"]];
    edges = associationLookup[payload, {"edges"}, {}];
    (AssociationQ[focus] && htmlFormat === "srcdoc") ||
        (StringQ[layoutSource] && ListQ[edges] && AnyTrue[
            edges,
            Function[edge,
                AssociationQ[edge] &&
                    ! MissingQ[associationLookup[edge, {"score"}, Missing["NotAvailable"]]] &&
                    ! MissingQ[associationLookup[edge, {"distance"}, Missing["NotAvailable"]]]
            ]
        ])
];

unwrapNetworkPayload[payload_Association, depth_: 0] := Module[
    {candidate, resolved, wrapperNames},
    If[networkPayloadShapeQ[payload], Return[payload]];
    If[depth >= 8, Return[Missing["NetworkPayloadNotFound"]]];
    wrapperNames = {
        "network", "graph", "result", "result_network", "resultNetwork",
        "actome_edges", "actomeEdges", "result_actome_edges", "resultActomeEdges"
    };
    resolved = Catch[
        Do[
            candidate = associationLookup[payload, {name}, Missing["NotAvailable"]];
            If[AssociationQ[candidate],
                resolved = unwrapNetworkPayload[candidate, depth + 1];
                If[AssociationQ[resolved] && networkPayloadShapeQ[resolved], Throw[resolved]]
            ],
            {name, wrapperNames}
        ]
    ];
    If[
        AssociationQ[resolved] && networkPayloadShapeQ[resolved],
        resolved,
        Missing["NetworkPayloadNotFound"]
    ]
];

expandTypedCompactPayload[network_Association] := Module[
    {proteins, ligands, ppiEdges, pliEdges, nodes, edges, proteinCount},
    proteins = associationLookup[network, {"proteins"}, Missing["NotAvailable"]];
    ligands = associationLookup[network, {"ligands"}, Missing["NotAvailable"]];
    ppiEdges = associationLookup[network, {"ppiEdges", "ppi_edges"}, Missing["NotAvailable"]];
    pliEdges = associationLookup[network, {"pliEdges", "pli_edges"}, Missing["NotAvailable"]];
    If[! ListQ[proteins] || ! ListQ[ligands] || ! ListQ[ppiEdges] || ! ListQ[pliEdges], Return[network]];
    proteinCount = Length[proteins];
    nodes = Join[
        (<|"id" -> ToString[#], "node_type" -> "protein", "__original" -> #|> &) /@ proteins,
        (<|"id" -> ToString[#], "node_type" -> "ligand", "__original" -> #|> &) /@ ligands
    ];
    edges = Join[
        Map[
            Function[edge,
                If[AssociationQ[edge],
                    <|
                        "i" -> associationLookup[edge, {"sourceIndex"}],
                        "j" -> associationLookup[edge, {"targetIndex"}],
                        "confidence" -> associationLookup[edge, {"confidence"}],
                        "interaction_type" -> "ppi",
                        "__original" -> edge
                    |>,
                    edge
                ]
            ],
            ppiEdges
        ],
        Map[
            Function[edge,
                If[AssociationQ[edge],
                    <|
                        "i" -> associationLookup[edge, {"sourceIndex"}],
                        "j" -> (proteinCount + associationLookup[edge, {"targetIndex"}]),
                        "confidence" -> associationLookup[edge, {"confidence"}],
                        "interaction_type" -> "pli",
                        "__original" -> edge
                    |>,
                    edge
                ]
            ],
            pliEdges
        ]
    ];
    <|"nodes" -> nodes, "edges" -> edges, "metadata" -> associationLookup[network, {"metadata"}, <||>]|>
];

normalizedNodeList[value_] := Which[
    ListQ[value], value,
    AssociationQ[value], KeyValueMap[
        If[AssociationQ[#2], Join[<|"id" -> ToString[#1]|>, #2], <|"id" -> ToString[#1], "value" -> #2|>] &,
        value
    ],
    True, {}
];

nodeIdentifier[node_Association] := Module[{value},
    value = associationLookup[
        node,
        {"id", "protein_id", "proteinId", "ligand_id", "ligandId", "uid", "uniprot_id", "uniprotId"},
        Missing["NotAvailable"]
    ];
    Which[
        StringQ[value] && StringTrim[value] =!= "", StringTrim[value],
        IntegerQ[value], ToString[value, InputForm],
        True, ""
    ]
];
nodeIdentifier[node_] := Which[
    StringQ[node] && StringTrim[node] =!= "", StringTrim[node],
    IntegerQ[node], ToString[node, InputForm],
    True, ""
];

nodeType[node_Association] := Module[{declared},
    declared = canonicalName @ associationLookup[node, {"node_type", "nodeType", "type", "kind"}, ""];
    Which[
        MemberQ[{"ligand", "compound", "drug", "smallmolecule"}, declared], "Ligand",
        MemberQ[{"protein", "peptide"}, declared], "Protein",
        ! MissingQ[associationLookup[node, {"smiles", "selfies"}]], "Ligand",
        True, "Protein"
    ]
];
nodeType[_] := "Protein";

vertexForNode[node_] := Module[{identifier = nodeIdentifier[node]},
    If[nodeType[node] === "Ligand", Ligand[identifier], Protein[identifier]]
];

annotationRulesForNode[node_Association, includeOriginal_] := Module[{rules},
    rules = {
        "Name" -> associationLookup[node, {"name", "label"}, Missing["NotAvailable"]],
        "Organism" -> associationLookup[node, {"organism"}, Missing["NotAvailable"]],
        "SequenceLength" -> associationLookup[node, {"sequence_length", "sequenceLength", "sl"}, Missing["NotAvailable"]],
        "UniProtID" -> associationLookup[node, {"uniprot_id", "uniprotId", "uid"}, Missing["NotAvailable"]],
        "SMILES" -> associationLookup[node, {"smiles"}, Missing["NotAvailable"]],
        "NodeType" -> nodeType[node]
    };
    If[TrueQ[includeOriginal], AppendTo[rules, "OriginalPayload" -> associationLookup[node, {"__original"}, node]]];
    Select[rules, ! MissingQ[Last[#]] &]
];

annotateExpression[expression_, rules_List] := Fold[Annotation[#1, #2] &, expression, rules];

networkInteractionType[network_Association, nodes_List] := Module[{declared},
    declared = canonicalName @ associationLookup[
        associationLookup[network, {"metadata"}, <||>],
        {"interaction_type", "interactionType", "task_type", "taskType"},
        associationLookup[network, {"interaction_type", "interactionType", "task_type", "taskType"}, ""]
    ];
    Which[
        MemberQ[{"pli", "proteinligand", "drugscreen"}, declared], "pli",
        declared =!= "", declared,
        AnyTrue[nodes, nodeType[#] === "Ligand" &], "pli",
        True, "ppi"
    ]
];

edgeInteractionType[edge_Association, default_String] := Module[{declared},
    declared = canonicalName @ associationLookup[edge, {"interaction_type", "interactionType", "type"}, default];
    If[declared === "", default, declared]
];

edgeEvidence[edge_Association, names_List, default_: Missing["NotAvailable"]] :=
    associationLookup[edge, names, default];

edgeConfidence[edge_Association] := edgeEvidence[edge, {"confidence", "score", "s"}, Missing["NotAvailable"]];

edgeNovelty[edge_Association] := Module[{explicit, string, biogrid, biogridMV, compact},
    explicit = edgeEvidence[edge, {"is_novel", "isNovel", "novel", "novelty"}, Missing["NotAvailable"]];
    If[! MissingQ[explicit], Return[explicit]];
    compact = ! MissingQ[edgeEvidence[edge, {"i"}]] && ! MissingQ[edgeEvidence[edge, {"j"}]];
    If[! compact, Return[Missing["NotAvailable"]]];
    string = edgeEvidence[edge, {"in_string", "inString", "st"}, Missing["NotAvailable"]];
    biogrid = edgeEvidence[edge, {"in_biogrid", "inBiogrid", "bg"}, Missing["NotAvailable"]];
    biogridMV = edgeEvidence[edge, {"in_biogrid_mv", "inBiogridMv", "bgmv"}, Missing["NotAvailable"]];
    If[AllTrue[{string, biogrid, biogridMV}, MissingQ], Return[Missing["NotAvailable"]]];
    ! AnyTrue[{string, biogrid, biogridMV}, TrueQ]
];

nestedEvidenceValue[edge_Association, directNames_List, nestedNames_List] := Module[{direct, evidence},
    direct = edgeEvidence[edge, directNames, Missing["NotAvailable"]];
    If[! MissingQ[direct], Return[direct]];
    evidence = edgeEvidence[edge, {"evidence"}, <||>];
    If[AssociationQ[evidence], associationLookup[evidence, nestedNames, Missing["NotAvailable"]], Missing["NotAvailable"]]
];

explicitDirectionality[edge_Association, network_Association, option_] := Module[{edgeValue, networkValue},
    If[TrueQ[option] || option === False, Return[TrueQ[option]]];
    edgeValue = edgeEvidence[edge, {"directed", "is_directed", "isDirected"}, Missing["NotAvailable"]];
    If[! MissingQ[edgeValue], Return[TrueQ[edgeValue]]];
    networkValue = associationLookup[
        associationLookup[network, {"metadata"}, <||>],
        {"directed", "is_directed", "isDirected"},
        associationLookup[network, {"directed", "is_directed", "isDirected"}, False]
    ];
    TrueQ[networkValue]
];

confidenceCost[confidence_, transform_] := Module[{candidate},
    If[transform === None || MissingQ[confidence] || ! NumericQ[confidence],
        Return[Missing["NotAvailable"]]
    ];
    candidate = Which[
        transform === Automatic,
            If[0 <= N[confidence] <= 1., 1. - N[confidence], 1. - Clip[N[confidence]/100., {0., 1.}]],
        Head[transform] === Function || MatchQ[transform, _Symbol], Quiet[Check[transform[confidence], Missing["NotAvailable"]]],
        True, Missing["NotAvailable"]
    ];
    If[NumericQ[candidate], candidate, Missing["NotAvailable"]]
];

edgeAnnotationRules[edge_Association, interactionType_String, transform_, includeOriginal_] := Module[
    {confidence, novelty, rules, cost},
    confidence = edgeConfidence[edge];
    novelty = edgeNovelty[edge];
    rules = {
        "Confidence" -> confidence,
        "Novel" -> novelty,
        "STRING" -> nestedEvidenceValue[edge, {"in_string", "inString", "st"}, {"stringScore", "string", "inString"}],
        "BioGRID" -> nestedEvidenceValue[edge, {"in_biogrid", "inBiogrid", "bg"}, {"biogrid", "inBiogrid"}],
        "BioGRIDMV" -> nestedEvidenceValue[edge, {"in_biogrid_mv", "inBiogridMv", "bgmv"}, {"biogridMv", "inBiogridMv"}],
        "InteractionType" -> interactionType,
        "Distance" -> edgeEvidence[edge, {"distance"}, Missing["NotAvailable"]]
    };
    cost = confidenceCost[confidence, transform];
    If[! MissingQ[cost], AppendTo[rules, EdgeWeight -> cost]];
    If[TrueQ[includeOriginal], AppendTo[rules, "OriginalPayload" -> associationLookup[edge, {"__original"}, edge]]];
    Select[rules, ! MissingQ[Last[#]] &]
];

resolveNamedEndpoint[id_, role_String, interactionType_String, wrappersByID_Association] := Module[
    {identifier = ToString[id], candidates, preferred},
    candidates = Lookup[wrappersByID, ToString[id], {}];
    If[Length[candidates] === 1, Return[First[candidates]]];
    preferred = If[interactionType === "pli" && role === "Target", Ligand[identifier], Protein[identifier]];
    If[MemberQ[candidates, preferred] || candidates === {}, preferred, First[candidates]]
];

namedEndpointIdentifier[value_] := Which[
    StringQ[value] && StringTrim[value] =!= "", StringTrim[value],
    IntegerQ[value], ToString[value, InputForm],
    True, Missing["InvalidEndpoint"]
];

Options[SynthyraGraph] = {
    ConfidenceCostFunction -> None,
    DirectedEdges -> Automatic,
    IncludeOriginalPayload -> True,
    VertexLabels -> Automatic
};

SynthyraGraph[payload_Association, OptionsPattern[]] := Module[
    {
        network, nodes, edges, nodeWrappers, wrappersByID, payloadByVertex,
        defaultInteractionType, resolvedVertices, edgeExpressions, failureTag,
        graph, graphMetadata, includeOriginal, transform, directionOption
    },
    network = unwrapNetworkPayload[payload];
    If[MissingQ[network],
        Return @ graphFailure[
            "MissingNetworkPayload",
            "The payload does not contain a recognized Synthyra network result."
        ]
    ];
    If[tmapTreePayloadQ[network],
        Return @ graphFailure[
            "TmapTreeIsNotInteractionNetwork",
            "TMAP edges are layout spanning-tree links, not biological interaction edges. Read result_actome_edges for interaction-network analysis."
        ]
    ];
    network = expandTypedCompactPayload[network];
    nodes = normalizedNodeList @ associationLookup[network, {"nodes", "vertices"}, {}];
    edges = associationLookup[network, {"edges", "links"}, {}];
    If[! ListQ[edges],
        Return @ graphFailure[
            "InvalidNetworkEdges",
            "The network edges member must be a list."
        ]
    ];
    If[! AllTrue[nodes, AssociationQ] || ! AllTrue[edges, AssociationQ],
        Return @ graphFailure[
            "InvalidNetworkPayload",
            "Network nodes and edges must be Associations."
        ]
    ];
    If[AnyTrue[nodes, nodeIdentifier[#] === "" &],
        Return @ graphFailure[
            "InvalidNetworkNodeIdentifier",
            "Every network node must declare a nonblank string or integer identifier."
        ]
    ];

    includeOriginal = TrueQ[OptionValue[IncludeOriginalPayload]];
    transform = OptionValue[ConfidenceCostFunction];
    directionOption = OptionValue[DirectedEdges];
    nodeWrappers = vertexForNode /@ nodes;
    wrappersByID = GroupBy[Transpose[{nodeIdentifier /@ nodes, nodeWrappers}], First -> Last];
    payloadByVertex = AssociationThread[nodeWrappers, nodes];
    resolvedVertices = nodeWrappers;
    defaultInteractionType = networkInteractionType[network, nodes];
    failureTag = Unique["SynthyraGraphFailure"];

    edgeExpressions = Catch[
        Map[
            Function[edge,
                Module[{interactionType, source, target, sourceIdentifier, targetIdentifier, i, j, directed, baseEdge, rules},
                    interactionType = edgeInteractionType[edge, defaultInteractionType];
                    If[
                        ! MissingQ[edgeEvidence[edge, {"i"}]] && ! MissingQ[edgeEvidence[edge, {"j"}]],
                        i = edgeEvidence[edge, {"i"}];
                        j = edgeEvidence[edge, {"j"}];
                        If[
                            ! IntegerQ[i] || ! IntegerQ[j] || i < 0 || j < 0 ||
                                i >= Length[nodeWrappers] || j >= Length[nodeWrappers],
                            Throw[
                                graphFailure[
                                    "CompactEdgeIndexOutOfRange",
                                    "Compact network edge indices must be zero-based indices into the nodes list.",
                                    <|"Edge" -> edge, "NodeCount" -> Length[nodeWrappers]|>
                                ],
                                failureTag
                            ]
                        ];
                        source = nodeWrappers[[i + 1]];
                        target = nodeWrappers[[j + 1]],
                        sourceIdentifier = namedEndpointIdentifier @ edgeEvidence[
                            edge,
                            {"source", "from"},
                            Missing["NotAvailable"]
                        ];
                        If[MissingQ[sourceIdentifier],
                            Throw[
                                graphFailure[
                                    "InvalidNamedEdgeEndpoint",
                                    "A verbose network edge must declare nonblank source and target identifiers.",
                                    <|"Endpoint" -> "Source"|>
                                ],
                                failureTag
                            ]
                        ];
                        targetIdentifier = namedEndpointIdentifier @ edgeEvidence[
                            edge,
                            {"target", "to"},
                            Missing["NotAvailable"]
                        ];
                        If[MissingQ[targetIdentifier],
                            Throw[
                                graphFailure[
                                    "InvalidNamedEdgeEndpoint",
                                    "A verbose network edge must declare nonblank source and target identifiers.",
                                    <|"Endpoint" -> "Target"|>
                                ],
                                failureTag
                            ]
                        ];
                        source = resolveNamedEndpoint[
                            sourceIdentifier,
                            "Source",
                            interactionType,
                            wrappersByID
                        ];
                        target = resolveNamedEndpoint[
                            targetIdentifier,
                            "Target",
                            interactionType,
                            wrappersByID
                        ];
                        If[! MemberQ[resolvedVertices, source],
                            AppendTo[resolvedVertices, source];
                            AssociateTo[payloadByVertex, source -> <|"id" -> First[source], "node_type" -> "protein"|>]
                        ];
                        If[! MemberQ[resolvedVertices, target],
                            AppendTo[resolvedVertices, target];
                            AssociateTo[payloadByVertex, target -> <|"id" -> First[target], "node_type" -> If[Head[target] === Ligand, "ligand", "protein"]|>]
                        ]
                    ];
                    directed = explicitDirectionality[edge, network, directionOption];
                    baseEdge = If[directed, DirectedEdge[source, target], UndirectedEdge[source, target]];
                    rules = edgeAnnotationRules[edge, interactionType, transform, includeOriginal];
                    annotateExpression[baseEdge, rules]
                ]
            ],
            edges
        ],
        failureTag
    ];
    If[FailureQ[edgeExpressions], Return[edgeExpressions]];

    graph = Graph[
        Map[
            Function[vertex,
                annotateExpression[
                    vertex,
                    annotationRulesForNode[Lookup[payloadByVertex, vertex, <|"id" -> First[vertex]|>], includeOriginal]
                ]
            ],
            DeleteDuplicates[resolvedVertices]
        ],
        edgeExpressions,
        VertexLabels -> OptionValue[VertexLabels]
    ];
    graphMetadata = associationLookup[network, {"metadata"}, <||>];
    graph = Annotate[graph, "SynthyraMetadata" -> graphMetadata];
    If[includeOriginal, graph = Annotate[graph, "OriginalPayload" -> payload]];
    graph
];

SynthyraGraph[other_, OptionsPattern[]] := graphFailure[
    "InvalidNetworkPayload",
    "SynthyraGraph expects an Association payload.",
    <|"InputHead" -> Head[other]|>
];

hypergraphMemberVertex[member_Association] := vertexForNode[member];
hypergraphMemberVertex[Protein[id_]] := Protein[id];
hypergraphMemberVertex[Ligand[id_]] := Ligand[id];
hypergraphMemberVertex[member_] := Protein[ToString[member]];

hyperedgeMembers[edge_Association] := associationLookup[edge, {"members", "vertices", "nodes"}, Missing["NotAvailable"]];
hyperedgeMembers[edge_List] := edge;
hyperedgeMembers[_] := Missing["NotAvailable"];

hyperedgeAnnotations[edge_Association] := Association @ Select[
    Normal[edge],
    ! MemberQ[{"members", "vertices", "nodes"}, canonicalName[First[#]]] &
];
hyperedgeAnnotations[_] := <||>;

SynthyraHypergraphData[payload_Association] := Module[
    {vertexPayloads, declaredVertices, rawEdges, normalizedEdges, memberLists, allVertices},
    vertexPayloads = normalizedNodeList @ associationLookup[payload, {"vertices", "nodes"}, {}];
    declaredVertices = vertexForNode /@ vertexPayloads;
    rawEdges = associationLookup[payload, {"hyperedges", "hyper_edges"}, Missing["NotAvailable"]];
    If[! ListQ[rawEdges],
        Return @ graphFailure[
            "InvalidHypergraphPayload",
            "The hypergraph payload must contain a hyperedges list."
        ]
    ];
    If[AnyTrue[rawEdges, MissingQ[hyperedgeMembers[#]] || ! ListQ[hyperedgeMembers[#]] &],
        Return @ graphFailure[
            "InvalidHyperedge",
            "Every hyperedge must provide a member list."
        ]
    ];
    normalizedEdges = Map[
        Function[edge,
            <|
                "Members" -> (hypergraphMemberVertex /@ hyperedgeMembers[edge]),
                "Annotations" -> hyperedgeAnnotations[edge],
                "OriginalPayload" -> edge
            |>
        ],
        rawEdges
    ];
    memberLists = Lookup[normalizedEdges, "Members", {}];
    allVertices = DeleteDuplicates @ Join[declaredVertices, Flatten[memberLists, 1]];
    <|
        "Vertices" -> allVertices,
        "Hyperedges" -> normalizedEdges,
        "MemberLists" -> memberLists,
        "Metadata" -> associationLookup[payload, {"metadata"}, <||>],
        "OriginalPayload" -> payload
    |>
];

SynthyraHypergraphData[other_] := graphFailure[
    "InvalidHypergraphPayload",
    "SynthyraHypergraphData expects an Association payload.",
    <|"InputHead" -> Head[other]|>
];
