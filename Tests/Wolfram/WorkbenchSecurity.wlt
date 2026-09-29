Needs["Synthyra`SynthyraLink`"];

VerificationTest[
    Module[{analysis, normal, original, signedURLs},
        signedURLs = {
            "https://storage.invalid/output?signature=one",
            "https://storage.invalid/actome?signature=two",
            "https://storage.invalid/download?signature=three"
        };
        analysis = SynthyraAnalysis[<|
            "dfa" -> <|"protein_id" -> "P1", "sequence" -> "AAAA"|>,
            "output_url" -> signedURLs[[1]],
            "result_actome_edges_url" -> signedURLs[[2]],
            "nested" -> <|"download_url" -> signedURLs[[3]], "public_url" -> "https://example.org/public"|>
        |>];
        normal = Normal[analysis];
        original = normal["OriginalResult"];
        {
            And @@ (FreeQ[normal, #] & /@ signedURLs),
            MissingQ[original["output_url"]],
            MissingQ[original["result_actome_edges_url"]],
            MissingQ[original["nested"]["download_url"]],
            original["nested"]["public_url"]
        }
    ],
    {True, True, True, True, "https://example.org/public"},
    TestID -> "workbench-redacts-signed-result-and-download-urls@@Tests/Wolfram/WorkbenchSecurity.wlt:3,1-28,2"
];

VerificationTest[
    Module[{bad, good},
        Global`workbenchPdbParserProbe = 0;
        bad = Synthyra`SynthyraLink`Private`workbenchParsePdbNumber[
            "Global`workbenchPdbParserProbe=1"
        ];
        good = Synthyra`SynthyraLink`Private`workbenchParsePdbNumber[" -12.345 "];
        {Global`workbenchPdbParserProbe, MissingQ[bad], good}
    ],
    {0, True, -12.345},
    SameTest -> (First[#1] === First[#2] && #1[[2]] === #2[[2]] && Abs[#1[[3]] - #2[[3]]] < 10^-12 &),
    TestID -> "pdb-numeric-parser-never-evaluates-input@@Tests/Wolfram/WorkbenchSecurity.wlt:30,1-42,2"
];

VerificationTest[
    Module[{analysis},
        analysis = SynthyraAnalysis[<|
            "tmap_tree" -> <|
                "nodes" -> {<|"id" -> "Q", "x" -> 0., "y" -> 0.|>},
                "edges" -> {},
                "focus" -> <|"query_ids" -> {"Q"}|>
            |>
        |>];
        analysis["QueryIDs"]
    ],
    {"Q"},
    TestID -> "tmap-focus-query-ids-normalize-without-actome@@Tests/Wolfram/WorkbenchSecurity.wlt:44,1-57,2"
];

VerificationTest[
    Module[{graph, subset},
        graph = Graph[
            Range[100],
            UndirectedEdge @@@ Partition[Range[100], 2, 1]
        ];
        subset = Synthyra`SynthyraLink`Private`workbenchGraphSubset[graph, {}, 0., 2, 7];
        {VertexCount[subset], VertexCount @ Synthyra`SynthyraLink`Private`workbenchGraphSubset[graph, {}, 0., 2, All]}
    ],
    {7, 100},
    TestID -> "queryless-network-still-honors-display-cap@@Tests/Wolfram/WorkbenchSecurity.wlt:59,1-70,2"
];

VerificationTest[
    Module[{prepared},
        prepared = Synthyra`SynthyraLink`Private`workbenchPrepareSubnetworkHeatmap[<|
            "ids" -> {"A", "B", "Q"},
            "scores" -> {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}},
            "cluster_order" -> {2, 0, 1},
            "query_indices" -> {2}
        |>];
        {
            prepared["IDs"],
            prepared["Scores"],
            prepared["QueryPositions"],
            prepared["ClusterOrderApplied"]
        }
    ],
    {
        {"Q", "A", "B"},
        {{9, 7, 8}, {3, 1, 2}, {6, 4, 5}},
        {1},
        True
    },
    TestID -> "workbench-applies-cluster-order-and-remaps-query-indices@@Tests/Wolfram/WorkbenchSecurity.wlt:72,1-94,2"
];

VerificationTest[
    Module[{prepared},
        prepared = Synthyra`SynthyraLink`Private`workbenchPrepareSubnetworkHeatmap[<|
            "ids" -> {"A", "B", "Q"},
            "scores" -> {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}},
            "cluster_order" -> {0, 0, 2},
            "query_indices" -> {2}
        |>];
        {prepared["IDs"], prepared["QueryPositions"], prepared["ClusterOrderApplied"]}
    ],
    {{"A", "B", "Q"}, {3}, False},
    TestID -> "invalid-cluster-order-is-ignored-without-losing-query-marker@@Tests/Wolfram/WorkbenchSecurity.wlt:96,1-108,2"
];

VerificationTest[
    Module[{record, columns, headlessSelection, interactiveSelector},
        record = <|
            "Oracle" -> "Subcellular",
            "label_names" -> {"Nucleus", "Cytoplasm"},
            "attributions" -> {{.1, .9}, {.2, .8}, {.3, .7}}
        |>;
        columns = Synthyra`SynthyraLink`Private`workbenchAttributionColumns[record];
        headlessSelection = Block[
            {$Notebooks = False},
            Synthyra`SynthyraLink`Private`workbenchAttributionSelector[
                columns,
                Function[column, {column["Label"], column["Values"]}]
            ]
        ];
        interactiveSelector = Block[
            {$Notebooks = True},
            Synthyra`SynthyraLink`Private`workbenchAttributionSelector[
                columns,
                Function[column, column["Label"]]
            ]
        ];
        {
            Lookup[columns, "Label"],
            Lookup[columns, "Values"],
            headlessSelection,
            Synthyra`SynthyraLink`Private`workbenchAttributionColumnIndices[
                columns,
                "Subcellular",
                "[2/2]"
            ],
            Head[interactiveSelector]
        }
    ],
    {
        {"Nucleus", "Cytoplasm"},
        {{.1, .2, .3}, {.9, .8, .7}},
        {"Nucleus", {.1, .2, .3}},
        {2},
        DynamicModule
    },
    TestID -> "all-attribution-columns-are-labeled-and-headless-selection-is-deterministic@@Tests/Wolfram/WorkbenchSecurity.wlt:110,1-152,2"
];

VerificationTest[
    Module[{normalized, constant, colors, expectedColors, components},
        normalized = Quiet @ Synthyra`SynthyraLink`Private`workbenchNormalizeAttributions[
            {-2., 0., 2., Indeterminate, Infinity}
        ];
        constant = Synthyra`SynthyraLink`Private`workbenchNormalizeAttributions[{5., 5.}];
        colors = Synthyra`SynthyraLink`Private`workbenchAttributionColor /@ {0., .35, .65, 1.};
        expectedColors = {
            RGBColor[1., 125./255., 69./255.],
            RGBColor[1., 219./255., 19./255.],
            RGBColor[101./255., 203./255., 243./255.],
            RGBColor[0., 83./255., 214./255.]
        };
        components[color_] := N @ List @@ color;
        Max[Abs[Take[normalized, 3] - {0., .5, 1.}]] < 10^-12 &&
            AllTrue[Drop[normalized, 3], MissingQ] &&
            Max[Abs[constant - {0., 0.}]] < 10^-12 &&
            Max[Abs[Flatten[components /@ colors] - Flatten[components /@ expectedColors]]] < 10^-12
    ],
    True,
    TestID -> "attribution-normalization-and-color-stops-match-frontend@@Tests/Wolfram/WorkbenchSecurity.wlt:154,1-175,2"
];

VerificationTest[
    Module[{pdb, plot, graphic, colors, orange, blue, colorComponents, closeColorQ},
        pdb = StringRiffle[{
            "ATOM      1  CA  ALA A   1      10.000  11.000  12.000  1.00 80.00           C",
            "ATOM      2  CA  GLY A   2      11.000  12.000  13.000  1.00 85.00           C",
            "ATOM      3  CA  SER A   3      12.000  13.000  14.000  1.00 90.00           C"
        }, "\n"];
        plot = Synthyra`SynthyraLink`Private`workbenchAttributionStructurePlot[
            pdb,
            {-1., 0., 1.},
            "Subcellular | Nucleus"
        ];
        If[FailureQ[plot], Return[False]];
        graphic = First[plot];
        colors = Cases[graphic, _RGBColor, Infinity];
        orange = Synthyra`SynthyraLink`Private`workbenchAttributionColor[0.];
        blue = Synthyra`SynthyraLink`Private`workbenchAttributionColor[1.];
        colorComponents[color_] := N @ List @@ color;
        closeColorQ[left_, right_] := Max[Abs[colorComponents[left] - colorComponents[right]]] < 10^-12;
        Count[graphic, _Tube, Infinity] === 4 &&
            AnyTrue[colors, closeColorQ[#, orange] &] &&
            AnyTrue[colors, closeColorQ[#, blue] &]
    ],
    True,
    TestID -> "oracle-attribution-colors-alpha-carbon-tube@@Tests/Wolfram/WorkbenchSecurity.wlt:177,1-202,2"
];

VerificationTest[
    Module[{record, aligned, raw, mismatched},
        record = <|
            "Oracle" -> "Boundary tokens",
            "label_names" -> {"A", "B"},
            "attributions" -> {
                {100., 200.},
                {1., 10.},
                {2., 20.},
                {3., 30.},
                {900., 999.}
            }
        |>;
        aligned = Synthyra`SynthyraLink`Private`workbenchAttributionColumns[record, 3];
        raw = Synthyra`SynthyraLink`Private`workbenchAttributionColumns[record];
        mismatched = Synthyra`SynthyraLink`Private`workbenchAttributionColumns[record, 4];
        {
            Lookup[aligned, "Values"],
            Lookup[aligned, "SourcePositionCount"],
            Lookup[aligned, "SpecialTokensRemoved"],
            Lookup[raw, "Values"],
            Lookup[mismatched, "Values"]
        }
    ],
    {
        {{1., 2., 3.}, {10., 20., 30.}},
        {5, 5},
        {2, 2},
        {{100., 1., 2., 3., 900.}, {200., 10., 20., 30., 999.}},
        {{100., 1., 2., 3., 900.}, {200., 10., 20., 30., 999.}}
    },
    TestID -> "attributions-drop-cls-and-eos-only-for-proven-sequence-shape"
];

VerificationTest[
    Module[{analysis, view, labels},
        analysis = SynthyraAnalysis[<|
            "dfa" -> <|
                "protein_id" -> "P1",
                "sequence" -> "AAAA",
                "oracle_predictions" -> {
                    <|"oracle_name" -> "test", "score" -> .5, "attributions" -> {{0.}, {1.}, {2.}, {3.}, {4.}, {5.}}|>
                }
            |>
        |>];
        view = Block[{$Notebooks = True}, SynthyraWorkbench[analysis]];
        labels = Cases[view, Rule[label_String, _DynamicModule] :> label, Infinity];
        {
            Head[view],
            ContainsAll[
                labels,
                {"Summary", "Network", "Structure", "Oracles", "CAMP", "Translator", "Enrichment", "Matrices", "TMAP Layout", "Sequence", "Raw"}
            ],
            FreeQ[view, _Graph | _Graphics3D | _Dataset],
            ByteCount[view] < 100000
        }
    ],
    {DynamicModule, True, True, True},
    TestID -> "interactive-workbench-defers-all-heavy-tab-builders"
];

VerificationTest[
    Module[{analysis, graph, queryVertices, neighborhood, vertices, edges},
        analysis = SynthyraAnalysis[<|
            "network" -> <|
                "network" -> <|
                    "nodes" -> {
                        <|"id" -> "COLLIDE", "node_type" -> "ligand", "smiles" -> "CCO"|>,
                        <|"id" -> "COLLIDE", "node_type" -> "protein"|>,
                        <|"id" -> "P2", "node_type" -> "protein"|>
                    },
                    "edges" -> {
                        <|"i" -> 0, "j" -> 2, "s" -> 92, "interaction_type" -> "pli"|>
                    },
                    "metadata" -> <|"task_type" -> "pli", "query_ids" -> {"COLLIDE"}|>
                |>,
                "actome_edges" -> <|
                    "nodes" -> {
                        <|"id" -> "COLLIDE", "node_type" -> "protein"|>,
                        <|"id" -> "P2", "node_type" -> "protein"|>,
                        <|"id" -> "P3", "node_type" -> "protein"|>
                    },
                    "edges" -> {
                        <|"i" -> 0, "j" -> 1, "s" -> 95, "st" -> True|>,
                        <|"i" -> 1, "j" -> 2, "s" -> 88, "st" -> True|>
                    },
                    "query_ids" -> {"P2"},
                    "node_tiers" -> <|"P2" -> 0, "COLLIDE" -> 1, "P3" -> 1|>
                |>
            |>
        |>];
        graph = Synthyra`SynthyraLink`Private`workbenchNetworkGraph[analysis];
        queryVertices = Synthyra`SynthyraLink`Private`workbenchNetworkQueryVertices[analysis, graph];
        neighborhood = Synthyra`SynthyraLink`Private`workbenchGraphSubset[
            graph,
            queryVertices,
            0.,
            1,
            All
        ];
        vertices = VertexList[graph];
        edges = EdgeList[graph];
        {
            analysis["TaskType"],
            analysis["QueryIDs"],
            MemberQ[vertices, Protein["COLLIDE"]],
            MemberQ[vertices, Ligand["COLLIDE"]],
            MemberQ[edges, UndirectedEdge[Ligand["COLLIDE"], Protein["P2"]]],
            MemberQ[edges, UndirectedEdge[Protein["COLLIDE"], Protein["P2"]]],
            queryVertices,
            ContainsAll[VertexList[neighborhood], {Ligand["COLLIDE"], Protein["P2"]}],
            AnnotationValue[
                {graph, UndirectedEdge[Ligand["COLLIDE"], Protein["P2"]]},
                "InteractionType"
            ]
        }
    ],
    {"pli", {"COLLIDE"}, True, True, True, True, {Ligand["COLLIDE"]}, True, "pli"},
    TestID -> "workbench-merges-pli-edges-and-seeds-colliding-ligand-query@@Tests/Wolfram/WorkbenchSecurity.wlt"
];

VerificationTest[
    Module[{analysis, graph, queryVertices},
        analysis = SynthyraAnalysis[<|
            "network" -> <|
                "network" -> <|
                    "nodes" -> {
                        <|"id" -> "P1", "node_type" -> "protein"|>,
                        <|"id" -> "P3", "node_type" -> "protein"|>
                    },
                    "edges" -> {
                        <|"source" -> "P1", "target" -> "P3", "confidence" -> .99|>
                    },
                    "metadata" -> <|"task_type" -> "ppi", "query_ids" -> {"P1"}|>
                |>,
                "actome_edges" -> <|
                    "nodes" -> {
                        <|"id" -> "P1", "node_type" -> "protein"|>,
                        <|"id" -> "P2", "node_type" -> "protein"|>
                    },
                    "edges" -> {
                        <|"i" -> 0, "j" -> 1, "s" -> 90, "st" -> True|>
                    },
                    "query_ids" -> {"P1"},
                    "node_tiers" -> <|"P1" -> 0, "P2" -> 1|>
                |>
            |>
        |>];
        graph = Synthyra`SynthyraLink`Private`workbenchNetworkGraph[analysis];
        queryVertices = Synthyra`SynthyraLink`Private`workbenchNetworkQueryVertices[analysis, graph];
        {
            analysis["TaskType"],
            SortBy[VertexList[graph], ToString],
            EdgeList[graph],
            queryVertices
        }
    ],
    {
        "ppi",
        {Protein["P1"], Protein["P2"]},
        {UndirectedEdge[Protein["P1"], Protein["P2"]]},
        {Protein["P1"]}
    },
    TestID -> "workbench-preserves-actome-first-ppi-network-behavior@@Tests/Wolfram/WorkbenchSecurity.wlt"
];
