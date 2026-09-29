(* Native coordinated-result normalization and exploratory workbench. *)

workbenchFailure[tag_String, message_String, data_: <||>] := Failure[
    tag,
    Join[<|"MessageTemplate" -> message|>, data]
];

$workbenchSensitiveNames = {
    "authorization", "apikey", "synthyraapikey", "credential", "credentials",
    "bearertoken", "accesstoken", "refreshtoken", "token", "secret", "clientsecret",
    "password"
};

workbenchSensitiveURLKeyQ[key_] := Module[{name = canonicalName[key]},
    StringEndsQ[name, "url"] && AnyTrue[
        {"output", "result", "download", "signed", "artifact"},
        StringContainsQ[name, #] &
    ]
];

workbenchSensitiveKeyQ[key_] :=
    MemberQ[$workbenchSensitiveNames, canonicalName[key]] || workbenchSensitiveURLKeyQ[key];

workbenchRedact[value_] := Which[
    MatchQ[value, _SynthyraClientObject | _SynthyraJobObject], Missing["Redacted"],
    AssociationQ[value], Association @ KeyValueMap[
        Function[{key, member},
            key -> If[workbenchSensitiveKeyQ[key], Missing["Redacted"], workbenchRedact[member]]
        ],
        value
    ],
    ListQ[value], workbenchRedact /@ value,
    True, value
];

workbenchPresentQ[value_] := ! MissingQ[value] && value =!= Null && value =!= <||> && value =!= {};

workbenchFirstPresent[values_List, default_: Missing["NotAvailable"]] :=
    SelectFirst[values, workbenchPresentQ, default];

workbenchKnownResultQ[result_Association] := AnyTrue[
    Keys[result],
    MemberQ[
        {
            "dfa", "network", "enrichment", "actomeedges", "tmaptree", "envelope",
            "resultdfabaseline", "resultesmfold", "resultdfaoracles", "resultoracles",
            "resultdfacamp", "resultdfatranslator", "resultnetwork",
            "resultactomeedges", "resultenrichment", "resulttmaptree"
        },
        canonicalName[#]
    ] &
];

workbenchUnwrapResult[result_Association, depth_: 0] := Module[{nested},
    If[workbenchKnownResultQ[result] || depth >= 6, Return[result]];
    nested = workbenchFirstPresent[{
        associationLookup[result, {"result"}, Missing["NotAvailable"]],
        associationLookup[result, {"data"}, Missing["NotAvailable"]]
    }];
    If[AssociationQ[nested], workbenchUnwrapResult[nested, depth + 1], result]
];

workbenchNetworkShapeQ[value_] := AssociationQ[value] &&
    ListQ[associationLookup[value, {"nodes", "vertices"}, Missing["NotAvailable"]]] &&
    ListQ[associationLookup[value, {"edges", "links"}, Missing["NotAvailable"]]];

workbenchActomeQ[value_] := workbenchNetworkShapeQ[value] && AnyTrue[
    {
        associationLookup[value, {"query_ids", "queryIds"}, Missing["NotAvailable"]],
        associationLookup[value, {"node_tiers", "nodeTiers"}, Missing["NotAvailable"]],
        associationLookup[value, {"subnetwork_heatmap", "subnetworkHeatmap"}, Missing["NotAvailable"]]
    },
    workbenchPresentQ
];

workbenchMergeAssociations[values_List] := Module[{associations},
    associations = Select[values, AssociationQ];
    If[associations === {}, <||>, Apply[Join, associations]]
];

workbenchNormalizeOracles[value_] := Module[{nested},
    If[ListQ[value], Return[value]];
    If[AssociationQ[value],
        nested = associationLookup[value, {"oracle_predictions", "oraclePredictions", "predictions"}, Missing["NotAvailable"]];
        If[ListQ[nested], Return[nested]]
    ];
    value
];

workbenchNormalizeAnnotations[value_] := Module[{nested},
    Which[
        AssociationQ[value],
            nested = associationLookup[value, {"annotations", "results"}, Missing["NotAvailable"]];
            If[ListQ[nested], nested, {value}],
        ListQ[value] && Length[value] === 1 && AssociationQ[First[value]],
            nested = associationLookup[First[value], {"annotations", "results"}, Missing["NotAvailable"]];
            If[ListQ[nested], nested, value],
        ListQ[value], value,
        True, {}
    ]
];

workbenchNormalizeTaskType[value_] := If[
    StringQ[value],
    Switch[
        canonicalName[value],
        "pli" | "proteinligand" | "drugscreen", "pli",
        "ppi" | "proteinprotein", "ppi",
        _, Missing["NotAvailable"]
    ],
    Missing["NotAvailable"]
];

workbenchNormalizeResult[input_Association] := Module[
    {
        root, dfa, baseline, structure, networkSection, biologicalNetwork, actome,
        enrichment, envelope, tmap, heatmapPlots, oracles, camp, translator,
        sequence, proteinID, queryIDs, networkMetadata, taskType
    },
    root = workbenchUnwrapResult @ workbenchRedact[input];

    dfa = associationLookup[root, {"dfa"}, <||>];
    If[! AssociationQ[dfa], dfa = <||>];
    baseline = workbenchFirstPresent[{
        associationLookup[root, {"result_dfa_baseline", "resultDfaBaseline"}, Missing["NotAvailable"]],
        associationLookup[root, {"result_esmfold", "resultEsmfold", "esmfold"}, Missing["NotAvailable"]],
        associationLookup[dfa, {"baseline", "esmfold"}, Missing["NotAvailable"]]
    }, <||>];
    If[! AssociationQ[baseline], baseline = <||>];
    structure = workbenchMergeAssociations[{dfa, baseline}];

    oracles = workbenchNormalizeOracles @ workbenchFirstPresent[{
        associationLookup[dfa, {"oracle_predictions", "oraclePredictions", "oracles"}, Missing["NotAvailable"]],
        associationLookup[root, {"result_dfa_oracles", "resultDfaOracles"}, Missing["NotAvailable"]],
        associationLookup[root, {"result_oracles", "resultOracles"}, Missing["NotAvailable"]],
        associationLookup[root, {"oracle_predictions", "oraclePredictions"}, Missing["NotAvailable"]]
    }, {}];
    camp = workbenchNormalizeAnnotations @ workbenchFirstPresent[{
        associationLookup[dfa, {"camp_annotations", "campAnnotations"}, Missing["NotAvailable"]],
        associationLookup[root, {"result_dfa_camp", "resultDfaCamp", "result_camp"}, Missing["NotAvailable"]],
        associationLookup[root, {"camp_annotations", "campAnnotations"}, Missing["NotAvailable"]]
    }, {}];
    translator = workbenchNormalizeAnnotations @ workbenchFirstPresent[{
        associationLookup[dfa, {"translator_annotations", "translatorAnnotations"}, Missing["NotAvailable"]],
        associationLookup[root, {"result_dfa_translator", "resultDfaTranslator", "result_translator"}, Missing["NotAvailable"]],
        associationLookup[root, {"translator_annotations", "translatorAnnotations"}, Missing["NotAvailable"]]
    }, {}];

    networkSection = workbenchFirstPresent[{
        associationLookup[root, {"network"}, Missing["NotAvailable"]],
        associationLookup[root, {"result_network", "resultNetwork"}, Missing["NotAvailable"]]
    }, <||>];
    If[! AssociationQ[networkSection], networkSection = <||>];
    biologicalNetwork = workbenchFirstPresent[{
        associationLookup[networkSection, {"network", "result_network", "resultNetwork"}, Missing["NotAvailable"]],
        If[workbenchNetworkShapeQ[networkSection] && ! workbenchActomeQ[networkSection], networkSection, Missing["NotAvailable"]],
        associationLookup[root, {"result_network", "resultNetwork"}, Missing["NotAvailable"]],
        If[workbenchNetworkShapeQ[root] && ! workbenchActomeQ[root], root, Missing["NotAvailable"]]
    }];
    networkMetadata = If[
        AssociationQ[biologicalNetwork],
        associationLookup[biologicalNetwork, {"metadata"}, <||>],
        <||>
    ];
    actome = workbenchFirstPresent[{
        associationLookup[networkSection, {"actome_edges", "actomeEdges", "result_actome_edges", "resultActomeEdges"}, Missing["NotAvailable"]],
        associationLookup[root, {"actome_edges", "actomeEdges", "result_actome_edges", "resultActomeEdges"}, Missing["NotAvailable"]],
        If[workbenchActomeQ[networkSection], networkSection, Missing["NotAvailable"]],
        If[workbenchActomeQ[root], root, Missing["NotAvailable"]]
    }];
    envelope = workbenchFirstPresent[{
        associationLookup[networkSection, {"envelope", "result_envelope", "resultEnvelope"}, Missing["NotAvailable"]],
        associationLookup[root, {"envelope", "result_envelope", "resultEnvelope"}, Missing["NotAvailable"]]
    }];
    tmap = workbenchFirstPresent[{
        associationLookup[networkSection, {"tmap_tree", "tmapTree", "result_tmap_tree", "resultTmapTree"}, Missing["NotAvailable"]],
        associationLookup[root, {"tmap_tree", "tmapTree", "result_tmap_tree", "resultTmapTree"}, Missing["NotAvailable"]]
    }];
    heatmapPlots = workbenchFirstPresent[{
        associationLookup[networkSection, {"heatmap_plots", "heatmapPlots", "result_heatmap_plots", "resultHeatmapPlots"}, Missing["NotAvailable"]],
        associationLookup[root, {"heatmap_plots", "heatmapPlots", "result_heatmap_plots", "resultHeatmapPlots"}, Missing["NotAvailable"]]
    }];
    enrichment = workbenchFirstPresent[{
        associationLookup[root, {"enrichment", "result_enrichment", "resultEnrichment"}, Missing["NotAvailable"]],
        associationLookup[networkSection, {"enrichment", "result_enrichment", "resultEnrichment"}, Missing["NotAvailable"]]
    }, <||>];

    sequence = workbenchFirstPresent[{
        associationLookup[structure, {"sequence"}, Missing["NotAvailable"]],
        associationLookup[root, {"sequence", "query_sequence", "querySequence"}, Missing["NotAvailable"]]
    }];
    proteinID = workbenchFirstPresent[{
        associationLookup[structure, {"protein_id", "proteinId", "id"}, Missing["NotAvailable"]],
        associationLookup[root, {"protein_id", "proteinId"}, Missing["NotAvailable"]]
    }];
    taskType = workbenchNormalizeTaskType @ workbenchFirstPresent[{
        associationLookup[networkMetadata, {"task_type", "taskType", "interaction_type", "interactionType"}, Missing["NotAvailable"]],
        associationLookup[biologicalNetwork, {"task_type", "taskType", "interaction_type", "interactionType"}, Missing["NotAvailable"]],
        associationLookup[networkSection, {"task_type", "taskType"}, Missing["NotAvailable"]],
        associationLookup[root, {"task_type", "taskType"}, Missing["NotAvailable"]],
        associationLookup[associationLookup[tmap, {"metadata"}, <||>], {"task_type", "taskType"}, Missing["NotAvailable"]]
    }];
    queryIDs = workbenchFirstPresent[{
        associationLookup[networkMetadata, {"query_ids", "queryIds"}, Missing["NotAvailable"]],
        associationLookup[biologicalNetwork, {"query_ids", "queryIds"}, Missing["NotAvailable"]],
        associationLookup[actome, {"query_ids", "queryIds"}, Missing["NotAvailable"]],
        associationLookup[root, {"query_ids", "queryIds"}, Missing["NotAvailable"]],
        associationLookup[tmap, {"query_ids", "queryIds"}, Missing["NotAvailable"]],
        associationLookup[
            associationLookup[tmap, {"focus"}, <||>],
            {"query_ids", "queryIds"},
            Missing["NotAvailable"]
        ],
        If[StringQ[proteinID], {proteinID}, Missing["NotAvailable"]]
    }, {}];

    <|
        "ProteinID" -> proteinID,
        "TaskType" -> taskType,
        "Sequence" -> sequence,
        "DFA" -> structure,
        "OraclePredictions" -> oracles,
        "CAMPAnnotations" -> camp,
        "TranslatorAnnotations" -> translator,
        "BiologicalNetwork" -> biologicalNetwork,
        "ActomeEdges" -> actome,
        "Enrichment" -> enrichment,
        "Envelope" -> envelope,
        "HeatmapPlots" -> heatmapPlots,
        "TMAPTree" -> tmap,
        "QueryIDs" -> queryIDs,
        "OriginalResult" -> root
    |>
];

SynthyraAnalysis[result_Association] := SynthyraAnalysisObject @ workbenchNormalizeResult[result];
SynthyraAnalysis[analysis_SynthyraAnalysisObject] := analysis;
SynthyraAnalysis[other_] := workbenchFailure[
    "InvalidAnalysisResult",
    "SynthyraAnalysis expects a completed result Association.",
    <|"InputHead" -> Head[other]|>
];

SynthyraAnalysisObject[data_Association]["Properties"] := Keys[data];
SynthyraAnalysisObject[data_Association][property_String] := Lookup[
    data,
    property,
    Missing["KeyAbsent", property]
];
SynthyraAnalysisObject /: Normal[SynthyraAnalysisObject[data_Association]] := data;

SynthyraAnalysisObject /: MakeBoxes[object : SynthyraAnalysisObject[data_Association], form_] := Module[
    {protein, actome, nodes, edges, summary},
    protein = Replace[Lookup[data, "ProteinID", Missing["NotAvailable"]], Missing[__] -> "unknown protein"];
    actome = Lookup[data, "ActomeEdges", Missing["NotAvailable"]];
    nodes = If[AssociationQ[actome], Length @ associationLookup[actome, {"nodes"}, {}], 0];
    edges = If[AssociationQ[actome], Length @ associationLookup[actome, {"edges"}, {}], 0];
    summary = Row[{ToString[protein], ", ", nodes, " actome vertices, ", edges, " biological edges"}];
    InterpretationBox[
        RowBox[{"SynthyraAnalysisObject", "[", ToBoxes[summary, form], "]"}],
        object
    ]
];

workbenchCoerceAnalysis[analysis_SynthyraAnalysisObject] := analysis;
workbenchCoerceAnalysis[result_Association] := SynthyraAnalysis[result];
workbenchCoerceAnalysis[other_] := workbenchFailure[
    "InvalidAnalysis",
    "Expected a SynthyraAnalysisObject or result Association.",
    <|"InputHead" -> Head[other]|>
];

workbenchOracleRecords[value_] := Which[
    ListQ[value], MapIndexed[
        Function[{record, index},
            If[AssociationQ[record],
                With[{name = associationLookup[record, {"oracle_name", "oracleName", "name"}, "Oracle " <> ToString[First[index]]]},
                    Join[<|"Oracle" -> name|>, record]
                ],
                <|"Oracle" -> "Oracle " <> ToString[First[index]], "Score" -> record|>
            ]
        ],
        value
    ],
    AssociationQ[value], KeyValueMap[
        Function[{name, record},
            If[AssociationQ[record], Join[<|"Oracle" -> ToString[name]|>, record], <|"Oracle" -> ToString[name], "Score" -> record|>]
        ],
        value
    ],
    True, {}
];

SynthyraOracleDataset[input_] := Module[{analysis, records},
    analysis = workbenchCoerceAnalysis[input];
    If[FailureQ[analysis], Return[analysis]];
    records = workbenchOracleRecords[analysis["OraclePredictions"]];
    Dataset[records]
];

workbenchEnrichmentRecords[value_] := Which[
    AssociationQ[value], With[{terms = associationLookup[value, {"terms", "results"}, Missing["NotAvailable"]]},
        If[ListQ[terms], Select[terms, AssociationQ], KeyValueMap[
            Function[{name, record},
                If[AssociationQ[record], Join[<|"Term" -> ToString[name]|>, record], <|"Term" -> ToString[name], "Value" -> record|>]
            ],
            value
        ]]
    ],
    ListQ[value], Select[value, AssociationQ],
    True, {}
];

SynthyraEnrichmentDataset[input_] := Module[{analysis, records},
    analysis = workbenchCoerceAnalysis[input];
    If[FailureQ[analysis], Return[analysis]];
    records = workbenchEnrichmentRecords[analysis["Enrichment"]];
    Dataset[records]
];

Options[SynthyraStructurePlot] = {
    "ColorBy" -> "Confidence",
    ImageSize -> 650,
    PlotTheme -> "Ribbons"
};

SynthyraStructurePlot[input_, opts : OptionsPattern[]] := Module[
    {analysis, dfa, cif, pdb, structureText, format, molecule},
    analysis = workbenchCoerceAnalysis[input];
    If[FailureQ[analysis], Return[analysis]];
    dfa = analysis["DFA"];
    If[! AssociationQ[dfa],
        Return @ workbenchFailure["StructureUnavailable", "This result does not contain a protein structure."]
    ];
    cif = associationLookup[dfa, {"cif_string", "cifString", "mmcif_string", "mmcifString"}, Missing["NotAvailable"]];
    pdb = associationLookup[dfa, {"pdb_string", "pdbString"}, Missing["NotAvailable"]];
    {structureText, format} = Which[
        StringQ[cif] && StringLength[StringTrim[cif]] > 0, {cif, "MMCIF"},
        StringQ[pdb] && StringLength[StringTrim[pdb]] > 0, {pdb, "PDB"},
        True, Return @ workbenchFailure["StructureUnavailable", "This result does not contain mmCIF or PDB structure text."]
    ];
    molecule = Quiet @ Check[ImportString[structureText, format], $Failed];
    If[Head[molecule] =!= BioMolecule,
        Return @ workbenchFailure["StructureImportFailed", "Wolfram Language could not import the returned structure text."]
    ];
    SynthyraStructurePlot[molecule, opts]
];

workbenchPlddtPercent[value_?NumericQ] := If[N[value] <= 1.5, 100. N[value], N[value]];

workbenchPlddtDisplay[value_] := If[
    NumericQ[value],
    ToString[NumberForm[workbenchPlddtPercent[value], {4, 1}]] <> "%",
    value
];

workbenchPlddtColor[value_?NumericQ] := With[
    {percent = workbenchPlddtPercent[value]},
    Which[
        percent < 50., RGBColor[1., .49, .27],
        percent < 70., RGBColor[1., .84, .08],
        percent < 90., RGBColor[.40, .80, .95],
        True, RGBColor[0., .33, .84]
    ]
];

workbenchParsePdbNumber[value_String] := Module[{text, parsed},
    text = StringTrim[value];
    If[
        ! StringMatchQ[
            text,
            RegularExpression["[+-]?(\\d+(\\.\\d*)?|\\.\\d+)([Ee][+-]?\\d+)?"]
        ],
        Return[Missing["InvalidNumber"]]
    ];
    parsed = Quiet @ Check[Interpreter["Number"][text], $Failed];
    If[NumericQ[parsed], N[parsed], Missing["InvalidNumber"]]
];
workbenchParsePdbNumber[_] := Missing["InvalidNumber"];

workbenchPdbCARecords[pdb_String] := Module[{lines, records},
    lines = Select[
        StringSplit[pdb, {"\r\n", "\n", "\r"}],
        StringStartsQ[#, "ATOM"] && StringLength[#] >= 66 &&
            StringTrim[StringTake[#, {13, 16}]] === "CA" &
    ];
    records = Map[
        Function[line,
            <|
                "Chain" -> StringTrim[StringTake[line, {22, 22}]],
                "Coordinate" -> (workbenchParsePdbNumber[StringTake[line, #]] & /@ {{31, 38}, {39, 46}, {47, 54}}),
                "Confidence" -> workbenchParsePdbNumber[StringTake[line, {61, 66}]]
            |>
        ],
        lines
    ];
    records = Select[records, VectorQ[Lookup[#, "Coordinate", {}], NumericQ] &];
    MapIndexed[Append[#1, "ResidueIndex" -> First[#2]] &, records]
];

workbenchFiniteRealQ[value_] := Quiet @ Check[
    NumericQ[value] && TrueQ[Element[N[value], Reals]] &&
        FreeQ[N[value], Indeterminate | ComplexInfinity | _DirectedInfinity],
    False
];

workbenchNormalizeAttributions[values_List] := Module[{valid, minimum, maximum, range},
    valid = N @ Select[values, workbenchFiniteRealQ];
    If[valid === {}, Return[ConstantArray[Missing["NotAvailable"], Length[values]]]];
    {minimum, maximum} = MinMax[valid];
    range = maximum - minimum;
    If[TrueQ[range == 0], range = 1.];
    Map[
        If[workbenchFiniteRealQ[#], (N[#] - minimum)/range, Missing["NotAvailable"]] &,
        values
    ]
];

$workbenchAttributionColorStops = {
    {0., RGBColor[1., 125./255., 69./255.]},
    {.35, RGBColor[1., 219./255., 19./255.]},
    {.65, RGBColor[101./255., 203./255., 243./255.]},
    {1., RGBColor[0., 83./255., 214./255.]}
};
$workbenchNoDataColor = GrayLevel[96./255.];

workbenchAttributionColor[value_?workbenchFiniteRealQ] := Module[
    {scaled, upper, lower, start, finish, fraction},
    scaled = Clip[N[value], {0., 1.}];
    upper = SelectFirst[
        Range[2, Length[$workbenchAttributionColorStops]],
        scaled <= $workbenchAttributionColorStops[[#, 1]] &,
        Length[$workbenchAttributionColorStops]
    ];
    lower = upper - 1;
    start = $workbenchAttributionColorStops[[lower]];
    finish = $workbenchAttributionColorStops[[upper]];
    fraction = (scaled - start[[1]])/(finish[[1]] - start[[1]]);
    Blend[{start[[2]], finish[[2]]}, fraction]
];
workbenchAttributionColor[_] := $workbenchNoDataColor;

workbenchAttributionColorAt[normalized_List, index_Integer] := If[
    1 <= index <= Length[normalized],
    workbenchAttributionColor[normalized[[index]]],
    $workbenchNoDataColor
];

workbenchAttributionStructurePlot[pdb_String, values_List, label_String] := Module[
    {records, normalized, segments, midpoint},
    records = workbenchPdbCARecords[pdb];
    If[Length[records] < 2, Return @ workbenchFailure[
        "AttributionStructureUnavailable",
        "The returned PDB text did not contain enough alpha-carbon records for attribution coloring."
    ]];
    normalized = workbenchNormalizeAttributions[values];
    segments = Flatten @ Map[
        Function[chain,
            Map[
                Function[index,
                    With[{left = chain[[index]], right = chain[[index + 1]]},
                        midpoint = Mean[{left["Coordinate"], right["Coordinate"]}];
                        {
                            workbenchAttributionColorAt[normalized, left["ResidueIndex"]],
                            Specularity[White, 18],
                            Tube[{left["Coordinate"], midpoint}, .65],
                            workbenchAttributionColorAt[normalized, right["ResidueIndex"]],
                            Specularity[White, 18],
                            Tube[{midpoint, right["Coordinate"]}, .65]
                        }
                    ]
                ],
                Range[Max[0, Length[chain] - 1]]
            ]
        ],
        GatherBy[records, Lookup[#, "Chain", ""] &]
    ];
    Legended[
        Graphics3D[
            segments,
            Boxed -> False,
            Background -> GrayLevel[.98],
            Lighting -> "Neutral",
            ImageSize -> 700,
            PlotLabel -> Style[label <> " structure coloring", 14, Bold, Black],
            PlotRange -> All,
            SphericalRegion -> True
        ],
        Placed[
            SwatchLegend[
                $workbenchAttributionColorStops[[All, 2]],
                {"minimum", "35%", "65%", "maximum"},
                LegendLabel -> "Min/max-normalized attribution"
            ],
            Right
        ]
    ]
];

workbenchPlddtStructurePlot[analysis_SynthyraAnalysisObject] := Module[
    {dfa, pdb, records, segments},
    dfa = analysis["DFA"];
    pdb = If[AssociationQ[dfa], associationLookup[dfa, {"pdb_string", "pdbString"}, Missing["NotAvailable"]], Missing["NotAvailable"]];
    If[! StringQ[pdb], Return @ workbenchFailure[
        "PlddtStructureUnavailable",
        "Per-residue pLDDT coloring requires returned PDB text."
    ]];
    records = workbenchPdbCARecords[pdb];
    records = Select[
        records,
        NumericQ[Lookup[#, "Confidence", Missing["NotAvailable"]]] &
    ];
    If[Length[records] < 2, Return @ workbenchFailure[
        "PlddtStructureUnavailable",
        "The returned PDB text did not contain enough alpha-carbon records for pLDDT coloring."
    ]];
    segments = Flatten @ Map[
        Function[chain,
            Map[
                Function[index,
                    With[{left = chain[[index]], right = chain[[index + 1]]},
                        {
                            workbenchPlddtColor @ Mean[{left["Confidence"], right["Confidence"]}],
                            Specularity[White, 18],
                            Tube[{left["Coordinate"], right["Coordinate"]}, .65]
                        }
                    ]
                ],
                Range[Max[0, Length[chain] - 1]]
            ]
        ],
        GatherBy[records, Lookup[#, "Chain", ""] &]
    ];
    Legended[
        Graphics3D[
            segments,
            Boxed -> False,
            Background -> GrayLevel[.98],
            Lighting -> "Neutral",
            ImageSize -> 700,
            PlotRange -> All,
            SphericalRegion -> True
        ],
        Placed[
            SwatchLegend[
                {
                    RGBColor[1., .49, .27], RGBColor[1., .84, .08],
                    RGBColor[.40, .80, .95], RGBColor[0., .33, .84]
                },
                {"< 50 very low", "50-70 low", "70-90 confident", "> 90 very high"},
                LegendLabel -> "Per-residue pLDDT"
            ],
            Right
        ]
    ]
];

workbenchMissingPanel[text_String] := Panel[
    Style[text, GrayLevel[0.4], Italic],
    Background -> GrayLevel[0.98],
    FrameMargins -> 18
];

workbenchCount[value_] := Which[
    ListQ[value], Length[value],
    AssociationQ[value], Length[value],
    True, 0
];

workbenchSummaryView[analysis_SynthyraAnalysisObject] := Module[
    {actome, nodes, edges, dfa, plddt, sequence, summary, available},
    actome = analysis["ActomeEdges"];
    nodes = If[AssociationQ[actome], associationLookup[actome, {"nodes"}, {}], {}];
    edges = If[AssociationQ[actome], associationLookup[actome, {"edges"}, {}], {}];
    dfa = analysis["DFA"];
    plddt = If[AssociationQ[dfa], associationLookup[dfa, {"plddt", "pLDDT"}, Missing["NotAvailable"]], Missing["NotAvailable"]];
    sequence = analysis["Sequence"];
    summary = {
        {Style["Protein", Bold], Replace[analysis["ProteinID"], Missing[__] -> "Not declared"]},
        {Style["Sequence length", Bold], If[StringQ[sequence], StringLength[sequence], "Not available"]},
        {Style["Predicted structure pLDDT", Bold], Replace[workbenchPlddtDisplay[plddt], Missing[__] -> "Not available"]},
        {Style["Oracle outputs", Bold], workbenchCount[analysis["OraclePredictions"]]},
        {Style["CAMP annotations", Bold], workbenchCount[analysis["CAMPAnnotations"]]},
        {Style["Translator annotations", Bold], workbenchCount[analysis["TranslatorAnnotations"]]},
        {Style["Actome vertices", Bold], Length[nodes]},
        {Style["Biological interaction edges", Bold], Length[edges]},
        {Style["Enrichment terms", Bold], Length @ workbenchEnrichmentRecords[analysis["Enrichment"]]}
    };
    available = {
        "Structure" -> AssociationQ[dfa] && AnyTrue[{"pdb_string", "cif_string"}, ! MissingQ[associationLookup[dfa, {#}]] &],
        "Biological actome" -> AssociationQ[actome],
        "TMAP layout" -> AssociationQ[analysis["TMAPTree"]],
        "Expansion envelope" -> AssociationQ[analysis["Envelope"]],
        "Enrichment" -> workbenchPresentQ[analysis["Enrichment"]]
    };
    Column[{
        Style["Synthyra coordinated analysis", 20, Bold, RGBColor[0.08, 0.24, 0.42]],
        Grid[summary, Alignment -> Left, Spacings -> {2, 1}, Dividers -> {False, {False, False, True}}],
        Style["Available result families", 14, Bold],
        Grid[
            ({First[#], If[TrueQ[Last[#]], Style["Available", Darker[Green]], Style["Not returned", Gray]]} &) /@ available,
            Alignment -> Left,
            Spacings -> {2, .7}
        ]
    }, Spacings -> 1.5]
];

workbenchVertexID[Protein[id_]] := ToString[id];
workbenchVertexID[Ligand[id_]] := ToString[id];
workbenchVertexID[vertex_] := ToString[vertex, InputForm];

workbenchConfidence[graph_Graph, edge_] := Module[{value},
    value = Quiet @ Check[AnnotationValue[{graph, edge}, "Confidence"], Missing["NotAvailable"]];
    If[NumericQ[value], Clip[If[N[value] <= 1., N[value], N[value]/100.], {0., 1.}], 0.]
];

workbenchEvidenceColor[graph_Graph, edge_] := Module[{known, novel},
    known = AnyTrue[
        {"STRING", "BioGRID", "BioGRIDMV"},
        Function[name,
            TrueQ @ Quiet @ Check[AnnotationValue[{graph, edge}, name], False]
        ]
    ];
    novel = TrueQ @ Quiet @ Check[AnnotationValue[{graph, edge}, "Novel"], False];
    Which[
        known, RGBColor[0.08, 0.55, 0.50],
        novel, RGBColor[0.55, 0.28, 0.72],
        True, GrayLevel[.45]
    ]
];

workbenchEdgeEndpoints[edge_] := Replace[
    edge,
    {
        UndirectedEdge[source_, target_] :> {source, target},
        DirectedEdge[source_, target_] :> {source, target}
    }
];

workbenchFrontierConfidence[graph_Graph, vertex_, frontier_List] := Module[
    {incident, touching, endpoints},
    incident = IncidenceList[graph, vertex];
    touching = Select[
        incident,
        Function[edge,
            endpoints = workbenchEdgeEndpoints[edge];
            ListQ[endpoints] && AnyTrue[
                DeleteCases[endpoints, vertex],
                MemberQ[frontier, #] &
            ]
        ]
    ];
    If[touching === {}, 0., Max[workbenchConfidence[graph, #] & /@ touching]]
];

workbenchRankedNeighborhoodVertices[graph_Graph, seeds_List, radius_, maximumVertices_] := Module[
    {visited, frontier, candidates, ranked, next, depth = 0, depthLimit, vertexLimit, budget},
    visited = DeleteDuplicates[seeds];
    frontier = visited;
    depthLimit = If[radius === All, Infinity, Max[0, radius]];
    vertexLimit = If[maximumVertices === All, Infinity, Max[Length[visited], maximumVertices]];
    While[
        frontier =!= {} && depth < depthLimit && Length[visited] < vertexLimit,
        candidates = Complement[
            DeleteDuplicates @ Flatten[AdjacencyList[graph, #] & /@ frontier],
            visited
        ];
        If[candidates === {}, Break[]];
        ranked = Reverse @ SortBy[
            candidates,
            {
                workbenchFrontierConfidence[graph, #, frontier] &,
                workbenchVertexID
            }
        ];
        budget = If[vertexLimit === Infinity, Length[ranked], vertexLimit - Length[visited]];
        next = Take[ranked, UpTo[Max[0, budget]]];
        visited = Join[visited, next];
        frontier = next;
        depth++
    ];
    visited
];

workbenchGraphSubset[
    graph_Graph,
    queryVertices_List,
    minimumConfidence_?NumericQ,
    radius_,
    maximumVertices_: All
] := Module[
    {drop, filtered, presentQueries, vertices},
    drop = Select[EdgeList[graph], workbenchConfidence[graph, #] < minimumConfidence &];
    filtered = If[drop === {}, graph, EdgeDelete[graph, drop]];
    presentQueries = Intersection[VertexList[filtered], queryVertices];
    If[
        presentQueries === {},
        If[maximumVertices === All, Return[filtered]];
        vertices = Take[
            Reverse @ SortBy[
                VertexList[filtered],
                {VertexDegree[filtered, #] &, workbenchVertexID}
            ],
            UpTo[Max[0, Floor[maximumVertices]]]
        ];
        Return[Subgraph[filtered, vertices, AnnotationRules -> Inherited]]
    ];
    If[radius === All && maximumVertices === All, Return[filtered]];
    vertices = workbenchRankedNeighborhoodVertices[
        filtered,
        presentQueries,
        radius,
        maximumVertices
    ];
    Subgraph[filtered, vertices, AnnotationRules -> Inherited]
];

workbenchStyledGraph[graph_Graph, queryVertices_List, layout_] := Module[
    {vertices, edges, querySet, vertexStyles, vertexSizes, labels, edgeStyles},
    vertices = VertexList[graph];
    edges = EdgeList[graph];
    querySet = Intersection[vertices, queryVertices];
    vertexStyles = Map[
        Function[vertex,
            vertex -> Which[
                MemberQ[querySet, vertex], Directive[RGBColor[0.84, 0.16, 0.18], EdgeForm[Directive[Black, Thick]]],
                MatchQ[vertex, _Ligand], RGBColor[0.95, 0.62, 0.18],
                True, RGBColor[0.18, 0.49, 0.72]
            ]
        ],
        vertices
    ];
    vertexSizes = Map[Function[vertex, vertex -> If[MemberQ[querySet, vertex], .34, .22]], vertices];
    labels = Map[Function[vertex, vertex -> Placed[workbenchVertexID[vertex], Tooltip]], vertices];
    edgeStyles = Map[
        Function[edge,
            With[{confidence = workbenchConfidence[graph, edge]},
                edge -> Directive[
                    workbenchEvidenceColor[graph, edge],
                    Opacity[.15 + .8 confidence],
                    AbsoluteThickness[.5 + 2.2 confidence]
                ]
            ]
        ],
        edges
    ];
    Graph[
        graph,
        GraphLayout -> layout,
        VertexStyle -> vertexStyles,
        VertexSize -> vertexSizes,
        VertexLabels -> labels,
        EdgeStyle -> edgeStyles,
        ImageSize -> 850,
        PerformanceGoal -> "Quality"
    ]
];

workbenchGraphAnnotationRules[graph_Graph] := Module[{rules},
    rules = AnnotationRules /. Options[graph, AnnotationRules];
    If[ListQ[rules], rules, {}]
];

workbenchMergeGraphs[graphs_List] := Module[
    {valid, vertices, edges, annotationRules},
    valid = Select[graphs, GraphQ];
    If[valid === {}, Return[$Failed]];
    vertices = DeleteDuplicates @ Flatten[VertexList /@ valid];
    edges = DeleteDuplicates @ Flatten[EdgeList /@ valid];
    annotationRules = Flatten[workbenchGraphAnnotationRules /@ valid, 1];
    Graph[
        vertices,
        edges,
        AnnotationRules -> annotationRules
    ]
];

workbenchPliGraphQ[graph_Graph] := AnyTrue[
    VertexList[graph],
    MatchQ[#, _Ligand] &
];

workbenchNetworkGraph[analysis_SynthyraAnalysisObject] := Module[
    {actomeGraph, biologicalGraph},
    actomeGraph = If[
        AssociationQ[analysis["ActomeEdges"]],
        Quiet @ SynthyraGraph[analysis["ActomeEdges"]],
        $Failed
    ];
    biologicalGraph = If[
        AssociationQ[analysis["BiologicalNetwork"]],
        Quiet @ SynthyraGraph[analysis["BiologicalNetwork"]],
        $Failed
    ];
    Which[
        GraphQ[actomeGraph] && GraphQ[biologicalGraph] && workbenchPliGraphQ[biologicalGraph],
            workbenchMergeGraphs[{actomeGraph, biologicalGraph}],
        GraphQ[actomeGraph], actomeGraph,
        GraphQ[biologicalGraph], biologicalGraph,
        True, $Failed
    ]
];

workbenchNetworkQueryVertices[
    analysis_SynthyraAnalysisObject,
    graph_Graph
] := Module[{rawIDs, queryIDs, candidates, taskType, preferred},
    rawIDs = analysis["QueryIDs"];
    If[! ListQ[rawIDs], rawIDs = {}];
    queryIDs = DeleteDuplicates @ Map[
        ToString,
        Select[rawIDs, StringQ[#] || IntegerQ[#] &]
    ];
    candidates = Select[
        VertexList[graph],
        MemberQ[queryIDs, workbenchVertexID[#]] &
    ];
    taskType = analysis["TaskType"];
    If[! StringQ[taskType], taskType = If[workbenchPliGraphQ[graph], "pli", "ppi"]];
    preferred = Switch[
        canonicalName[taskType],
        "pli", Select[candidates, MatchQ[#, _Ligand] &],
        "ppi", Select[candidates, MatchQ[#, _Protein] &],
        _, {}
    ];
    If[preferred === {}, candidates, preferred]
];

workbenchNetworkView[analysis_SynthyraAnalysisObject, layout_] := Module[
    {graph, queryVertices},
    graph = workbenchNetworkGraph[analysis];
    If[FailureQ[graph], Return @ workbenchMissingPanel["The returned biological network could not be normalized."]];
    If[! GraphQ[graph], Return @ workbenchMissingPanel["No biological interaction network was returned."]];
    queryVertices = workbenchNetworkQueryVertices[analysis, graph];
    Column[{
        Panel[
            Row[{
                Style["Biological interaction graph", Bold],
                "  ", Length[VertexList[graph]], " vertices  ", Length[EdgeList[graph]], " edges. ",
                "Confidence controls visibility and is not treated as path cost. ",
                Style["Teal", RGBColor[0.08, 0.55, 0.50]], " = known evidence, ",
                Style["purple", RGBColor[0.55, 0.28, 0.72]], " = Atlas-only, gray = unknown."
            }],
            Background -> RGBColor[.94, .97, 1.]
        ],
        With[{baseGraph = graph, seeds = queryVertices, graphLayout = layout},
            If[
                TrueQ[$Notebooks],
                Manipulate[
                    workbenchStyledGraph[
                        workbenchGraphSubset[
                            baseGraph,
                            seeds,
                            minimumConfidence,
                            hops,
                            maximumVertices
                        ],
                        seeds,
                        graphLayout
                    ],
                    {{minimumConfidence, 0.9, "Minimum confidence"}, 0., 1., .05, Appearance -> "Labeled"},
                    {{hops, 2, "Query neighborhood"}, {1 -> "1 hop", 2 -> "2 hops", 3 -> "3 hops", All -> "All"}},
                    {{maximumVertices, 50, "Maximum displayed vertices"}, {25, 50, 100, 200, All}},
                    TrackedSymbols :> {minimumConfidence, hops, maximumVertices}
                ],
                workbenchStyledGraph[
                    workbenchGraphSubset[baseGraph, seeds, .9, 2, 50],
                    seeds,
                    graphLayout
                ]
            ]
        ]
    }, Spacings -> 1]
];

workbenchStructureView[analysis_SynthyraAnalysisObject] := Module[
    {plot, plddtPlot, attributionView, dfa, plddt, views},
    plot = SynthyraStructurePlot[analysis];
    If[FailureQ[plot], Return @ workbenchMissingPanel[Lookup[plot[[2]], "MessageTemplate", "No structure is available."]]];
    plddtPlot = workbenchPlddtStructurePlot[analysis];
    dfa = analysis["DFA"];
    plddt = associationLookup[dfa, {"plddt", "pLDDT"}, Missing["NotAvailable"]];
    views = <|"Native ribbons" -> plot|>;
    If[! FailureQ[plddtPlot], AssociateTo[views, "pLDDT confidence" -> plddtPlot]];
    attributionView = workbenchOracleAttributionStructureView[analysis];
    If[
        ! MissingQ[attributionView] && ! FailureQ[attributionView],
        AssociateTo[views, "Oracle attributions" -> attributionView]
    ];
    Column[{
        If[
            MissingQ[plddt],
            Nothing,
            Panel[
                Row[{Style["Predicted pLDDT: ", Bold], workbenchPlddtDisplay[plddt]}],
                Background -> GrayLevel[.97]
            ]
        ],
        TabView[Normal[views]]
    }, Alignment -> Center]
];

workbenchOracleScore[record_Association] := Module[{score, labels, index},
    score = associationLookup[record, {"score", "value"}, Missing["NotAvailable"]];
    labels = associationLookup[record, {"label_names", "labelNames", "labels"}, {}];
    Which[
        NumericQ[score], {ToString[Lookup[record, "Oracle", "Oracle"]], N[score]},
        ListQ[score] && score =!= {} && AllTrue[score, NumericQ],
            index = First @ Ordering[score, -1];
            {
                ToString[Lookup[record, "Oracle", "Oracle"]] <>
                    If[ListQ[labels] && Length[labels] >= index, " (" <> ToString[labels[[index]]] <> ")", ""],
                N[score[[index]]]
            },
        True, Nothing
    ]
];

workbenchAttributionMatrix[value_] := Which[
    ListQ[value] && value =!= {} && AllTrue[value, ListQ] &&
        Apply[SameQ, Length /@ value] && Min[Length /@ value] >= 1,
        value,
    ListQ[value] && value =!= {} && AllTrue[value, ! ListQ[#] &],
        List /@ value,
    True,
        {}
];

workbenchAttributionColumns[record_Association, residueCount_: Automatic] := Module[
    {matrix, labels, oracle, columnCount, label, sourcePositionCount, residueRows, specialTokensRemoved},
    matrix = workbenchAttributionMatrix @ associationLookup[record, {"attributions"}, Missing["NotAvailable"]];
    If[matrix === {}, Return[{}]];
    columnCount = Length[First[matrix]];
    sourcePositionCount = Length[matrix];
    (* The production ESM++ tokenizer emits <cls>, residues, <eos>. Only
       remove boundaries when the sequence length proves that exact shape. *)
    {residueRows, specialTokensRemoved} = If[
        IntegerQ[residueCount] && residueCount > 0 && sourcePositionCount === residueCount + 2,
        {2 ;; sourcePositionCount - 1, 2},
        {All, 0}
    ];
    labels = associationLookup[record, {"label_names", "labelNames", "labels"}, {}];
    If[! ListQ[labels], labels = {}];
    oracle = ToString @ workbenchFirstPresent[{
        Lookup[record, "Oracle", Missing["NotAvailable"]],
        associationLookup[record, {"oracle_name", "oracleName", "name"}, Missing["NotAvailable"]]
    }, "Oracle"];
    Table[
        label = Which[
            columnCount === 1 && Length[labels] =!= 1, "Attribution",
            Length[labels] >= column, ToString[labels[[column]]],
            True, "Class " <> ToString[column]
        ];
        With[
            {
                columnIndex = column,
                columnLabel = label,
                displayLabel = oracle <> " | " <> label <>
                    If[columnCount > 1, " [" <> ToString[column] <> "/" <> ToString[columnCount] <> "]", ""]
            },
            <|
                "Oracle" -> oracle,
                "Label" -> columnLabel,
                "ColumnIndex" -> columnIndex,
                "ColumnCount" -> columnCount,
                "DisplayLabel" -> displayLabel,
                "SourcePositionCount" -> sourcePositionCount,
                "SpecialTokensRemoved" -> specialTokensRemoved,
                (* Keep the source matrix once and materialize only the selected
                   residue track. Association evaluates RuleDelayed on Lookup. *)
                "Values" :> matrix[[residueRows, columnIndex]]
            |>
        ],
        {column, columnCount}
    ]
];

workbenchAllAttributionColumns[records_List, residueCount_: Automatic] := Flatten[
    workbenchAttributionColumns[#, residueCount] & /@ Select[records, AssociationQ],
    1
];

workbenchAttributionColumnPlot[column_Association] := Module[
    {values, normalized, label, cells, lineValues, strip},
    values = Lookup[column, "Values", {}];
    If[! ListQ[values] || values === {}, Return[Missing["NotAvailable"]]];
    normalized = workbenchNormalizeAttributions[values];
    label = ToString @ Lookup[column, "DisplayLabel", "Oracle attribution"];
    cells = MapIndexed[
        Function[{value, index},
            {
                EdgeForm[None],
                workbenchAttributionColor[value],
                Tooltip[
                    Rectangle[{First[index] - 1, 0}, {First[index], 1}],
                    Row[{"Residue ", First[index], ": ", values[[First[index]]]}]
                ]
            }
        ],
        normalized
    ];
    strip = Legended[
        Graphics[
            cells,
            AspectRatio -> 1/12,
            Frame -> True,
            FrameTicks -> {{None, None}, {Automatic, None}},
            ImageSize -> 800,
            PlotLabel -> label <> " per-residue attribution",
            PlotRange -> {{0, Length[values]}, {0, 1}}
        ],
        Placed[
            SwatchLegend[
                $workbenchAttributionColorStops[[All, 2]],
                {"minimum", "35%", "65%", "maximum"},
                LegendLabel -> "Min/max-normalized attribution"
            ],
            Below
        ]
    ];
    lineValues = Map[
        If[workbenchFiniteRealQ[#], N[#], Missing["NotAvailable"]] &,
        values
    ];
    Column[{
        strip,
        ListLinePlot[
            lineValues,
            PlotStyle -> RGBColor[.32, .24, .66],
            Filling -> Axis,
            AxesLabel -> {"Residue", "Attribution"},
            PlotRange -> All,
            ImageSize -> 800
        ]
    }, Spacings -> .4]
];

workbenchAttributionPlot[
    record_Association,
    columnIndex_: 1,
    residueCount_: Automatic
] := Module[{columns},
    columns = workbenchAttributionColumns[record, residueCount];
    If[
        columns === {} || ! IntegerQ[columnIndex] || ! (1 <= columnIndex <= Length[columns]),
        Missing["NotAvailable"],
        workbenchAttributionColumnPlot[columns[[columnIndex]]]
    ]
];

workbenchAttributionColumnIndices[columns_List, oracle_String, search_String] := Module[
    {normalizedSearch},
    normalizedSearch = ToLowerCase @ StringTrim[search];
    Select[
        Range[Length[columns]],
        Function[index,
            ToString @ Lookup[columns[[index]], "Oracle", ""] === oracle &&
                (
                    normalizedSearch === "" ||
                    StringContainsQ[
                        ToLowerCase @ ToString @ Lookup[columns[[index]], "DisplayLabel", ""],
                        normalizedSearch
                    ]
                )
        ]
    ]
];

$workbenchAttributionChoiceLimit = 200;

workbenchAttributionSelector[columns_List, renderer_] := Module[{oracles},
    If[columns === {}, Return[Missing["NotAvailable"]]];
    oracles = DeleteDuplicates[ToString /@ Lookup[columns, "Oracle", "Oracle"]];
    If[
        TrueQ[$Notebooks],
        DynamicModule[
            {
                selectedOracle = First[oracles],
                search = "",
                selected = 1
            },
            Column[{
                Grid[
                    {
                        {
                            Style["Oracle", Bold],
                            PopupMenu[
                                Dynamic[
                                    selectedOracle,
                                    Function[value,
                                        selectedOracle = value;
                                        search = "";
                                        With[
                                            {matches = workbenchAttributionColumnIndices[columns, value, ""]},
                                            If[matches =!= {}, selected = First[matches]]
                                        ]
                                    ]
                                ],
                                Thread[oracles -> oracles]
                            ]
                        },
                        {
                            Style["Find label", Bold],
                            InputField[
                                Dynamic[
                                    search,
                                    Function[value,
                                        search = value;
                                        With[
                                            {matches = workbenchAttributionColumnIndices[columns, selectedOracle, value]},
                                            If[matches =!= {}, selected = First[matches]]
                                        ]
                                    ]
                                ],
                                String,
                                FieldHint -> "type a label or column index"
                            ]
                        },
                        {
                            Style["Label", Bold],
                            Dynamic[
                                Module[{matches, visible},
                                    matches = workbenchAttributionColumnIndices[columns, selectedOracle, search];
                                    visible = Take[matches, UpTo[$workbenchAttributionChoiceLimit]];
                                    If[
                                        visible === {},
                                        Style["No matching attribution label", GrayLevel[.45], Italic],
                                        PopupMenu[
                                            Dynamic[selected],
                                            Thread[
                                                visible -> Map[
                                                    Function[index,
                                                        ToString @ Lookup[columns[[index]], "Label", "Attribution"] <>
                                                            " [" <>
                                                            ToString @ Lookup[columns[[index]], "ColumnIndex", index] <>
                                                            "/" <>
                                                            ToString @ Lookup[columns[[index]], "ColumnCount", Length[visible]] <>
                                                            "]"
                                                    ],
                                                    visible
                                                ]
                                            ]
                                        ]
                                    ]
                                ],
                                TrackedSymbols :> {selectedOracle, search}
                            ]
                        }
                    },
                    Alignment -> Left,
                    Spacings -> {1.5, .7}
                ],
                Dynamic[
                    Module[{matchCount},
                        matchCount = Length @ workbenchAttributionColumnIndices[columns, selectedOracle, search];
                        If[
                            matchCount > $workbenchAttributionChoiceLimit,
                            Style[
                                "Showing the first " <> ToString[$workbenchAttributionChoiceLimit] <>
                                    " of " <> ToString[matchCount] <> " matching labels; refine the search to reach any column.",
                                GrayLevel[.4],
                                Italic
                            ],
                            Nothing
                        ]
                    ],
                    TrackedSymbols :> {selectedOracle, search}
                ],
                Dynamic[
                    renderer[columns[[selected]]],
                    TrackedSymbols :> {selected},
                    SynchronousUpdating -> False
                ]
            }, Spacings -> 1]
        ],
        renderer[First[columns]]
    ]
];

workbenchAttributionStructureColumnView[
    analysis_SynthyraAnalysisObject,
    column_Association
] := Module[{dfa, pdb, structurePlot, sequencePlot},
    dfa = analysis["DFA"];
    pdb = If[AssociationQ[dfa], associationLookup[dfa, {"pdb_string", "pdbString"}, Missing["NotAvailable"]], Missing["NotAvailable"]];
    If[! StringQ[pdb], Return[Missing["NotAvailable"]]];
    structurePlot = workbenchAttributionStructurePlot[
        pdb,
        Lookup[column, "Values", {}],
        ToString @ Lookup[column, "DisplayLabel", "Oracle attribution"]
    ];
    If[FailureQ[structurePlot], Return[structurePlot]];
    sequencePlot = workbenchAttributionColumnPlot[column];
    Column[{structurePlot, sequencePlot}, Alignment -> Center, Spacings -> 1]
];

workbenchOracleAttributionStructureView[analysis_SynthyraAnalysisObject] := Module[
    {columns, dfa, pdb, sequence, residueCount},
    sequence = analysis["Sequence"];
    residueCount = If[StringQ[sequence], StringLength[sequence], Automatic];
    columns = workbenchAllAttributionColumns[
        workbenchOracleRecords[analysis["OraclePredictions"]],
        residueCount
    ];
    If[columns === {}, Return[Missing["NotAvailable"]]];
    dfa = analysis["DFA"];
    pdb = If[AssociationQ[dfa], associationLookup[dfa, {"pdb_string", "pdbString"}, Missing["NotAvailable"]], Missing["NotAvailable"]];
    If[! StringQ[pdb], Return[Missing["NotAvailable"]]];
    workbenchAttributionSelector[
        columns,
        Function[column, workbenchAttributionStructureColumnView[analysis, column]]
    ]
];

workbenchOracleView[analysis_SynthyraAnalysisObject] := Module[
    {records, scores, chart, attributionColumns, attributionView, sequence, residueCount},
    records = workbenchOracleRecords[analysis["OraclePredictions"]];
    If[records === {}, Return @ workbenchMissingPanel["No oracle predictions were returned."]];
    scores = Cases[workbenchOracleScore /@ records, {_String, _?NumericQ}];
    chart = If[
        scores === {},
        Nothing,
        BarChart[
            scores[[All, 2]],
            ChartLabels -> Placed[scores[[All, 1]], After],
            ChartStyle -> ColorData[97] /@ Range[Length[scores]],
            BarOrigin -> Left,
            ImageSize -> 750,
            PlotLabel -> "Scalar scores or the highest class score"
        ]
    ];
    sequence = analysis["Sequence"];
    residueCount = If[StringQ[sequence], StringLength[sequence], Automatic];
    attributionColumns = workbenchAllAttributionColumns[records, residueCount];
    attributionView = If[
        attributionColumns === {},
        Missing["NotAvailable"],
        workbenchAttributionSelector[attributionColumns, workbenchAttributionColumnPlot]
    ];
    Column[{
        chart,
        If[MissingQ[attributionView], Nothing, Column[{
            Style["Per-residue attribution tracks", 14, Bold],
            attributionView
        }]],
        Dataset[records]
    }, Spacings -> 1.5]
];

workbenchAnnotationView[value_, label_String] := Module[{records},
    records = Which[
        ListQ[value], value,
        AssociationQ[value], {value},
        True, {}
    ];
    If[records === {}, workbenchMissingPanel["No " <> label <> " annotations were returned."], Dataset[records]]
];

workbenchEnrichmentView[analysis_SynthyraAnalysisObject] := Module[{records, scored, top, chart},
    records = workbenchEnrichmentRecords[analysis["Enrichment"]];
    If[records === {}, Return @ workbenchMissingPanel["No enrichment terms were returned."]];
    scored = Cases[
        records,
        record_Association :> With[
            {
                p = associationLookup[record, {"adjusted_p_value", "adjustedPValue", "p_value", "pValue"}, Missing["NotAvailable"]],
                term = associationLookup[record, {"term", "name", "Term"}, "Term"]
            },
            If[NumericQ[p] && p > 0, {ToString[term], -Log10[Clip[N[p], {10.^-300, 1.}]]}, Nothing]
        ]
    ];
    top = Take[Reverse @ SortBy[scored, Last], UpTo[15]];
    chart = If[
        top === {},
        Nothing,
        BarChart[
            top[[All, 2]],
            ChartLabels -> Placed[top[[All, 1]], After],
            BarOrigin -> Left,
            ChartStyle -> RGBColor[.18, .55, .42],
            AxesLabel -> {None, "-log10 adjusted p"},
            ImageSize -> 800
        ]
    ];
    Column[{chart, Dataset[records]}, Spacings -> 1.5]
];

workbenchValidClusterOrderQ[order_, size_Integer] :=
    ListQ[order] && Length[order] === size && VectorQ[order, IntegerQ] &&
        Sort[order] === Range[0, size - 1];

workbenchPrepareSubnetworkHeatmap[subnetwork_Association] := Module[
    {scores, ids, order, queryIndices, dimensions, size, applyOrder, displayOrder, queryPositions},
    scores = associationLookup[subnetwork, {"scores"}, Missing["NotAvailable"]];
    If[! MatrixQ[scores, NumericQ], Return[Missing["InvalidMatrix"]]];
    dimensions = Dimensions[scores];
    If[Length[dimensions] =!= 2 || First[dimensions] =!= Last[dimensions], Return[Missing["InvalidMatrix"]]];
    size = First[dimensions];
    ids = associationLookup[subnetwork, {"ids"}, {}];
    order = associationLookup[subnetwork, {"cluster_order", "clusterOrder"}, {}];
    queryIndices = associationLookup[subnetwork, {"query_indices", "queryIndices"}, {}];
    If[! ListQ[queryIndices], queryIndices = {}];
    queryIndices = DeleteDuplicates @ Select[
        queryIndices,
        IntegerQ[#] && 0 <= # < size &
    ];
    applyOrder = workbenchValidClusterOrderQ[order, size];
    displayOrder = If[applyOrder, order, Range[0, size - 1]];
    queryPositions = Cases[
        queryIndices,
        index_ :> With[{position = FirstPosition[displayOrder, index, Missing["NotAvailable"]]},
            If[MissingQ[position], Nothing, First[position]]
        ]
    ];
    <|
        "Scores" -> scores[[1 + displayOrder, 1 + displayOrder]],
        "IDs" -> If[ListQ[ids] && Length[ids] === size, ids[[1 + displayOrder]], ids],
        "QueryPositions" -> queryPositions,
        "ClusterOrderApplied" -> applyOrder
    |>
];

workbenchMatrixPlot[matrix_, rowIDs_, columnIDs_, title_String, queryPositions_: {}] := Module[
    {rowTicks, columnTicks, dimensions, rowCount, columnCount, validQueryPositions, queryMarkers, tickLabel},
    dimensions = Dimensions[matrix];
    {rowCount, columnCount} = If[Length[dimensions] === 2, dimensions, {0, 0}];
    validQueryPositions = DeleteDuplicates @ Select[
        queryPositions,
        IntegerQ[#] && 1 <= # <= Min[rowCount, columnCount] &
    ];
    tickLabel[label_, position_] := If[
        MemberQ[validQueryPositions, position],
        Style[label, RGBColor[.84, .16, .18], Bold],
        label
    ];
    rowTicks = If[
        ListQ[rowIDs] && Length[rowIDs] <= 30,
        MapIndexed[{First[#2], tickLabel[#1, First[#2]]} &, rowIDs],
        Automatic
    ];
    columnTicks = If[
        ListQ[columnIDs] && Length[columnIDs] <= 30,
        MapIndexed[{First[#2], tickLabel[#1, First[#2]]} &, columnIDs],
        Automatic
    ];
    queryMarkers = Flatten @ Map[
        Function[position,
            {
                Line[{{position - 1, 0}, {position - 1, rowCount}}],
                Line[{{position, 0}, {position, rowCount}}],
                Line[{{0, rowCount - position}, {columnCount, rowCount - position}}],
                Line[{{0, rowCount - position + 1}, {columnCount, rowCount - position + 1}}]
            }
        ],
        validQueryPositions
    ];
    MatrixPlot[
        matrix,
        ColorFunction -> (Blend[{RGBColor[.04, .12, .29], RGBColor[.12, .63, .53], RGBColor[.99, .91, .15]}, #] &),
        PlotLegends -> Automatic,
        FrameTicks -> {{rowTicks, None}, {columnTicks, None}},
        PlotLabel -> title,
        Epilog -> If[
            queryMarkers === {},
            {},
            {Directive[RGBColor[.84, .16, .18], Opacity[.8], AbsoluteThickness[1.5]], queryMarkers}
        ],
        ImageSize -> 750
    ]
];

workbenchDecodeImage[value_] := Module[{bytes, image},
    If[! StringQ[value] || StringLength[value] === 0, Return[Missing["NotAvailable"]]];
    bytes = Quiet @ Check[BaseDecode[value], $Failed];
    If[Head[bytes] =!= ByteArray, Return[Missing["NotAvailable"]]];
    image = Quiet @ Check[ImportByteArray[bytes, "PNG"], $Failed];
    If[image === $Failed, Missing["NotAvailable"], image]
];

workbenchMatricesView[analysis_SynthyraAnalysisObject] := Module[
    {views = <||>, actome, subnetwork, preparedSubnetwork, envelope, decoded, legacy, ids, heatmaps, screen},
    actome = analysis["ActomeEdges"];
    subnetwork = If[AssociationQ[actome], associationLookup[actome, {"subnetwork_heatmap", "subnetworkHeatmap"}, Missing["NotAvailable"]], Missing["NotAvailable"]];
    If[AssociationQ[subnetwork],
        preparedSubnetwork = workbenchPrepareSubnetworkHeatmap[subnetwork];
        If[AssociationQ[preparedSubnetwork], AssociateTo[
            views,
            "Actome subnetwork" -> workbenchMatrixPlot[
                preparedSubnetwork["Scores"],
                preparedSubnetwork["IDs"],
                preparedSubnetwork["IDs"],
                "Clustered actome-neighborhood scores (query rows and columns outlined in red)",
                preparedSubnetwork["QueryPositions"]
            ]
        ]]
    ];

    envelope = analysis["Envelope"];
    If[AssociationQ[envelope],
        decoded = Quiet @ Check[SynthyraScoreMatrix[envelope], $Failed];
        If[AssociationQ[decoded],
            AssociateTo[views, "Expansion candidates" -> workbenchMatrixPlot[
                Normal[decoded["Matrix"]],
                decoded["RowIDs"],
                decoded["ColumnIDs"],
                "Expansion-envelope candidate scores"
            ]],
            legacy = associationLookup[envelope, {"candidate_matrix", "candidateMatrix"}, Missing["NotAvailable"]];
            ids = associationLookup[envelope, {"candidate_ids", "candidateIds"}, {}];
            If[MatrixQ[legacy, NumericQ], AssociateTo[views, "Expansion candidates" -> workbenchMatrixPlot[legacy, ids, ids, "Expansion-envelope candidate scores"]]]
        ]
    ];

    heatmaps = analysis["HeatmapPlots"];
    If[AssociationQ[heatmaps],
        screen = workbenchDecodeImage @ associationLookup[heatmaps, {"screen_b64", "screenB64"}, Missing["NotAvailable"]];
        If[Head[screen] === Image, AssociateTo[views, "Rendered heatmap" -> screen]]
    ];

    If[
        views === <||>,
        workbenchMissingPanel["No matrix or heatmap payload was returned."],
        Column[{
            Panel[
                "These are the returned actome-neighborhood and expansion-envelope views. They are not the internal full (Q+P) by (Q+P) proteome matrix.",
                Background -> RGBColor[1., .98, .9]
            ],
            TabView[Normal[views]]
        }, Spacings -> 1]
    ]
];

workbenchTmapEndpoint[value_, positions_Association, coordinates_List] := Which[
    StringQ[value] && KeyExistsQ[positions, value], positions[value],
    IntegerQ[value] && 0 <= value < Length[coordinates], coordinates[[value + 1]],
    IntegerQ[value] && 1 <= value <= Length[coordinates], coordinates[[value]],
    True, Missing["NotAvailable"]
];

workbenchTmapView[analysis_SynthyraAnalysisObject] := Module[
    {tmap, nodes, edges, validNodes, ids, coordinates, positions, lines, queryIDs, points},
    tmap = analysis["TMAPTree"];
    If[! AssociationQ[tmap], Return @ workbenchMissingPanel["No TMAP layout was returned."]];
    nodes = associationLookup[tmap, {"nodes"}, {}];
    edges = associationLookup[tmap, {"edges"}, {}];
    validNodes = Select[
        nodes,
        AssociationQ[#] &&
            NumericQ[associationLookup[#, {"x"}, Missing["NotAvailable"]]] &&
            NumericQ[associationLookup[#, {"y"}, Missing["NotAvailable"]]] &
    ];
    If[validNodes === {}, Return @ workbenchMissingPanel["The TMAP payload does not contain native coordinates."]];
    ids = ToString[associationLookup[#, {"id", "name"}, "node"]] & /@ validNodes;
    coordinates = ({N @ associationLookup[#, {"x"}], N @ associationLookup[#, {"y"}]} &) /@ validNodes;
    positions = AssociationThread[ids, coordinates];
    lines = Cases[
        edges,
        edge_Association :> With[
            {
                source = workbenchTmapEndpoint[associationLookup[edge, {"source", "i"}], positions, coordinates],
                target = workbenchTmapEndpoint[associationLookup[edge, {"target", "j"}], positions, coordinates]
            },
            If[ListQ[source] && ListQ[target], Line[{source, target}], Nothing]
        ]
    ];
    queryIDs = Select[analysis["QueryIDs"], StringQ];
    points = MapThread[
        Function[{id, coordinate},
            {
                If[MemberQ[queryIDs, id], RGBColor[.84, .16, .18], RGBColor[.18, .49, .72]],
                If[MemberQ[queryIDs, id], PointSize[.018], PointSize[.009]],
                Tooltip[Point[coordinate], id]
            }
        ],
        {ids, coordinates}
    ];
    Column[{
        Panel[
            Row[{Style["TMAP layout tree", Bold], ". The gray links define the layout spanning tree, not biological interactions."}],
            Background -> RGBColor[1., .94, .92]
        ],
        Graphics[
            {{GrayLevel[.72], Opacity[.6], AbsoluteThickness[.7], lines}, points},
            Frame -> True,
            FrameLabel -> {"TMAP x", "TMAP y"},
            PlotRange -> All,
            PlotRangePadding -> Scaled[.04],
            ImageSize -> 850,
            Background -> White
        ]
    }, Spacings -> 1]
];

workbenchSequenceView[analysis_SynthyraAnalysisObject] := Module[{sequence, protein, dfa, foldseek3Di},
    sequence = analysis["Sequence"];
    protein = Replace[analysis["ProteinID"], Missing[__] -> "Protein"];
    dfa = analysis["DFA"];
    foldseek3Di = If[
        AssociationQ[dfa],
        associationLookup[dfa, {"foldseek_3di", "foldseek3di", "three_di"}, Missing["NotAvailable"]],
        Missing["NotAvailable"]
    ];
    If[! StringQ[sequence] || StringLength[sequence] === 0,
        Return @ workbenchMissingPanel["No amino-acid sequence was returned."]
    ];
    Column[{
        Row[{Style[ToString[protein], Bold], "  ", StringLength[sequence], " residues"}],
        Pane[
            Style[StringRiffle[StringPartition[sequence, UpTo[60]], "\n"], FontFamily -> "Consolas", 12],
            ImageSize -> {850, 350},
            Scrollbars -> True
        ],
        If[
            StringQ[foldseek3Di] && StringLength[foldseek3Di] > 0,
            Column[{
                Style["Foldseek 3Di", Bold],
                Pane[
                    Style[StringRiffle[StringPartition[foldseek3Di, UpTo[60]], "\n"], FontFamily -> "Consolas", 12],
                    ImageSize -> {850, 180},
                    Scrollbars -> True
                ]
            }],
            Nothing
        ]
    }, Spacings -> 1]
];

workbenchRawPreview[value_] := Which[
    StringQ[value] && StringLength[value] > 500, "<string: " <> ToString[StringLength[value]] <> " characters>",
    AssociationQ[value], Association @ KeyValueMap[#1 -> workbenchRawPreview[#2] &, value],
    ListQ[value] && Length[value] > 200, Join[workbenchRawPreview /@ Take[value, 200], {"<additional entries omitted from preview>"}],
    ListQ[value], workbenchRawPreview /@ value,
    True, value
];

workbenchRawView[analysis_SynthyraAnalysisObject] := Column[{
    Panel[
        "Large structure and base64 strings are summarized by length in this display. Normal[analysis][\"OriginalResult\"] retains the credential-free normalized payload.",
        Background -> GrayLevel[.97]
    ],
    Pane[Dataset @ workbenchRawPreview[analysis["OriginalResult"]], ImageSize -> {900, 600}, Scrollbars -> True]
}];

workbenchLazyPlaceholder[name_String] := Panel[
    Row[{
        ProgressIndicator[Appearance -> "Percolate"],
        Spacer[8],
        "Rendering " <> name <> "..."
    }],
    Background -> GrayLevel[.98],
    FrameMargins -> 18
];

workbenchLazyTabView[builders_List, imageSize_] := DynamicModule[
    {selected = builders[[1, 1]]},
    TabView[
        Map[
            Function[specification,
                With[
                    {
                        name = specification[[1]],
                        build = specification[[2]]
                    },
                    name -> DynamicModule[
                        {loaded = False, value = Null},
                        Dynamic[
                            If[
                                selected === name,
                                If[! TrueQ[loaded], value = build[]; loaded = True];
                                value,
                                workbenchLazyPlaceholder[name]
                            ],
                            TrackedSymbols :> {selected},
                            SynchronousUpdating -> False
                        ]
                    ]
                ]
            ],
            builders
        ],
        Dynamic[selected],
        ImageSize -> imageSize
    ]
];

Options[SynthyraWorkbench] = {
    ImageSize -> 950,
    GraphLayout -> "SpringElectricalEmbedding"
};

SynthyraWorkbench[input_, OptionsPattern[]] := Module[
    {analysis, builders, imageSize, layout},
    analysis = workbenchCoerceAnalysis[input];
    If[FailureQ[analysis], Return[analysis]];
    imageSize = OptionValue[ImageSize];
    layout = OptionValue[GraphLayout];
    builders = {
        {"Summary", Function[{}, workbenchSummaryView[analysis]]},
        {"Network", Function[{}, workbenchNetworkView[analysis, layout]]},
        {"Structure", Function[{}, workbenchStructureView[analysis]]},
        {"Oracles", Function[{}, workbenchOracleView[analysis]]},
        {"CAMP", Function[{}, workbenchAnnotationView[analysis["CAMPAnnotations"], "CAMP"]]},
        {"Translator", Function[{}, workbenchAnnotationView[analysis["TranslatorAnnotations"], "Translator"]]},
        {"Enrichment", Function[{}, workbenchEnrichmentView[analysis]]},
        {"Matrices", Function[{}, workbenchMatricesView[analysis]]},
        {"TMAP Layout", Function[{}, workbenchTmapView[analysis]]},
        {"Sequence", Function[{}, workbenchSequenceView[analysis]]},
        {"Raw", Function[{}, workbenchRawView[analysis]]}
    };
    If[
        TrueQ[$Notebooks],
        workbenchLazyTabView[builders, imageSize],
        TabView[
            Map[Function[specification, specification[[1]] -> specification[[2]][]], builders],
            ImageSize -> imageSize
        ]
    ]
];
