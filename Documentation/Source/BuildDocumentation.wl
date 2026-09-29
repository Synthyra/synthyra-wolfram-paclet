(* Build the paclet's documentation: a reference page for every public symbol, the guide, and the
   TP53 tutorial.

   Each reference page takes its usage from the symbol's ::usage message, so the text lives in one
   place, and adds the notes and examples below. Examples are evaluated with their outputs stored
   in the page. They replay from Documentation/Source/ExampleRecording.wxf, so a build needs no
   network or key; run with the argument "record" (and SYNTHYRA_API_KEY set) to evaluate them
   against production and rewrite that recording. The tutorial is Examples/TP53.nb evaluated from
   the TP53Demo recording. Rasterizing graphics needs a front end, which wolframscript starts.

       wolframscript -file Documentation/Source/BuildDocumentation.wl [record]
*)

pacletRoot = ExpandFileName @ FileNameJoin[{DirectoryName[$InputFileName], "..", ".."}];
PacletDirectoryLoad[pacletRoot];
Needs["Synthyra`SynthyraLink`"];

englishDirectory = FileNameJoin[{pacletRoot, "Documentation", "English"}];
referenceDirectory = FileNameJoin[{englishDirectory, "ReferencePages", "Symbols"}];
exampleRecording = FileNameJoin[{pacletRoot, "Documentation", "Source", "ExampleRecording.wxf"}];
recordQ = MemberQ[Rest[$ScriptCommandLine], "record"];

(* ---------------------------------------------------------------------------------------------
   What each page says beyond its usage message. "Examples" are evaluated in order in one session;
   "Shown" examples are displayed without evaluation (they need a key, a file, or an LLM). *)

$pages = <|
    "SynthyraConnect" -> <|
        "Notes" -> {
            "The API key comes from the Authentication option, then the SYNTHYRA_API_KEY environment variable, then SystemCredential[\"Synthyra/APIKey\"], then a dialog in a notebook.",
            "The client holds a handle to the key, never the key itself, so it is safe to display, save, or share.",
            "SynthyraConnect sets $SynthyraClient, which every Synthyra function uses by default."
        },
        "Shown" -> {"SynthyraConnect[]", "SynthyraConnect[\"Development\"]", "SystemCredential[\"Synthyra/APIKey\"] = \"your key\";"}
    |>,
    "$SynthyraClient" -> <|
        "Notes" -> {"Every function with a \"Client\" option uses $SynthyraClient when the option is Automatic, and calls SynthyraConnect[] when it is None."},
        "Shown" -> {"SynthyraConnect[]; $SynthyraClient"}
    |>,
    "SynthyraProteinSequence" -> <|
        "Notes" -> {
            "A protein may be an amino-acid string, a peptide BioSequence, a UniProt accession, a gene symbol, an Entity[\"Protein\", ...], or a BioMolecule.",
            "A gene symbol resolves to the reviewed UniProt entry of the organism the \"Organism\" option names, human by default. Strings shorter than 10 residues are read as gene symbols; wrap a short peptide in BioSequence.",
            "label -> protein sets the key a result carries. Lookups are cached for the session."
        },
        "Examples" -> {
            "SynthyraProteinSequence[\"TP53\"]",
            "SynthyraProteinSequence[\"Q00987\"]",
            "SynthyraProteinSequence[\"Trp53\", \"Organism\" -> \"Mouse\"]",
            "SynthyraProteinSequence[{\"p53\" -> \"TP53\", \"MDM2\"}]"
        }
    |>,
    "SynthyraFoldProtein" -> <|
        "Notes" -> {
            "Structures are predicted by ESMFold2 through Synthyra's batch fold service; several proteins fold in one job.",
            "Each atom's B-factor holds the model's confidence, pLDDT, on a 0 to 1 scale; SynthyraStructurePlot colors by it.",
            "\"Output\" -> \"Association\" also gives the model, mean pLDDT from 0 to 100, pTM, per-residue pLDDT by chain, and the PDB text.",
            "\"Model\" selects a served ESMFold2 model and \"Seed\" fixes the sampling seed."
        },
        "Examples" -> {
            "sumo1 = SynthyraFoldProtein[\"SUMO1\"]",
            "SynthyraStructurePlot[sumo1]",
            "SynthyraFoldProtein[\"SUMO1\", \"Output\" -> \"Association\"][[{\"Model\", \"MeanPLDDT\", \"PTM\"}]]"
        }
    |>,
    "SynthyraFoldComplex" -> <|
        "Notes" -> {
            "Chains are lettered A, B, C, ... in the order given; the result is one BioMolecule.",
            "With \"Output\" -> \"Association\", \"IPTM\" is the model's confidence in the interfaces between chains."
        },
        "Examples" -> {
            "complex = SynthyraFoldComplex[{\"SUMO1\", \"UBE2I\"}, \"Output\" -> \"Association\"];\ncomplex[[{\"MeanPLDDT\", \"PTM\", \"IPTM\"}]]",
            "SynthyraStructurePlot[complex, \"ColorBy\" -> \"Chain\"]"
        }
    |>,
    "SynthyraStructurePlot" -> <|
        "Notes" -> {
            "\"ColorBy\" -> \"Confidence\", the default, colors residues in the pLDDT bands AlphaFold and ESMFold report: above 90, 70 to 90, 50 to 70, and below 50.",
            "\"ColorBy\" -> values takes one number per residue, and an Association of lists by chain for a complex; \"Chain\" and None use BioMoleculePlot3D's own coloring.",
            "Any BioMolecule works, including those from ImportString[pdb, \"PDB\"] and BioMoleculeData."
        },
        "Examples" -> {
            "sumo1 = SynthyraFoldProtein[\"SUMO1\"];\nSynthyraStructurePlot[sumo1]",
            "SynthyraStructurePlot[sumo1, \"ColorBy\" -> N @ Range[StringLength[SynthyraProteinSequence[\"SUMO1\"][\"SequenceString\"]]]]"
        }
    |>,
    "SynthyraProteinInteractionScore" -> <|
        "Notes" -> {
            "The score is Atlas-PPI's probability that the two proteins physically interact.",
            "Lists or Associations give every pair as a Dataset, rows from the first argument and columns from the second; large sets are sent in several requests."
        },
        "Examples" -> {
            "SynthyraProteinInteractionScore[\"TP53\", \"MDM2\"]",
            "SynthyraProteinInteractionScore[{\"TP53\", \"MDM2\"}, {\"MDM4\", \"USP7\", \"UBE2I\"}]"
        }
    |>,
    "SynthyraLigandBindingScore" -> <|
        "Notes" -> {
            "The score is Atlas-PLI's probability that the molecule binds the protein.",
            "Chemical names are resolved by Molecule, then by PubChem."
        },
        "Examples" -> {
            "SynthyraLigandBindingScore[\"MDM2\", \"nutlin-3a\"]",
            "SynthyraLigandBindingScore[\"PTGS2\", {\"aspirin\", \"ibuprofen\", \"caffeine\"}]",
            "SynthyraLigandBindingScore[\"PTGS2\", Molecule[\"celecoxib\"]]"
        }
    |>,
    "SynthyraProteinProperties" -> <|
        "Notes" -> {
            "Each row is one Oracle probe. A classifier's \"Prediction\" is its most likely label with that label's \"Probability\", and \"Labels\" lists the leading labels; a regressor's \"Prediction\" is its value.",
            "Oracles read the sequence alone, so they apply to designed and unannotated proteins as well."
        },
        "Examples" -> {"SynthyraProteinProperties[\"SUMO1\"]"}
    |>,
    "SynthyraInteractome" -> <|
        "Notes" -> {
            "Vertices are gene symbols. Each edge carries \"Confidence\" from 0 to 100 as an annotation and as EdgeWeight from 0 to 1, and \"BioGRID\", \"STRING\", and \"Novel\" say whether the interaction is already recorded.",
            "The graph's \"Enrichment\" annotation is a Dataset of the enriched terms of the neighborhood, with each adjusted p-value as its base-10 logarithm.",
            "\"Neighbors\", \"Threshold\", and \"NeighborDepth\" bound the partners kept; organisms are human, mouse, yeast, E. coli, fruit fly, C. elegans, and Arabidopsis. A job takes a few minutes."
        },
        "Examples" -> {
            "g = SynthyraInteractome[\"SUMO1\"]",
            "TakeLargestBy[EdgeList[g, \"SUMO1\" \\[UndirectedEdge] _], AnnotationValue[{g, #}, \"Confidence\"] &, 5]",
            "AnnotationValue[g, \"Enrichment\"][1 ;; 5]"
        }
    |>,
    "SynthyraLLMTools" -> <|
        "Notes" -> {"Each tool takes proteins as UniProt accessions, gene symbols, or sequences and returns text a language model reads."},
        "Examples" -> {"Column @ SynthyraLLMTools[]"},
        "Shown" -> {"LLMSynthesize[\"Does TP53 bind MDM2, and which drugs block it?\", LLMEvaluator -> <|\"Tools\" -> SynthyraLLMTools[]|>]"}
    |>,
    "SynthyraRecord" -> <|
        "Notes" -> {"A recording holds each request's method, URL, and body and each response, never a request header, so it holds no credential."},
        "Shown" -> {"SynthyraRecord[SynthyraProteinInteractionScore[\"TP53\", \"MDM2\"], \"tp53.wxf\"]"}
    |>,
    "SynthyraUseRecording" -> <|
        "Notes" -> {
            "A request made more than once replays its responses in order, then repeats the last; a request the recording lacks gives a \"NotRecorded\" Failure.",
            "With no client connected, replay creates one that needs no key."
        },
        "Shown" -> {"SynthyraUseRecording[\"TP53Demo\"];\nSynthyraProteinInteractionScore[\"TP53\", \"MDM2\" -> \"Q00987\"]", "SynthyraUseRecording[None]"}
    |>
|>;

(* ---------------------------------------------------------------------------------------------
   Evaluation *)

(* Graphics, and any output whose boxes are large, such as a BioMolecule carrying its structure,
   become images, which keeps a page small and renders without the kernel. *)
$largeOutputBytes = 200000;

outputBoxes[value_] := Module[{boxes = ToBoxes[value, StandardForm]},
    If[! FreeQ[value, _Graphics3D | _Graphics | _Graph | _Legended] || ByteCount[boxes] > $largeOutputBytes,
        boxes = ToBoxes[UsingFrontEnd @ Rasterize[value, ImageResolution -> 96], StandardForm]
    ];
    boxes
];

(* The front end's own parse of the code, so inputs display as typed. *)
inputBoxes[code_String] := Replace[
    UsingFrontEnd @ FrontEndExecute[FrontEnd`UndocumentedTestFEParserPacket[code, True]],
    {{BoxData[boxes_], ___} :> boxes, _ :> code}
];

exampleCells[name_String, codes_List, evaluateQ_] := Flatten @ MapIndexed[
    Function[{code, index}, Module[{value},
        If[! evaluateQ,
            Return[Cell[BoxData[inputBoxes[code]], "Input", CellID -> Hash[{name, code}, "CRC32"]], Module]
        ];
        value = ToExpression[code];
        If[FailureQ[value], Print["::error::", name, " example failed: ", code, ": ", value["Message"]]; Exit[1]];
        {
            Cell[BoxData[inputBoxes[code]], "Input", CellLabel -> "In[" <> ToString[First[index]] <> "]:=", CellID -> Hash[{name, code}, "CRC32"]],
            If[value === Null, Nothing,
                Cell[BoxData[outputBoxes[value]], "Output", CellLabel -> "Out[" <> ToString[First[index]] <> "]=", CellID -> Hash[{name, code, "Output"}, "CRC32"]]]
        }
    ]],
    codes
];

(* ---------------------------------------------------------------------------------------------
   Reference pages *)

documentationCellID[parts___String] := Hash[{"Synthyra/SynthyraLink", parts}, "CRC32"];

(* A usage line's leading call, as an inline formula, and its description. *)
usageCell[name_String, line_String] := Module[{depth = 0, end = None, characters = Characters[line]},
    Do[
        Switch[characters[[i]], "[", depth++, "]", depth--; If[depth == 0 && end === None, end = i]],
        {i, Length[characters]}
    ];
    If[end === None,
        Cell[line, "Usage"],
        Cell[TextData[{
            Cell[BoxData[inputBoxes[StringTake[line, end]]], "InlineFormula"],
            " " <> StringTrim[StringDrop[line, end]]
        }], "Usage", CellID -> documentationCellID[name, line]]
    ]
];

categorization[name_String] := Cell[CellGroupData[{
    Cell["Categorization", "CategorizationSection", CellID -> documentationCellID[name, "CategorizationSection"]],
    Cell["Symbol", "Categorization", CellLabel -> "Entity Type", CellID -> documentationCellID[name, "EntityType"]],
    Cell["Synthyra/SynthyraLink", "Categorization", CellLabel -> "Paclet Name", CellID -> documentationCellID[name, "PacletName"]],
    Cell["Synthyra`SynthyraLink`", "Categorization", CellLabel -> "Context", CellID -> documentationCellID[name, "Context"]],
    Cell["Synthyra/SynthyraLink/ref/" <> name, "Categorization", CellLabel -> "URI", CellID -> documentationCellID[name, "URI"]]
}, Closed]];

referencePage[name_String] := Module[{usage, page = Lookup[$pages, name, <||>]},
    usage = ToExpression["Synthyra`SynthyraLink`" <> name <> "::usage"];
    If[! StringQ[usage], Print["::error::", name, " has no usage message"]; Exit[1]];
    Notebook[Flatten @ {
        Cell[name, "ObjectName", CellID -> documentationCellID[name, "ObjectName"]],
        usageCell[name, #] & /@ StringSplit[usage, "\n"],
        Cell["Details", "NotesSection", CellID -> documentationCellID[name, "Notes"]],
        Cell[#, "Notes", CellID -> documentationCellID[name, #]] & /@ Lookup[page, "Notes", {}],
        Cell["Examples", "PrimaryExamplesSection", CellID -> documentationCellID[name, "Examples"]],
        Cell[BoxData[inputBoxes["Needs[\"Synthyra`SynthyraLink`\"]"]], "Input", CellID -> documentationCellID[name, "Needs"]],
        exampleCells[name, Lookup[page, "Examples", {}], True],
        exampleCells[name, Lookup[page, "Shown", {}], False],
        Cell["Related Guides", "MoreAboutSection", CellID -> documentationCellID[name, "Guides"]],
        Cell[TextData[ButtonBox["SynthyraLink", BaseStyle -> "Link", ButtonData -> "paclet:Synthyra/SynthyraLink/guide/SynthyraLink"]], "MoreAbout", CellID -> documentationCellID[name, "GuideLink"]],
        categorization[name]
    },
        Saveable -> False,
        WindowTitle -> name,
        TaggingRules -> <|"Paclet" -> "Synthyra/SynthyraLink"|>,
        StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "FunctionPageStylesExt.nb", CharacterEncoding -> "UTF-8"]
    ]
];

(* ---------------------------------------------------------------------------------------------
   Guide *)

link[name_String] := ButtonBox[name, BaseStyle -> "Link", ButtonData -> "paclet:Synthyra/SynthyraLink/ref/" <> name];

guideRow[names_List, text_String] := Cell[
    TextData[Flatten @ {Riffle[link /@ names, ", "], " \[LongDash] " <> text}],
    "GuideText",
    CellID -> documentationCellID["guide", First[names]]
];

$guideSections = {
    {"Proteins and structures", {
        {{"SynthyraProteinSequence"}, "a protein from a gene symbol, UniProt accession, entity, or sequence"},
        {{"SynthyraFoldProtein", "SynthyraFoldComplex"}, "predicted structures as BioMolecule objects, with per-residue confidence"},
        {{"SynthyraStructurePlot"}, "3D structures colored by confidence or by any per-residue values"}
    }},
    {"Interactions and properties", {
        {{"SynthyraProteinInteractionScore"}, "the probability that two proteins interact, for single pairs or whole sets"},
        {{"SynthyraLigandBindingScore"}, "the probability that a small molecule binds a protein"},
        {{"SynthyraInteractome"}, "a protein's predicted partners across its proteome as a Graph, with known interactions and enrichment"},
        {{"SynthyraProteinProperties"}, "predicted solubility, stability, localization, function, and more"}
    }},
    {"Language models", {
        {{"SynthyraLLMTools"}, "the functions above as LLMTool objects"}
    }},
    {"Sessions", {
        {{"SynthyraConnect", "$SynthyraClient"}, "connect with an API key, which never enters an expression"},
        {{"SynthyraRecord", "SynthyraUseRecording"}, "record a session and replay it offline"}
    }},
    {"The Synthyra API", {
        {{"SynthyraExecute", "SynthyraClientObject"}, "call any operation of the Synthyra API"},
        {{"SynthyraJobObject", "SynthyraJobStatus", "SynthyraWait", "SynthyraJobResult", "SynthyraCancelJob"}, "follow and collect asynchronous jobs"},
        {{"SynthyraGraph", "SynthyraHypergraphData", "SynthyraScoreMatrix"}, "convert networks and score matrices to Wolfram objects"},
        {{"SynthyraAnalysis", "SynthyraAnalysisObject", "SynthyraWorkbench", "SynthyraOracleDataset", "SynthyraEnrichmentDataset"}, "explore a coordinated analysis result"}
    }}
};

guideNotebook[] := Notebook[Flatten @ {
    Cell["SynthyraLink", "GuideTitle", CellID -> documentationCellID["guide", "title"]],
    Cell["SynthyraLink brings Synthyra's protein models into the Wolfram Language: structures as BioMolecule objects, interaction networks as Graph objects, and predictions as Dataset objects, from a gene symbol, a UniProt accession, or a sequence.", "GuideAbstract", CellID -> documentationCellID["guide", "abstract"]],
    Map[
        Function[section, {
            Cell[First[section], "GuideFunctionsSubsection", CellID -> documentationCellID["guide", First[section]]],
            guideRow @@@ Last[section]
        }],
        $guideSections
    ],
    Cell["Tutorials", "GuideTutorialsSection", CellID -> documentationCellID["guide", "tutorials"]],
    Cell[TextData[ButtonBox["TP53 with Synthyra", BaseStyle -> "Link", ButtonData -> "paclet:Synthyra/SynthyraLink/tutorial/TP53WithSynthyra"]], "GuideTutorial", CellID -> documentationCellID["guide", "tp53"]]
},
    Saveable -> False,
    WindowTitle -> "SynthyraLink",
    TaggingRules -> <|"Paclet" -> "Synthyra/SynthyraLink"|>,
    StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "GuidePageStylesExt.nb", CharacterEncoding -> "UTF-8"]
];

(* ---------------------------------------------------------------------------------------------
   Tutorial: Examples/TP53.nb, evaluated from its recording. *)

tutorialNotebook[] := Module[{source, counter = 0},
    source = Get[FileNameJoin[{pacletRoot, "Examples", "TP53.nb"}]];
    SynthyraUseRecording["TP53Demo"];
    Notebook[
        Flatten @ Replace[
            First[source],
            {
                Cell[code_String, "Input", ___, CellTags -> tags_, ___] /; IntersectingQ[Flatten[{tags}], {"Live", "LLM"}] :>
                    Cell[BoxData[inputBoxes[code]], "Input"],
                Cell[code_String, "Input", ___] :> Module[{value = ToExpression[code]},
                    If[FailureQ[value], Print["::error::tutorial cell failed: ", code]; Exit[1]];
                    counter++;
                    {
                        Cell[BoxData[inputBoxes[code]], "Input", CellLabel -> "In[" <> ToString[counter] <> "]:="],
                        If[value === Null, Nothing, Cell[BoxData[outputBoxes[value]], "Output", CellLabel -> "Out[" <> ToString[counter] <> "]="]]
                    }
                ],
                Cell[text_String, "Title", ___] :> Cell[text, "TechNoteTitle"],
                Cell[text_String, "Subtitle", ___] :> Cell[text, "TechNoteText"],
                Cell[text_String, "Section", ___] :> Cell[text, "TechNoteSection"],
                Cell[text_String, "Text", ___] :> Cell[text, "TechNoteText"]
            },
            {1}
        ],
        Saveable -> False,
        WindowTitle -> "TP53 with Synthyra",
        TaggingRules -> <|"Paclet" -> "Synthyra/SynthyraLink"|>,
        StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "TechNotePageStylesExt.nb", CharacterEncoding -> "UTF-8"]
    ]
];

(* ---------------------------------------------------------------------------------------------
   Build *)

publicSymbols = StringDelete[#, "Synthyra`SynthyraLink`"] & /@ Select[
    Names["Synthyra`SynthyraLink`*"],
    StringQ @ ToExpression[# <> "::usage"] &
];

(* Saved through the front end, which stores images compressed rather than as pixel lists. *)
writeNotebook[path_String, notebook_Notebook] := (
    Quiet @ CreateDirectory[DirectoryName[path], CreateIntermediateDirectories -> True];
    UsingFrontEnd @ Module[{object = NotebookPut[notebook, Visible -> False]},
        NotebookSave[object, path];
        NotebookClose[object]
    ];
    Print[FileNameTake[path], ": ", FileByteCount[path], " bytes"]
);

buildReferencePages[] := Scan[
    writeNotebook[FileNameJoin[{referenceDirectory, # <> ".nb"}], referencePage[#]] &,
    publicSymbols
];

If[recordQ,
    SynthyraConnect[];
    SynthyraRecord[buildReferencePages[], exampleRecording],
    SynthyraUseRecording[exampleRecording];
    buildReferencePages[]
];
SynthyraUseRecording[None];

writeNotebook[FileNameJoin[{englishDirectory, "Guides", "SynthyraLink.nb"}], guideNotebook[]];
writeNotebook[FileNameJoin[{englishDirectory, "Tutorials", "TP53WithSynthyra.nb"}], tutorialNotebook[]];
SynthyraUseRecording[None];
