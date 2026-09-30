(* Protein-level functions: one call per question, with Wolfram types in and out.

   Every function takes "Client" -> Automatic, which uses $SynthyraClient, the client the latest
   SynthyraConnect made, and connects to production when there is none. A protein may be given as
   an amino-acid sequence, a BioSequence, a UniProt accession, a gene symbol, an
   Entity["Protein", ...], or label -> protein to set the label its results carry. A list or an
   Association of proteins asks for every one of them. *)

$SynthyraClient = None;

proteinFailure[tag_String, template_String, parameters_Association : <||>] := Failure[
    tag,
    <|"MessageTemplate" -> template, "MessageParameters" -> parameters|>
];

resolveClient[Automatic] := If[
    MatchQ[$SynthyraClient, _SynthyraClientObject],
    $SynthyraClient,
    SynthyraConnect[]
];
resolveClient[client_SynthyraClientObject] := client;
resolveClient[other_] := proteinFailure[
    "InvalidClient",
    "\"Client\" must be Automatic or a SynthyraClientObject, not `Client`.",
    <|"Client" -> Head[other]|>
];

firstFailure[values_List] := SelectFirst[values, FailureQ, None];

(* A bad input as a failure message shows it: its InputForm, cut to 60 characters. *)
shortForm[value_] := StringTake[ToString[value, InputForm], UpTo[60]];

(* ---------------------------------------------------------------------------------------------
   Organisms: the UniProt taxon a gene symbol is looked up in, and the name the interactome
   service knows the reference proteome by. *)

$synthyraOrganisms = <|
    "human" -> {9606, "human"}, "homosapiens" -> {9606, "human"},
    "mouse" -> {10090, "mouse"}, "musmusculus" -> {10090, "mouse"},
    "yeast" -> {559292, "yeast"}, "saccharomycescerevisiae" -> {559292, "yeast"},
    "ecoli" -> {83333, "ecoli"}, "escherichiacoli" -> {83333, "ecoli"},
    "fruitfly" -> {7227, "fruit_fly"}, "drosophilamelanogaster" -> {7227, "fruit_fly"},
    "celegans" -> {6239, "c_elegans"}, "caenorhabditiselegans" -> {6239, "c_elegans"},
    "arabidopsis" -> {3702, "arabidopsis"}, "arabidopsisthaliana" -> {3702, "arabidopsis"}
|>;

resolveOrganism[name_String] := Replace[
    Lookup[$synthyraOrganisms, canonicalName[name], Missing["UnknownOrganism"]],
    {
        {taxon_, service_} :> <|"Taxon" -> taxon, "Service" -> service|>,
        _Missing :> proteinFailure[
            "UnknownOrganism",
            "Unknown organism `Organism`; use one of `Known`.",
            <|"Organism" -> name, "Known" -> {"Human", "Mouse", "Yeast", "E. coli", "Fruit fly", "C. elegans", "Arabidopsis"}|>
        ]
    }
];
resolveOrganism[taxon_Integer?Positive] := Module[{match},
    match = SelectFirst[Values[$synthyraOrganisms], First[#] === taxon &, Missing["NotServed"]];
    <|"Taxon" -> taxon, "Service" -> If[MissingQ[match], match, Last[match]]|>
];
resolveOrganism[other_] := proteinFailure[
    "UnknownOrganism",
    "\"Organism\" must be an organism name or a positive NCBI taxon id, not `Organism`.",
    <|"Organism" -> other|>
];

(* ---------------------------------------------------------------------------------------------
   Proteins. A resolved protein is <|"ID" -> label or Automatic, "Sequence" -> residues|>. *)

$uniProtURL = "https://rest.uniprot.org/uniprotkb/";
$uniProtAccession = RegularExpression[
    "([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-[0-9]+)?"
];
$aminoAcids = RegularExpression["[ACDEFGHIKLMNPQRSTVWYXBZUO]+"];
$geneSymbol = RegularExpression["[A-Za-z0-9][A-Za-z0-9._-]{0,24}"];
(* An amino-acid string shorter than this is read as a gene symbol; wrap a short peptide in
   BioSequence["Peptide", ...] to fold or score it. *)
$minimumBareSequenceLength = 10;
$synthyraSequenceCache = <||>;

(* Outside lookups go through the paclet's transport, so a recording carries them too. *)
publicRead[url_String] := Quiet @ Check[$SynthyraHTTPTransport[HTTPRequest[url], 60], $Failed];

uniProtText[url_String, what_String] := Module[{response},
    response = publicRead[url];
    Which[
        ! AssociationQ[response],
            proteinFailure["UniProtUnavailable", "UniProt could not be reached while looking up `What`.", <|"What" -> what|>],
        response["StatusCode"] =!= 200,
            proteinFailure[
                "UniProtLookupFailed",
                "UniProt answered HTTP `Status` while looking up `What`.",
                <|"Status" -> response["StatusCode"], "What" -> what|>
            ],
        True, response["Body"]
    ]
];

fastaSequence[text_String] := StringJoin @ Select[StringSplit[text, {"\r\n", "\n"}], ! StringStartsQ[#, ">"] &];

cachedSequence[key_, compute_] := Module[{value},
    If[KeyExistsQ[$synthyraSequenceCache, key], Return[$synthyraSequenceCache[key]]];
    value = compute[];
    If[! FailureQ[value], $synthyraSequenceCache[key] = value];
    value
];

accessionSequence[accession_String] := cachedSequence[{"UniProt", accession}, Function[
    Module[{text = uniProtText[$uniProtURL <> accession <> ".fasta", "UniProt entry " <> accession], sequence},
        If[FailureQ[text], Return[text, Module]];
        sequence = fastaSequence[text];
        If[sequence === "",
            proteinFailure["UniProtNotFound", "UniProt has no sequence for `Accession`.", <|"Accession" -> accession|>],
            sequence
        ]
    ]
]];

(* The reviewed entry of a gene: <|"Accession" -> ..., "Sequence" -> ...|>. *)
geneEntry[symbol_String, organism_Association] := cachedSequence[{"Gene", ToUpperCase[symbol], organism["Taxon"]}, Function[
    Module[{query, text, rows},
        query = URLQueryEncode[{
            "query" -> "gene_exact:" <> symbol <> " AND organism_id:" <> ToString[organism["Taxon"]] <> " AND reviewed:true",
            "fields" -> "accession,sequence",
            "format" -> "tsv",
            "size" -> "1"
        }];
        text = uniProtText[$uniProtURL <> "search?" <> query, "gene " <> symbol];
        If[FailureQ[text], Return[text, Module]];
        rows = StringSplit[#, "\t"] & /@ Rest[StringSplit[text, {"\r\n", "\n"}]];
        If[rows === {} || Length[First[rows]] < 2,
            proteinFailure[
                "GeneNotFound",
                "No reviewed UniProt entry has gene symbol `Gene` in taxon `Taxon`.",
                <|"Gene" -> symbol, "Taxon" -> organism["Taxon"]|>
            ],
            <|"Accession" -> First[rows][[1]], "Sequence" -> First[rows][[2]]|>
        ]
    ]
]];

(* A resolved protein keeps its UniProt accession when it has one, which the interactome sends as
   the query's id so the service can match it against known interactions. *)
proteinRecord[id_, sequence_String] := <|"ID" -> id, "Sequence" -> ToUpperCase[sequence]|>;
proteinRecord[id_, entry_Association] := <|"ID" -> id, "Sequence" -> ToUpperCase[entry["Sequence"]], "Accession" -> entry["Accession"]|>;
proteinRecord[_, failure_?FailureQ] := failure;

resolveProtein[label_String -> spec_, organism_] := Replace[
    resolveProtein[spec, organism],
    record_Association :> Append[record, "ID" -> label]
];
resolveProtein[BioSequence["Peptide", sequence_String, ___], _] := proteinRecord[Automatic, sequence];
resolveProtein[sequence_BioSequence, _] := proteinFailure[
    "NotAProtein",
    "Only peptide BioSequence objects are proteins; this one is `Type`.",
    <|"Type" -> First[sequence]|>
];
resolveProtein[entity : Entity["Protein", _], _] := proteinRecord[
    ToString @ CanonicalName[entity],
    cachedSequence[{"Entity", entity}, Function[
        Replace[Quiet @ EntityValue[entity, "Sequence"], {
            sequence_String :> sequence,
            BioSequence["Peptide", sequence_String, ___] :> sequence,
            _ :> proteinFailure["EntityWithoutSequence", "`Entity` has no protein sequence.", <|"Entity" -> entity|>]
        }]
    ]]
];
resolveProtein[molecule_BioMolecule, _] := Module[{peptides},
    peptides = Cases[Values @ Quiet @ molecule["BioSequences"], BioSequence["Peptide", sequence_String, ___] :> sequence];
    If[peptides === {},
        proteinFailure["NotAProtein", "The BioMolecule has no peptide chain."],
        proteinRecord[Automatic, First[peptides]]
    ]
];
resolveProtein[text_String, organism_] := Module[{compact = StringDelete[text, WhitespaceCharacter], organismData},
    Which[
        StringMatchQ[compact, $uniProtAccession],
            proteinRecord[compact, Replace[accessionSequence[compact], sequence_String :> <|"Accession" -> compact, "Sequence" -> sequence|>]],
        StringLength[compact] >= $minimumBareSequenceLength && StringMatchQ[ToUpperCase[compact], $aminoAcids],
            proteinRecord[Automatic, compact],
        StringMatchQ[compact, $geneSymbol],
            organismData = resolveOrganism[organism];
            If[FailureQ[organismData], organismData, proteinRecord[compact, geneEntry[compact, organismData]]],
        True,
            proteinFailure[
                "UnrecognizedProtein",
                "`Input` is not an amino-acid sequence, UniProt accession, or gene symbol.",
                <|"Input" -> shortForm[text]|>
            ]
    ]
];
resolveProtein[other_, _] := proteinFailure[
    "UnrecognizedProtein",
    "Cannot use `Input` as a protein; give a sequence, BioSequence, UniProt accession, gene symbol, or Entity[\"Protein\", ...].",
    <|"Input" -> shortForm[other]|>
];

manyQ[input_] := ListQ[input] || AssociationQ[input];
inputList[input_Association] := Normal[input];
inputList[input_List] := input;
inputList[input_] := {input};

(* Labels for results: an unlabelled item takes prefix <> its position, and a repeated label its
   position in brackets, so no two results share a key. *)
labelRecords[records_List, prefix_String] := Module[{labelled, counts},
    labelled = MapIndexed[
        If[#1["ID"] === Automatic, Append[#1, "ID" -> prefix <> ToString[First[#2]]], #1] &,
        records
    ];
    counts = Counts[#["ID"] & /@ labelled];
    MapIndexed[
        If[counts[#1["ID"]] > 1, Append[#1, "ID" -> #1["ID"] <> " (" <> ToString[First[#2]] <> ")"], #1] &,
        labelled
    ]
];

resolveProteins[input_, organism_] := Module[{records = resolveProtein[#, organism] & /@ inputList[input], failure},
    failure = firstFailure[records];
    If[failure =!= None, failure, labelRecords[records, "protein "]]
];

Options[SynthyraProteinSequence] = {"Organism" -> "Human"};

SynthyraProteinSequence[input_?manyQ, opts : OptionsPattern[]] := Module[{records},
    records = resolveProteins[input, OptionValue["Organism"]];
    If[FailureQ[records], records, Association[(#["ID"] -> BioSequence["Peptide", #["Sequence"]]) & /@ records]]
];
SynthyraProteinSequence[input_, OptionsPattern[]] := Replace[
    resolveProtein[input, OptionValue["Organism"]],
    record_Association :> BioSequence["Peptide", record["Sequence"]]
];

(* ---------------------------------------------------------------------------------------------
   Ligands. A resolved ligand is <|"ID" -> label or Automatic, "SMILES" -> text|>. *)

$synthyraLigandCache = <||>;

smilesQ[text_String] := ! StringContainsQ[text, WhitespaceCharacter] && ListQ[Quiet @ ImportString[text, "SMILES"]];

pubChemSMILES[name_String] := Module[{response, smiles},
    If[KeyExistsQ[$synthyraLigandCache, name], Return[$synthyraLigandCache[name]]];
    response = publicRead[
        "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/" <> URLEncode[name] <> "/property/IsomericSMILES/TXT"
    ];
    smiles = If[AssociationQ[response] && response["StatusCode"] === 200,
        First[StringSplit[StringTrim[response["Body"]], {"\r\n", "\n"}], Missing["NotFound"]],
        Missing["NotFound"]
    ];
    If[StringQ[smiles], $synthyraLigandCache[name] = smiles];
    smiles
];

resolveLigand[label_String -> spec_] := Replace[resolveLigand[spec], record_Association :> Append[record, "ID" -> label]];
resolveLigand[molecule_Molecule] := Module[{smiles = Quiet @ molecule["SMILES"]},
    If[StringQ[smiles],
        <|"ID" -> Automatic, "SMILES" -> smiles|>,
        proteinFailure["InvalidMolecule", "The Molecule has no SMILES representation."]
    ]
];
resolveLigand[entity : Entity["Chemical", _]] := Replace[
    resolveLigand[Quiet @ Molecule[entity]],
    record_Association :> Append[record, "ID" -> ToString @ CanonicalName[entity]]
];
resolveLigand[text_String] := Module[{trimmed = StringTrim[text], molecule, smiles},
    If[smilesQ[trimmed], Return[<|"ID" -> Automatic, "SMILES" -> trimmed|>]];
    molecule = Quiet @ Molecule[trimmed];
    smiles = If[MoleculeQ[molecule], Quiet @ molecule["SMILES"], pubChemSMILES[trimmed]];
    If[StringQ[smiles],
        <|"ID" -> trimmed, "SMILES" -> smiles|>,
        proteinFailure["UnrecognizedLigand", "`Input` is neither SMILES nor a chemical name Wolfram or PubChem knows.", <|"Input" -> trimmed|>]
    ]
];
resolveLigand[other_] := proteinFailure[
    "UnrecognizedLigand",
    "Cannot use `Input` as a ligand; give SMILES, a chemical name, a Molecule, or Entity[\"Chemical\", ...].",
    <|"Input" -> shortForm[other]|>
];

resolveLigands[input_] := Module[{records = resolveLigand /@ inputList[input], failure},
    failure = firstFailure[records];
    If[failure =!= None, failure, labelRecords[records, "ligand "]]
];

(* ---------------------------------------------------------------------------------------------
   Jobs. In a notebook a job shows what it is waiting for. *)

awaitResult[job_, label_String, timeout_] := If[
    TrueQ[$Notebooks],
    Monitor[
        SynthyraJobResult[job, Timeout -> timeout],
        Row[{ProgressIndicator[Appearance -> "Necklace"], "  ", label}]
    ],
    SynthyraJobResult[job, Timeout -> timeout]
];

awaitJob[job_, label_String, timeout_] := If[
    TrueQ[$Notebooks],
    Monitor[
        SynthyraWait[job, Timeout -> timeout],
        Row[{ProgressIndicator[Appearance -> "Necklace"], "  ", label}]
    ],
    SynthyraWait[job, Timeout -> timeout]
];

(* ---------------------------------------------------------------------------------------------
   Interaction scores. The score routes pair inputs_a[[i]] with inputs_b[[i]], and take at most
   1000 pairs and 400 distinct sequences per request, so an all-pairs question is sent in chunks. *)

$scorePairLimit = 1000;
(* Seconds a synchronous scoring or Oracle request may take; a cold model container needs minutes. *)
$synchronousTimeout = 600.;
$scoreSequenceLimit = 400;

scoreChunks[pairs_List, recordsA_List, recordsB_List] := Module[{chunks = {}, current = {}, seen = <||>, keys},
    Do[
        keys = {"a" -> recordsA[[pair[[1]], "Key"]], "b" -> recordsB[[pair[[2]], "Key"]]};
        If[
            Length[current] >= $scorePairLimit ||
                Length[Union[Keys[seen], keys]] > $scoreSequenceLimit,
            AppendTo[chunks, current]; current = {}; seen = <||>
        ];
        AppendTo[current, pair];
        Scan[(seen[#] = True) &, keys],
        {pair, pairs}
    ];
    If[current =!= {}, AppendTo[chunks, current]];
    chunks
];

scorePairs[client_, operation_String, recordsA_List, recordsB_List, pairs_List] := Module[{chunks, scores},
    chunks = scoreChunks[pairs, recordsA, recordsB];
    scores = Map[
        Function[chunk, Module[{response, values},
            response = client[operation, <|
                "inputs_a" -> (recordsA[[#[[1]], "Input"]] & /@ chunk),
                "inputs_b" -> (recordsB[[#[[2]], "Input"]] & /@ chunk),
                "ids_a" -> (recordsA[[#[[1]], "ID"]] & /@ chunk),
                "ids_b" -> (recordsB[[#[[2]], "ID"]] & /@ chunk)
            |>, Timeout -> $synchronousTimeout];
            If[FailureQ[response], Return[response, Module]];
            values = associationLookup[response, {"scores"}, Missing["NotAvailable"]];
            If[! VectorQ[values, NumericQ] || Length[values] =!= Length[chunk],
                proteinFailure["UnexpectedScores", "`Operation` returned `Count` scores for `Pairs` pairs.",
                    <|"Operation" -> operation, "Count" -> If[ListQ[values], Length[values], 0], "Pairs" -> Length[chunk]|>],
                values
            ]
        ]],
        chunks
    ];
    If[firstFailure[scores] =!= None, firstFailure[scores], Flatten[scores]]
];

scoreTable[operation_String, a_, b_, recordsA_, recordsB_, clientOption_] := Module[{client, pairs, scores},
    If[FailureQ[recordsA], Return[recordsA]];
    If[FailureQ[recordsB], Return[recordsB]];
    client = resolveClient[clientOption];
    If[FailureQ[client], Return[client]];
    pairs = Tuples[{Range[Length[recordsA]], Range[Length[recordsB]]}];
    scores = scorePairs[client, operation, recordsA, recordsB, pairs];
    If[FailureQ[scores], Return[scores]];
    If[! manyQ[a] && ! manyQ[b],
        First[scores],
        Dataset @ AssociationThread[
            #["ID"] & /@ recordsA,
            AssociationThread[#["ID"] & /@ recordsB, #] & /@ Partition[scores, Length[recordsB]]
        ]
    ]
];

withInput[records_List, key_String] := Append[#, <|"Input" -> #[key], "Key" -> #[key]|>] & /@ records;
withInput[failure_, _] := failure;

Options[SynthyraProteinInteractionScore] = {"Client" -> Automatic, "Organism" -> "Human"};

SynthyraProteinInteractionScore[a_, b_, OptionsPattern[]] := scoreTable[
    "ScoreAtlasPPI",
    a,
    b,
    withInput[resolveProteins[a, OptionValue["Organism"]], "Sequence"],
    withInput[resolveProteins[b, OptionValue["Organism"]], "Sequence"],
    OptionValue["Client"]
];

Options[SynthyraLigandBindingScore] = {"Client" -> Automatic, "Organism" -> "Human"};

SynthyraLigandBindingScore[proteins_, ligands_, OptionsPattern[]] := scoreTable[
    "ScoreAtlasPLI",
    proteins,
    ligands,
    withInput[resolveProteins[proteins, OptionValue["Organism"]], "Sequence"],
    withInput[resolveLigands[ligands], "SMILES"],
    OptionValue["Client"]
];

(* ---------------------------------------------------------------------------------------------
   Structures. Folds go through the served batch route, one job for every protein asked about;
   each result is a BioMolecule whose B-factors hold ESMFold2's per-atom confidence (pLDDT). *)

foldOptions[model_, seed_] := Association @ Join[
    If[model === Automatic, {}, {"model" -> model}],
    If[seed === Automatic, {}, {"seed" -> seed}]
];

pdbBioMolecule[pdb_String, title_String] := Module[{molecule},
    molecule = Quiet @ Check[ImportString["TITLE     " <> StringTake[title, UpTo[60]] <> "\n" <> pdb, "PDB"], $Failed];
    If[Head[molecule] === BioMolecule,
        molecule,
        proteinFailure["StructureImportFailed", "Wolfram Language could not read the structure predicted for `Name`.", <|"Name" -> title|>]
    ]
];

residueConfidence[molecule_BioMolecule] := Module[{factors = Quiet @ molecule["BFactors"], percent},
    If[! AssociationQ[factors], Return[<||>]];
    factors = Map[Mean[N @ Normal[#]] &, factors, {2}];
    percent = If[Max[Flatten[Values[factors]], 0] <= 1.5, 100., 1.];
    percent * factors
];

foldOutput[record_Association, output_String] := Module[{rows, row, molecule, name},
    name = ToString @ associationLookup[record, {"item_id"}, "structure"];
    If[associationLookup[record, {"status"}, ""] =!= "ok",
        Return @ proteinFailure[
            "FoldFailed",
            "Synthyra could not fold `Name`: `Reason`",
            <|"Name" -> name, "Reason" -> ToString @ associationLookup[record, {"error", "message"}, "no reason given"]|>
        ]
    ];
    rows = associationLookup[associationLookup[record, {"result"}, <||>], {"rows"}, {}];
    row = SelectFirst[rows, AssociationQ[#] && associationLookup[#, {"status"}, ""] === "ok" &, Missing["NotAvailable"]];
    If[MissingQ[row] || ! StringQ[associationLookup[row, {"pdb_string"}, Missing["NotAvailable"]]],
        Return @ proteinFailure["FoldFailed", "The fold of `Name` returned no structure.", <|"Name" -> name|>]
    ];
    molecule = pdbBioMolecule[row["pdb_string"], name];
    If[FailureQ[molecule] || output === "BioMolecule", Return[molecule]];
    <|
        "Structure" -> molecule,
        "Name" -> name,
        "Model" -> associationLookup[row, {"model_id", "model"}, Missing["NotAvailable"]],
        "MeanPLDDT" -> 100. associationLookup[row, {"plddt"}, Missing["NotAvailable"]],
        "PTM" -> associationLookup[row, {"ptm"}, Missing["NotAvailable"]],
        "IPTM" -> associationLookup[row, {"iptm"}, Missing["NotAvailable"]],
        "ResiduePLDDT" -> residueConfidence[molecule],
        "PDB" -> row["pdb_string"]
    |>
];

runFold[clientOption_, items_List, options_Association, output_, timeout_, label_String] := Module[
    {client, job, records},
    If[! MemberQ[{"BioMolecule", "Association"}, output],
        Return @ proteinFailure["InvalidOption", "\"Output\" must be \"BioMolecule\" or \"Association\"."]
    ];
    client = resolveClient[clientOption];
    If[FailureQ[client], Return[client]];
    job = client["SubmitFoldBatch", Join[<|"items" -> items|>, If[options === <||>, <||>, <|"options" -> options|>]]];
    If[FailureQ[job], Return[job]];
    records = awaitResult[job, label, timeout];
    If[FailureQ[records], Return[records]];
    If[! ListQ[records] || Length[records] =!= Length[items],
        Return @ proteinFailure["UnexpectedFoldResult", "The fold job returned `Count` results for `Items` inputs.",
            <|"Count" -> If[ListQ[records], Length[records], 0], "Items" -> Length[items]|>]
    ];
    foldOutput[#, output] & /@ SortBy[records, associationLookup[#, {"item_index"}, 0] &]
];

Options[SynthyraFoldProtein] = {
    "Client" -> Automatic,
    "Organism" -> "Human",
    "Model" -> Automatic,
    "Seed" -> Automatic,
    "Output" -> "BioMolecule",
    Timeout -> 1800.
};

SynthyraFoldProtein[input_, OptionsPattern[]] := Module[{records, results},
    records = resolveProteins[input, OptionValue["Organism"]];
    If[FailureQ[records], Return[records]];
    results = runFold[
        OptionValue["Client"],
        <|"id" -> #["ID"], "sequence" -> #["Sequence"]|> & /@ records,
        foldOptions[OptionValue["Model"], OptionValue["Seed"]],
        OptionValue["Output"],
        OptionValue[Timeout],
        "Folding " <> StringRiffle[#["ID"] & /@ records, ", "]
    ];
    Which[
        FailureQ[results], results,
        manyQ[input], AssociationThread[#["ID"] & /@ records, results],
        True, First[results]
    ]
];

$chainLetters = CharacterRange["A", "Z"];

Options[SynthyraFoldComplex] = Append[Options[SynthyraFoldProtein], "Ligands" -> {}];

SynthyraFoldComplex[chains_?manyQ, OptionsPattern[]] := Module[{records, ligands, name, item, results},
    records = resolveProteins[chains, OptionValue["Organism"]];
    If[FailureQ[records], Return[records]];
    If[Length[records] > Length[$chainLetters],
        Return @ proteinFailure["TooManyChains", "A complex may have at most `Limit` protein chains.", <|"Limit" -> Length[$chainLetters]|>]
    ];
    ligands = If[OptionValue["Ligands"] === {}, {}, resolveLigands[OptionValue["Ligands"]]];
    If[FailureQ[ligands], Return[ligands]];
    name = StringRiffle[#["ID"] & /@ Join[records, ligands], ":"];
    item = <|
        "id" -> name,
        "chains" -> MapThread[<|"id" -> #2, "type" -> "protein", "sequence" -> #1["Sequence"]|> &, {records, Take[$chainLetters, Length[records]]}],
        If[ligands === {}, Nothing, "ligands" -> MapIndexed[<|"id" -> "L" <> ToString[First[#2]], "smiles" -> #1["SMILES"]|> &, ligands]]
    |>;
    results = runFold[
        OptionValue["Client"],
        {item},
        foldOptions[OptionValue["Model"], OptionValue["Seed"]],
        OptionValue["Output"],
        OptionValue[Timeout],
        "Folding the complex " <> name
    ];
    If[FailureQ[results], results, First[results]]
];

(* The pLDDT bands AlphaFold and ESMFold report confidence in. *)
$confidenceBands = {
    {90., RGBColor[0., 0.33, 0.84], "Very high (pLDDT > 90)"},
    {70., RGBColor[0.4, 0.8, 0.95], "Confident (70 to 90)"},
    {50., RGBColor[1., 0.84, 0.08], "Low (50 to 70)"},
    {-Infinity, RGBColor[1., 0.49, 0.27], "Very low (< 50)"}
};

confidenceColor[value_?NumericQ] := SelectFirst[$confidenceBands, value > First[#] &][[2]];

residueColorRules[values_Association, color_] := Flatten @ KeyValueMap[
    Function[{chain, perResidue}, MapIndexed[{chain, First[#2]} -> color[#1] &, perResidue]],
    values
];

valueColorRules[values_Association] := Module[{all = Select[Flatten[Values[values]], NumericQ], range},
    range = If[all === {} || Min[all] == Max[all], {0, 1}, MinMax[all]];
    {
        residueColorRules[values, If[NumericQ[#], ColorData["TemperatureMap"][Rescale[#, range]], GrayLevel[0.7]] &],
        BarLegend[{"TemperatureMap", range}]
    }
];

structureColoring[molecule_, "Confidence"] := Module[{values = residueConfidence[molecule]},
    If[values === <||>,
        {Automatic, None},
        {
            Append[residueColorRules[values, confidenceColor], _ -> GrayLevel[0.7]],
            SwatchLegend[$confidenceBands[[All, 2]], $confidenceBands[[All, 3]]]
        }
    ]
];
structureColoring[_, None | "Chain" | Automatic] := {Automatic, None};
structureColoring[molecule_, values_List] := structureColoring[molecule, <|First[Keys[molecule["BioSequences"]]] -> values|>];
structureColoring[_, values_Association] := Module[{rules, legend},
    {rules, legend} = valueColorRules[values];
    {Append[rules, _ -> GrayLevel[0.7]], legend}
];
structureColoring[_, other_] := proteinFailure[
    "InvalidOption",
    "\"ColorBy\" must be \"Confidence\", \"Chain\", None, a list of per-residue values, or an Association of them by chain, not `Value`.",
    <|"Value" -> shortForm[other]|>
];

(* The options are the Workbench's, which defines SynthyraStructurePlot for analysis objects. *)
SynthyraStructurePlot[fold_Association /; Head[Lookup[fold, "Structure"]] === BioMolecule, opts : OptionsPattern[]] :=
    SynthyraStructurePlot[fold["Structure"], opts];

SynthyraStructurePlot[molecule_BioMolecule, OptionsPattern[]] := Module[{coloring, plot},
    coloring = structureColoring[molecule, OptionValue["ColorBy"]];
    If[FailureQ[coloring], Return[coloring]];
    plot = Quiet @ Check[
        BioMoleculePlot3D[
            molecule,
            ColorRules -> First[coloring],
            PlotTheme -> OptionValue[PlotTheme],
            ImageSize -> OptionValue[ImageSize]
        ],
        $Failed
    ];
    Which[
        plot === $Failed, proteinFailure["StructureRenderingFailed", "BioMoleculePlot3D could not render the structure."],
        Last[coloring] === None, plot,
        True, Legended[plot, Last[coloring]]
    ]
];

(* ---------------------------------------------------------------------------------------------
   Properties: the Oracle probes, one row per probe. A binary probe's prediction is its more
   likely label; a multi-label probe's is its most likely label, with the rest under "Labels". *)

oracleRow[prediction_Association] := Module[{name, score, mode, labels, top},
    name = ToString @ associationLookup[prediction, {"oracle_name", "name"}, "oracle"];
    score = associationLookup[prediction, {"score"}, Missing["NotAvailable"]];
    mode = ToString @ associationLookup[prediction, {"score_mode"}, ""];
    labels = associationLookup[prediction, {"label_names"}, Missing["NotAvailable"]];
    Which[
        NumericQ[score] && ListQ[labels] && Length[labels] == 2,
            <|"Property" -> name, "Prediction" -> If[score >= 0.5, labels[[2]], labels[[1]]],
              "Probability" -> Max[score, 1 - score], "Labels" -> AssociationThread[labels, {1 - score, score}]|>,
        VectorQ[score, NumericQ] && ListQ[labels] && Length[labels] == Length[score],
            top = Ordering[score, -1][[1]];
            <|"Property" -> name, "Prediction" -> labels[[top]], "Probability" -> score[[top]],
              "Labels" -> TakeLargest[AssociationThread[labels, score], UpTo[5]]|>,
        NumericQ[score],
            <|"Property" -> name, "Prediction" -> score, "Probability" -> Missing["NotApplicable"], "Labels" -> Missing["NotApplicable"]|>,
        True,
            <|"Property" -> name, "Prediction" -> score, "Probability" -> Missing["NotAvailable"], "Labels" -> Missing["NotAvailable"],
              "Mode" -> mode|>
    ]
];

Options[SynthyraProteinProperties] = {"Client" -> Automatic, "Organism" -> "Human"};

SynthyraProteinProperties[input_, OptionsPattern[]] := Module[{records, client, response, results, tables},
    records = resolveProteins[input, OptionValue["Organism"]];
    If[FailureQ[records], Return[records]];
    client = resolveClient[OptionValue["Client"]];
    If[FailureQ[client], Return[client]];
    response = client["RunOracles", <|"sequences" -> (#["Sequence"] & /@ records), "ids" -> (#["ID"] & /@ records)|>, Timeout -> $synchronousTimeout];
    If[FailureQ[response], Return[response]];
    results = associationLookup[response, {"results"}, {}];
    If[! ListQ[results] || Length[results] =!= Length[records],
        Return @ proteinFailure["UnexpectedOracleResult", "The Oracle service returned `Count` results for `Proteins` proteins.",
            <|"Count" -> If[ListQ[results], Length[results], 0], "Proteins" -> Length[records]|>]
    ];
    tables = Dataset[oracleRow /@ Select[associationLookup[#, {"predictions"}, {}], AssociationQ]] & /@ results;
    If[manyQ[input], AssociationThread[#["ID"] & /@ records, tables], First[tables]]
];

(* ---------------------------------------------------------------------------------------------
   Interactomes: Atlas scores each query against its organism's reference proteome and keeps the
   partners above the threshold. Vertices are gene names; each edge carries the Atlas
   probability as its "Confidence" annotation and EdgeWeight, and the enrichment of the
   neighborhood is the graph's "Enrichment" annotation. *)

(* Network nodes are named by UniProt entry name (UBP11 for USP11), so vertices take each
   partner's primary gene symbol from one UniProt query, and keep the node name where it has none.
   The query depends only on the network, so a recording of it replays in any session. *)
geneSymbols[accessions_List] := Module[{valid, symbols = <||>, query, text, rows},
    valid = Sort @ DeleteDuplicates @ Select[accessions, StringQ[#] && StringMatchQ[#, $uniProtAccession] &];
    Do[
        query = URLQueryEncode[{
            "query" -> StringRiffle[("accession:" <> #) & /@ batch, " OR "],
            "fields" -> "accession,gene_primary",
            "format" -> "tsv",
            "size" -> "500"
        }];
        text = uniProtText[$uniProtURL <> "search?" <> query, "gene symbols"];
        If[StringQ[text],
            rows = StringSplit[#, "	"] & /@ Rest[StringSplit[text, {"
", "
"}]];
            (* A protein several genes encode, such as a histone, lists them all; the first is primary. *)
            Scan[If[Length[#] >= 2 && StringTrim[#[[2]]] =!= "", symbols[#[[1]]] = StringTrim @ First @ StringSplit[#[[2]], ";"]] &, rows]
        ],
        {batch, Partition[valid, UpTo[100]]}
    ];
    AssociationMap[Lookup[symbols, #, Missing["NotAvailable"]] &, accessions]
];

interactomeGraph[network_Association, enrichment_] := Module[{graph, nodes, symbols, names, taken, terms},
    graph = SynthyraGraph[network, IncludeOriginalPayload -> False];
    If[FailureQ[graph], Return[graph]];
    nodes = Select[associationLookup[network, {"nodes"}, {}], AssociationQ];
    symbols = geneSymbols[ToString[associationLookup[#, {"uniprot_id"}, ""]] & /@ nodes];
    taken = <||>;
    names = Association @ Map[
        Function[node, Module[{id = ToString @ associationLookup[node, {"id"}, ""], name},
            name = Lookup[symbols, ToString @ associationLookup[node, {"uniprot_id"}, ""], Missing["NotAvailable"]];
            If[! StringQ[name], name = ToString @ associationLookup[node, {"name"}, id]];
            (* The query can also appear as its own proteome entry, a homo-oligomer edge. *)
            If[KeyExistsQ[taken, name] && StringMatchQ[ToString @ associationLookup[node, {"uniprot_id"}, ""], $uniProtAccession],
                name = name <> " (" <> ToString @ associationLookup[node, {"uniprot_id"}, ""] <> ")"
            ];
            If[KeyExistsQ[taken, name], name = id];
            taken[name] = True;
            Protein[id] -> name
        ]],
        nodes
    ];
    graph = VertexReplace[graph, Normal[names]];
    graph = Graph[
        graph,
        EdgeWeight -> (N[AnnotationValue[{graph, #}, "Confidence"]] / 100. & /@ EdgeList[graph]),
        VertexLabels -> Automatic
    ];
    terms = If[AssociationQ[enrichment], associationLookup[enrichment, {"terms"}, {}], {}];
    Annotate[
        graph,
        "Enrichment" -> Dataset[
            <|
                "Term" -> associationLookup[#, {"term"}, Missing["NotAvailable"]],
                "Library" -> associationLookup[#, {"library"}, Missing["NotAvailable"]],
                (* A Dataset prints a small p-value as a long fixed-point decimal, so the column is
                   its base-10 logarithm, the usual scale for enrichment. *)
                "Log10AdjustedPValue" -> Replace[associationLookup[#, {"adjusted_p_value"}, Missing["NotAvailable"]], p_?Positive :> Log10[N[p]]],
                "Genes" -> associationLookup[#, {"genes"}, {}]
            |> & /@ Select[terms, AssociationQ]
        ]
    ]
];

Options[SynthyraInteractome] = {
    "Client" -> Automatic,
    "Organism" -> "Human",
    "Neighbors" -> 50,
    "Threshold" -> 0.7,
    "NeighborDepth" -> 1,
    "CrossReference" -> True,
    Timeout -> 3600.
};

SynthyraInteractome[input_, OptionsPattern[]] := Module[
    {organism, records, client, job, ready, jobID, network, enrichment},
    organism = resolveOrganism[OptionValue["Organism"]];
    If[FailureQ[organism], Return[organism]];
    If[MissingQ[organism["Service"]],
        Return @ proteinFailure["OrganismNotServed", "Synthyra has no reference interactome for taxon `Taxon`.", <|"Taxon" -> organism["Taxon"]|>]
    ];
    records = resolveProteins[input, OptionValue["Organism"]];
    If[FailureQ[records], Return[records]];
    client = resolveClient[OptionValue["Client"]];
    If[FailureQ[client], Return[client]];
    job = client["SubmitPrediction", <|
        "job_type" -> "network",
        "name" -> "SynthyraLink interactome: " <> StringRiffle[#["ID"] & /@ records, ", "],
        "organism" -> organism["Service"],
        "query_sequences" -> (<|"id" -> Lookup[#, "Accession", #["ID"]], "sequence" -> #["Sequence"]|> & /@ records),
        "neighbors" -> OptionValue["Neighbors"],
        "threshold" -> OptionValue["Threshold"],
        "neighbor_depth" -> OptionValue["NeighborDepth"],
        "cross_reference_string" -> TrueQ[OptionValue["CrossReference"]]
    |>];
    If[FailureQ[job], Return[job]];
    ready = awaitJob[job, "Building the interactome of " <> StringRiffle[#["ID"] & /@ records, ", "], OptionValue[Timeout]];
    If[FailureQ[ready], Return[ready]];
    jobID = First[ready]["JobID"];
    (* The full result also carries layout and rendering data the graph does not use. *)
    network = client["GetJobPartial", <|"job_id" -> jobID, "field_name" -> "result_network"|>];
    If[FailureQ[network], Return[network]];
    enrichment = client["GetJobPartial", <|"job_id" -> jobID, "field_name" -> "result_enrichment"|>];
    interactomeGraph[network, If[FailureQ[enrichment], <||>, enrichment]]
];

(* ---------------------------------------------------------------------------------------------
   LLM tools: the functions above, with text results a language model can read. *)

llmText[value_?FailureQ] := "Failed: " <> ToString[value["Message"]];
llmText[value_] := ToString[value, InputForm];

llmScore[score_?NumericQ] := ToString[NumberForm[score, {3, 3}]];
llmScore[other_] := llmText[other];

llmProperties[table_Dataset] := StringRiffle[
    (#["Property"] <> ": " <> ToString[#["Prediction"]] <>
        If[NumericQ[#["Probability"]], " (probability " <> ToString[NumberForm[#["Probability"], {3, 2}]] <> ")", ""]) & /@ Normal[table],
    "\n"
];
llmProperties[other_] := llmText[other];

llmPartners[graph_?GraphQ, query_String] := Module[{vertex, partners},
    vertex = SelectFirst[VertexList[graph], ToUpperCase[ToString[#]] === ToUpperCase[query] &, First[VertexList[graph]]];
    partners = SortBy[
        {If[First[#] === vertex, Last[#], First[#]], AnnotationValue[{graph, #}, "Confidence"]} & /@ EdgeList[graph, vertex \[UndirectedEdge] _ | _ \[UndirectedEdge] vertex],
        -Last[#] &
    ];
    StringRiffle[(ToString[First[#]] <> " (" <> ToString[Last[#]] <> "%)") & /@ partners, ", "]
];
llmPartners[other_, _] := llmText[other];

llmFold[result_Association] := StringTemplate["Predicted the structure of `Name` with `Model`: mean pLDDT `PLDDT`, pTM `PTM`."][<|
    "Name" -> result["Name"],
    "Model" -> result["Model"],
    "PLDDT" -> Round[result["MeanPLDDT"], 0.1],
    "PTM" -> Round[result["PTM"], 0.01]
|>];
llmFold[other_] := llmText[other];

(* ---------------------------------------------------------------------------------------------
   A returned Failure also prints its reason as a message: inside Part, a plot, or a table a
   Failure is easy to miss, and every later cell then fails for a reason the notebook never
   showed. Only the outermost call reports, so a failure one function passes to another prints
   once. *)

$insideSynthyraCall = False;

reportFailure[function_Symbol, False, failure_?FailureQ] := (
    Message[MessageName[function, "failed"], failure["Message"]];
    failure
);
reportFailure[_Symbol, _, value_] := value;

(* The body runs as its own call, so a Return inside it ends there and its value is still reported. *)
SetAttributes[evaluateInside, HoldAll];
evaluateInside[body_] := Block[{$insideSynthyraCall = True}, body];

Scan[
    Function[function,
        MessageName[function, "failed"] = "`1`";
        DownValues[function] = Replace[
            DownValues[function],
            {
                (* A body ending in /; keeps its test outside the report. *)
                (lhs_ :> Verbatim[Condition][body_, test_]) :>
                    (lhs :> Condition[reportFailure[function, $insideSynthyraCall, evaluateInside[body]], test]),
                (lhs_ :> body_) :>
                    (lhs :> reportFailure[function, $insideSynthyraCall, evaluateInside[body]])
            },
            {1}
        ]
    ],
    {
        SynthyraProteinSequence, SynthyraFoldProtein, SynthyraFoldComplex, SynthyraStructurePlot,
        SynthyraProteinInteractionScore, SynthyraLigandBindingScore, SynthyraProteinProperties,
        SynthyraInteractome
    }
];

(* A failure handed to the plot is an earlier call's, which reported it; pass it on unchanged. *)
SynthyraStructurePlot[failure_?FailureQ, OptionsPattern[]] := failure;

SynthyraLLMTools[] := {
    LLMTool[
        {"synthyra_protein_interaction", "Probability from 0 to 1 that two proteins physically interact, from Synthyra's Atlas-PPI model. Each protein is a UniProt accession, a gene symbol, or an amino-acid sequence."},
        {"protein_a" -> "String", "protein_b" -> "String"},
        llmScore[SynthyraProteinInteractionScore[#["protein_a"], #["protein_b"]]] &
    ],
    LLMTool[
        {"synthyra_ligand_binding", "Probability from 0 to 1 that a small molecule binds a protein, from Synthyra's Atlas-PLI model. The ligand is SMILES or a chemical name; the protein is a UniProt accession, gene symbol, or sequence."},
        {"protein" -> "String", "ligand" -> "String"},
        llmScore[SynthyraLigandBindingScore[#["protein"], #["ligand"]]] &
    ],
    LLMTool[
        {"synthyra_protein_properties", "Predicted properties of a protein from Synthyra's Oracle probes: solubility, thermostability, E. coli expression, subcellular location, pH optimum, kcat, homodimerization, taxon, and enzyme and GO classes."},
        {"protein" -> "String"},
        llmProperties[SynthyraProteinProperties[#["protein"]]] &
    ],
    LLMTool[
        {"synthyra_interaction_partners", "The predicted interaction partners of a human protein across the human proteome, strongest first with Atlas confidence in percent. Takes a few minutes."},
        {"protein" -> "String"},
        llmPartners[SynthyraInteractome[#["protein"]], #["protein"]] &
    ],
    LLMTool[
        {"synthyra_fold_protein", "Predict a protein's 3D structure with ESMFold2 and report its confidence (mean pLDDT from 0 to 100, and pTM from 0 to 1)."},
        {"protein" -> "String"},
        llmFold[SynthyraFoldProtein[#["protein"], "Output" -> "Association"]] &
    ]
};
