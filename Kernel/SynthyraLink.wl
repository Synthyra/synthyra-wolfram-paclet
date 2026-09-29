BeginPackage["Synthyra`SynthyraLink`"];

SynthyraConnect::usage =
    "SynthyraConnect[] creates a production Synthyra client. SynthyraConnect[\"Development\"] creates a development client.";

SynthyraClientObject::usage =
    "SynthyraClientObject[...] is an opaque handle for a configured Synthyra API client.";

SynthyraExecute::usage =
    "SynthyraExecute[client, name, request] executes a generated Synthyra operation using native HTTPS.";

SynthyraJobObject::usage =
    "SynthyraJobObject[...] is an opaque handle for an asynchronous Synthyra operation.";

SynthyraJobStatus::usage =
    "SynthyraJobStatus[job] retrieves the current status of an asynchronous Synthyra job.";

SynthyraWait::usage =
    "SynthyraWait[job] polls until the job reaches a terminal state or the timeout expires.";

SynthyraJobResult::usage =
    "SynthyraJobResult[job] waits for successful completion and retrieves the job result.";

SynthyraCancelJob::usage =
    "SynthyraCancelJob[job] requests cancellation when the operation declares a cancellation endpoint.";

SynthyraScoreMatrix::usage =
    "SynthyraScoreMatrix[payload] decodes a row-major uint8 score matrix into a lossless Association containing a NumericArray and identifiers.";

SynthyraGraph::usage =
    "SynthyraGraph[payload] converts verbose or compact Synthyra interaction-network data to a native Graph.";

SynthyraHypergraphData::usage =
    "SynthyraHypergraphData[payload] returns lossless vertices, hyperedges, member lists, annotations, and the original payload.";

SynthyraAnalysis::usage =
    "SynthyraAnalysis[result] normalizes a completed coordinated-analysis result into a SynthyraAnalysisObject without retaining a client or credential.";

SynthyraAnalysisObject::usage =
    "SynthyraAnalysisObject[...] is a local, credential-free representation of coordinated structure, oracle, annotation, network, enrichment, matrix, and TMAP results.";

SynthyraWorkbench::usage =
    "SynthyraWorkbench[analysis] creates a native Wolfram Language tabbed interface for a coordinated Synthyra analysis.";

SynthyraStructurePlot::usage =
    "SynthyraStructurePlot[molecule] renders a BioMolecule with BioMoleculePlot3D, colored by per-residue prediction confidence (pLDDT).
SynthyraStructurePlot[molecule, \"ColorBy\" -> values] colors residues by a list of values, or an Association of lists by chain.
SynthyraStructurePlot[analysis] plots the structure in a coordinated analysis result.";

$SynthyraClient::usage =
    "$SynthyraClient is the client Synthyra functions use when \"Client\" -> Automatic; SynthyraConnect sets it.";

SynthyraProteinSequence::usage =
    "SynthyraProteinSequence[protein] gives the BioSequence of a UniProt accession, gene symbol, Entity[\"Protein\", ...], or sequence.
SynthyraProteinSequence[{p1, p2, ...}] gives an Association of them.";

SynthyraRecord::usage =
    "SynthyraRecord[expr, file] evaluates expr and saves every HTTP exchange it makes to file, without request headers or credentials, for SynthyraUseRecording.";

SynthyraUseRecording::usage =
    "SynthyraUseRecording[file] answers every Synthyra, UniProt, and PubChem request from a recording made by SynthyraRecord, so a session runs offline. SynthyraUseRecording[\"TP53Demo\"] uses the recording of the TP53 demo notebook that ships with the paclet. SynthyraUseRecording[None] returns to the network.";

SynthyraFoldProtein::usage =
    "SynthyraFoldProtein[protein] predicts the structure of a protein with ESMFold2 and returns a BioMolecule whose B-factors hold per-atom confidence.
SynthyraFoldProtein[{p1, p2, ...}] folds several proteins in one job.";

SynthyraFoldComplex::usage =
    "SynthyraFoldComplex[{p1, p2, ...}] predicts the structure of a complex of protein chains. The \"Ligands\" option adds small molecules.";

SynthyraProteinInteractionScore::usage =
    "SynthyraProteinInteractionScore[a, b] gives the Atlas probability, from 0 to 1, that proteins a and b physically interact.
SynthyraProteinInteractionScore[{a1, ...}, {b1, ...}] gives a Dataset of every pair.";

SynthyraLigandBindingScore::usage =
    "SynthyraLigandBindingScore[protein, ligand] gives the Atlas probability, from 0 to 1, that a small molecule binds a protein. A ligand is SMILES, a chemical name, a Molecule, or Entity[\"Chemical\", ...].
SynthyraLigandBindingScore[{p1, ...}, {l1, ...}] gives a Dataset of every pair.";

SynthyraProteinProperties::usage =
    "SynthyraProteinProperties[protein] gives a Dataset of the properties Synthyra's Oracle probes predict for a protein.";

SynthyraInteractome::usage =
    "SynthyraInteractome[protein] gives the Graph of a protein's predicted interaction partners across its organism's proteome, with Atlas confidence on each edge and the partners' functional enrichment as the \"Enrichment\" annotation.";

SynthyraLLMTools::usage =
    "SynthyraLLMTools[] gives LLMTool objects for protein interaction, ligand binding, properties, interaction partners, and folding, for use with LLMSynthesize and ChatObject.";

SynthyraOracleDataset::usage =
    "SynthyraOracleDataset[analysis] returns normalized oracle predictions as a Dataset.";

SynthyraEnrichmentDataset::usage =
    "SynthyraEnrichmentDataset[analysis] returns normalized enrichment terms as a Dataset.";

Protein::usage =
    "Protein[id] is the symbolic vertex representation for a protein identifier.";

Ligand::usage =
    "Ligand[id] is the symbolic vertex representation for a ligand identifier.";

ReturnType::usage =
    "ReturnType is an option for SynthyraExecute. Use \"Dataset\" to wrap list-valued JSON results in Dataset.";

Target::usage =
    "Target is an option for SynthyraExecute. Target -> File[path] streams a binary response to a file.";

RawResponse::usage =
    "RawResponse is an option for SynthyraExecute that returns only sanitized status, headers, and body.";

MaxRetries::usage =
    "MaxRetries is an option that limits retries of safe read operations after transient server responses.";

RetryBackoff::usage =
    "RetryBackoff is an option that gives retry delays in seconds or a function of the retry number.";

Timeout::usage =
    "Timeout is an option specifying an HTTP request timeout or, for SynthyraWait, the maximum polling duration in seconds.";

PollInterval::usage =
    "PollInterval is an option for SynthyraWait specifying seconds between status checks.";

WaitForCompletion::usage =
    "WaitForCompletion is an option for SynthyraJobResult controlling whether a nonterminal job is polled.";

ConfidenceCostFunction::usage =
    "ConfidenceCostFunction is an option for SynthyraGraph. None leaves EdgeWeight unset; a function explicitly maps confidence to graph cost.";

IncludeOriginalPayload::usage =
    "IncludeOriginalPayload is an option controlling retention of original network records in graph annotations.";

Begin["`Private`"];

$SynthyraKernelDirectory = DirectoryName[$InputFileName];
$SynthyraPacletVersion = "0.2.0";

Get[FileNameJoin[{$SynthyraKernelDirectory, "Private", "Runtime.wl"}]];
Get[FileNameJoin[{$SynthyraKernelDirectory, "Private", "Matrix.wl"}]];
Get[FileNameJoin[{$SynthyraKernelDirectory, "Private", "Graphs.wl"}]];
Get[FileNameJoin[{$SynthyraKernelDirectory, "Private", "Workbench.wl"}]];
Get[FileNameJoin[{$SynthyraKernelDirectory, "Private", "Protein.wl"}]];
Get[FileNameJoin[{$SynthyraKernelDirectory, "Private", "Replay.wl"}]];

End[];
EndPackage[];
