# Examples

## TP53 with Synthyra

`TP53.nb` is the flagship demo: TP53's sequence, its ESMFold2 structure colored by confidence,
its predicted properties, its interactome with the partners BioGRID and STRING already know, the
strongest partners and MDM2 scored directly, MDM2 inhibitors against a control, and the folded
MDM2 and p53 complex. Evaluate it live after `SynthyraConnect[]`, or offline after
`SynthyraUseRecording["TP53Demo"]`, which answers every request from `Data/TP53Demo.wxf`, a
recording of this notebook against production. `Tools/RecordDemo.wls` rewrites the recording
after the notebook changes, and a test replays every cell of the notebook from it.

## Intra-actome graph demo

`IntraActomeGraphDemo.wl` demonstrates the complete Mathematica workflow:

1. Read `result_actome_edges` from a completed Synthyra job with `GetJobPartial`.
2. Convert the compact actome response to a native graph with `SynthyraGraph`.
3. Select one to three local hops with `NeighborhoodGraph`, then preserve Synthyra annotations with `Subgraph[..., AnnotationRules -> Inherited]`.
4. Color communities found by `FindGraphCommunities`.
5. Render confidence as edge opacity and thickness without treating confidence as path cost.
6. Rank the displayed proteins with unweighted betweenness centrality.

The default mode imports `Fixtures/intra-actome-subgraph.json`. Its topology and confidence values are explicitly synthetic, so it is safe for talks, screenshots, and documentation and consumes no credits.

Evaluate the credit-free demo from a notebook:

```wl
Get["C:\\path\\to\\SynthyraLink\\Examples\\IntraActomeGraphDemo.wl"]
```

To read a live production result, first configure a credential and provide a completed intra-actome job ID. Query IDs must use the identifier convention stored in that result. Leave `$LiveQueryIDs = Automatic` when the returned compact actome retains its `query_ids` member.

```wl
SystemCredential["Synthyra/APIKey"] = "your-api-key";

$UseLiveSynthyra = True;
$LiveJobID = "your-completed-job-id";
$LiveQueryIDs = {"P04637", "Q00987"};

Get["C:\\path\\to\\SynthyraLink\\Examples\\IntraActomeGraphDemo.wl"]
```

The live path performs one authenticated `GET` and does not submit new compute. It may retry a transient safe-read failure up to twice. It never submits an actome or silently consumes job credits.

After evaluation, `intraActomeGraph` is the completed job's compact interaction network, `twoHopSubgraph` is a native Wolfram `Graph`, `intraActomeExplorer` is the interactive view, and `topCentrality` is a `Dataset`.

## Coordinated-analysis workbench

`ProductionAnalysisWorkbench.wl` builds a native tabbed interface across a coordinated result's structure, oracle outputs, CAMP and Translator annotations, biological actome graph, enrichment, returned matrix views, TMAP layout, sequence, and raw metadata.

The workbench is fixture and saved-artifact first:

1. Evaluating `ProductionAnalysisWorkbench.wl` without configuration uses its synthetic fixture and consumes no credits.
2. Reading an already-completed production job is an explicit opt-in safe-read path. It does not submit a new job.

To use the explicit completed-job reader, provide the ID of an already-completed coordinated job before evaluating the example:

```wl
$UseCompletedSynthyraJob = True;
$CompletedSynthyraJobID = "your-completed-job-id";

Get["C:\\path\\to\\SynthyraLink\\Examples\\ProductionAnalysisWorkbench.wl"]
```

The completed-job branch performs only status and result `GET` requests. For large results, `SynthyraJobResult` can stream the API-provided signed HTTPS artifact when the Gateway result route fails. It never submits compute. `SynthyraAnalysis` creates the credential-free `SynthyraAnalysisObject` stored in `synthyraAnalysis`, and `SynthyraWorkbench` creates `synthyraWorkbench` with eleven native tabs. `SynthyraStructurePlot`, `SynthyraOracleDataset`, and `SynthyraEnrichmentDataset` provide focused views from the same analysis object.

The Network tab uses `actome_edges` as the biological interaction graph. The TMAP tab renders `tmap_tree` coordinates separately because its spanning-tree links are layout artifacts, not protein interactions. Returned neighborhood and candidate matrices are labeled by their actual scope and are not presented as the full dense proteome matrix.
