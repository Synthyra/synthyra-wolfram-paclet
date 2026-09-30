# SynthyraLink

`Synthyra/SynthyraLink` brings Synthyra's protein models into the Wolfram Language. Name a
protein by gene symbol, UniProt accession, `Entity["Protein", ...]`, or sequence, and get back
native objects: predicted structures as `BioMolecule`, interaction networks as `Graph`, and
interaction, binding, and property predictions as numbers and `Dataset`. The functions are also
available to language models as `LLMTool` objects.

The paclet talks to Synthyra's API over HTTPS with your API key. For now `SynthyraConnect[]`
connects to development (`https://apidev.synthyra.com`), which serves the ESMFold2-300 fold default
ahead of production; `SynthyraConnect["Production"]` connects to `https://api.synthyra.com`. Its source is Apache-2.0; API access, billing, and returned data are governed by the
Synthyra API terms. It needs Wolfram Language 15.0 or later.

## Install

```wl
PacletInstall["Synthyra/SynthyraLink"]
Needs["Synthyra`SynthyraLink`"]
```

From a clone of this repository, `PacletDirectoryLoad["path/to/synthyra-wolfram-paclet"]` in
place of `PacletInstall`.

## A tour

```wl
SynthyraConnect[]                                   (* development for now; uses SYNTHYRA_API_KEY or SystemCredential *)

SynthyraProteinSequence["TP53"]                     (* BioSequence, from UniProt *)
fold = SynthyraFoldProtein["TP53"]                  (* BioMolecule from ESMFold2, pLDDT in its B-factors *)
SynthyraStructurePlot[fold]                         (* colored by confidence *)

SynthyraProteinInteractionScore["TP53", "MDM2"]     (* Atlas-PPI probability *)
SynthyraLigandBindingScore["MDM2", {"nutlin-3a", "aspirin"}]
SynthyraProteinProperties["TP53"]                   (* Oracle predictions as a Dataset *)

net = SynthyraInteractome["TP53"]                   (* Graph of predicted partners across the proteome *)
AnnotationValue[net, "Enrichment"]

SynthyraFoldComplex[{"SUMO1", "UBE2I"}]             (* a complex, chains A and B *)

LLMSynthesize["Which proteins bind TP53 most strongly?", LLMEvaluator -> <|"Tools" -> SynthyraLLMTools[]|>]
```

Lists and Associations score or fold many proteins at once; `label -> protein` names a result.
Interactome edges carry the Atlas confidence and say whether BioGRID or STRING already records
the interaction.

## The TP53 demo, with or without a network

`Examples/TP53.nb` walks through structure, properties, the interactome, partners, drugs, and the
MDM2 complex for p53. Opened from a clone, its first cell loads the paclet from the clone. It ships
with a recording of itself run against production, so it also runs offline and without a key: set
`offline = True` in its connect cell, or evaluate

```wl
SystemOpen @ PacletObject["Synthyra/SynthyraLink"]["AssetLocation", "TP53Notebook"]
SynthyraUseRecording["TP53Demo"]     (* answer every request from the recording *)
SynthyraUseRecording[None]           (* back to the network *)
```

`SynthyraRecord[expr, file]` records any session the same way. A recording holds requests'
methods, URLs, and bodies and their responses, never a request header or key.

## Credentials

`SynthyraConnect[]` takes the key from, in order, `Authentication -> "..."`, the
`SYNTHYRA_API_KEY` environment variable, `SystemCredential["Synthyra/APIKey"]`, and a masked
dialog in a notebook. To store it in the operating system's secure storage once:

```wl
SystemCredential["Synthyra/APIKey"] = "your-api-key";
```

The key lives in a private in-memory registry. A `SynthyraClientObject` holds only a random
handle, so displaying, saving, or sharing a client never reveals the key, and failures redact
credentials, cookies, and authorization headers.

## The whole API

The protein functions sit on a generated client for every public API operation:

```wl
client = SynthyraConnect[];
client["Operations"]
client["OperationInformation", "RunTranslator"]
client["RunTranslator", <|"sequences" -> {"MKT..."}, "ids" -> {"query-1"}|>]

job = client["SubmitPrediction", <|"job_type" -> "network", "organism" -> "human",
    "query_sequences" -> {<|"id" -> "P04637", "sequence" -> "MEEP..."|>}|>];
SynthyraWait[job]; SynthyraJobResult[job]
```

`SynthyraGraph`, `SynthyraHypergraphData`, and `SynthyraScoreMatrix` turn networks and score
matrices into `Graph`, hypergraph data, and `NumericArray`. `SynthyraAnalysis` and
`SynthyraWorkbench` explore a completed coordinated analysis in a tabbed interface.

## Metered calls

Most operations consume credits. Read-only requests retry after HTTP 429, 502, 503, or 504; a
submission is retried only when the server says it did not run (503 `warming_up` or
`auth_unavailable`). Documentation and tests replay recordings and make no live calls.

## Development

| Path | Holds |
|---|---|
| `Kernel/` | `SynthyraLink.wl` (public symbols and usage), `Private/` (runtime, protein layer, recordings, graphs, matrices, workbench), `Generated/` (operation tables; never edit) |
| `Contract/` | The API contract the generator reads, and the overlay that selects and describes its operations |
| `Documentation/` | Reference pages, guide, and tutorial, built by `Documentation/Source/BuildDocumentation.wl` |
| `Examples/` | `TP53.nb`, its recording `Data/TP53Demo.wxf`, and the graph and workbench demos |
| `Tests/Wolfram/` | Test suites, offline |
| `Tools/` | The contract generator and its tests, and the scripts below |

```bash
wolframscript -file Tools/RunAllWolframTests.wls                  # the Wolfram suites
wolframscript -file Tools/PacletCI.wls                            # CheckPaclet, TestPaclet, BuildPaclet
wolframscript -file Documentation/Source/BuildDocumentation.wl    # rebuild the documentation offline
wolframscript -file Tools/RecordDemo.wls                          # rerun TP53.nb live and re-record it (needs a key)
python Tools/refresh_contract.py --check                          # has the live API drifted from the contract?
```

CI (`.github/workflows/paclet.yml`) runs the generator checks and, on the Wolfram Engine 15.0
container, `PacletCI.wls` and a clean install of the built archive. It needs the
`WOLFRAMSCRIPT_ENTITLEMENTID` repository secret.
