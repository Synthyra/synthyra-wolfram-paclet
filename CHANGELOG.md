# Changelog

All notable changes to SynthyraLink are documented here. Versions follow semantic versioning against the reviewed public Gateway contract.

## 0.2.0 - Unreleased

- Protein functions with Wolfram types in and out: `SynthyraProteinSequence`, `SynthyraFoldProtein`,
  `SynthyraFoldComplex`, `SynthyraStructurePlot`, `SynthyraProteinInteractionScore`,
  `SynthyraLigandBindingScore`, `SynthyraProteinProperties`, `SynthyraInteractome`, and
  `SynthyraLLMTools`. Proteins may be gene symbols, UniProt accessions, entities, sequences, or
  `BioSequence` objects; ligands SMILES, names, `Molecule`, or entities.
- A protein function that returns a `Failure` also prints its reason as a message, once per call,
  so a failure inside `Part`, a plot, or a table still shows why.
- `SynthyraConnect[]` connects to development until the ESMFold2-300 fold default reaches
  production; `SynthyraConnect["Production"]` names production.
- `SynthyraRecord` and `SynthyraUseRecording` record a session and replay it offline, for a
  client of either environment; the TP53 demo
  notebook ships with its recording.
- `SynthyraConnect` sets `$SynthyraClient`, the default client of every function.
- Reference pages for every public symbol with evaluated examples, a guide, and the TP53 tutorial.
- The contract is generated from the live API and a reviewed overlay, replacing signed release
  artifacts.
- Batch job results follow the manifest to its rows; `Retry-After` is honored; a submission retries
  only when the server says it did not run; failures name the field and the server's message.
- The native HTTPS client, credential handling, job polling, score matrices, graph conversion, and
  the analysis workbench from 0.1.
- Requires Wolfram Language 15.0.
