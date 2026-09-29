<!-- SPDX-License-Identifier: Apache-2.0 -->
<!-- This table is generated. Do not edit by hand. -->

# Generated operation reference

Contract: **Synthyra Gateway API 0.1.0**  
SHA-256: `e2d1a6de243224bbab3447ee4fa662b7668d5627046f0cd8032aae0311f263fd`  
Source commit: `not declared`  
Public operations: **51**

| Wolfram request | `operationId` | Method | Path | Result | Stability | Authentication | Summary |
|---|---|---:|---|---|---|---|---|
| `CancelProtifyJob` | `cancelProtifyJob` | `POST` | `/v1/protify/cancel` | JSON | beta | Bearer | Cancel Protify Job |
| `CreateInterActomeTmap` | `createInterActomeTmap` | `POST` | `/v1/actome/tmap/inter` | TmapTree | beta | Bearer | Actome Tmap Inter |
| `CreateIntraActomeTmap` | `createIntraActomeTmap` | `POST` | `/v1/actome/tmap/intra` | TmapTree | beta | Bearer | Actome Tmap Intra |
| `DeleteJob` | `deleteJob` | `DELETE` | `/v1/job/{job_id}` | JSON | stable | Bearer | Delete Job |
| `DownloadDeepResearch` | `downloadDeepResearch` | `GET` | `/v1/deep-research/download/{job_id}` | Binary | experimental | Bearer | Download Deep Research |
| `DownloadProtifyArtifacts` | `downloadProtifyArtifacts` | `GET` | `/v1/protify/download/{job_id}` | Binary | experimental | Bearer | Download Protify Artifacts |
| `GetBatchChunk` | `getBatchChunk` | `GET` | `/v1/batch/{job_id}/chunk/{chunk_index}` | JSONL | stable | Bearer | Get Batch Chunk |
| `GetBatchManifest` | `getBatchManifest` | `GET` | `/v1/batch/{job_id}/manifest` | JSON | stable | Bearer | Get Batch Manifest |
| `GetBatchResultsJSONL` | `getBatchResultsJsonl` | `GET` | `/v1/batch/{job_id}/results.jsonl` | JSONL | stable | Bearer | Get Batch Results Jsonl |
| `GetBatchStatus` | `getBatchStatus` | `GET` | `/v1/batch/{job_id}` | JSON | stable | Bearer | Get Batch Status |
| `GetDeepResearchJob` | `getDeepResearchJob` | `GET` | `/v1/deep-research/job/{job_id}` | JSON | experimental | Bearer | Get Deep Research Job |
| `GetHealth` | `getHealth` | `GET` | `/health` | JSON | stable | Anonymous | Health |
| `GetHealthReady` | `getHealthReady` | `GET` | `/health/ready` | JSON | stable | Anonymous | Health Ready |
| `GetJob` | `getJob` | `GET` | `/v1/job/{job_id}` | JSON | stable | Bearer | Get Job |
| `GetJobPartial` | `getJobPartial` | `GET` | `/v1/job/{job_id}/partial/{field_name}` | JSON | stable | Bearer | Get Job Partial |
| `GetJobResult` | `getJobResult` | `GET` | `/v1/job/{job_id}/result` | JSON | stable | Bearer | Get Job Result |
| `GetProtifyJob` | `getProtifyJob` | `GET` | `/v1/protify/job` | JSON | beta | Bearer | Get Protify Job |
| `GetProtifyLogs` | `getProtifyLogs` | `GET` | `/v1/protify/logs` | JSON | beta | Bearer | Get Protify Logs |
| `GetProtifyResults` | `getProtifyResults` | `GET` | `/v1/protify/results` | JSON | experimental | Bearer | Get Protify Results |
| `GetSharedAnalysis` | `getSharedAnalysis` | `GET` | `/v1/share/{token}` | JSON | stable | Optional bearer | Get Shared Analysis |
| `GetSharedField` | `getSharedField` | `GET` | `/v1/share/{token}/{field}` | JSON | stable | Bearer | Get Shared Field |
| `ListJobs` | `listJobs` | `GET` | `/v1/jobs` | JSON | stable | Bearer | List Jobs |
| `ListProtifyBenchmarks` | `listProtifyBenchmarks` | `GET` | `/v1/protify/benchmarks` | JSON | stable | Anonymous | List Protify Benchmarks |
| `ListProtifyDatasets` | `listProtifyDatasets` | `GET` | `/v1/protify/datasets` | JSON | stable | Anonymous | List Protify Datasets |
| `ListProtifyJobs` | `listProtifyJobs` | `GET` | `/v1/protify/jobs` | JSON | beta | Bearer | List Protify Jobs |
| `ListProtifyModels` | `listProtifyModels` | `GET` | `/v1/protify/models` | JSON | stable | Anonymous | List Protify Models |
| `ListProtifyProbes` | `listProtifyProbes` | `GET` | `/v1/protify/probes` | JSON | stable | Anonymous | List Protify Probes |
| `RunAllOracles` | `runAllOracles` | `POST` | `/v1/oracle/run` | JSON | stable | Bearer | Oracle Run All |
| `RunAtlasCamp` | `runAtlasCamp` | `POST` | `/v1/atlas-camp/run` | JSON | beta | Bearer | Atlas Camp Run |
| `RunCamp` | `runCamp` | `POST` | `/v1/camp/run` | JSON | stable | Bearer | Camp Run |
| `RunCampMsa` | `runCampMsa` | `POST` | `/v1/camp/msa` | JSON | stable | Bearer | Camp Msa |
| `RunOracle` | `runOracle` | `POST` | `/v1/oracle/{probe_name}` | JSON | stable | Bearer | Oracle Run Single |
| `RunOracles` | `runOracles` | `POST` | `/v1/oracles/run` | JSON | stable | Bearer | Oracles Run |
| `RunTranslator` | `runTranslator` | `POST` | `/v1/translator/run` | JSON | stable | Bearer | Translator Run |
| `ScoreAtlasPLI` | `scoreAtlasPli` | `POST` | `/v1/atlas-pli/score` | JSON | stable | Bearer | Atlas Pli Score |
| `ScoreAtlasPPI` | `scoreAtlasPpi` | `POST` | `/v1/atlas-ppi/score` | JSON | stable | Bearer | Atlas Ppi Score |
| `SubmitAtlasCampBatch` | `submitAtlasCampBatch` | `POST` | `/v1/atlas-camp/batch` | Job | stable | Bearer | Atlas Camp Batch |
| `SubmitAtlasPLIBatch` | `submitAtlasPliBatch` | `POST` | `/v1/atlas-pli/score/batch` | Job | stable | Bearer | Atlas Pli Score Batch |
| `SubmitAtlasPPIBatch` | `submitAtlasPpiBatch` | `POST` | `/v1/atlas-ppi/score/batch` | Job | stable | Bearer | Atlas Ppi Score Batch |
| `SubmitCampBatch` | `submitCampBatch` | `POST` | `/v1/camp/batch` | Job | stable | Bearer | Camp Batch |
| `SubmitCoordinatedPrediction` | `submitCoordinatedPrediction` | `POST` | `/v1/predict/coordinated` | Job | beta | Bearer | Submit Coordinated Job |
| `SubmitDeepResearch` | `submitDeepResearch` | `POST` | `/v1/deep-research/generate` | Job | experimental | Bearer | Submit Deep Research Job |
| `SubmitDfaPrediction` | `submitDfaPrediction` | `POST` | `/v1/predict/dfa` | Job | stable | Bearer | Submit Dfa Job |
| `SubmitFoldBatch` | `submitFoldBatch` | `POST` | `/v1/fold/batch` | Job | stable | Bearer | Fold Batch |
| `SubmitFoldseek3diBatch` | `submitFoldseek3diBatch` | `POST` | `/v1/foldseek/3di/batch` | Job | stable | Bearer | Foldseek 3Di Batch |
| `SubmitInlineSummary` | `submitInlineSummary` | `POST` | `/v1/job/{job_id}/summarize` | Job | experimental | Bearer | Submit Inline Summary |
| `SubmitOracleBatch` | `submitOracleBatch` | `POST` | `/v1/oracle/{probe_name}/batch` | Job | stable | Bearer | Oracle Single Batch |
| `SubmitOraclesBatch` | `submitOraclesBatch` | `POST` | `/v1/oracles/batch` | Job | stable | Bearer | Oracles Batch |
| `SubmitPrediction` | `submitPrediction` | `POST` | `/v1/predict` | Job | stable | Bearer | Submit Job |
| `SubmitProtifyTraining` | `submitProtifyTraining` | `POST` | `/v1/protify/train` | Job | experimental | Bearer | Submit Protify Train |
| `SubmitTranslatorBatch` | `submitTranslatorBatch` | `POST` | `/v1/translator/batch` | Job | stable | Bearer | Translator Batch |

