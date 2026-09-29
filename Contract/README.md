# API contract

The paclet's operation tables in `Kernel/Generated/` are generated from the files here, never
from a live server at build time.

| File | Holds |
|---|---|
| `gateway-openapi.json` | The live `https://api.synthyra.com/openapi.json`, byte for byte, as last fetched |
| `overlay.json` | The paclet's view of it: which operations it exposes, keyed by method and path, and the `x-synthyra-*` metadata the generator needs (Wolfram name, exposure, stability, result kind, job handling) |
| `openapi.normalized.json` | The merge of the two, in canonical JSON, which the generator reads |
| `openapi.normalized.sha256` | That file's digest, which the generator verifies before reading it |

Refresh after the API changes:

```bash
python Tools/refresh_contract.py          # fetch, merge, write the contract and its digest
python Tools/generate.py                  # regenerate Kernel/Generated/
python Tools/refresh_contract.py --check  # exit 3 when the live API has drifted
```

A route the live API adds or removes fails the merge until `overlay.json` describes or excludes
it, so the paclet never exposes an operation nobody reviewed.
