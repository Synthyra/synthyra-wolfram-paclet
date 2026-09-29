# Tools

| Tool | Does |
|---|---|
| `refresh_contract.py` | Fetches the live API contract and merges `Contract/overlay.json` into `Contract/openapi.normalized.json`; `--check` reports drift |
| `generate.py` | Generates `Kernel/Generated/` from the normalized contract; `--check` fails when the generated files are stale |
| `update_paclet_version.py` | Bumps or checks the version in `PacletInfo.wl` and the kernel |
| `synthyra_codegen/`, `tests/` | The generator and its tests; `tests/test_wolfram_suite.py` also runs the Wolfram suites (marker `wolfram`) |
| `RunAllWolframTests.wls` | Every suite in `Tests/Wolfram/`, with a JSON summary |
| `PacletCI.wls` | `CheckPaclet`, `TestPaclet`, and `BuildPaclet` through PacletCICD; with `SYNTHYRALINK_BUILD_DIRECTORY` set it builds from a copy there |
| `CleanInstallSmoke.wls` | Installs the archive named by `SYNTHYRALINK_PACLET_ARCHIVE` in a clean kernel, checks credential redaction, and replays the TP53 demo from the installed paclet |
| `RecordDemo.wls` | Runs `Examples/TP53.nb` against production and rewrites `Examples/Data/TP53Demo.wxf`; the one live check of every protein function, it needs `SYNTHYRA_API_KEY` and bills the demo's calls |

The generator is Python 3.12 with pinned dependencies:

```bash
python -m pip install --require-hashes --requirement Tools/requirements.lock
python Tools/generate.py --check
python -m pytest Tools/tests -m "not wolfram"
```

A clean second generation is byte-identical to the first.
