"""Run the paclet's Wolfram Language suites (Tests/Wolfram/*.wlt) through wolframscript.

Marked `wolfram`: it needs a licensed Wolfram kernel, so `ws test --projects` runs it only with
`--include wolfram`. Requested on a machine without wolframscript, it fails rather than skips.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess

from pathlib import Path

import pytest


WINDOWS_WOLFRAMSCRIPT = Path("C:/Program Files/Wolfram Research/WolframScript/wolframscript.exe")
SUITE_TIMEOUT_SECONDS = 1800


def wolframscript() -> str:
    found = os.environ.get("WOLFRAMSCRIPT") or shutil.which("wolframscript")
    if found is None and WINDOWS_WOLFRAMSCRIPT.is_file():
        found = str(WINDOWS_WOLFRAMSCRIPT)
    assert found is not None, "the wolfram marker needs wolframscript on PATH or in WOLFRAMSCRIPT"
    return found


@pytest.mark.wolfram
def test_wolfram_suites_pass(project_root: Path, tmp_path: Path) -> None:
    summary_path = tmp_path / "summary.json"
    completed = subprocess.run(
        [wolframscript(), "-file", str(project_root / "Tools" / "RunAllWolframTests.wls"), str(summary_path)],
        capture_output=True,
        text=True,
        timeout=SUITE_TIMEOUT_SECONDS,
        check=False,
    )
    assert summary_path.is_file(), f"the Wolfram suites wrote no summary:\n{completed.stdout[-4000:]}\n{completed.stderr[-4000:]}"
    summary = json.loads(summary_path.read_text(encoding="utf-8"))
    assert summary["failed"] == 0, f"failed Wolfram tests: {summary['failed_ids']}\n{summary['failed_debug']}"
    assert summary["succeeded"] > 0
    assert completed.returncode == 0, completed.stdout[-4000:]
