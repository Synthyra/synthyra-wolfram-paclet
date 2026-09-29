from __future__ import annotations

from pathlib import Path

import pytest

from synthyra_codegen.exceptions import GenerationDriftError
from synthyra_codegen.parser import parse_contract
from synthyra_codegen.render import GENERATED_PATHS, generate, render


def test_generated_outputs_are_deterministic(contract_ir, tmp_path: Path) -> None:
    first = render(contract_ir)
    second = render(contract_ir)

    assert first == second
    assert tuple(item.relative_path for item in first) == GENERATED_PATHS
    generate(contract_ir, tmp_path)
    snapshot = {path: (tmp_path / path).read_bytes() for path in GENERATED_PATHS}
    generate(contract_ir, tmp_path)
    assert snapshot == {path: (tmp_path / path).read_bytes() for path in GENERATED_PATHS}
    generate(contract_ir, tmp_path, check=True)


def test_check_mode_reports_precise_drift(contract_ir, tmp_path: Path) -> None:
    generate(contract_ir, tmp_path)
    changed = tmp_path / "Kernel" / "Generated" / "Operations.wl"
    changed.write_text("stale", encoding="utf-8")

    with pytest.raises(GenerationDriftError, match="Kernel/Generated/Operations.wl"):
        generate(contract_ir, tmp_path, check=True)


def test_checked_in_outputs_are_golden(contract_ir, project_root: Path) -> None:
    generate(contract_ir, project_root, check=True)


def test_manifest_and_mock_fixtures_contain_all_operations(
    contract_ir, project_root: Path
) -> None:
    import base64
    import json

    manifest = json.loads(
        (project_root / "Kernel/Generated/OperationManifest.json").read_text(
            encoding="utf-8"
        )
    )
    fixtures = json.loads(
        (project_root / "Tests/Generated/MockResponses.json").read_text(
            encoding="utf-8"
        )
    )
    operation_ids = {item.operation_id for item in contract_ir.operations}

    assert set(manifest["operations"]) == operation_ids
    assert manifest["publicOperationCount"] == len(contract_ir.operations)
    assert manifest["sourceCommit"] == contract_ir.source_commit
    assert set(fixtures["fixtures"]) == operation_ids
    binary = fixtures["fixtures"]["downloadDeepResearch"]["bodyBase64"]
    assert base64.b64decode(binary, validate=True)
    jsonl = fixtures["fixtures"]["getBatchResultsJsonl"]["body"]
    assert jsonl.endswith("\n")
    assert all(json.loads(line) is not None for line in jsonl.splitlines())


def test_optional_bearer_is_visible_in_generated_metadata_and_docs(
    contract_document: dict,
) -> None:
    generated = {
        item.relative_path.as_posix(): item.content.decode("utf-8")
        for item in render(parse_contract(contract_document))
        if item.relative_path.suffix in {".wl", ".md"}
    }

    operations = generated["Kernel/Generated/Operations.wl"]
    docs = generated["Documentation/Generated/Operations.md"]
    assert '"Anonymous" -> True' in operations
    assert '"Schemes" -> {"BearerAuth"}' in operations
    assert "Optional bearer" in docs
