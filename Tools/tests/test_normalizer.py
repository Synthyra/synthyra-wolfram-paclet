from __future__ import annotations

import json
from pathlib import Path

import pytest

from synthyra_codegen.exceptions import ContractError
from synthyra_codegen.normalizer import (
    load_verified_contract,
    normalize_file,
    normalized_bytes,
    sidecar_path,
)


def test_normalization_is_stable_and_digest_verified(tmp_path: Path) -> None:
    source = tmp_path / "source.json"
    destination = tmp_path / "normalized.json"
    source.write_text('{"z": 1, "a": {"b": 2}}', encoding="utf-8")

    first_digest = normalize_file(source, destination)
    first_payload = destination.read_bytes()
    second_digest = normalize_file(destination, destination)

    assert first_digest == second_digest
    assert destination.read_bytes() == first_payload
    assert destination.read_bytes() == normalized_bytes({"a": {"b": 2}, "z": 1})
    assert sidecar_path(destination).read_text(encoding="ascii").startswith(first_digest)
    document, loaded_digest = load_verified_contract(destination)
    assert document == {"a": {"b": 2}, "z": 1}
    assert loaded_digest == first_digest


def test_digest_mismatch_fails_closed(tmp_path: Path) -> None:
    source = tmp_path / "source.json"
    destination = tmp_path / "normalized.json"
    source.write_text(json.dumps({"openapi": "3.1.0"}), encoding="utf-8")
    normalize_file(source, destination)
    destination.write_bytes(destination.read_bytes() + b" ")

    with pytest.raises(ContractError, match="contract digest mismatch"):
        load_verified_contract(destination)


def test_live_contract_url_is_rejected() -> None:
    with pytest.raises(ContractError, match="live URLs are forbidden"):
        normalize_file(
            Path("https://api.synthyra.com/openapi.json"), Path("ignored.json")
        )

