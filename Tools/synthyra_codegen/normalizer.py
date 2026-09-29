"""Canonical JSON bytes for the contract, and its SHA-256 sidecar."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any
from urllib.parse import urlparse

from .exceptions import ContractError


def _reject_url(value: str) -> None:
    parsed = urlparse(value)
    if parsed.scheme.lower() in {"http", "https"}:
        raise ContractError(
            "live URLs are forbidden here; Tools/refresh_contract.py fetches the live document"
        )


def normalized_bytes(document: dict[str, Any]) -> bytes:
    """Return stable UTF-8 JSON bytes with recursively sorted object keys."""

    return (
        json.dumps(
            document,
            ensure_ascii=False,
            indent=2,
            sort_keys=True,
            separators=(",", ": "),
        )
        + "\n"
    ).encode("utf-8")


def sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def sidecar_path(contract_path: Path) -> Path:
    return contract_path.with_suffix(".sha256")


def normalize_file(source: Path, destination: Path) -> str:
    _reject_url(str(source))
    raw = json.loads(source.read_text(encoding="utf-8"))
    if not isinstance(raw, dict):
        raise ContractError("the OpenAPI document root must be an object")
    payload = normalized_bytes(raw)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(payload)
    digest = sha256_bytes(payload)
    sidecar_path(destination).write_text(
        f"{digest}  {destination.name}\n", encoding="ascii", newline="\n"
    )
    return digest


def load_verified_contract(path: Path) -> tuple[dict[str, Any], str]:
    _reject_url(str(path))
    payload = path.read_bytes()
    digest = sha256_bytes(payload)
    expected_path = sidecar_path(path)
    if not expected_path.is_file():
        raise ContractError(f"missing contract digest sidecar: {expected_path}")
    expected = expected_path.read_text(encoding="ascii").split()[0].lower()
    if expected != digest:
        raise ContractError(
            f"contract digest mismatch for {path}: expected {expected}, calculated {digest}"
        )
    document = json.loads(payload)
    if not isinstance(document, dict):
        raise ContractError("the OpenAPI document root must be an object")
    if normalized_bytes(document) != payload:
        raise ContractError(
            f"{path} is not normalized; rebuild it with Tools/refresh_contract.py"
        )
    return document, digest

