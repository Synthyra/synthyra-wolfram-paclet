#!/usr/bin/env python3
"""Rebuild Contract/openapi.normalized.json from the live Gateway and Contract/overlay.json.

The live document is kept beside the contract as Contract/gateway-openapi.json, byte for byte,
so the merge can be repeated and tested offline.

    python Tools/refresh_contract.py            # fetch, merge, write the contract and its digest
    python Tools/refresh_contract.py --check    # fail with exit 3 when the live Gateway has drifted
    python Tools/refresh_contract.py --source saved-openapi.json --check

After a refresh, regenerate the kernel tables with `python Tools/generate.py`. A route the
server adds or removes fails the merge until the overlay describes or excludes it.
"""

from __future__ import annotations

import argparse
import json
import sys
import urllib.request

from pathlib import Path
from urllib.parse import urlparse

from synthyra_codegen.compatibility import classify_compatibility
from synthyra_codegen.exceptions import ContractError
from synthyra_codegen.normalizer import load_verified_contract, normalized_bytes, sha256_bytes, sidecar_path
from synthyra_codegen.overlay import live_digest, load_overlay, merge, operation_diff
from synthyra_codegen.parser import parse_contract


ROOT = Path(__file__).resolve().parents[1]
LIVE_URL = "https://api.synthyra.com/openapi.json"
TIMEOUT_SECONDS = 60


def fetch(url: str) -> bytes:
    if urlparse(url).scheme != "https":
        raise ContractError(f"refusing to fetch the contract over a non-HTTPS URL: {url}")
    request = urllib.request.Request(url, headers={"Accept": "application/json", "User-Agent": "SynthyraLink-refresh"})
    with urllib.request.urlopen(request, timeout=TIMEOUT_SECONDS) as response:
        return response.read()


def build(live_payload: bytes, overlay_path: Path) -> dict:
    live = json.loads(live_payload)
    if not isinstance(live, dict):
        raise ContractError("the live OpenAPI document root must be an object")
    document = merge(live, load_overlay(overlay_path), live_sha256=live_digest(live_payload))
    parse_contract(document)
    return document


def report_drift(current: dict, fresh: dict) -> str:
    diff = operation_diff(current, fresh)
    lines = [f"  {label}: {', '.join(keys)}" for label, keys in diff.items() if keys]
    try:
        compatibility = classify_compatibility(parse_contract(current), parse_contract(fresh))
        lines.append(f"  compatibility: {compatibility.level}")
        lines += [
            f"    {issue.level} {issue.code} {issue.operation_id}: {issue.detail}" for issue in compatibility.issues
        ]
    except ContractError as error:
        lines.append(f"  compatibility: unknown ({error})")
    return "\n".join(lines) or "  only contract metadata changed"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--url", default=LIVE_URL, help="live OpenAPI document (HTTPS only)")
    parser.add_argument("--source", type=Path, help="a saved OpenAPI document, used instead of --url")
    parser.add_argument("--overlay", type=Path, default=ROOT / "Contract" / "overlay.json")
    parser.add_argument("--contract", type=Path, default=ROOT / "Contract" / "openapi.normalized.json")
    parser.add_argument("--live-copy", type=Path, default=ROOT / "Contract" / "gateway-openapi.json")
    parser.add_argument("--check", action="store_true", help="compare only; never write")
    args = parser.parse_args(argv)

    try:
        live_payload = args.source.read_bytes() if args.source else fetch(args.url)
        fresh = build(live_payload, args.overlay)
        current, _ = load_verified_contract(args.contract)
    except (ContractError, OSError, json.JSONDecodeError) as error:
        print(f"refresh failed: {error}", file=sys.stderr)
        return 2

    if normalized_bytes(fresh) == normalized_bytes(current):
        if not args.check and not args.live_copy.is_file():
            args.live_copy.write_bytes(live_payload)
        print(f"in sync: {len(fresh['paths'])} paths, contract {sha256_bytes(normalized_bytes(fresh))}")
        return 0
    if args.check:
        print("the live Gateway differs from the checked-in contract:\n" + report_drift(current, fresh))
        return 3

    payload = normalized_bytes(fresh)
    args.live_copy.write_bytes(live_payload)
    args.contract.write_bytes(payload)
    digest = sha256_bytes(payload)
    sidecar_path(args.contract).write_text(f"{digest}  {args.contract.name}\n", encoding="ascii", newline="\n")
    print("updated the contract:\n" + report_drift(current, fresh))
    print(f"contract sha256: {digest}\nnext: python Tools/generate.py")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
