"""Command-line entry point for deterministic generation."""

from __future__ import annotations

import argparse
from pathlib import Path

from .exceptions import ContractError, GenerationDriftError
from .normalizer import load_verified_contract
from .parser import parse_contract
from .render import generate


def project_root() -> Path:
    return Path(__file__).resolve().parents[2]


def main(argv: list[str] | None = None) -> int:
    root = project_root()
    parser = argparse.ArgumentParser(
        description="Generate SynthyraLink from a checked-in normalized OpenAPI contract."
    )
    parser.add_argument(
        "--contract",
        type=Path,
        default=root / "Contract" / "openapi.normalized.json",
        help="Local normalized contract snapshot (live URLs are rejected).",
    )
    parser.add_argument("--output-root", type=Path, default=root)
    parser.add_argument(
        "--check",
        action="store_true",
        help="Fail if generated files differ; never write in this mode.",
    )
    args = parser.parse_args(argv)
    try:
        document, digest = load_verified_contract(args.contract)
        contract = parse_contract(document, digest=digest)
        paths = generate(contract, args.output_root, check=args.check)
    except (ContractError, GenerationDriftError) as exc:
        parser.error(str(exc))
    action = "verified" if args.check else "generated"
    print(f"{action} {len(paths)} files from {len(contract.operations)} public operations")
    print(f"contract sha256: {contract.contract_digest}")
    return 0

