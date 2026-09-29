from __future__ import annotations

import copy
import sys
from pathlib import Path

import pytest


PROJECT_ROOT = Path(__file__).resolve().parents[2]
TOOLS_ROOT = PROJECT_ROOT / "Tools"
sys.path.insert(0, str(TOOLS_ROOT))

from synthyra_codegen.normalizer import load_verified_contract  # noqa: E402
from synthyra_codegen.parser import parse_contract  # noqa: E402


@pytest.fixture(scope="session")
def project_root() -> Path:
    return PROJECT_ROOT


@pytest.fixture(scope="session")
def contract_path(project_root: Path) -> Path:
    return project_root / "Contract" / "openapi.normalized.json"


@pytest.fixture()
def contract_document(contract_path: Path) -> dict:
    document, _ = load_verified_contract(contract_path)
    return copy.deepcopy(document)


@pytest.fixture(scope="session")
def contract_ir(contract_path: Path):
    document, digest = load_verified_contract(contract_path)
    return parse_contract(document, digest=digest)

