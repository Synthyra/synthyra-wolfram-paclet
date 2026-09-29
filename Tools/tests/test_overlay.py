from __future__ import annotations

import copy
import json

from pathlib import Path

import pytest

from synthyra_codegen.exceptions import ContractError
from synthyra_codegen.normalizer import load_verified_contract, normalized_bytes
from synthyra_codegen.overlay import live_digest, load_overlay, mark_opaque, merge


@pytest.fixture(scope="module")
def live_payload(project_root: Path) -> bytes:
    return (project_root / "Contract" / "gateway-openapi.json").read_bytes()


@pytest.fixture()
def live(live_payload: bytes) -> dict:
    return json.loads(live_payload)


@pytest.fixture()
def overlay(project_root: Path) -> dict:
    return load_overlay(project_root / "Contract" / "overlay.json")


def test_checked_in_contract_is_the_merge_of_its_inputs(
    live: dict, live_payload: bytes, overlay: dict, contract_path: Path
) -> None:
    contract, _ = load_verified_contract(contract_path)
    merged = merge(live, overlay, live_sha256=live_digest(live_payload))
    assert normalized_bytes(merged) == normalized_bytes(contract)
    assert contract["x-synthyra-live-openapi-sha256"] == live_digest(live_payload)


def test_unlisted_live_route_fails(live: dict, overlay: dict) -> None:
    live["paths"]["/v1/new-feature"] = {"post": {"responses": {}}}
    with pytest.raises(ContractError, match="served but not in the overlay: POST /v1/new-feature"):
        merge(live, overlay, live_sha256="0" * 64)


def test_removed_live_route_fails(live: dict, overlay: dict) -> None:
    del live["paths"]["/v1/atlas-ppi/score"]
    with pytest.raises(ContractError, match="in the overlay but not served: POST /v1/atlas-ppi/score"):
        merge(live, overlay, live_sha256="0" * 64)


def test_request_shape_comes_from_the_server(live: dict, overlay: dict) -> None:
    schema = live["components"]["schemas"]["AtlasScoreRequest"]
    schema["properties"]["new_option"] = {"type": "boolean"}
    merged = merge(live, overlay, live_sha256="0" * 64)
    assert "new_option" in merged["components"]["schemas"]["AtlasScoreRequest"]["properties"]


def test_overlay_may_not_replace_a_served_request_body(live: dict, overlay: dict) -> None:
    overlay = copy.deepcopy(overlay)
    overlay["operations"]["POST /v1/atlas-ppi/score"]["requestBody"] = {"content": {}}
    with pytest.raises(ContractError, match="server declares a request body"):
        merge(live, overlay, live_sha256="0" * 64)


def test_schema_defined_twice_fails(live: dict, overlay: dict) -> None:
    overlay["schemas"]["AtlasScoreRequest"] = {"type": "object", "properties": {}}
    with pytest.raises(ContractError, match="defined both live and in the overlay"):
        merge(live, overlay, live_sha256="0" * 64)


def test_example_for_unknown_parameter_fails(live: dict, overlay: dict) -> None:
    overlay["operations"]["GET /v1/job/{job_id}"]["parameterExamples"]["missing"] = "x"
    with pytest.raises(ContractError, match="parameters the server lacks"):
        merge(live, overlay, live_sha256="0" * 64)


def test_untyped_objects_become_opaque() -> None:
    marked = mark_opaque(
        {
            "type": "object",
            "properties": {
                "free": {"type": "object", "additionalProperties": True},
                "typed": {"type": "object", "additionalProperties": {"type": "number"}},
            },
        }
    )
    free, typed = marked["properties"]["free"], marked["properties"]["typed"]
    assert free["x-synthyra-opaque"] is True and free["properties"] == {}
    assert typed["properties"] == {} and "x-synthyra-opaque" not in typed


def test_exclusions_need_reasons(tmp_path: Path, project_root: Path) -> None:
    raw = json.loads((project_root / "Contract" / "overlay.json").read_text(encoding="utf-8"))
    raw["excluded"]["POST /v1/bli/rank"] = " "
    path = tmp_path / "overlay.json"
    path.write_text(json.dumps(raw), encoding="utf-8")
    with pytest.raises(ContractError, match="needs a reason"):
        load_overlay(path)
