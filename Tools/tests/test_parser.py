from __future__ import annotations

import copy

import pytest

from synthyra_codegen.exceptions import ContractError
from synthyra_codegen.parser import parse_contract, request_name


def test_authoritative_contract_covers_every_public_gateway_operation(contract_ir) -> None:
    operations = {item.operation_id: item for item in contract_ir.operations}

    assert len(operations) == contract_ir.source["x-synthyra-public-operation-count"] == 51
    assert contract_ir.source["x-synthyra-live-openapi-sha256"]
    assert {"getHealth", "getHealthReady"} <= set(operations)
    assert all("pde" not in item.path for item in operations.values())
    assert operations["getHealth"].anonymous is True
    assert operations["scoreAtlasPpi"].security == ("BearerAuth",)
    assert operations["submitPrediction"].job.status_operation_id == "getJob"
    assert operations["submitPrediction"].job.result_operation_id == "getJobResult"
    assert operations["submitPrediction"].safe_to_retry is False
    assert operations["downloadDeepResearch"].result_kind == "Binary"
    assert operations["getBatchResultsJsonl"].result_kind == "JSONL"
    assert operations["createIntraActomeTmap"].result_kind == "TmapTree"
    assert {item.result_kind for item in operations.values()} == {
        "Binary",
        "Job",
        "JSON",
        "JSONL",
        "TmapTree",
    }


def test_parameter_serialization_is_retained(contract_ir) -> None:
    operation = next(
        item for item in contract_ir.operations if item.operation_id == "listJobs"
    )
    parameters = {item.name: item for item in operation.parameters}

    assert parameters["page_size"].location == "query"
    assert parameters["page_size"].style == "form"
    assert parameters["page_size"].explode is True
    assert parameters["page_size"].schema.kind == "integer"
    assert parameters["status"].schema.nullable is True


def test_request_name_preserves_biological_acronyms() -> None:
    assert request_name("submit_ppi_network") == "SubmitPPINetwork"
    assert request_name("getPLIById") == "GetPLIByID"


def test_remote_ref_fails_closed(contract_document: dict) -> None:
    contract_document["paths"]["/v1/predict"]["post"]["requestBody"][
        "content"
    ]["application/json"]["schema"] = {"$ref": "https://example.com/schema.json"}

    with pytest.raises(ContractError, match=r"unsupported non-local \$ref"):
        parse_contract(contract_document)


def test_non_nullable_union_fails_closed(contract_document: dict) -> None:
    contract_document["components"]["schemas"]["HealthResponse"]["properties"][
        "status"
    ] = {"oneOf": [{"type": "string"}, {"type": "integer"}]}

    with pytest.raises(ContractError, match="must contain exactly one null branch"):
        parse_contract(contract_document)


def test_missing_exposure_metadata_fails_closed(contract_document: dict) -> None:
    del contract_document["paths"]["/health"]["get"]["x-synthyra-exposure"]

    with pytest.raises(ContractError, match="x-synthyra-exposure"):
        parse_contract(contract_document)


def test_untyped_object_requires_explicit_opaque_marker(contract_document: dict) -> None:
    schema = contract_document["components"]["schemas"]["HealthResponse"]
    schema["properties"]["opaque"] = {"type": "object"}

    with pytest.raises(ContractError, match="untyped object is forbidden"):
        parse_contract(contract_document)


def test_component_local_ref_resolution(contract_ir) -> None:
    schemas = {item.name: item.schema for item in contract_ir.schemas}
    response = schemas["VerboseNetwork"]
    edges = next(prop for prop in response.properties if prop.name == "edges")

    assert edges.schema.items.kind == "reference"
    assert edges.schema.items.reference == "NetworkEdge"


def test_optional_bearer_authentication_is_explicit(contract_document: dict) -> None:
    contract = parse_contract(contract_document)
    operation = next(
        item for item in contract.operations if item.operation_id == "getSharedAnalysis"
    )
    assert operation.anonymous is True
    assert operation.security == ("BearerAuth",)


def test_pdf_download_is_a_supported_binary_result(contract_document: dict) -> None:
    contract = parse_contract(contract_document)
    operation = next(
        item for item in contract.operations if item.operation_id == "downloadDeepResearch"
    )
    assert operation.responses[0].media_type == "application/pdf"
    assert operation.result_kind == "Binary"
