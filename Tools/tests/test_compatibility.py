from __future__ import annotations

import copy

from synthyra_codegen.compatibility import classify_compatibility
from synthyra_codegen.parser import parse_contract


def _report(old_document: dict, new_document: dict):
    return classify_compatibility(
        parse_contract(copy.deepcopy(old_document)),
        parse_contract(copy.deepcopy(new_document)),
    )


def test_identical_contract_is_patch(contract_document: dict) -> None:
    report = _report(contract_document, contract_document)
    assert report.level == "patch"
    assert report.issues == ()


def test_additive_public_operation_is_minor(contract_document: dict) -> None:
    new = copy.deepcopy(contract_document)
    new["paths"]["/version"] = {
        "get": {
            "operationId": "getVersion",
            "summary": "Version",
            "security": [],
            "x-synthyra-exposure": "public",
            "x-synthyra-stability": "stable",
            "x-synthyra-result-kind": "json",
            "responses": {
                "200": {
                    "description": "Version",
                    "content": {
                        "application/json": {
                            "schema": {"$ref": "#/components/schemas/HealthResponse"}
                        }
                    },
                }
            },
        }
    }
    new["x-synthyra-public-operation-count"] += 1

    report = _report(contract_document, new)
    assert report.level == "minor"
    assert any(issue.code == "operation-added" for issue in report.issues)


def test_stable_operation_removal_is_major(contract_document: dict) -> None:
    new = copy.deepcopy(contract_document)
    del new["paths"]["/health"]
    new["x-synthyra-public-operation-count"] -= 1

    report = _report(contract_document, new)
    assert report.level == "major"
    assert any(issue.code == "operation-removed" for issue in report.breaking)


def test_beta_operation_removal_is_minor(contract_document: dict) -> None:
    new = copy.deepcopy(contract_document)
    del new["paths"]["/v1/actome/tmap/intra"]
    new["x-synthyra-public-operation-count"] -= 1

    report = _report(contract_document, new)
    assert report.level == "minor"
    assert not report.breaking


def test_new_required_input_is_major(contract_document: dict) -> None:
    new = copy.deepcopy(contract_document)
    request = new["components"]["schemas"]["PredictRequest"]
    request["properties"]["client_trace"] = {"type": "string"}
    request["required"].append("client_trace")

    report = _report(contract_document, new)
    assert report.level == "major"
    assert any(issue.code == "required-property-added" for issue in report.breaking)


def test_authentication_change_is_major(contract_document: dict) -> None:
    new = copy.deepcopy(contract_document)
    new["paths"]["/v1/predict"]["post"]["security"] = []

    report = _report(contract_document, new)
    assert report.level == "major"
    assert any(issue.code == "authentication-changed" for issue in report.breaking)


def test_narrowed_response_is_major(contract_document: dict) -> None:
    old = copy.deepcopy(contract_document)
    old["components"]["schemas"]["HealthResponse"]["properties"]["status"][
        "enum"
    ] = ["ok", "degraded"]

    report = _report(old, contract_document)
    assert report.level == "major"
    assert any(issue.code == "enum-values-removed" for issue in report.breaking)


def test_tightened_stable_request_constraint_is_major(contract_document: dict) -> None:
    new = copy.deepcopy(contract_document)
    new["components"]["schemas"]["CampMsaRequest"]["properties"]["query_name"][
        "minLength"
    ] = 10

    report = _report(contract_document, new)

    assert report.level == "major"
    assert any(issue.code == "constraint-narrowed" for issue in report.breaking)


def test_relaxed_stable_request_constraint_is_minor(contract_document: dict) -> None:
    old = copy.deepcopy(contract_document)
    old["components"]["schemas"]["CampMsaRequest"]["properties"]["query_name"][
        "minLength"
    ] = 10

    report = _report(old, contract_document)

    assert report.level == "minor"
    assert any(issue.code == "constraint-widened" for issue in report.issues)
    assert not report.breaking


def test_typed_additional_properties_are_compared(contract_document: dict) -> None:
    new = copy.deepcopy(contract_document)
    query_sequences = new["components"]["schemas"]["PredictRequest"]["properties"][
        "query_sequences"
    ]
    map_schema = query_sequences["anyOf"][0]["items"]
    map_schema["additionalProperties"] = {"type": "integer"}

    report = _report(contract_document, new)

    assert report.level == "major"
    assert any(
        issue.code == "schema-type-changed"
        and "additional properties" in issue.detail
        for issue in report.breaking
    )
