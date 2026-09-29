"""Render Wolfram metadata, documentation, manifests, and mock fixtures."""

from __future__ import annotations

import base64
import binascii
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from jinja2 import Environment, FileSystemLoader, StrictUndefined

from .exceptions import GenerationDriftError
from .model import (
    ContractIR,
    JobIR,
    OperationIR,
    ParameterIR,
    PropertyIR,
    RequestBodyIR,
    ResponseIR,
    TypeIR,
)
from .parser import BINARY_MEDIA_TYPES, JSONL_MEDIA_TYPES


GENERATED_PATHS = (
    Path("Kernel/Generated/Loader.wl"),
    Path("Kernel/Generated/Schemas.wl"),
    Path("Kernel/Generated/Operations.wl"),
    Path("Kernel/Generated/Validators.wl"),
    Path("Kernel/Generated/OperationManifest.json"),
    Path("Documentation/Generated/Operations.md"),
    Path("Tests/Generated/MockResponses.json"),
)


KIND_NAMES = {
    "reference": "Reference",
    "string": "String",
    "integer": "Integer",
    "number": "Number",
    "boolean": "Boolean",
    "array": "Array",
    "object": "Object",
    "null": "Null",
}


@dataclass(frozen=True)
class RenderedFile:
    relative_path: Path
    content: bytes


def type_data(schema: TypeIR) -> dict[str, Any]:
    data: dict[str, Any] = {
        "Kind": KIND_NAMES[schema.kind],
        "Nullable": schema.nullable,
    }
    if schema.format:
        data["Format"] = schema.format
    if schema.reference:
        data["Reference"] = schema.reference
    if schema.enum:
        data["Enum"] = list(schema.enum)
    if schema.description:
        data["Description"] = schema.description
    if schema.default is not None:
        data["Default"] = schema.default
    if schema.example is not None:
        data["Example"] = schema.example
    if schema.constraints:
        data["Constraints"] = dict(schema.constraints)
    if schema.items is not None:
        data["Items"] = type_data(schema.items)
    if schema.properties:
        data["Properties"] = {
            prop.name: property_data(prop) for prop in schema.properties
        }
    if schema.additional_properties is not None:
        data["AdditionalProperties"] = (
            type_data(schema.additional_properties)
            if isinstance(schema.additional_properties, TypeIR)
            else schema.additional_properties
        )
    if schema.opaque:
        data["Opaque"] = True
    return data


def property_data(prop: PropertyIR) -> dict[str, Any]:
    data = {"Required": prop.required, "Schema": type_data(prop.schema)}
    if prop.description:
        data["Description"] = prop.description
    return data


def parameter_data(parameter: ParameterIR) -> dict[str, Any]:
    result = {
        "Name": parameter.name,
        "In": parameter.location.title(),
        "Required": parameter.required,
        "Style": parameter.style,
        "Explode": parameter.explode,
        "Schema": type_data(parameter.schema),
    }
    if parameter.description:
        result["Description"] = parameter.description
    return result


def request_body_data(body: RequestBodyIR | None) -> dict[str, Any] | None:
    if body is None:
        return None
    result = {
        "Required": body.required,
        "ContentType": body.media_type,
        "Schema": type_data(body.schema),
    }
    if body.description:
        result["Description"] = body.description
    return result


def response_data(response: ResponseIR) -> dict[str, Any]:
    result: dict[str, Any] = {
        "Status": response.status,
        "Description": response.description,
        "ContentType": response.media_type,
        "Schema": type_data(response.schema) if response.schema else None,
    }
    if response.example is not None:
        result["Example"] = response.example
    return result


def job_data(job: JobIR | None) -> dict[str, Any] | None:
    if job is None:
        return None
    result: dict[str, Any] = {
        "StatusOperationId": job.status_operation_id,
        "ResultOperationId": job.result_operation_id,
        "IdentifierParameter": job.identifier_parameter,
        "TerminalStates": list(job.terminal_states),
        "SuccessStates": list(job.success_states),
        "FailureStates": list(job.failure_states),
    }
    if job.cancel_operation_id:
        result["CancelOperationId"] = job.cancel_operation_id
    return result


def operation_data(operation: OperationIR) -> dict[str, Any]:
    result: dict[str, Any] = {
        "OperationId": operation.operation_id,
        "RequestName": operation.request_name,
        "Method": operation.method,
        "Path": operation.path,
        "Summary": operation.summary,
        "Description": operation.description,
        "Tags": list(operation.tags),
        "Parameters": [parameter_data(item) for item in operation.parameters],
        "RequestBody": request_body_data(operation.request_body),
        "Responses": [response_data(item) for item in operation.responses],
        "ResultKind": operation.result_kind,
        "Stability": operation.stability.title(),
        "Exposure": operation.exposure.title(),
        "Authentication": {
            "Anonymous": operation.anonymous,
            "Schemes": list(operation.security),
        },
        "SafeToRetry": operation.safe_to_retry,
    }
    if operation.job is not None:
        result["Async"] = job_data(operation.job)
    return result


def _wl(value: Any) -> str:
    if value is None:
        return "Null"
    if value is True:
        return "True"
    if value is False:
        return "False"
    if isinstance(value, str):
        return json.dumps(value, ensure_ascii=False)
    if isinstance(value, (int, float)):
        return repr(value)
    if isinstance(value, (list, tuple)):
        return "{" + ", ".join(_wl(item) for item in value) + "}"
    if isinstance(value, dict):
        return (
            "<|"
            + ", ".join(f"{_wl(str(key))} -> {_wl(value[key])}" for key in sorted(value))
            + "|>"
        )
    raise TypeError(f"cannot render {type(value).__name__} as Wolfram Language")


def _environment() -> Environment:
    return Environment(
        loader=FileSystemLoader(Path(__file__).with_name("templates")),
        undefined=StrictUndefined,
        autoescape=False,
        keep_trailing_newline=True,
        newline_sequence="\n",
    )


def _render_template(name: str, **context: Any) -> bytes:
    text = _environment().get_template(name).render(**context)
    if not text.endswith("\n"):
        text += "\n"
    return text.encode("utf-8")


def _json_bytes(value: Any) -> bytes:
    return (
        json.dumps(value, ensure_ascii=False, indent=2, sort_keys=True) + "\n"
    ).encode("utf-8")


def _schema_example(
    schema: TypeIR,
    named_schemas: dict[str, TypeIR],
    *,
    seen: frozenset[str] = frozenset(),
) -> Any:
    if schema.example is not None:
        return schema.example
    if schema.enum:
        return schema.enum[0]
    if schema.kind == "reference":
        assert schema.reference is not None
        if schema.reference in seen:
            return None
        return _schema_example(
            named_schemas[schema.reference],
            named_schemas,
            seen=seen | {schema.reference},
        )
    if schema.kind == "string":
        if schema.format == "date-time":
            return "2026-01-01T00:00:00Z"
        if schema.format == "date":
            return "2026-01-01"
        return "example"
    if schema.kind == "integer":
        return 0
    if schema.kind == "number":
        return 0.0
    if schema.kind == "boolean":
        return False
    if schema.kind == "array":
        minimum = dict(schema.constraints).get("minItems", 0)
        count = max(0, int(minimum))
        return [
            _schema_example(schema.items, named_schemas, seen=seen)
            for _ in range(count)
        ]
    if schema.kind == "object":
        return {
            prop.name: _schema_example(prop.schema, named_schemas, seen=seen)
            for prop in schema.properties
            if prop.required
        }
    return None


def _response_example(response: ResponseIR, named_schemas: dict[str, TypeIR]) -> Any:
    example = response.example
    if isinstance(example, dict) and example and all(
        isinstance(value, dict) and ("value" in value or "externalValue" in value)
        for value in example.values()
    ):
        first = example[sorted(example)[0]]
        if "externalValue" in first:
            raise ValueError("external response examples are forbidden in checked-in fixtures")
        example = first["value"]
    if example is not None:
        return example
    if response.schema is None:
        return None
    return _schema_example(response.schema, named_schemas)


def _mock_fixtures(contract: ContractIR) -> dict[str, Any]:
    named_schemas = {schema.name: schema.schema for schema in contract.schemas}
    fixtures: dict[str, Any] = {}
    for operation in contract.operations:
        successes = [response for response in operation.responses if response.status.startswith("2")]
        response = successes[0]
        content_type = response.media_type or ""
        fixture: dict[str, Any] = {
            "status": int(response.status) if response.status.isdigit() else response.status,
            "headers": {
                "content-type": content_type,
                "x-request-id": f"fixture-{operation.operation_id}",
            },
        }
        example = _response_example(response, named_schemas)
        if content_type in BINARY_MEDIA_TYPES:
            encoded = "AAE="
            if isinstance(example, str):
                try:
                    base64.b64decode(example, validate=True)
                    encoded = example
                except (binascii.Error, ValueError):
                    pass
            fixture["bodyBase64"] = encoded
        elif content_type in JSONL_MEDIA_TYPES:
            if isinstance(example, str):
                fixture["body"] = example
            elif isinstance(example, list):
                fixture["body"] = "\n".join(
                    json.dumps(item, ensure_ascii=False, sort_keys=True) for item in example
                ) + "\n"
            else:
                fixture["body"] = json.dumps(
                    example, ensure_ascii=False, sort_keys=True
                ) + "\n"
        else:
            fixture["body"] = example
        fixtures[operation.operation_id] = fixture
    return {
        "contractDigest": contract.contract_digest,
        "fixtures": fixtures,
    }


def render(contract: ContractIR) -> tuple[RenderedFile, ...]:
    operations = {
        operation.request_name: operation_data(operation)
        for operation in sorted(contract.operations, key=lambda item: item.request_name)
    }
    manifest = {
        operation.operation_id: operation.request_name
        for operation in contract.operations
    }
    schemas = {schema.name: type_data(schema.schema) for schema in contract.schemas}
    serializer_specs = {
        operation.request_name: {
            "OperationId": operation.operation_id,
            "Parameters": [parameter_data(item) for item in operation.parameters],
            "RequestBody": request_body_data(operation.request_body),
        }
        for operation in sorted(contract.operations, key=lambda item: item.request_name)
    }
    validator_specs = schemas
    docs_rows = [
        {
            "request_name": operation.request_name,
            "operation_id": operation.operation_id,
            "method": operation.method,
            "path": operation.path,
            "result_kind": operation.result_kind,
            "stability": operation.stability,
            "authentication": (
                "Optional bearer"
                if operation.anonymous and operation.security
                else "Anonymous" if operation.anonymous else "Bearer"
            ),
            "summary": operation.summary,
        }
        for operation in sorted(contract.operations, key=lambda item: item.request_name)
    ]
    operation_manifest = {
        "contractDigest": contract.contract_digest,
        "contractVersion": contract.version,
        "generatorFormatVersion": 1,
        "publicOperationCount": len(contract.operations),
        "sourceCommit": contract.source_commit,
        "operations": manifest,
    }

    return (
        RenderedFile(
            Path("Kernel/Generated/Loader.wl"),
            _render_template("loader.wl.j2"),
        ),
        RenderedFile(
            Path("Kernel/Generated/Schemas.wl"),
            _render_template(
                "schemas.wl.j2",
                contract_digest=contract.contract_digest,
                schemas_wl=_wl(schemas),
            ),
        ),
        RenderedFile(
            Path("Kernel/Generated/Operations.wl"),
            _render_template(
                "operations.wl.j2",
                contract_digest=contract.contract_digest,
                operations_wl=_wl(operations),
                manifest_wl=_wl(manifest),
            ),
        ),
        RenderedFile(
            Path("Kernel/Generated/Validators.wl"),
            _render_template(
                "validators.wl.j2",
                contract_digest=contract.contract_digest,
                validators_wl=_wl(validator_specs),
                serializers_wl=_wl(serializer_specs),
            ),
        ),
        RenderedFile(
            Path("Kernel/Generated/OperationManifest.json"),
            _json_bytes(operation_manifest),
        ),
        RenderedFile(
            Path("Documentation/Generated/Operations.md"),
            _render_template(
                "operations.md.j2",
                contract_title=contract.title,
                contract_version=contract.version,
                contract_digest=contract.contract_digest,
                source_commit=contract.source_commit,
                public_operation_count=len(contract.operations),
                rows=docs_rows,
            ),
        ),
        RenderedFile(
            Path("Tests/Generated/MockResponses.json"),
            _json_bytes(_mock_fixtures(contract)),
        ),
    )


def generate(contract: ContractIR, output_root: Path, *, check: bool = False) -> tuple[Path, ...]:
    """Write generated files, or verify that checked-in files are byte-identical."""

    rendered = render(contract)
    if tuple(item.relative_path for item in rendered) != GENERATED_PATHS:
        raise AssertionError("generated path registry is out of sync")
    drift: list[Path] = []
    for item in rendered:
        destination = output_root / item.relative_path
        if check:
            if not destination.is_file() or destination.read_bytes() != item.content:
                drift.append(item.relative_path)
            continue
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(item.content)
    if drift:
        formatted = ", ".join(path.as_posix() for path in drift)
        raise GenerationDriftError(f"generated files are stale or missing: {formatted}")
    return tuple(item.relative_path for item in rendered)
