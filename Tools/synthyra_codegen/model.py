"""Immutable intermediate representation for SynthyraLink generation."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


JSONValue = None | bool | int | float | str | list["JSONValue"] | dict[str, "JSONValue"]


@dataclass(frozen=True)
class PropertyIR:
    name: str
    schema: "TypeIR"
    required: bool
    description: str = ""


@dataclass(frozen=True)
class TypeIR:
    """A deliberately small, lossless subset of JSON Schema 2020-12."""

    kind: str
    nullable: bool = False
    format: str | None = None
    reference: str | None = None
    enum: tuple[JSONValue, ...] = ()
    properties: tuple[PropertyIR, ...] = ()
    items: "TypeIR | None" = None
    additional_properties: bool | "TypeIR" | None = None
    description: str = ""
    default: JSONValue = None
    example: JSONValue = None
    constraints: tuple[tuple[str, JSONValue], ...] = ()
    opaque: bool = False


@dataclass(frozen=True)
class SchemaIR:
    name: str
    schema: TypeIR


@dataclass(frozen=True)
class ParameterIR:
    name: str
    location: str
    required: bool
    style: str
    explode: bool
    schema: TypeIR
    description: str = ""


@dataclass(frozen=True)
class RequestBodyIR:
    required: bool
    media_type: str
    schema: TypeIR
    description: str = ""


@dataclass(frozen=True)
class ResponseIR:
    status: str
    description: str
    media_type: str | None
    schema: TypeIR | None
    example: JSONValue = None


@dataclass(frozen=True)
class SecuritySchemeIR:
    name: str
    kind: str
    scheme: str | None = None
    bearer_format: str | None = None


@dataclass(frozen=True)
class JobIR:
    status_operation_id: str
    result_operation_id: str
    identifier_parameter: str
    terminal_states: tuple[str, ...]
    success_states: tuple[str, ...]
    failure_states: tuple[str, ...]
    cancel_operation_id: str | None = None


@dataclass(frozen=True)
class OperationIR:
    operation_id: str
    request_name: str
    method: str
    path: str
    summary: str
    description: str
    tags: tuple[str, ...]
    exposure: str
    stability: str
    result_kind: str
    parameters: tuple[ParameterIR, ...]
    request_body: RequestBodyIR | None
    responses: tuple[ResponseIR, ...]
    security: tuple[str, ...]
    anonymous: bool
    safe_to_retry: bool
    job: JobIR | None = None


@dataclass(frozen=True)
class ContractIR:
    title: str
    version: str
    openapi_version: str
    contract_digest: str
    source_commit: str | None
    declared_public_operation_count: int | None
    servers: tuple[str, ...]
    security_schemes: tuple[SecuritySchemeIR, ...]
    schemas: tuple[SchemaIR, ...]
    operations: tuple[OperationIR, ...]
    source: dict[str, Any] = field(compare=False, repr=False)
