"""Parse the supported OpenAPI 3.1 subset into an immutable IR."""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import replace
from typing import Any

from .exceptions import ContractError
from .model import (
    ContractIR,
    JobIR,
    OperationIR,
    ParameterIR,
    PropertyIR,
    RequestBodyIR,
    ResponseIR,
    SchemaIR,
    SecuritySchemeIR,
    TypeIR,
)
from .normalizer import normalized_bytes
from .resolver import LocalRefResolver


HTTP_METHODS = ("get", "put", "post", "delete", "options", "head", "patch", "trace")
JSON_MEDIA_TYPES = {"application/json", "application/problem+json"}
JSONL_MEDIA_TYPES = {
    "application/jsonl",
    "application/jsonlines",
    "application/x-jsonlines",
    "application/x-ndjson",
}
BINARY_MEDIA_TYPES = {
    "application/octet-stream",
    "application/pdf",
    "application/zip",
    "application/gzip",
}
SUPPORTED_EXPOSURES = {"public", "internal", "disabled", "modal"}
SUPPORTED_STABILITIES = {"stable", "beta", "experimental", "deprecated"}
RESULT_KIND_ALIASES = {
    "json": "JSON",
    "graph": "Graph",
    "network": "Graph",
    "verbose-network": "Graph",
    "compact-actome": "Graph",
    "tmap-tree": "TmapTree",
    "hypergraph": "Hypergraph",
    "score-matrix": "ScoreMatrix",
    "uint8-matrix": "ScoreMatrix",
    "base64-uint8-matrix": "ScoreMatrix",
    "jsonl": "JSONL",
    "binary": "Binary",
    "download": "Binary",
    "job": "Job",
    "async-job": "Job",
}
ANNOTATION_KEYS = {
    "title",
    "description",
    "default",
    "example",
    "examples",
    "deprecated",
    "readOnly",
    "writeOnly",
    "$schema",
    "externalDocs",
}
CONSTRAINT_KEYS = {
    "minimum",
    "maximum",
    "exclusiveMinimum",
    "exclusiveMaximum",
    "multipleOf",
    "minLength",
    "maxLength",
    "pattern",
    "minItems",
    "maxItems",
    "uniqueItems",
    "minProperties",
    "maxProperties",
}
ACRONYMS = {
    "api": "API",
    "dna": "DNA",
    "id": "ID",
    "json": "JSON",
    "jsonl": "JSONL",
    "pli": "PLI",
    "ppi": "PPI",
    "rna": "RNA",
    "url": "URL",
}


def _location(path: str, method: str | None = None) -> str:
    return f"paths.{path}.{method}" if method else f"paths.{path}"


def _mapping(value: Any, *, location: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise ContractError(f"{location}: expected an object")
    return value


def _string(value: Any, *, location: str) -> str:
    if not isinstance(value, str) or not value:
        raise ContractError(f"{location}: expected a non-empty string")
    return value


def request_name(operation_id: str) -> str:
    """Convert a stable operationId to a readable Wolfram request name."""

    separated = re.sub(r"([a-z0-9])([A-Z])", r"\1 \2", operation_id)
    tokens = [token for token in re.split(r"[^A-Za-z0-9]+", separated) if token]
    if not tokens:
        raise ContractError(f"operationId {operation_id!r} cannot form a Wolfram request name")
    rendered = [ACRONYMS.get(token.lower(), token[:1].upper() + token[1:]) for token in tokens]
    name = "".join(rendered)
    if name[0].isdigit():
        name = "Operation" + name
    return name


class ContractParser:
    def __init__(self, document: dict[str, Any], *, digest: str | None = None) -> None:
        self.document = document
        self.resolver = LocalRefResolver(document)
        self.digest = digest or hashlib.sha256(normalized_bytes(document)).hexdigest()

    def parse(self) -> ContractIR:
        openapi = _string(self.document.get("openapi"), location="openapi")
        if not openapi.startswith("3.1."):
            raise ContractError(f"openapi: expected OpenAPI 3.1.x, found {openapi!r}")

        info = _mapping(self.document.get("info"), location="info")
        title = _string(info.get("title"), location="info.title")
        version = _string(info.get("version"), location="info.version")
        servers = self._parse_servers()
        security_schemes = self._parse_security_schemes()
        schemas = self._parse_schemas()
        operations = self._parse_operations(security_schemes)
        self._validate_jobs(operations)
        source_commit = self.document.get("x-synthyra-source-commit")
        if source_commit is not None and (
            not isinstance(source_commit, str)
            or re.fullmatch(r"[0-9a-fA-F]{40}", source_commit) is None
        ):
            raise ContractError(
                "x-synthyra-source-commit: expected a full 40-character Git commit"
            )
        declared_count = self.document.get("x-synthyra-public-operation-count")
        if declared_count is not None and (
            not isinstance(declared_count, int) or isinstance(declared_count, bool)
        ):
            raise ContractError(
                "x-synthyra-public-operation-count: expected an integer"
            )
        if declared_count is not None and declared_count != len(operations):
            raise ContractError(
                "x-synthyra-public-operation-count: declared "
                f"{declared_count}, parsed {len(operations)}"
            )

        return ContractIR(
            title=title,
            version=version,
            openapi_version=openapi,
            contract_digest=self.digest,
            source_commit=source_commit.lower() if source_commit else None,
            declared_public_operation_count=declared_count,
            servers=servers,
            security_schemes=security_schemes,
            schemas=schemas,
            operations=operations,
            source=self.document,
        )

    def _parse_servers(self) -> tuple[str, ...]:
        result: list[str] = []
        for index, server in enumerate(self.document.get("servers", [])):
            item = _mapping(server, location=f"servers[{index}]")
            url = _string(item.get("url"), location=f"servers[{index}].url")
            if not url.lower().startswith("https://"):
                raise ContractError(f"servers[{index}].url: only HTTPS servers are supported")
            result.append(url.rstrip("/"))
        if not result:
            raise ContractError("servers: at least one explicit HTTPS server is required")
        return tuple(result)

    def _parse_security_schemes(self) -> tuple[SecuritySchemeIR, ...]:
        components = _mapping(self.document.get("components", {}), location="components")
        schemes = _mapping(
            components.get("securitySchemes", {}), location="components.securitySchemes"
        )
        parsed: list[SecuritySchemeIR] = []
        for name in sorted(schemes):
            raw = _mapping(schemes[name], location=f"components.securitySchemes.{name}")
            if raw.get("type") != "http" or str(raw.get("scheme", "")).lower() != "bearer":
                raise ContractError(
                    f"components.securitySchemes.{name}: only HTTP bearer authentication is supported"
                )
            parsed.append(
                SecuritySchemeIR(
                    name=name,
                    kind="HTTPBearer",
                    scheme="bearer",
                    bearer_format=raw.get("bearerFormat"),
                )
            )
        if not parsed:
            raise ContractError("components.securitySchemes: a bearer security scheme is required")
        return tuple(parsed)

    def _parse_schemas(self) -> tuple[SchemaIR, ...]:
        components = _mapping(self.document.get("components", {}), location="components")
        raw_schemas = _mapping(components.get("schemas", {}), location="components.schemas")
        return tuple(
            SchemaIR(
                name=name,
                schema=self._parse_type(
                    raw_schemas[name],
                    location=f"components.schemas.{name}",
                    allow_opaque=bool(
                        isinstance(raw_schemas[name], dict)
                        and raw_schemas[name].get("x-synthyra-opaque") is True
                    ),
                ),
            )
            for name in sorted(raw_schemas)
        )

    def _parse_type(
        self, raw: Any, *, location: str, allow_opaque: bool = False
    ) -> TypeIR:
        if isinstance(raw, bool):
            raise ContractError(f"{location}: boolean JSON Schemas are unsupported")
        schema = _mapping(raw, location=location)

        if "$ref" in schema:
            structural_siblings = set(schema) - {"$ref"} - ANNOTATION_KEYS
            if structural_siblings:
                raise ContractError(
                    f"{location}: $ref structural siblings are unsupported: "
                    f"{sorted(structural_siblings)}"
                )
            reference = _string(schema["$ref"], location=f"{location}.$ref")
            self.resolver.resolve(reference, location=location)
            name = self.resolver.schema_name(reference, location=location)
            return TypeIR(
                kind="reference",
                reference=name,
                description=str(schema.get("description", "")),
                default=schema.get("default"),
                example=schema.get("example"),
            )

        for union_key in ("oneOf", "anyOf"):
            if union_key in schema:
                siblings = set(schema) - {union_key} - ANNOTATION_KEYS
                if siblings:
                    raise ContractError(
                        f"{location}: {union_key} with structural siblings is unsupported: "
                        f"{sorted(siblings)}"
                    )
                alternatives = schema[union_key]
                if not isinstance(alternatives, list) or len(alternatives) != 2:
                    raise ContractError(
                        f"{location}: only a two-branch nullable {union_key} is supported"
                    )
                null_indexes = [
                    index
                    for index, item in enumerate(alternatives)
                    if isinstance(item, dict)
                    and (
                        item.get("type") == "null"
                        or item.get("enum") == [None]
                        or item.get("const", object()) is None
                    )
                ]
                if len(null_indexes) != 1:
                    raise ContractError(
                        f"{location}: {union_key} must contain exactly one null branch"
                    )
                value_index = 1 - null_indexes[0]
                value = self._parse_type(
                    alternatives[value_index], location=f"{location}.{union_key}[{value_index}]"
                )
                return replace(
                    value,
                    nullable=True,
                    description=str(schema.get("description", value.description)),
                    default=schema.get("default", value.default),
                    example=schema.get("example", value.example),
                )

        if "allOf" in schema:
            alternatives = schema["allOf"]
            siblings = set(schema) - {"allOf"} - ANNOTATION_KEYS
            if siblings or not isinstance(alternatives, list) or len(alternatives) != 1:
                raise ContractError(
                    f"{location}: only a single-entry allOf wrapper is supported"
                )
            return self._parse_type(alternatives[0], location=f"{location}.allOf[0]")

        nullable = bool(schema.get("nullable", False))
        raw_type = schema.get("type")
        if isinstance(raw_type, list):
            type_values = list(raw_type)
            if len(type_values) == 2 and "null" in type_values:
                nullable = True
                raw_type = next(item for item in type_values if item != "null")
            else:
                raise ContractError(
                    f"{location}.type: only a nullable [type, null] union is supported"
                )

        enum_values: tuple[Any, ...] = ()
        if "const" in schema:
            enum_values = (schema["const"],)
        elif "enum" in schema:
            if not isinstance(schema["enum"], list) or not schema["enum"]:
                raise ContractError(f"{location}.enum: expected a non-empty array")
            nullable = nullable or None in schema["enum"]
            enum_values = tuple(item for item in schema["enum"] if item is not None)

        if raw_type is None and enum_values:
            kinds = {self._json_kind(item) for item in enum_values}
            if len(kinds) != 1:
                raise ContractError(f"{location}.enum: mixed scalar types are unsupported")
            raw_type = kinds.pop()
        if raw_type is None and ("properties" in schema or "additionalProperties" in schema):
            raw_type = "object"

        known = (
            ANNOTATION_KEYS
            | CONSTRAINT_KEYS
            | {
                "type",
                "nullable",
                "format",
                "enum",
                "const",
                "properties",
                "required",
                "additionalProperties",
                "items",
                "x-synthyra-opaque",
            }
        )
        unsupported = sorted(key for key in schema if key not in known and not key.startswith("x-"))
        if unsupported:
            raise ContractError(f"{location}: unsupported JSON Schema keywords: {unsupported}")

        if raw_type == "null":
            return TypeIR(kind="null", nullable=True)
        if raw_type not in {"string", "integer", "number", "boolean", "array", "object"}:
            if not schema and allow_opaque:
                raw_type = "object"
            else:
                raise ContractError(
                    f"{location}: schema has no supported type; mark genuinely opaque "
                    "values with x-synthyra-opaque: true"
                )

        common = {
            "nullable": nullable,
            "format": schema.get("format"),
            "enum": enum_values,
            "description": str(schema.get("description", "")),
            "default": schema.get("default"),
            "example": schema.get("example"),
            "constraints": tuple(
                (key, schema[key]) for key in sorted(CONSTRAINT_KEYS) if key in schema
            ),
        }
        if raw_type == "array":
            if "items" not in schema:
                raise ContractError(f"{location}.items: arrays require a typed items schema")
            return TypeIR(
                kind="array",
                items=self._parse_type(schema["items"], location=f"{location}.items"),
                **common,
            )
        if raw_type == "object":
            properties_raw = _mapping(schema.get("properties", {}), location=f"{location}.properties")
            required_raw = schema.get("required", [])
            if not isinstance(required_raw, list) or not all(
                isinstance(item, str) for item in required_raw
            ):
                raise ContractError(f"{location}.required: expected an array of property names")
            unknown_required = sorted(set(required_raw) - set(properties_raw))
            if unknown_required:
                raise ContractError(
                    f"{location}.required: unknown properties {unknown_required}"
                )
            properties = tuple(
                PropertyIR(
                    name=name,
                    schema=self._parse_type(
                        properties_raw[name], location=f"{location}.properties.{name}"
                    ),
                    required=name in required_raw,
                    description=(
                        str(properties_raw[name].get("description", ""))
                        if isinstance(properties_raw[name], dict)
                        else ""
                    ),
                )
                for name in sorted(properties_raw)
            )
            additional_raw = schema.get("additionalProperties")
            additional: bool | TypeIR | None
            if isinstance(additional_raw, dict):
                additional = self._parse_type(
                    additional_raw,
                    location=f"{location}.additionalProperties",
                    allow_opaque=allow_opaque,
                )
            elif isinstance(additional_raw, bool) or additional_raw is None:
                additional = additional_raw
            else:
                raise ContractError(
                    f"{location}.additionalProperties: expected boolean or schema"
                )
            opaque = bool(schema.get("x-synthyra-opaque") is True or allow_opaque)
            if not properties and additional in (None, True) and not opaque:
                raise ContractError(
                    f"{location}: untyped object is forbidden; define properties or mark it "
                    "x-synthyra-opaque: true"
                )
            return TypeIR(
                kind="object",
                properties=properties,
                additional_properties=additional,
                opaque=opaque,
                **common,
            )
        return TypeIR(kind=str(raw_type), **common)

    @staticmethod
    def _json_kind(value: Any) -> str:
        if isinstance(value, bool):
            return "boolean"
        if isinstance(value, int):
            return "integer"
        if isinstance(value, float):
            return "number"
        if isinstance(value, str):
            return "string"
        raise ContractError(f"enum value {value!r} is not a supported scalar")

    def _parse_operations(
        self, security_schemes: tuple[SecuritySchemeIR, ...]
    ) -> tuple[OperationIR, ...]:
        raw_paths = _mapping(self.document.get("paths"), location="paths")
        global_security = self.document.get("security", [])
        scheme_names = {scheme.name for scheme in security_schemes}
        parsed: list[OperationIR] = []
        seen_ids: set[str] = set()
        seen_names: set[str] = set()

        for path in sorted(raw_paths):
            path_item = _mapping(raw_paths[path], location=_location(path))
            path_parameters = path_item.get("parameters", [])
            for method in HTTP_METHODS:
                if method not in path_item:
                    continue
                operation_location = _location(path, method)
                operation = _mapping(path_item[method], location=operation_location)
                exposure = operation.get("x-synthyra-exposure")
                if exposure not in SUPPORTED_EXPOSURES:
                    raise ContractError(
                        f"{operation_location}.x-synthyra-exposure: expected one of "
                        f"{sorted(SUPPORTED_EXPOSURES)}"
                    )
                if exposure != "public":
                    continue

                operation_id = _string(
                    operation.get("operationId"), location=f"{operation_location}.operationId"
                )
                if operation_id in seen_ids:
                    raise ContractError(f"duplicate public operationId {operation_id!r}")
                seen_ids.add(operation_id)
                wolfram_name = operation.get("x-synthyra-wolfram-name") or request_name(operation_id)
                if not isinstance(wolfram_name, str) or not re.fullmatch(
                    r"[A-Za-z$][A-Za-z0-9$]*", wolfram_name
                ):
                    raise ContractError(
                        f"{operation_location}.x-synthyra-wolfram-name: invalid request name"
                    )
                if wolfram_name in seen_names:
                    raise ContractError(f"duplicate Wolfram request name {wolfram_name!r}")
                seen_names.add(wolfram_name)

                stability = operation.get("x-synthyra-stability")
                if stability not in SUPPORTED_STABILITIES:
                    raise ContractError(
                        f"{operation_location}.x-synthyra-stability: expected one of "
                        f"{sorted(SUPPORTED_STABILITIES)}"
                    )
                raw_result_kind = operation.get("x-synthyra-result-kind")
                if raw_result_kind not in RESULT_KIND_ALIASES:
                    raise ContractError(
                        f"{operation_location}.x-synthyra-result-kind: unsupported value "
                        f"{raw_result_kind!r}"
                    )
                result_kind = RESULT_KIND_ALIASES[raw_result_kind]
                parameters = self._parse_parameters(
                    path,
                    method,
                    path_parameters,
                    operation.get("parameters", []),
                )
                request_body = self._parse_request_body(
                    operation.get("requestBody"), location=operation_location
                )
                responses = self._parse_responses(operation, location=operation_location)
                security, anonymous = self._parse_security(
                    operation.get("security", global_security),
                    scheme_names,
                    location=f"{operation_location}.security",
                )
                job = self._parse_job(operation.get("x-synthyra-job"), location=operation_location)
                if result_kind == "Job" and job is None:
                    raise ContractError(
                        f"{operation_location}: job result kind requires x-synthyra-job metadata"
                    )
                if result_kind != "Job" and job is not None:
                    raise ContractError(
                        f"{operation_location}: x-synthyra-job is only valid for job operations"
                    )
                parsed.append(
                    OperationIR(
                        operation_id=operation_id,
                        request_name=wolfram_name,
                        method=method.upper(),
                        path=path,
                        summary=str(operation.get("summary", "")),
                        description=str(operation.get("description", "")),
                        tags=tuple(str(tag) for tag in operation.get("tags", [])),
                        exposure=exposure,
                        stability=stability,
                        result_kind=result_kind,
                        parameters=parameters,
                        request_body=request_body,
                        responses=responses,
                        security=security,
                        anonymous=anonymous,
                        safe_to_retry=method in {"get", "head", "options"},
                        job=job,
                    )
                )
        if not parsed:
            raise ContractError("paths: no public operations were found")
        return tuple(sorted(parsed, key=lambda item: item.operation_id))

    def _parse_parameters(
        self,
        path: str,
        method: str,
        path_parameters: Any,
        operation_parameters: Any,
    ) -> tuple[ParameterIR, ...]:
        if not isinstance(path_parameters, list) or not isinstance(operation_parameters, list):
            raise ContractError(f"{_location(path, method)}.parameters: expected an array")
        merged: dict[tuple[str, str], dict[str, Any]] = {}
        for origin, values in (("path", path_parameters), ("operation", operation_parameters)):
            for index, raw in enumerate(values):
                location = f"{_location(path, method)}.{origin}Parameters[{index}]"
                item = self._resolve_component(raw, location=location)
                name = _string(item.get("name"), location=f"{location}.name")
                parameter_location = item.get("in")
                if parameter_location not in {"path", "query", "header"}:
                    raise ContractError(
                        f"{location}.in: only path, query, and header parameters are supported"
                    )
                merged[(name, parameter_location)] = item

        parsed: list[ParameterIR] = []
        for (name, parameter_location), item in sorted(merged.items()):
            location = f"{_location(path, method)}.parameters.{parameter_location}.{name}"
            if "content" in item:
                raise ContractError(f"{location}: content-based parameters are unsupported")
            required = bool(item.get("required", False))
            if parameter_location == "path" and not required:
                raise ContractError(f"{location}: path parameters must be required")
            default_style = "form" if parameter_location == "query" else "simple"
            style = item.get("style", default_style)
            if style != default_style:
                raise ContractError(
                    f"{location}.style: unsupported {parameter_location} style {style!r}"
                )
            default_explode = parameter_location == "query"
            explode = bool(item.get("explode", default_explode))
            if item.get("allowReserved") is True:
                raise ContractError(f"{location}.allowReserved: true is unsupported")
            if "schema" not in item:
                raise ContractError(f"{location}.schema: a typed schema is required")
            schema = self._parse_type(item["schema"], location=f"{location}.schema")
            if schema.kind in {"object", "null"}:
                raise ContractError(
                    f"{location}.schema: object and null parameters are unsupported"
                )
            parsed.append(
                ParameterIR(
                    name=name,
                    location=parameter_location,
                    required=required,
                    style=style,
                    explode=explode,
                    schema=schema,
                    description=str(item.get("description", "")),
                )
            )
        return tuple(parsed)

    def _parse_request_body(self, raw: Any, *, location: str) -> RequestBodyIR | None:
        if raw is None:
            return None
        body = self._resolve_component(raw, location=f"{location}.requestBody")
        content = _mapping(body.get("content"), location=f"{location}.requestBody.content")
        media_types = sorted(content)
        supported = [media for media in media_types if self._is_json(media)]
        if len(supported) != 1 or len(media_types) != 1:
            raise ContractError(
                f"{location}.requestBody.content: exactly one JSON media type is supported; "
                f"found {media_types}"
            )
        media_type = supported[0]
        media = _mapping(content[media_type], location=f"{location}.requestBody.{media_type}")
        if "schema" not in media:
            raise ContractError(f"{location}.requestBody.{media_type}.schema: required")
        return RequestBodyIR(
            required=bool(body.get("required", False)),
            media_type=media_type,
            schema=self._parse_type(
                media["schema"], location=f"{location}.requestBody.{media_type}.schema"
            ),
            description=str(body.get("description", "")),
        )

    def _parse_responses(
        self, operation: dict[str, Any], *, location: str
    ) -> tuple[ResponseIR, ...]:
        raw_responses = _mapping(operation.get("responses"), location=f"{location}.responses")
        parsed: list[ResponseIR] = []
        opaque_success = operation.get("x-synthyra-opaque-response") is True
        for status in sorted(raw_responses, key=self._status_sort_key):
            response = self._resolve_component(
                raw_responses[status], location=f"{location}.responses.{status}"
            )
            description = str(response.get("description", ""))
            content = response.get("content")
            if not content:
                if str(status).startswith("2") and str(status) != "204":
                    raise ContractError(
                        f"{location}.responses.{status}: successful response needs typed content"
                    )
                parsed.append(
                    ResponseIR(
                        status=str(status),
                        description=description,
                        media_type=None,
                        schema=None,
                    )
                )
                continue
            content_map = _mapping(content, location=f"{location}.responses.{status}.content")
            for media_type in sorted(content_map):
                if not (
                    self._is_json(media_type)
                    or media_type in JSONL_MEDIA_TYPES
                    or media_type in BINARY_MEDIA_TYPES
                ):
                    raise ContractError(
                        f"{location}.responses.{status}: unsupported media type {media_type!r}"
                    )
                media = _mapping(
                    content_map[media_type],
                    location=f"{location}.responses.{status}.{media_type}",
                )
                if "schema" not in media:
                    raise ContractError(
                        f"{location}.responses.{status}.{media_type}.schema: required"
                    )
                parsed.append(
                    ResponseIR(
                        status=str(status),
                        description=description,
                        media_type=media_type,
                        schema=self._parse_type(
                            media["schema"],
                            location=f"{location}.responses.{status}.{media_type}.schema",
                            allow_opaque=bool(
                                opaque_success and str(status).startswith("2")
                            ),
                        ),
                        example=media.get("example", media.get("examples")),
                    )
                )
        if not any(item.status.startswith("2") for item in parsed):
            raise ContractError(f"{location}.responses: a successful response is required")
        return tuple(parsed)

    def _parse_security(
        self, raw: Any, scheme_names: set[str], *, location: str
    ) -> tuple[tuple[str, ...], bool]:
        if not isinstance(raw, list):
            raise ContractError(f"{location}: expected an array")
        if raw == []:
            return (), True
        parsed: set[str] = set()
        anonymous = False
        for index, requirement in enumerate(raw):
            item = _mapping(requirement, location=f"{location}[{index}]")
            if item == {}:
                anonymous = True
                continue
            if len(item) != 1:
                raise ContractError(
                    f"{location}[{index}]: exactly one bearer scheme per alternative is supported"
                )
            name, scopes = next(iter(item.items()))
            if name not in scheme_names:
                raise ContractError(f"{location}[{index}]: unknown security scheme {name!r}")
            if scopes not in ([], None):
                raise ContractError(f"{location}[{index}]: bearer scopes are unsupported")
            parsed.add(name)
        if len(parsed) != 1:
            raise ContractError(
                f"{location}: expected one bearer scheme, optionally alongside anonymous access"
            )
        return tuple(sorted(parsed)), anonymous

    def _parse_job(self, raw: Any, *, location: str) -> JobIR | None:
        if raw is None:
            return None
        job = _mapping(raw, location=f"{location}.x-synthyra-job")
        required = {"statusOperationId", "resultOperationId", "terminalStates"}
        missing = sorted(required - set(job))
        if missing:
            raise ContractError(f"{location}.x-synthyra-job: missing fields {missing}")
        terminal_states = self._string_array(
            job["terminalStates"], location=f"{location}.x-synthyra-job.terminalStates"
        )
        success_states = self._string_array(
            job.get("successStates", ["succeeded", "completed"]),
            location=f"{location}.x-synthyra-job.successStates",
        )
        failure_states = self._string_array(
            job.get("failureStates", ["failed", "cancelled"]),
            location=f"{location}.x-synthyra-job.failureStates",
        )
        if not set(success_states + failure_states).issubset(terminal_states):
            raise ContractError(
                f"{location}.x-synthyra-job: success/failure states must be terminal states"
            )
        return JobIR(
            status_operation_id=_string(
                job["statusOperationId"],
                location=f"{location}.x-synthyra-job.statusOperationId",
            ),
            result_operation_id=_string(
                job["resultOperationId"],
                location=f"{location}.x-synthyra-job.resultOperationId",
            ),
            identifier_parameter=_string(
                job.get("identifierParameter", "jobId"),
                location=f"{location}.x-synthyra-job.identifierParameter",
            ),
            cancel_operation_id=(
                _string(
                    job["cancelOperationId"],
                    location=f"{location}.x-synthyra-job.cancelOperationId",
                )
                if job.get("cancelOperationId") is not None
                else None
            ),
            terminal_states=terminal_states,
            success_states=success_states,
            failure_states=failure_states,
        )

    @staticmethod
    def _string_array(raw: Any, *, location: str) -> tuple[str, ...]:
        if not isinstance(raw, list) or not raw or not all(
            isinstance(item, str) and item for item in raw
        ):
            raise ContractError(f"{location}: expected a non-empty string array")
        return tuple(raw)

    @staticmethod
    def _status_sort_key(status: Any) -> tuple[int, str]:
        value = str(status)
        return (0, f"{int(value):03d}") if value.isdigit() else (1, value)

    @staticmethod
    def _is_json(media_type: str) -> bool:
        bare = media_type.split(";", 1)[0].strip().lower()
        return bare in JSON_MEDIA_TYPES or bare.endswith("+json")

    def _resolve_component(self, raw: Any, *, location: str) -> dict[str, Any]:
        item = _mapping(raw, location=location)
        if "$ref" not in item:
            return item
        if len(item) != 1:
            raise ContractError(f"{location}: component $ref siblings are unsupported")
        return _mapping(self.resolver.resolve(item["$ref"], location=location), location=location)

    @staticmethod
    def _validate_jobs(operations: tuple[OperationIR, ...]) -> None:
        operation_by_id = {operation.operation_id: operation for operation in operations}
        operation_ids = set(operation_by_id)
        for operation in operations:
            if operation.job is None:
                continue
            references = {
                "statusOperationId": operation.job.status_operation_id,
                "resultOperationId": operation.job.result_operation_id,
            }
            if operation.job.cancel_operation_id:
                references["cancelOperationId"] = operation.job.cancel_operation_id
            for field, target in references.items():
                if target not in operation_ids:
                    raise ContractError(
                        f"operation {operation.operation_id!r} job {field} references "
                        f"non-public operation {target!r}"
                    )
                if field in {"statusOperationId", "resultOperationId", "cancelOperationId"}:
                    parameters = {
                        parameter.name for parameter in operation_by_id[target].parameters
                    }
                    if operation.job.identifier_parameter not in parameters:
                        raise ContractError(
                            f"operation {operation.operation_id!r} job {field} target {target!r} "
                            f"has no {operation.job.identifier_parameter!r} parameter"
                        )


def parse_contract(document: dict[str, Any], *, digest: str | None = None) -> ContractIR:
    """Parse and validate a normalized OpenAPI document."""

    # Round-trip catches non-JSON Python values before code generation.
    try:
        json.dumps(document, allow_nan=False)
    except (TypeError, ValueError) as exc:
        raise ContractError(f"contract contains a non-JSON value: {exc}") from exc
    return ContractParser(document, digest=digest).parse()
