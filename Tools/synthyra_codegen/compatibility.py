"""Semver-oriented compatibility classification for reviewed contract updates."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal

from .model import ContractIR, OperationIR, ParameterIR, TypeIR


Level = Literal["patch", "minor", "major"]


@dataclass(frozen=True)
class CompatibilityIssue:
    level: Level
    code: str
    operation_id: str
    detail: str


@dataclass(frozen=True)
class CompatibilityReport:
    level: Level
    issues: tuple[CompatibilityIssue, ...]

    @property
    def breaking(self) -> tuple[CompatibilityIssue, ...]:
        return tuple(issue for issue in self.issues if issue.level == "major")

    def as_dict(self) -> dict[str, object]:
        return {
            "classification": self.level,
            "breaking": bool(self.breaking),
            "issues": [
                {
                    "level": issue.level,
                    "code": issue.code,
                    "operationId": issue.operation_id,
                    "detail": issue.detail,
                }
                for issue in self.issues
            ],
        }


def _issue_level(operation: OperationIR, *, breaking: bool) -> Level:
    if breaking and operation.stability == "stable":
        return "major"
    return "minor"


def _resolve(schema: TypeIR, schemas: dict[str, TypeIR]) -> TypeIR:
    return schemas[schema.reference] if schema.kind == "reference" else schema


_LOWER_BOUND_CONSTRAINTS = {
    "minimum",
    "exclusiveMinimum",
    "minLength",
    "minItems",
    "minProperties",
}
_UPPER_BOUND_CONSTRAINTS = {
    "maximum",
    "exclusiveMaximum",
    "maxLength",
    "maxItems",
    "maxProperties",
}


def _constraint_relation(
    name: str,
    *,
    old_present: bool,
    old_value: object,
    new_present: bool,
    new_value: object,
) -> Literal["narrowed", "widened", "changed"] | None:
    """Describe how a JSON Schema constraint changes accepted instances."""

    if old_present and new_present and old_value == new_value:
        return None
    if name == "uniqueItems":
        old_unique = bool(old_value) if old_present else False
        new_unique = bool(new_value) if new_present else False
        if old_unique == new_unique:
            return None
        return "narrowed" if new_unique else "widened"
    if not old_present:
        return "narrowed"
    if not new_present:
        return "widened"
    if name in _LOWER_BOUND_CONSTRAINTS | _UPPER_BOUND_CONSTRAINTS:
        numeric = (
            isinstance(old_value, (int, float))
            and not isinstance(old_value, bool)
            and isinstance(new_value, (int, float))
            and not isinstance(new_value, bool)
        )
        if not numeric:
            return "changed"
        if name in _LOWER_BOUND_CONSTRAINTS:
            return "narrowed" if new_value > old_value else "widened"
        return "narrowed" if new_value < old_value else "widened"
    if name in {"pattern", "multipleOf"}:
        return "changed"
    return "changed"


def _compare_constraints(
    old: TypeIR,
    new: TypeIR,
    *,
    operation: OperationIR,
    location: str,
    direction: Literal["request", "response"],
) -> list[CompatibilityIssue]:
    old_constraints = dict(old.constraints)
    new_constraints = dict(new.constraints)
    issues: list[CompatibilityIssue] = []
    for name in sorted(old_constraints.keys() | new_constraints.keys()):
        relation = _constraint_relation(
            name,
            old_present=name in old_constraints,
            old_value=old_constraints.get(name),
            new_present=name in new_constraints,
            new_value=new_constraints.get(name),
        )
        if relation is None:
            continue
        # Input narrowing and any response constraint change require review. A
        # request widening is additive and therefore a minor change.
        breaking = direction == "response" or relation in {"narrowed", "changed"}
        issues.append(
            CompatibilityIssue(
                _issue_level(operation, breaking=breaking),
                f"constraint-{relation}",
                operation.operation_id,
                f"{location} constraint {name} changed from "
                f"{old_constraints.get(name, '<absent>')!r} to "
                f"{new_constraints.get(name, '<absent>')!r}",
            )
        )
    return issues


def _compare_types(
    old: TypeIR,
    new: TypeIR,
    *,
    old_schemas: dict[str, TypeIR],
    new_schemas: dict[str, TypeIR],
    operation: OperationIR,
    location: str,
    direction: Literal["request", "response"],
    seen: frozenset[tuple[str | None, str | None]] = frozenset(),
) -> list[CompatibilityIssue]:
    ref_pair = (old.reference, new.reference)
    if old.kind == "reference" or new.kind == "reference":
        if ref_pair in seen:
            return []
        if old.kind != "reference" or new.kind != "reference":
            return [
                CompatibilityIssue(
                    _issue_level(operation, breaking=True),
                    "schema-kind-changed",
                    operation.operation_id,
                    f"{location} changed from {old.kind} to {new.kind}",
                )
            ]
        return _compare_types(
            _resolve(old, old_schemas),
            _resolve(new, new_schemas),
            old_schemas=old_schemas,
            new_schemas=new_schemas,
            operation=operation,
            location=location,
            direction=direction,
            seen=seen | {ref_pair},
        )

    issues: list[CompatibilityIssue] = []
    if old.kind != new.kind or old.format != new.format:
        issues.append(
            CompatibilityIssue(
                _issue_level(operation, breaking=True),
                "schema-type-changed",
                operation.operation_id,
                f"{location} changed from {old.kind}/{old.format} to {new.kind}/{new.format}",
            )
        )
        return issues
    if old.nullable != new.nullable:
        request_relaxation = direction == "request" and not old.nullable and new.nullable
        issues.append(
            CompatibilityIssue(
                "minor" if request_relaxation else _issue_level(operation, breaking=True),
                "schema-nullability-changed",
                operation.operation_id,
                f"{location} nullable changed from {old.nullable} to {new.nullable}",
            )
        )

    issues.extend(
        _compare_constraints(
            old,
            new,
            operation=operation,
            location=location,
            direction=direction,
        )
    )

    old_enum, new_enum = set(old.enum), set(new.enum)
    if old_enum or new_enum:
        removed = old_enum - new_enum
        added = new_enum - old_enum
        if removed:
            issues.append(
                CompatibilityIssue(
                    _issue_level(operation, breaking=True),
                    "enum-values-removed",
                    operation.operation_id,
                    f"{location} removed enum values {sorted(map(repr, removed))}",
                )
            )
        if added:
            response_widening = direction == "response" and bool(old_enum)
            issues.append(
                CompatibilityIssue(
                    _issue_level(operation, breaking=response_widening),
                    "enum-values-added",
                    operation.operation_id,
                    f"{location} added enum values {sorted(map(repr, added))}",
                )
            )

    if old.kind == "array" and old.items and new.items:
        issues.extend(
            _compare_types(
                old.items,
                new.items,
                old_schemas=old_schemas,
                new_schemas=new_schemas,
                operation=operation,
                location=f"{location}[]",
                direction=direction,
                seen=seen,
            )
        )
    if old.kind == "object":
        old_props = {prop.name: prop for prop in old.properties}
        new_props = {prop.name: prop for prop in new.properties}
        for name in sorted(old_props.keys() - new_props.keys()):
            issues.append(
                CompatibilityIssue(
                    _issue_level(operation, breaking=True),
                    "property-removed",
                    operation.operation_id,
                    f"{location}.{name} was removed",
                )
            )
        for name in sorted(new_props.keys() - old_props.keys()):
            required_input = direction == "request" and new_props[name].required
            issues.append(
                CompatibilityIssue(
                    _issue_level(operation, breaking=required_input),
                    "required-property-added" if required_input else "property-added",
                    operation.operation_id,
                    f"{location}.{name} was added"
                    + (" as required input" if required_input else ""),
                )
            )
        for name in sorted(old_props.keys() & new_props.keys()):
            old_prop, new_prop = old_props[name], new_props[name]
            became_required_input = (
                direction == "request" and not old_prop.required and new_prop.required
            )
            lost_response_guarantee = (
                direction == "response" and old_prop.required and not new_prop.required
            )
            if became_required_input or lost_response_guarantee:
                issues.append(
                    CompatibilityIssue(
                        _issue_level(operation, breaking=True),
                        "property-requiredness-changed",
                        operation.operation_id,
                        f"{location}.{name} required changed from "
                        f"{old_prop.required} to {new_prop.required}",
                    )
                )
            elif old_prop.required != new_prop.required:
                issues.append(
                    CompatibilityIssue(
                        "minor",
                        "property-requiredness-relaxed",
                        operation.operation_id,
                        f"{location}.{name} required changed from "
                        f"{old_prop.required} to {new_prop.required}",
                    )
                )
            issues.extend(
                _compare_types(
                    old_prop.schema,
                    new_prop.schema,
                    old_schemas=old_schemas,
                    new_schemas=new_schemas,
                    operation=operation,
                    location=f"{location}.{name}",
                    direction=direction,
                    seen=seen,
                )
            )
        old_additional = True if old.additional_properties is None else old.additional_properties
        new_additional = True if new.additional_properties is None else new.additional_properties
        if isinstance(old_additional, TypeIR) and isinstance(new_additional, TypeIR):
            issues.extend(
                _compare_types(
                    old_additional,
                    new_additional,
                    old_schemas=old_schemas,
                    new_schemas=new_schemas,
                    operation=operation,
                    location=f"{location} additional properties",
                    direction=direction,
                    seen=seen,
                )
            )
        elif old_additional != new_additional:
            request_widening = direction == "request" and (
                (old_additional is False and new_additional is not False)
                or (isinstance(old_additional, TypeIR) and new_additional is True)
            )
            issues.append(
                CompatibilityIssue(
                    "minor" if request_widening else _issue_level(operation, breaking=True),
                    "additional-properties-changed",
                    operation.operation_id,
                    f"{location} additional-properties policy changed",
                )
            )
    return issues


def _parameters(operation: OperationIR) -> dict[tuple[str, str], ParameterIR]:
    return {(item.location, item.name): item for item in operation.parameters}


def _compare_operation(
    old: OperationIR,
    new: OperationIR,
    *,
    old_schemas: dict[str, TypeIR],
    new_schemas: dict[str, TypeIR],
) -> list[CompatibilityIssue]:
    issues: list[CompatibilityIssue] = []

    def breaking(code: str, detail: str) -> None:
        issues.append(
            CompatibilityIssue(
                _issue_level(old, breaking=True), code, old.operation_id, detail
            )
        )

    if (old.method, old.path) != (new.method, new.path):
        breaking(
            "route-changed",
            f"route changed from {old.method} {old.path} to {new.method} {new.path}",
        )
    if old.request_name != new.request_name:
        breaking(
            "wolfram-request-name-changed",
            f"Wolfram request name changed from {old.request_name} to {new.request_name}",
        )
    if (old.anonymous, old.security) != (new.anonymous, new.security):
        breaking("authentication-changed", "authentication requirements changed")
    if old.result_kind != new.result_kind:
        breaking(
            "result-kind-changed",
            f"result kind changed from {old.result_kind} to {new.result_kind}",
        )
    if old.job != new.job:
        breaking("async-contract-changed", "asynchronous polling metadata changed")
    if old.stability == "stable" and new.stability != "stable":
        breaking("stability-demoted", f"stable operation became {new.stability}")

    old_parameters, new_parameters = _parameters(old), _parameters(new)
    for key in sorted(old_parameters.keys() - new_parameters.keys()):
        breaking("parameter-removed", f"parameter {key[0]}:{key[1]} was removed")
    for key in sorted(new_parameters.keys() - old_parameters.keys()):
        parameter = new_parameters[key]
        issues.append(
            CompatibilityIssue(
                _issue_level(old, breaking=parameter.required),
                "required-parameter-added" if parameter.required else "parameter-added",
                old.operation_id,
                f"parameter {key[0]}:{key[1]} was added"
                + (" as required" if parameter.required else ""),
            )
        )
    for key in sorted(old_parameters.keys() & new_parameters.keys()):
        old_parameter, new_parameter = old_parameters[key], new_parameters[key]
        if not old_parameter.required and new_parameter.required:
            breaking("parameter-became-required", f"parameter {key[0]}:{key[1]} became required")
        if (old_parameter.style, old_parameter.explode) != (
            new_parameter.style,
            new_parameter.explode,
        ):
            breaking("parameter-serialization-changed", f"parameter {key[0]}:{key[1]} serialization changed")
        issues.extend(
            _compare_types(
                old_parameter.schema,
                new_parameter.schema,
                old_schemas=old_schemas,
                new_schemas=new_schemas,
                operation=old,
                location=f"parameter {key[0]}:{key[1]}",
                direction="request",
            )
        )

    if old.request_body is None and new.request_body is not None:
        issues.append(
            CompatibilityIssue(
                _issue_level(old, breaking=new.request_body.required),
                "required-body-added" if new.request_body.required else "body-added",
                old.operation_id,
                "request body was added",
            )
        )
    elif old.request_body is not None and new.request_body is None:
        breaking("request-body-removed", "request body was removed")
    elif old.request_body and new.request_body:
        if not old.request_body.required and new.request_body.required:
            breaking("body-became-required", "request body became required")
        if old.request_body.media_type != new.request_body.media_type:
            breaking("request-content-type-changed", "request content type changed")
        issues.extend(
            _compare_types(
                old.request_body.schema,
                new.request_body.schema,
                old_schemas=old_schemas,
                new_schemas=new_schemas,
                operation=old,
                location="request body",
                direction="request",
            )
        )

    old_success = {
        (response.status, response.media_type): response
        for response in old.responses
        if response.status.startswith("2")
    }
    new_success = {
        (response.status, response.media_type): response
        for response in new.responses
        if response.status.startswith("2")
    }
    for key in sorted(old_success.keys() - new_success.keys()):
        breaking("success-response-removed", f"successful response {key} was removed")
    for key in sorted(new_success.keys() - old_success.keys()):
        issues.append(
            CompatibilityIssue(
                "minor",
                "success-response-added",
                old.operation_id,
                f"successful response {key} was added",
            )
        )
    for key in sorted(old_success.keys() & new_success.keys()):
        old_response, new_response = old_success[key], new_success[key]
        if old_response.schema is None or new_response.schema is None:
            if old_response.schema != new_response.schema:
                breaking("response-schema-changed", f"successful response {key} schema changed")
            continue
        issues.extend(
            _compare_types(
                old_response.schema,
                new_response.schema,
                old_schemas=old_schemas,
                new_schemas=new_schemas,
                operation=old,
                location=f"response {key}",
                direction="response",
            )
        )
    return issues


def classify_compatibility(old: ContractIR, new: ContractIR) -> CompatibilityReport:
    """Classify a public contract change as patch, minor, or major."""

    old_operations = {item.operation_id: item for item in old.operations}
    new_operations = {item.operation_id: item for item in new.operations}
    issues: list[CompatibilityIssue] = []
    for operation_id in sorted(old_operations.keys() - new_operations.keys()):
        old_operation = old_operations[operation_id]
        issues.append(
            CompatibilityIssue(
                _issue_level(old_operation, breaking=True),
                "operation-removed",
                operation_id,
                f"{old_operation.stability} operation was removed",
            )
        )
    for operation_id in sorted(new_operations.keys() - old_operations.keys()):
        issues.append(
            CompatibilityIssue(
                "minor", "operation-added", operation_id, "public operation was added"
            )
        )

    old_schemas = {item.name: item.schema for item in old.schemas}
    new_schemas = {item.name: item.schema for item in new.schemas}
    for operation_id in sorted(old_operations.keys() & new_operations.keys()):
        issues.extend(
            _compare_operation(
                old_operations[operation_id],
                new_operations[operation_id],
                old_schemas=old_schemas,
                new_schemas=new_schemas,
            )
        )
    issues = sorted(issues, key=lambda item: (item.operation_id, item.code, item.detail))
    level: Level = (
        "major"
        if any(issue.level == "major" for issue in issues)
        else "minor" if issues else "patch"
    )
    return CompatibilityReport(level=level, issues=tuple(issues))
