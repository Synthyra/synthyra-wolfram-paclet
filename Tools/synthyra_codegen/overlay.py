"""Build the paclet contract from the live Gateway OpenAPI document and the paclet overlay.

The Gateway serves FastAPI's own OpenAPI document at `/openapi.json`. It is authoritative for
what the server accepts: paths, methods, parameters, and request schemas. It says nothing the
paclet needs about results: response models, job polling, stable Wolfram names, or which routes
are in service. `Contract/overlay.json` holds exactly that, keyed by `"METHOD /path"`, and
every live route must be either described there or excluded there with a reason.

Merge rules, per operation the overlay describes:
- from the live document: `summary`, `description`, `parameters`, and the request schema;
- from the overlay: `operationId`, `tags`, `security`, `responses`, every `x-synthyra-*` key,
  examples for parameters and the request body, and a `requestBody` only where the live
  route declares none (a handler that reads the raw request);
- components: live request schemas reachable from the merged operations, with untyped
  objects marked opaque, plus the overlay's own schemas. A name in both is an error.
"""

from __future__ import annotations

import copy
import hashlib
import json
import re

from pathlib import Path
from typing import Any

from .exceptions import ContractError
from .normalizer import normalized_bytes


HTTP_METHODS = ("get", "post", "put", "patch", "delete")
OPERATION_KEY = re.compile(r"(GET|POST|PUT|PATCH|DELETE) (/\S*)")
OVERLAY_OPERATION_KEYS = {
    "operationId",
    "tags",
    "security",
    "responses",
    "parameterExamples",
    "requestExample",
    "requestBody",
}
REFERENCE = re.compile(r"#/components/schemas/([A-Za-z0-9_.-]+)")


def operation_key(method: str, path: str) -> str:
    return f"{method.upper()} {path}"


def live_operations(live: dict[str, Any]) -> dict[str, dict[str, Any]]:
    """Every operation of an OpenAPI document by `"METHOD /path"`."""
    operations = {}
    for path, item in live.get("paths", {}).items():
        for method in HTTP_METHODS:
            if method in item:
                operations[operation_key(method, path)] = item[method]
    return operations


def load_overlay(path: Path) -> dict[str, Any]:
    overlay = json.loads(path.read_text(encoding="utf-8"))
    for section in ("document", "operations", "excluded", "schemas"):
        if not isinstance(overlay.get(section), dict):
            raise ContractError(f"{path.name}: `{section}` must be an object")
    for key in (*overlay["operations"], *overlay["excluded"]):
        if OPERATION_KEY.fullmatch(key) is None:
            raise ContractError(f"{path.name}: {key!r} is not a `METHOD /path` key")
    both = sorted(set(overlay["operations"]) & set(overlay["excluded"]))
    if both:
        raise ContractError(f"{path.name}: described and excluded at once: {both}")
    for key, reason in overlay["excluded"].items():
        if not isinstance(reason, str) or not reason.strip():
            raise ContractError(f"{path.name}: excluded {key} needs a reason")
    for key, entry in overlay["operations"].items():
        unknown = {name for name in entry if name not in OVERLAY_OPERATION_KEYS and not name.startswith("x-synthyra-")}
        if unknown:
            raise ContractError(f"{path.name}: {key} has unknown keys {sorted(unknown)}")
    return overlay


def mark_opaque(schema: Any) -> Any:
    """Mark every untyped object opaque, as the generator requires for free-form values.

    FastAPI renders `dict[str, Any]` as `{"type": "object", "additionalProperties": true}`.
    The generator refuses an object without declared properties unless it is marked opaque,
    and a typed map (`additionalProperties` holding a schema) only needs `properties: {}`.
    """
    if isinstance(schema, list):
        return [mark_opaque(item) for item in schema]
    if not isinstance(schema, dict):
        return schema
    marked = {key: mark_opaque(value) for key, value in schema.items()}
    if marked.get("type") == "object" and "properties" not in marked:
        marked["properties"] = {}
        if marked.get("additionalProperties") is True:
            marked["x-synthyra-opaque"] = True
    return marked


def merge_operation(key: str, live_operation: dict[str, Any], entry: dict[str, Any]) -> dict[str, Any]:
    merged = {
        name: copy.deepcopy(value)
        for name, value in entry.items()
        if name not in {"parameterExamples", "requestExample", "requestBody"}
    }
    for name in ("summary", "description"):
        if name in live_operation:
            merged[name] = live_operation[name]

    examples = entry.get("parameterExamples", {})
    parameters = copy.deepcopy(live_operation.get("parameters", []))
    names = {parameter["name"] for parameter in parameters}
    stray = sorted(set(examples) - names)
    if stray:
        raise ContractError(f"{key}: overlay gives examples for parameters the server lacks: {stray}")
    for parameter in parameters:
        if parameter["name"] in examples:
            parameter["example"] = examples[parameter["name"]]
    if parameters:
        merged["parameters"] = parameters

    live_body = live_operation.get("requestBody")
    if live_body is not None and "requestBody" in entry:
        raise ContractError(f"{key}: the server declares a request body, so the overlay may not")
    body = copy.deepcopy(live_body if live_body is not None else entry.get("requestBody"))
    if body is not None and "requestExample" in entry:
        body["content"]["application/json"]["example"] = entry["requestExample"]
    if body is not None:
        merged["requestBody"] = body
    return merged


def referenced_schemas(value: Any) -> set[str]:
    return set(REFERENCE.findall(json.dumps(value)))


def reachable_schemas(roots: set[str], schemas: dict[str, Any]) -> set[str]:
    reached: set[str] = set()
    pending = sorted(roots)
    while pending:
        name = pending.pop()
        if name in reached:
            continue
        if name not in schemas:
            raise ContractError(f"schema {name!r} is referenced but defined neither live nor in the overlay")
        reached.add(name)
        pending.extend(sorted(referenced_schemas(schemas[name]) - reached))
    return reached


def coverage_errors(live: dict[str, Any], overlay: dict[str, Any]) -> list[str]:
    """Live routes the overlay neither describes nor excludes, and overlay routes not served."""
    served = set(live_operations(live))
    known = set(overlay["operations"]) | set(overlay["excluded"])
    errors = [f"served but not in the overlay: {key}" for key in sorted(served - known)]
    errors += [f"in the overlay but not served: {key}" for key in sorted(set(overlay["operations"]) - served)]
    return errors


def merge(live: dict[str, Any], overlay: dict[str, Any], *, live_sha256: str) -> dict[str, Any]:
    """The paclet contract: the overlay's operations, shaped by what the server accepts."""
    errors = coverage_errors(live, overlay)
    if errors:
        raise ContractError("live Gateway and overlay disagree:\n  " + "\n  ".join(errors))

    served = live_operations(live)
    paths: dict[str, dict[str, Any]] = {}
    for key, entry in overlay["operations"].items():
        method, path = key.split(" ", 1)
        paths.setdefault(path, {})[method.lower()] = merge_operation(key, served[key], entry)

    live_schemas = {
        name: mark_opaque(schema) for name, schema in live.get("components", {}).get("schemas", {}).items()
    }
    clashes = sorted(set(live_schemas) & set(overlay["schemas"]))
    candidates = {**live_schemas, **copy.deepcopy(overlay["schemas"])}
    reached = reachable_schemas(referenced_schemas(paths), candidates)
    clashes = [name for name in clashes if name in reached]
    if clashes:
        raise ContractError(f"schemas defined both live and in the overlay: {clashes}")

    document = copy.deepcopy(overlay["document"])
    document["paths"] = paths
    document.setdefault("components", {})["schemas"] = {name: candidates[name] for name in sorted(reached)}
    document["x-synthyra-public-operation-count"] = len(overlay["operations"])
    document["x-synthyra-live-openapi-sha256"] = live_sha256
    return document


def live_digest(live_payload: bytes) -> str:
    return hashlib.sha256(live_payload).hexdigest()


def operation_diff(before: dict[str, Any], after: dict[str, Any]) -> dict[str, list[str]]:
    """Which operations a contract change adds, removes, or alters, by `"METHOD /path"`."""
    old, new = live_operations(before), live_operations(after)
    return {
        "added": sorted(set(new) - set(old)),
        "removed": sorted(set(old) - set(new)),
        "changed": sorted(
            key for key in set(old) & set(new) if normalized_bytes(old[key]) != normalized_bytes(new[key])
        ),
    }
