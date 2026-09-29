"""Strict local JSON Pointer resolution."""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from typing import Any

from .exceptions import ContractError


class LocalRefResolver:
    """Resolve only document-local refs and reject ambiguous traversal."""

    def __init__(self, document: Mapping[str, Any]) -> None:
        self.document = document

    def resolve(self, reference: str, *, location: str) -> Any:
        if not reference.startswith("#/"):
            raise ContractError(
                f"{location}: unsupported non-local $ref {reference!r}; "
                "the checked-in contract must be self-contained"
            )

        current: Any = self.document
        for encoded_token in reference[2:].split("/"):
            token = encoded_token.replace("~1", "/").replace("~0", "~")
            if isinstance(current, Mapping):
                if token not in current:
                    raise ContractError(
                        f"{location}: unresolved $ref {reference!r} at token {token!r}"
                    )
                current = current[token]
            elif isinstance(current, Sequence) and not isinstance(current, (str, bytes)):
                try:
                    index = int(token)
                    current = current[index]
                except (ValueError, IndexError) as exc:
                    raise ContractError(
                        f"{location}: unresolved array token {token!r} in $ref {reference!r}"
                    ) from exc
            else:
                raise ContractError(
                    f"{location}: $ref {reference!r} traverses through a scalar at {token!r}"
                )
        return current

    @staticmethod
    def schema_name(reference: str, *, location: str) -> str:
        prefix = "#/components/schemas/"
        if not reference.startswith(prefix):
            raise ContractError(
                f"{location}: schema $ref {reference!r} is outside components/schemas"
            )
        encoded = reference.removeprefix(prefix)
        if "/" in encoded or not encoded:
            raise ContractError(f"{location}: invalid named schema $ref {reference!r}")
        return encoded.replace("~1", "/").replace("~0", "~")

