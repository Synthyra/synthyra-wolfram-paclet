"""Generator-specific exceptions with user-facing contract locations."""


class ContractError(ValueError):
    """Raised when an OpenAPI construct cannot be represented safely."""


class GenerationDriftError(RuntimeError):
    """Raised by check mode when generated files are not current."""

