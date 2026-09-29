#!/usr/bin/env python3
"""Apply a classified patch or minor bump to all paclet version declarations."""

from __future__ import annotations

import argparse
import re
from pathlib import Path


PACLET_VERSION = re.compile(r'("Version"\s*->\s*")(\d+\.\d+\.\d+)(")')
RUNTIME_VERSION = re.compile(
    r'(\$SynthyraPacletVersion\s*=\s*")(\d+\.\d+\.\d+)(";)'
)
PYPROJECT_VERSION = re.compile(r'(?m)^(version\s*=\s*")(\d+\.\d+\.\d+)(")$')
CHANGELOG_VERSION = re.compile(r"(?m)^## (\d+\.\d+\.\d+) - Unreleased$")


def _replace_once(path: Path, pattern: re.Pattern[str], replacement: str) -> None:
    text = path.read_text(encoding="utf-8")
    updated, count = pattern.subn(replacement, text, count=1)
    if count != 1:
        raise ValueError(f"{path}: expected exactly one version declaration")
    path.write_text(updated, encoding="utf-8", newline="\n")


def bump_version(version: str, level: str) -> str:
    major, minor, patch = (int(part) for part in version.split("."))
    if level == "minor":
        return f"{major}.{minor + 1}.0"
    if level == "patch":
        return f"{major}.{minor}.{patch + 1}"
    raise ValueError("Only classified patch and minor updates can be automated")


def current_version(root: Path) -> str:
    declarations = {
        "PacletInfo.wl": (root / "PacletInfo.wl", PACLET_VERSION, 2),
        "Kernel/SynthyraLink.wl": (
            root / "Kernel" / "SynthyraLink.wl",
            RUNTIME_VERSION,
            2,
        ),
        "pyproject.toml": (root / "pyproject.toml", PYPROJECT_VERSION, 2),
        "ResourceDefinition.nb": (
            root / "ResourceDefinition.nb",
            PACLET_VERSION,
            2,
        ),
        "CHANGELOG.md": (root / "CHANGELOG.md", CHANGELOG_VERSION, 1),
    }
    versions: dict[str, str] = {}
    for name, (path, pattern, group) in declarations.items():
        match = pattern.search(path.read_text(encoding="utf-8"))
        if match is None:
            raise ValueError(f"{path}: version declaration not found")
        versions[name] = match.group(group)
    unique = set(versions.values())
    if len(unique) != 1:
        details = ", ".join(f"{name}={version}" for name, version in versions.items())
        raise ValueError(f"Paclet version declarations are inconsistent: {details}")
    return unique.pop()


def update_versions(root: Path, level: str) -> tuple[str, str]:
    paclet_info = root / "PacletInfo.wl"
    old = current_version(root)
    new = bump_version(old, level)

    _replace_once(paclet_info, PACLET_VERSION, rf"\g<1>{new}\g<3>")
    _replace_once(
        root / "Kernel" / "SynthyraLink.wl",
        RUNTIME_VERSION,
        rf"\g<1>{new}\g<3>",
    )
    _replace_once(
        root / "pyproject.toml",
        PYPROJECT_VERSION,
        rf"\g<1>{new}\g<3>",
    )
    _replace_once(root / "ResourceDefinition.nb", PACLET_VERSION, rf"\g<1>{new}\g<3>")

    changelog = root / "CHANGELOG.md"
    changelog_text = changelog.read_text(encoding="utf-8")
    unreleased = re.compile(rf"(?m)^## {re.escape(old)} - Unreleased$")
    if unreleased.search(changelog_text):
        changelog_text = unreleased.sub(f"## {new} - Unreleased", changelog_text, count=1)
    else:
        heading = re.search(r"(?m)^## ", changelog_text)
        if heading is None:
            changelog_text = changelog_text.rstrip() + f"\n\n## {new} - Unreleased\n"
        else:
            changelog_text = (
                changelog_text[: heading.start()]
                + f"## {new} - Unreleased\n\n"
                + changelog_text[heading.start() :]
            )
    changelog.write_text(changelog_text, encoding="utf-8", newline="\n")
    return old, new


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    action = parser.add_mutually_exclusive_group(required=True)
    action.add_argument("--bump", choices=("patch", "minor"))
    action.add_argument("--check", action="store_true")
    parser.add_argument(
        "--root",
        type=Path,
        default=Path(__file__).resolve().parents[1],
        help="Paclet repository root (defaults to the parent of Tools).",
    )
    args = parser.parse_args()
    if args.check:
        print(f"Paclet version declarations are synchronized at {current_version(args.root.resolve())}.")
        return 0
    old, new = update_versions(args.root.resolve(), args.bump)
    print(f"Paclet version: {old} -> {new} ({args.bump})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
