from __future__ import annotations

import shutil
from pathlib import Path

import pytest

from update_paclet_version import PACLET_VERSION, bump_version, current_version, update_versions


@pytest.mark.parametrize(
    ("version", "level", "expected"),
    [("0.1.0", "patch", "0.1.1"), ("0.1.9", "minor", "0.2.0")],
)
def test_bump_version(version: str, level: str, expected: str) -> None:
    assert bump_version(version, level) == expected


def test_update_versions_keeps_all_declarations_in_sync(
    project_root: Path, tmp_path: Path
) -> None:
    for relative in (
        Path("PacletInfo.wl"),
        Path("Kernel/SynthyraLink.wl"),
        Path("pyproject.toml"),
        Path("ResourceDefinition.nb"),
        Path("CHANGELOG.md"),
    ):
        destination = tmp_path / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(project_root / relative, destination)

    current = PACLET_VERSION.search(
        (tmp_path / "PacletInfo.wl").read_text(encoding="utf-8")
    ).group(2)
    expected = bump_version(current, "minor")
    old, new = update_versions(tmp_path, "minor")

    assert (old, new) == (current, expected)
    assert f'"Version" -> "{new}"' in (tmp_path / "PacletInfo.wl").read_text()
    assert f'$SynthyraPacletVersion = "{new}";' in (
        tmp_path / "Kernel/SynthyraLink.wl"
    ).read_text()
    assert f'version = "{new}"' in (tmp_path / "pyproject.toml").read_text()
    assert f'"Version" -> "{new}"' in (tmp_path / "ResourceDefinition.nb").read_text()
    assert f"## {new} - Unreleased" in (tmp_path / "CHANGELOG.md").read_text()
    assert current_version(tmp_path) == new


def test_current_version_rejects_inconsistent_declarations(
    project_root: Path, tmp_path: Path
) -> None:
    for relative in (
        Path("PacletInfo.wl"),
        Path("Kernel/SynthyraLink.wl"),
        Path("pyproject.toml"),
        Path("ResourceDefinition.nb"),
        Path("CHANGELOG.md"),
    ):
        destination = tmp_path / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(project_root / relative, destination)
    paclet_info = tmp_path / "PacletInfo.wl"
    expected = current_version(project_root)
    paclet_info.write_text(
        paclet_info.read_text(encoding="utf-8").replace(
            f'"Version" -> "{expected}"', '"Version" -> "9.9.9"'
        ),
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match="inconsistent"):
        current_version(tmp_path)
