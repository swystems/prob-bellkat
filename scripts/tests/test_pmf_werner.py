import json
from pathlib import Path

import pytest


SERIES_KEYS = ("cdf_min", "cdf_max")
SUFFIXES = {
    "static": ".json",
    "pure": "_pure.json",
    "mixed": "_mixed.json",
}

REPO_ROOT = Path.cwd().resolve()
OUTPUT_DIR = REPO_ROOT / "output"


def discover_complete_examples():
    grouped_paths = {}

    for kind, suffix in SUFFIXES.items():
        for path in OUTPUT_DIR.glob(f"*{suffix}"):
            example = path.name[: -len(suffix)]
            grouped_paths.setdefault(example, {})[kind] = path

    return {
        example: paths
        for example, paths in grouped_paths.items()
        if set(paths) == set(SUFFIXES)
    }


def load_extremal_series(path):
    with path.open("r", encoding="utf-8") as handle:
        payload = json.load(handle)

    return payload["extremal"]["series"]


def pmf_from_cdf(cdf):
    if not cdf:
        return []

    pmf = [cdf[0]]
    pmf.extend(curr - prev for prev, curr in zip(cdf, cdf[1:]))
    return pmf


EXAMPLES = discover_complete_examples()


def test_complete_example_sets_exist():
    assert EXAMPLES, (
        "No complete static/pure/mixed output trios were found in output/. "
        "Expected files named output/<example>.json, "
        "output/<example>_pure.json, and output/<example>_mixed.json."
    )


@pytest.mark.parametrize("example", sorted(EXAMPLES))
def test_pure_plus_mixed_matches_static(example):
    paths = EXAMPLES[example]
    static_series = load_extremal_series(paths["static"])
    pure_series = load_extremal_series(paths["pure"])
    mixed_series = load_extremal_series(paths["mixed"])

    for series_key in SERIES_KEYS:
        static_pmf = pmf_from_cdf(static_series[series_key])
        pure_pmf = pmf_from_cdf(pure_series[series_key])
        mixed_pmf = pmf_from_cdf(mixed_series[series_key])

        assert len(static_pmf) == len(pure_pmf) == len(mixed_pmf), (
            f"{example} has mismatched lengths for {series_key}: "
            f"static={len(static_pmf)}, pure={len(pure_pmf)}, mixed={len(mixed_pmf)}"
        )

        reconstructed_static = [
            pure_prob + mixed_prob
            for pure_prob, mixed_prob in zip(pure_pmf, mixed_pmf)
        ]

        assert reconstructed_static == pytest.approx(
            static_pmf, rel=1e-12, abs=1e-12
        ), f"{example} failed for {series_key}"
