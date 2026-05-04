import os
import pickle
import subprocess
import sys
from pathlib import Path
from typing import Any

import matplotlib.pyplot as plt
import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent))
from plot_extremal import derive_pmf_series, load_extremal_series

# SHOW_PLOTS=1 RUN_VALIDATION_RUNNER=1 pytest -s scripts/tests/test_validation.py
TRUE_ENV_VALUES = {"1", "true", "yes", "on"}
STAR_LINKS = ("AC", "BC")
REFERENCE_ATOL = 1e-6
PMF_REFERENCE_ATOL = {"oneswap": 5e-4}
WERNER_REFERENCE_ATOL = {"oneswap": 5e-6}
STAR_REFERENCE_ATOL = REFERENCE_ATOL


def _env_flag(name: str) -> bool:
    return os.getenv(name, "0").strip().lower() in TRUE_ENV_VALUES


def _should_show_plots() -> bool:
    return _env_flag("SHOW_PLOTS")


def _plot_title_prefix(name: str) -> str:
    if name == "oneswap":
        return "One Swap Example"
    if name == "onedist":
        return "One Distill Example"
    if name == "distswap":
        return "Distill Swap Example"
    return f"{name} Example"


def _load_distribution_data(name: str) -> dict[str, np.ndarray | float | None]:
    with open(f"scripts/tests/dump/{name}_pmf.pkl", "rb") as f:
        pmf_reference = np.array(pickle.load(f))

    pmf_main_min = None
    pmf_main_max = None
    has_main = True
    try:
        series_main = load_extremal_series(f"output/quantP_{name}.json")
        pmf_min, pmf_max = derive_pmf_series(series_main)
        pmf_main_min = np.array(pmf_min)
        pmf_main_max = np.array(pmf_max)
    except FileNotFoundError:
        has_main = False

    series_pure = load_extremal_series(f"output/quantP_{name}_pure.json")
    series_mixed = load_extremal_series(f"output/quantP_{name}_mixed.json")
    pmf_pure_min, pmf_pure_max = derive_pmf_series(series_pure)
    pmf_mixed_min, pmf_mixed_max = derive_pmf_series(series_mixed)

    pmf_pure = np.array(pmf_pure_min)
    pmf_mixed = np.array(pmf_mixed_min)
    pmf_reconstructed = pmf_pure + pmf_mixed
    pmf_mdp = pmf_reconstructed if pmf_main_min is None else np.array(pmf_main_min)

    w_out = np.array([
        pure / total if total > 0 else 0.0
        for pure, total in zip(pmf_pure, pmf_pure + pmf_mixed)
    ])

    w_reference = None
    try:
        with open(f"scripts/tests/dump/{name}_werner.pkl", "rb") as f:
            w_reference = np.array(pickle.load(f))
    except FileNotFoundError:
        pass

    l1_pmf = float(np.sum(np.abs(pmf_mdp - pmf_reference)))
    rmse_pmf = float(np.sqrt(np.mean((pmf_mdp - pmf_reference) ** 2)))
    max_pmf = float(np.max(np.abs(pmf_mdp - pmf_reference)))
    if w_reference is None:
        l1_werner = None
        rmse_werner = None
        max_werner = None
    else:
        finite_reference = np.isfinite(w_reference)
        w_diff = w_out[finite_reference] - w_reference[finite_reference]
        l1_werner = float(np.sum(np.abs(w_diff)))
        rmse_werner = float(np.sqrt(np.mean(w_diff ** 2)))
        max_werner = float(np.max(np.abs(w_diff)))

    return {
        "name": name,
        "has_main": has_main,
        "pmf_reference": pmf_reference,
        "pmf_mdp": pmf_mdp,
        "pmf_main_min": pmf_main_min,
        "pmf_main_max": pmf_main_max,
        "pmf_pure": pmf_pure,
        "pmf_mixed": pmf_mixed,
        "pmf_pure_min": np.array(pmf_pure_min),
        "pmf_pure_max": np.array(pmf_pure_max),
        "pmf_mixed_min": np.array(pmf_mixed_min),
        "pmf_mixed_max": np.array(pmf_mixed_max),
        "w_out": w_out,
        "w_reference": w_reference,
        "l1_pmf": l1_pmf,
        "rmse_pmf": rmse_pmf,
        "max_pmf": max_pmf,
        "l1_werner": l1_werner,
        "rmse_werner": rmse_werner,
        "max_werner": max_werner,
    }


def _load_reference_series(path: str) -> tuple[np.ndarray, np.ndarray]:
    with open(path, "rb") as f:
        reference = pickle.load(f)

    if isinstance(reference, dict):
        items = sorted(reference.items())
        return (
            np.array([t for t, _ in items]),
            np.array([value for _, value in items]),
        )

    values = np.array(reference)
    return np.arange(len(values)), values


def _select_timesteps(series: np.ndarray, timesteps: np.ndarray) -> np.ndarray:
    if (
        np.issubdtype(timesteps.dtype, np.integer)
        and len(timesteps) > 0
        and int(np.max(timesteps)) < len(series)
    ):
        return series[timesteps.astype(int)]

    if len(series) == len(timesteps):
        return series

    raise AssertionError(
        "Cannot align computed series with reference timesteps: "
        f"{len(series)} computed points vs {len(timesteps)} reference points."
    )


def _load_json_pmf(path: str) -> tuple[np.ndarray, np.ndarray]:
    series = load_extremal_series(path)
    pmf_min, pmf_max = derive_pmf_series(series)
    return np.array(pmf_min), np.array(pmf_max)


def _werner_from_json(pure_path: str, mixed_path: str) -> dict[str, np.ndarray]:
    pure_min, pure_max = _load_json_pmf(pure_path)
    mixed_min, mixed_max = _load_json_pmf(mixed_path)

    if len(pure_min) != len(mixed_min) or len(pure_max) != len(mixed_max):
        raise AssertionError("Pure and mixed PMF series must have matching lengths.")

    werner_min = np.array([
        pure / total if total > 0 else 0.0
        for pure, total in zip(pure_min, pure_min + mixed_min)
    ])
    werner_max = np.array([
        pure / total if total > 0 else 0.0
        for pure, total in zip(pure_max, pure_max + mixed_max)
    ])

    return {
        "pure_min": pure_min,
        "pure_max": pure_max,
        "mixed_min": mixed_min,
        "mixed_max": mixed_max,
        "werner_min": werner_min,
        "werner_max": werner_max,
    }


def _series_metrics(computed: np.ndarray, reference: np.ndarray) -> dict[str, float]:
    return {
        "l1": float(np.sum(np.abs(computed - reference))),
        "rmse": float(np.sqrt(np.mean((computed - reference) ** 2))),
        "max": float(np.max(np.abs(computed - reference))),
    }


def _load_star_validation_data() -> dict[str, dict[str, dict[str, Any]]]:
    data: dict[str, dict[str, dict[str, Any]]] = {"pmf": {}, "werner": {}}

    for link in STAR_LINKS:
        link_lower = link.lower()
        pmf_t, pmf_reference = _load_reference_series(
            f"scripts/tests/dump/qcnc_star_pmf_{link_lower}.pkl"
        )
        pmf_min, pmf_max = _load_json_pmf(f"output/quantP5_Star_{link}.json")
        pmf_computed = _select_timesteps(pmf_min, pmf_t)
        pmf_computed_max = _select_timesteps(pmf_max, pmf_t)

        werner_t, werner_reference = _load_reference_series(
            f"scripts/tests/dump/qcnc_star_werner_{link_lower}.pkl"
        )
        werner_json = _werner_from_json(
            f"output/quantP5_Star_{link}_pure.json",
            f"output/quantP5_Star_{link}_mixed.json",
        )
        werner_computed = _select_timesteps(werner_json["werner_min"], werner_t)
        werner_computed_max = _select_timesteps(werner_json["werner_max"], werner_t)

        data["pmf"][link] = {
            "t": pmf_t,
            "computed": pmf_computed,
            "computed_max": pmf_computed_max,
            "reference": pmf_reference,
            "metrics": _series_metrics(pmf_computed, pmf_reference),
        }
        data["werner"][link] = {
            "t": werner_t,
            "computed": werner_computed,
            "computed_max": werner_computed_max,
            "reference": werner_reference,
            "pure_min": _select_timesteps(werner_json["pure_min"], werner_t),
            "pure_max": _select_timesteps(werner_json["pure_max"], werner_t),
            "mixed_min": _select_timesteps(werner_json["mixed_min"], werner_t),
            "mixed_max": _select_timesteps(werner_json["mixed_max"], werner_t),
            "metrics": _series_metrics(werner_computed, werner_reference),
        }

    return data


def _assert_pmf_is_deterministic(data: dict[str, np.ndarray | float | None]) -> None:
    if bool(data["has_main"]):
        assert np.allclose(data["pmf_main_min"], data["pmf_main_max"], atol=1e-6), (
            "PMF min and max should be equal (no non-determinism)"
        )
    assert np.allclose(data["pmf_pure_min"], data["pmf_pure_max"], atol=1e-6), (
        "Pure PMF min and max should be equal (no non-determinism)"
    )
    assert np.allclose(data["pmf_mixed_min"], data["pmf_mixed_max"], atol=1e-6), (
        "Mixed PMF min and max should be equal (no non-determinism)"
    )


def _assert_pure_plus_mixed_equals_total(data: dict[str, np.ndarray | float | None]) -> None:
    if not bool(data["has_main"]):
        pytest.skip("Static quantP output is unavailable for this case; only pure/mixed outputs are validated.")

    pmf_pure = np.asarray(data["pmf_pure"])
    pmf_mixed = np.asarray(data["pmf_mixed"])
    pmf_mdp = np.asarray(data["pmf_mdp"])

    assert np.allclose(pmf_pure + pmf_mixed, pmf_mdp, atol=1e-6), (
        "Pure + Mixed PMF should equal total PMF"
    )


def _assert_matches_reference_pickles(data: dict[str, np.ndarray | float | None]) -> None:
    name = str(data["name"])
    pmf_atol = PMF_REFERENCE_ATOL.get(name, REFERENCE_ATOL)
    werner_atol = WERNER_REFERENCE_ATOL.get(name, REFERENCE_ATOL)
    pmf_reference = np.asarray(data["pmf_reference"])
    pmf_mdp = np.asarray(data["pmf_mdp"])

    assert pmf_mdp.shape == pmf_reference.shape, (
        f"{name} PMF shape mismatch: computed={pmf_mdp.shape}, reference={pmf_reference.shape}"
    )
    assert np.allclose(pmf_mdp, pmf_reference, atol=pmf_atol, rtol=0.0), (
        f"{name} PMF does not match the reference pickle"
    )

    assert data["w_reference"] is not None, f"{name} is missing a Werner reference pickle"
    w_out = np.asarray(data["w_out"])
    w_reference = np.asarray(data["w_reference"])
    assert w_out.shape == w_reference.shape, (
        f"{name} Werner shape mismatch: computed={w_out.shape}, reference={w_reference.shape}"
    )

    finite_reference = np.isfinite(w_reference)
    assert np.any(finite_reference), f"{name} Werner reference has no finite values"
    assert np.allclose(
        w_out[finite_reference],
        w_reference[finite_reference],
        atol=werner_atol,
        rtol=0.0,
    ), (
        f"{name} Werner output does not match the reference pickle"
    )


def _report_metrics_and_optionally_plot(data: dict[str, np.ndarray | float | None]) -> None:
    pmf_reference = np.asarray(data["pmf_reference"])
    pmf_mdp = np.asarray(data["pmf_mdp"])

    mean_reference = sum(i * p for i, p in enumerate(pmf_reference))
    mean_mdp = sum(i * p for i, p in enumerate(pmf_mdp))

    print(f"[{data['name']}] Mean PMF reference: {mean_reference:.6e}")
    print(f"[{data['name']}] Mean PMF computed: {mean_mdp:.6e}")
    if not bool(data["has_main"]):
        print(f"[{data['name']}] Static quantP output missing; using pure+mixed reconstruction.")
    print(f"[{data['name']}] Validation vs expected:")
    print(
        f"  PMF  L1 = {data['l1_pmf']:.6e}, "
        f"RMSE = {data['rmse_pmf']:.6e}, "
        f"MaxAbs = {data['max_pmf']:.6e}"
    )
    if data["w_reference"] is not None:
        print(
            f"  W    L1 = {data['l1_werner']:.6e}, "
            f"RMSE = {data['rmse_werner']:.6e}, "
            f"MaxAbs = {data['max_werner']:.6e}"
        )

    if _should_show_plots():
        title_prefix = _plot_title_prefix(str(data["name"]))
        _plot_results(
            pmf_mdp=np.asarray(data["pmf_mdp"]),
            pmf_lilla=np.asarray(data["pmf_reference"]),
            w_out=np.asarray(data["w_out"]),
            w_lilla=None if data["w_reference"] is None else np.asarray(data["w_reference"]),
            title_prefix=title_prefix,
        )


def _plot_results(
    pmf_mdp: np.ndarray,
    pmf_lilla: np.ndarray,
    w_out: np.ndarray,
    w_lilla: np.ndarray | None,
    title_prefix: str,
) -> None:
    plt.figure(figsize=(6, 2.5), dpi=300)
    plt.title(f"{title_prefix} - Output PMF")
    plt.plot(range(len(pmf_mdp)), pmf_mdp, label="MDP $Pr(T=t)$", marker="o", markersize=3, linewidth=1)
    plt.plot(range(len(pmf_lilla)), pmf_lilla, label="Li et al. $Pr(T=t)$", marker="x", markersize=3, linestyle="--", linewidth=1)
    plt.xlabel("Timestep $t$")
    plt.ylabel("Output Probability")
    plt.legend()
    plt.tight_layout()
    plt.show()

    plt.figure(figsize=(6, 2.5), dpi=300)
    plt.title(f"{title_prefix} - Werner Parameter")
    plt.plot(range(len(w_out)), w_out, label="MDP $W_{out}$", marker="o", markersize=3, linewidth=1)
    if w_lilla is not None:
        plt.plot(range(len(w_lilla)), w_lilla, label="Li et al. $W_{out}$", marker="x", markersize=3, linestyle="--", linewidth=1)
    plt.xlabel("Timestep $t$")
    plt.ylabel("Werner parameter")
    plt.legend()
    plt.tight_layout()
    plt.show()


def _report_star_metrics_and_optionally_plot(
    star_data: dict[str, dict[str, dict[str, Any]]],
) -> None:
    for group_name, group_data in star_data.items():
        print(f"[qcnc_star] {group_name.upper()} validation vs expected:")
        for link in STAR_LINKS:
            metrics = group_data[link]["metrics"]
            print(
                f"  {link} L1 = {metrics['l1']:.6e}, "
                f"RMSE = {metrics['rmse']:.6e}, "
                f"MaxAbs = {metrics['max']:.6e}"
            )

    if not _should_show_plots():
        return

    _plot_star_comparison(
        title="Star Example - Output PMF",
        ylabel="Output Probability",
        group_data=star_data["pmf"],
    )
    _plot_star_comparison(
        title="Star Example - Werner Parameter",
        ylabel="Werner parameter",
        group_data=star_data["werner"],
    )


def _plot_star_comparison(
    title: str,
    ylabel: str,
    group_data: dict[str, dict[str, Any]],
) -> None:
    plt.figure(figsize=(6, 2.5), dpi=300)
    plt.title(title)

    styles = {
        "AC": {"computed": ("o", "-"), "reference": ("x", "--")},
        "BC": {"computed": ("s", "-"), "reference": ("+", "--")},
    }

    for link in STAR_LINKS:
        metric = "Pr" if ylabel == "Output Probability" else "W"
        t = group_data[link]["t"]
        computed_marker, computed_style = styles[link]["computed"]
        reference_marker, reference_style = styles[link]["reference"]
        plt.plot(
            t,
            group_data[link]["computed"],
            label=f"MDP ${metric}^{{{link}}}(T=t)$",
            marker=computed_marker,
            markersize=3,
            linestyle=computed_style,
            linewidth=1,
        )
        plt.plot(
            t,
            group_data[link]["reference"],
            label=f"QCNC ${metric}^{{{link}}}(T=t)$",
            marker=reference_marker,
            markersize=3,
            linestyle=reference_style,
            linewidth=1,
        )

    plt.xlabel("Timestep $t$")
    plt.ylabel(ylabel)
    plt.legend()
    plt.tight_layout()
    plt.show()


@pytest.fixture(scope="session", autouse=True)
def prepare_validation_outputs() -> None:
    if not _env_flag("RUN_VALIDATION_RUNNER"):
        return

    subprocess.run(["bash", "scripts/tests/test_validation_runner.sh", "all"], check=True)


@pytest.fixture(scope="module")
def oneswap_data() -> dict[str, np.ndarray | float | None]:
    return _load_distribution_data("oneswap")


@pytest.fixture(scope="module")
def onedist_data() -> dict[str, np.ndarray | float | None]:
    return _load_distribution_data("onedist")


@pytest.fixture(scope="module")
def distswap_data() -> dict[str, np.ndarray | float | None]:
    return _load_distribution_data("distswap")


@pytest.fixture(scope="module")
def star_data() -> dict[str, dict[str, dict[str, Any]]]:
    return _load_star_validation_data()


@pytest.mark.parametrize("fixture_name", ("oneswap_data", "onedist_data", "distswap_data"))
def test_distribution_matches_reference_pickles(
    request: pytest.FixtureRequest,
    fixture_name: str,
) -> None:
    _assert_matches_reference_pickles(request.getfixturevalue(fixture_name))


def test_oneswap_pmf_is_deterministic(oneswap_data: dict[str, np.ndarray | float | None]) -> None:
    _assert_pmf_is_deterministic(oneswap_data)


def test_oneswap_pure_plus_mixed_equals_total(oneswap_data: dict[str, np.ndarray | float | None]) -> None:
    _assert_pure_plus_mixed_equals_total(oneswap_data)


def test_oneswap_reports_metrics_and_optionally_plots(
    oneswap_data: dict[str, np.ndarray | float | None],
) -> None:
    _report_metrics_and_optionally_plot(oneswap_data)


def test_onedist_pmf_is_deterministic(onedist_data: dict[str, np.ndarray | float | None]) -> None:
    _assert_pmf_is_deterministic(onedist_data)


def test_onedist_pure_plus_mixed_equals_total(onedist_data: dict[str, np.ndarray | float | None]) -> None:
    _assert_pure_plus_mixed_equals_total(onedist_data)


def test_onedist_reports_metrics_and_optionally_plots(
    onedist_data: dict[str, np.ndarray | float | None],
) -> None:
    _report_metrics_and_optionally_plot(onedist_data)


def test_distswap_pmf_is_deterministic(distswap_data: dict[str, np.ndarray | float | None]) -> None:
    _assert_pmf_is_deterministic(distswap_data)


def test_distswap_pure_plus_mixed_equals_total(distswap_data: dict[str, np.ndarray | float | None]) -> None:
    _assert_pure_plus_mixed_equals_total(distswap_data)


def test_distswap_reports_metrics_and_optionally_plots(
    distswap_data: dict[str, np.ndarray | float | None],
) -> None:
    _report_metrics_and_optionally_plot(distswap_data)


def test_star_pmf_is_deterministic(star_data: dict[str, dict[str, dict[str, Any]]]) -> None:
    for link in STAR_LINKS:
        assert np.allclose(
            star_data["pmf"][link]["computed"],
            star_data["pmf"][link]["computed_max"],
            atol=STAR_REFERENCE_ATOL,
        ), f"Star {link} PMF min and max should be equal (no non-determinism)"


def test_star_werner_is_deterministic(star_data: dict[str, dict[str, dict[str, Any]]]) -> None:
    for link in STAR_LINKS:
        assert np.allclose(
            star_data["werner"][link]["pure_min"],
            star_data["werner"][link]["pure_max"],
            atol=STAR_REFERENCE_ATOL,
        ), f"Star {link} pure PMF min and max should be equal (no non-determinism)"
        assert np.allclose(
            star_data["werner"][link]["mixed_min"],
            star_data["werner"][link]["mixed_max"],
            atol=STAR_REFERENCE_ATOL,
        ), f"Star {link} mixed PMF min and max should be equal (no non-determinism)"
        assert np.allclose(
            star_data["werner"][link]["computed"],
            star_data["werner"][link]["computed_max"],
            atol=STAR_REFERENCE_ATOL,
        ), f"Star {link} Werner min and max should be equal (no non-determinism)"


def test_star_matches_reference_pickles(star_data: dict[str, dict[str, dict[str, Any]]]) -> None:
    for group_name in ("pmf", "werner"):
        for link in STAR_LINKS:
            assert np.allclose(
                star_data[group_name][link]["computed"],
                star_data[group_name][link]["reference"],
                atol=STAR_REFERENCE_ATOL,
            ), f"Star {link} {group_name} does not match the QCNC paper"


def test_star_reports_metrics_and_optionally_plots(
    star_data: dict[str, dict[str, dict[str, Any]]],
) -> None:
    _report_star_metrics_and_optionally_plot(star_data)
