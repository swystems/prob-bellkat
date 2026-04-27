import os
import pickle
import subprocess
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent))
from plot_extremal import derive_pmf_series, load_extremal_series

# SHOW_PLOTS=1 RUN_VALIDATION_RUNNER=1 pytest -s scripts/tests/test_validation.py
TRUE_ENV_VALUES = {"1", "true", "yes", "on"}


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
        l1_werner = float(np.sum(np.abs(w_out - w_reference)))
        rmse_werner = float(np.sqrt(np.mean((w_out - w_reference) ** 2)))
        max_werner = float(np.max(np.abs(w_out - w_reference)))

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


@pytest.fixture(scope="session", autouse=True)
def prepare_validation_outputs() -> None:
    if not _env_flag("RUN_VALIDATION_RUNNER"):
        return

    subprocess.run(["bash", "scripts/validation_runner.sh", "all"], check=True)


@pytest.fixture(scope="module")
def oneswap_data() -> dict[str, np.ndarray | float | None]:
    return _load_distribution_data("oneswap")


@pytest.fixture(scope="module")
def onedist_data() -> dict[str, np.ndarray | float | None]:
    return _load_distribution_data("onedist")


@pytest.fixture(scope="module")
def distswap_data() -> dict[str, np.ndarray | float | None]:
    return _load_distribution_data("distswap")


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
