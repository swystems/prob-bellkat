import os
import pickle
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent))
from plot_extremal import derive_pmf_series, load_extremal_series

# ONESWAP_SHOW_PLOTS=1 pytest -s scripts/tests/test_oneswap
def _should_show_plots() -> bool:
    return os.getenv("ONESWAP_SHOW_PLOTS", "0").strip().lower() in {"1", "true", "yes", "on"}


def _plot_results(pmf_mdp: np.ndarray, pmf_lilla: np.ndarray, w_out: np.ndarray, w_lilla: np.ndarray | None) -> None:
    plt.figure(figsize=(6, 2.5), dpi=300)
    plt.plot(range(len(pmf_mdp)), pmf_mdp, label="MDP $Pr(T=t)$", marker="o", markersize=3, linewidth=1)
    plt.plot(range(len(pmf_lilla)), pmf_lilla, label="Li et al. $Pr(T=t)$", marker="x", markersize=3, linestyle="--", linewidth=1)
    plt.xlabel("Timestep $t$")
    plt.ylabel("Output Probability")
    plt.legend()
    plt.tight_layout()
    plt.show()

    plt.figure(figsize=(6, 2.5), dpi=300)
    plt.plot(range(len(w_out)), w_out, label="MDP $W_{out}$", marker="o", markersize=3, linewidth=1)
    if w_lilla is not None:
        plt.plot(range(len(w_lilla)), w_lilla, label="Li et al. $W_{out}$", marker="x", markersize=3, linestyle="--", linewidth=1)
    plt.xlabel("Timestep $t$")
    plt.ylabel("Werner parameter")
    plt.legend()
    plt.tight_layout()
    plt.show()


@pytest.fixture(scope="module")
def oneswap_data() -> dict[str, np.ndarray | float | None]:
    with open("scripts/tests/dump/oneswap_pmf.pkl", "rb") as f:
        pmf_lilla = np.array(pickle.load(f))

    series_main = load_extremal_series("output/quantP_oneswap.json")
    pmf_min, pmf_max = derive_pmf_series(series_main)

    series_pure = load_extremal_series("output/quantP_oneswap_pure.json")
    series_mixed = load_extremal_series("output/quantP_oneswap_mixed.json")
    pmf_pure_min, pmf_pure_max = derive_pmf_series(series_pure)
    pmf_mixed_min, pmf_mixed_max = derive_pmf_series(series_mixed)

    pmf_mdp = np.array(pmf_min)
    pmf_pure = np.array(pmf_pure_min)
    pmf_mixed = np.array(pmf_mixed_min)

    w_out = np.array([
        pure / total if total > 0 else 0.0
        for pure, total in zip(pmf_pure, pmf_pure + pmf_mixed)
    ])

    w_lilla = None
    try:
        with open("scripts/tests/dump/oneswap_werner.pkl", "rb") as f:
            w_lilla = np.array(pickle.load(f))
    except FileNotFoundError:
        pass

    l1_pmf = float(np.sum(np.abs(pmf_mdp - pmf_lilla)))
    rmse_pmf = float(np.sqrt(np.mean((pmf_mdp - pmf_lilla) ** 2)))
    max_pmf = float(np.max(np.abs(pmf_mdp - pmf_lilla)))

    return {
        "pmf_lilla": pmf_lilla,
        "pmf_mdp": pmf_mdp,
        "pmf_main_min": np.array(pmf_min),
        "pmf_main_max": np.array(pmf_max),
        "pmf_pure": pmf_pure,
        "pmf_mixed": pmf_mixed,
        "pmf_pure_min": np.array(pmf_pure_min),
        "pmf_pure_max": np.array(pmf_pure_max),
        "pmf_mixed_min": np.array(pmf_mixed_min),
        "pmf_mixed_max": np.array(pmf_mixed_max),
        "w_out": w_out,
        "w_lilla": w_lilla,
        "l1_pmf": l1_pmf,
        "rmse_pmf": rmse_pmf,
        "max_pmf": max_pmf,
    }


def test_oneswap_pmf_is_deterministic(oneswap_data: dict[str, np.ndarray | float | None]) -> None:
    assert np.allclose(oneswap_data["pmf_main_min"], oneswap_data["pmf_main_max"], atol=1e-6), (
        "PMF min and max should be equal (no non-determinism)"
    )
    assert np.allclose(oneswap_data["pmf_pure_min"], oneswap_data["pmf_pure_max"], atol=1e-6), (
        "Pure PMF min and max should be equal (no non-determinism)"
    )
    assert np.allclose(oneswap_data["pmf_mixed_min"], oneswap_data["pmf_mixed_max"], atol=1e-6), (
        "Mixed PMF min and max should be equal (no non-determinism)"
    )


def test_oneswap_pure_plus_mixed_equals_total(oneswap_data: dict[str, np.ndarray | float | None]) -> None:
    pmf_pure = np.asarray(oneswap_data["pmf_pure"])
    pmf_mixed = np.asarray(oneswap_data["pmf_mixed"])
    pmf_mdp = np.asarray(oneswap_data["pmf_mdp"])

    assert np.allclose(pmf_pure + pmf_mixed, pmf_mdp, atol=1e-6), (
        "Pure + Mixed PMF should equal total PMF"
    )


def test_oneswap_reports_metrics_and_optionally_plots(
    oneswap_data: dict[str, np.ndarray | float | None],
) -> None:
    pmf_lilla = np.asarray(oneswap_data["pmf_lilla"])
    pmf_mdp = np.asarray(oneswap_data["pmf_mdp"])

    mean_lilla = sum(i * p for i, p in enumerate(pmf_lilla))
    mean_mdp = sum(i * p for i, p in enumerate(pmf_mdp))

    print(f"Mean PMF Li et al.: {mean_lilla:.6e}")
    print(f"Mean PMF computed: {mean_mdp:.6e}")
    print("Validation vs expected:")
    print(
        f"  PMF  L1 = {oneswap_data['l1_pmf']:.6e}, "
        f"RMSE = {oneswap_data['rmse_pmf']:.6e}, "
        f"MaxAbs = {oneswap_data['max_pmf']:.6e}"
    )

    if _should_show_plots():
        _plot_results(
            pmf_mdp=np.asarray(oneswap_data["pmf_mdp"]),
            pmf_lilla=np.asarray(oneswap_data["pmf_lilla"]),
            w_out=np.asarray(oneswap_data["w_out"]),
            w_lilla=None if oneswap_data["w_lilla"] is None else np.asarray(oneswap_data["w_lilla"]),
        )

