from __future__ import annotations

import numpy as np
import pytest

pytest.importorskip("numba")

from scripts.validation.swap_scheme_monte_carlo import simulate
from scripts.validation.validate_swap_schemes import (
    bin_distribution,
    bin_monte_carlo_samples,
    merge_timing_rows,
)


def test_bin_distribution_uses_pmf_density_and_conditional_werner() -> None:
    pmf = np.array([0.0, 0.1, 0.2, 0.3, 0.4])
    werner = np.array([np.nan, 0.9, 0.8, 0.7, 0.6])
    binned = bin_distribution(pmf, werner, np.array([0, 2, 5]))

    assert binned.time == pytest.approx([0.5, 3.0])
    assert binned.widths == pytest.approx([2.0, 3.0])
    assert binned.pmf == pytest.approx([0.05, 0.3])
    assert binned.werner == pytest.approx(
        [
            0.9,
            (0.2 * 0.8 + 0.3 * 0.7 + 0.4 * 0.6) / 0.9,
        ]
    )


def test_monte_carlo_samples_use_the_same_bins() -> None:
    binned = bin_monte_carlo_samples(
        "doubling",
        times=np.array([0, 1, 2, 2, 4]),
        werner=np.array([1.0, 0.8, 0.7, 0.5, 0.4]),
        edges=np.array([0, 2, 5]),
        shots=5,
        runtime_seconds=1.25,
    )

    assert binned.time == pytest.approx([0.5, 3.0])
    assert binned.sample_count.tolist() == [2, 3]
    assert binned.pmf == pytest.approx([0.2, 0.2])
    assert binned.werner == pytest.approx([0.9, (0.7 + 0.5 + 0.4) / 3.0])
    assert binned.coverage == pytest.approx(1.0)


def test_plots_only_timing_merge_preserves_qbkat_rows(tmp_path) -> None:
    timing_path = tmp_path / "timings.csv"
    timing_path.write_text(
        "method,protocol,event,seconds,details\n"
        "QBKAT,doubling,pure+mixed,200.0,exact\n"
        "Monte Carlo,doubling,simulation,4.0,old\n",
        encoding="utf-8",
    )

    merged = merge_timing_rows(
        timing_path,
        [
            {
                "method": "Monte Carlo",
                "protocol": "doubling",
                "event": "simulation",
                "seconds": "3.0",
                "details": "new",
            }
        ],
    )

    assert [(row["method"], row["seconds"]) for row in merged] == [
        ("QBKAT", "200.0"),
        ("Monte Carlo", "3.0"),
    ]


@pytest.mark.parametrize("scheme", ("doubling", "sequential"))
def test_deterministic_hardware_has_unit_completion_time(scheme: str) -> None:
    times, werner = simulate(
        scheme,  # type: ignore[arg-type]
        n_links=4,
        sample_size=16,
        p_gen=1.0,
        p_swap=1.0,
        w0=0.9,
        t_coh=1000.0,
        seed=7,
    )

    assert np.all(times == 1)
    assert werner == pytest.approx(np.full(16, 0.9**4))
