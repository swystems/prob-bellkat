"""Fast Monte Carlo sampling for the swap-scheme validation.

The recursive sampling algorithm is adapted from ``repeater_mc.py`` in
Boxi Li, Tim Coopmans, and David Elkouss's
``repeater-cut-off-optimization`` project:

https://github.com/BoxiLi/repeater-cut-off-optimization

The adapted source is Copyright 2018-2019 QuTech (TUDelft and TNO) and is
licensed under the Apache License, Version 2.0:
https://www.apache.org/licenses/LICENSE-2.0

This module keeps only its no-cutoff doubling logic and adds the analogous
asymmetric left-to-right recursion needed by the QBKAT validation.
"""

from __future__ import annotations

import math
from typing import Literal

try:
    import numba as nb
except ImportError as exc:  # pragma: no cover - exercised only without the optional dependency
    raise SystemExit(
        "Monte Carlo validation requires numba. Activate the project Python "
        "environment or install it with `python -m pip install numba`."
    ) from exc

import numpy as np


@nb.njit(cache=True)
def _seed_numba(seed: int) -> None:
    np.random.seed(seed)


@nb.njit(cache=True)
def _age_earlier_pair(
    t_left: int,
    w_left: float,
    t_right: int,
    w_right: float,
    t_coh: float,
    decay_factor: float,
) -> tuple[float, float]:
    """Apply memory decoherence to whichever input pair finishes first."""
    delta = abs(t_left - t_right)
    decay = math.exp(-decay_factor * delta / t_coh)
    if t_left < t_right:
        w_left *= decay
    elif t_right < t_left:
        w_right *= decay
    return w_left, w_right


@nb.njit(cache=True)
def _sample_elementary(p_gen: float, w0: float) -> tuple[int, float]:
    return int(np.random.geometric(p_gen)), w0


@nb.njit(cache=True)
def _sample_doubling_level(
    level: int,
    p_gen: float,
    p_swap: float,
    w0: float,
    t_coh: float,
    decay_factor: float,
) -> tuple[int, float]:
    """Sample a balanced repeater tree containing ``2**level`` links."""
    if level == 0:
        return _sample_elementary(p_gen, w0)

    elapsed = 0
    while True:
        t_left, w_left = _sample_doubling_level(
            level - 1, p_gen, p_swap, w0, t_coh, decay_factor
        )
        t_right, w_right = _sample_doubling_level(
            level - 1, p_gen, p_swap, w0, t_coh, decay_factor
        )
        elapsed += max(t_left, t_right)
        w_left, w_right = _age_earlier_pair(
            t_left, w_left, t_right, w_right, t_coh, decay_factor
        )
        if np.random.random() <= p_swap:
            return elapsed, w_left * w_right
        # A failed swap consumes both inputs, so this level starts again.


@nb.njit(cache=True)
def _sample_sequential_links(
    n_links: int,
    p_gen: float,
    p_swap: float,
    w0: float,
    t_coh: float,
    decay_factor: float,
) -> tuple[int, float]:
    """Sample proactive left-to-right swapping over ``n_links`` links."""
    if n_links == 1:
        return _sample_elementary(p_gen, w0)

    elapsed = 0
    while True:
        t_left, w_left = _sample_sequential_links(
            n_links - 1, p_gen, p_swap, w0, t_coh, decay_factor
        )
        t_right, w_right = _sample_elementary(p_gen, w0)
        elapsed += max(t_left, t_right)
        w_left, w_right = _age_earlier_pair(
            t_left, w_left, t_right, w_right, t_coh, decay_factor
        )
        if np.random.random() <= p_swap:
            return elapsed, w_left * w_right
        # A failed swap consumes the extended pair and the elementary pair.


@nb.njit(cache=True)
def _run_doubling_samples(
    level: int,
    sample_size: int,
    p_gen: float,
    p_swap: float,
    w0: float,
    t_coh: float,
    decay_factor: float,
) -> tuple[np.ndarray, np.ndarray]:
    times = np.empty(sample_size, dtype=np.int64)
    werner = np.empty(sample_size, dtype=np.float64)
    for index in range(sample_size):
        times[index], werner[index] = _sample_doubling_level(
            level, p_gen, p_swap, w0, t_coh, decay_factor
        )
    return times, werner


@nb.njit(cache=True)
def _run_sequential_samples(
    n_links: int,
    sample_size: int,
    p_gen: float,
    p_swap: float,
    w0: float,
    t_coh: float,
    decay_factor: float,
) -> tuple[np.ndarray, np.ndarray]:
    times = np.empty(sample_size, dtype=np.int64)
    werner = np.empty(sample_size, dtype=np.float64)
    for index in range(sample_size):
        times[index], werner[index] = _sample_sequential_links(
            n_links, p_gen, p_swap, w0, t_coh, decay_factor
        )
    return times, werner


def simulate(
    scheme: Literal["doubling", "sequential"],
    *,
    n_links: int,
    sample_size: int,
    p_gen: float,
    p_swap: float,
    w0: float,
    t_coh: float,
    seed: int,
    decay_factor: float = 2.0,
) -> tuple[np.ndarray, np.ndarray]:
    """Sample completion times and conditional output Werner parameters.

    ``decay_factor=2`` matches the validation model: the two endpoint memories
    of a waiting pair each decay with coherence time ``t_coh``.
    """
    if n_links < 1:
        raise ValueError("n_links must be positive")
    if not 0.0 < p_gen <= 1.0:
        raise ValueError("p_gen must be in (0, 1]")
    if not 0.0 < p_swap <= 1.0:
        raise ValueError("p_swap must be in (0, 1]")
    if not 0.0 <= w0 <= 1.0:
        raise ValueError("w0 must be in [0, 1]")
    if sample_size < 1:
        raise ValueError("sample_size must be positive")
    if t_coh <= 0:
        raise ValueError("t_coh must be positive")
    if decay_factor <= 0:
        raise ValueError("decay_factor must be positive")

    _seed_numba(seed)
    if scheme == "doubling":
        if n_links & (n_links - 1):
            raise ValueError("doubling requires n_links to be a power of two")
        return _run_doubling_samples(
            int(math.log2(n_links)),
            sample_size,
            p_gen,
            p_swap,
            w0,
            t_coh,
            decay_factor,
        )
    if scheme == "sequential":
        return _run_sequential_samples(
            n_links,
            sample_size,
            p_gen,
            p_swap,
            w0,
            t_coh,
            decay_factor,
        )
    raise ValueError(f"unknown scheme: {scheme}")


def warm_up() -> None:
    """Compile both Numba kernels before wall-clock measurements start."""
    common = {
        "n_links": 4,
        "sample_size": 1,
        "p_gen": 0.5,
        "p_swap": 0.5,
        "w0": 0.9,
        "t_coh": 1000.0,
        "decay_factor": 2.0,
    }
    simulate("doubling", seed=0, **common)
    simulate("sequential", seed=1, **common)
