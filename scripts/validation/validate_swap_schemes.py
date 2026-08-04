from __future__ import annotations

import csv
import json
import math
import pickle
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import numpy as np

from scripts.analysis.swap_comparison.common import (
    COLORS,
    build_command,
    executable_command,
    run_command,
)
from scripts.plot.config import (
    JOINT_PLOTS_WSPACE,
    SWAP_COMPARISON_COMBINED_HEIGHT_INCHES,
    SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES,
    SWAP_COMPARISON_JOINT_LEGEND_Y,
    SWAP_COMPARISON_JOINT_TOP,
    TIME_AXIS_LABEL,
    VALIDATION_HEIGHT_INCHES,
    VALIDATION_LINE_WIDTH_INCHES,
    configure_matplotlib,
    get_plot_profile,
    hide_overlapping_inner_x_tick_label,
    output_path,
    save_figure,
    style_axes,
    use_informative_y_ticks,
)
from scripts.plot.plot_extremal import (
    derive_average_werner_series,
    derive_pmf_series,
    load_extremal_series,
)
from scripts.plot.scheme_labels import scheme_plot_label


PROTOCOLS = ("swap-asap", "doubling", "left-to-right", "right-to-left")
DEFAULT_PROTOCOLS = ("swap-asap", "doubling", "left-to-right")
GOODENOUGH_SEGMENTS = 4

PROTOCOL_COLORS = {
    "doubling": COLORS[0],
    "left-to-right": COLORS[1],
    "right-to-left": COLORS[1],
    "swap-asap": COLORS[2],
}

REFERENCE_COLORS = {
    "doubling": "#005AB5",
    "left-to-right": "#7A1FA2",
    "right-to-left": "#7A1FA2",
}

QBKAT_LINESTYLE = "-"
REFERENCE_LINESTYLE = "--"
REFERENCE_LINEWIDTH = 1.6
MC_MARKERS = {
    "doubling": "o",
    "left-to-right": "s",
    "right-to-left": "s",
}
MC_COLORS = {
    "doubling": "#00A6D6",
    "left-to-right": "#E6007E",
    "right-to-left": "#E6007E",
}
MC_MARKERSIZE = 1.35
MC_ALPHA = 0.3
MC_DECAY_FACTOR = 2.0
MC_LINKS = 4
WERNER_START_TIME = 1  # The conditional Werner value is undefined at t=0.
PMF_KEYS = ("pmf", "delivery_pmf", "probabilities", "probability")
CDF_KEYS = ("cdf", "delivery_cdf")
WERNER_KEYS = ("werner", "w_out", "lambda", "lambda_series", "average_werner")
FINAL_LAMBDA_KEYS = ("e_lambda", "E_Lambda", "average_lambda", "final_lambda")


def validation_label(protocol: str, source: str) -> str:
    acronym = scheme_plot_label(protocol)
    if source == "qbkat":
        return f"QBKAT {acronym}"
    if source == "mc":
        return f"MC {acronym}"
    if source == "reference":
        author = "Li" if protocol == "doubling" else "La Corte"
        return rf"{author} $\mathit{{et\ al.}}$ {acronym}"
    raise KeyError(source)


@dataclass(frozen=True)
class ProtocolSeries:
    protocol: str
    pure_path: Path
    mixed_path: Path
    pmf: np.ndarray
    cdf: np.ndarray
    werner: np.ndarray
    pure_mass: float
    total_mass: float
    tail: float
    extrema_gap: float


@dataclass(frozen=True)
class ReferenceSeries:
    protocol: str
    source: Path
    pmf: np.ndarray | None = None
    cdf: np.ndarray | None = None
    werner: np.ndarray | None = None
    final_lambda: float | None = None


@dataclass(frozen=True)
class BinnedSeries:
    time: np.ndarray
    pmf: np.ndarray
    werner: np.ndarray
    widths: np.ndarray


@dataclass(frozen=True)
class MonteCarloSeries:
    protocol: str
    time: np.ndarray
    pmf: np.ndarray
    werner: np.ndarray
    widths: np.ndarray
    sample_count: np.ndarray
    shots: int
    runtime_seconds: float
    coverage: float


def memory_lambda(t_coh: int) -> float:
    return math.exp(-1.0 / t_coh)


def goodenough_lambda_n(segments: int, p_gen: float, lambda_memory: float) -> float:
    """Appendix D.1 recursion for E[Lambda_n] without a cut-off policy.

    The paper's n counts elementary segments. A 5-node line has four elementary
    segments, so the 5-node swap-ASAP oracle is E[Lambda_4].
    """
    if segments < 1:
        raise ValueError("segments must be positive")
    if not 0.0 < p_gen < 1.0:
        raise ValueError("p_gen must be in (0, 1)")

    q = 1.0 - p_gen

    def c_coeff(a: int, b: int) -> float:
        numerator = -(q**a) * (lambda_memory**b) * (1.0 / lambda_memory - lambda_memory)
        denominator = (
            (1.0 - (lambda_memory ** (b - 1)) * (q**a))
            * (1.0 - (lambda_memory ** (b + 1)) * (q**a))
        )
        return numerator / denominator

    def d_coeff(a: int, b: int) -> float:
        return (q**a) / ((lambda_memory ** (1 - b)) - (q**a))

    terms: dict[tuple[int, int], float] = {(1, 0): 1.0}
    for _ in range(segments - 1):
        next_terms: dict[tuple[int, int], float] = {}
        for (a, b), coefficient in terms.items():
            next_terms[(a + 1, b)] = (
                next_terms.get((a + 1, b), 0.0)
                + coefficient * c_coeff(a, b)
            )
            next_terms[(1, 1)] = (
                next_terms.get((1, 1), 0.0)
                + coefficient * d_coeff(a, b)
            )
        terms = next_terms

    z_n = sum(
        coefficient
        * ((q**a) * (lambda_memory**b))
        / (1.0 - (q**a) * (lambda_memory**b))
        for (a, b), coefficient in terms.items()
    )
    return ((1.0 - q) / q) ** segments * z_n


def goodenough_lambda_4(p_gen: float, t_coh: int) -> float:
    return goodenough_lambda_n(GOODENOUGH_SEGMENTS, p_gen, memory_lambda(t_coh))


def protocol_tag(protocol: str) -> str:
    return protocol.replace("-", "_")


def output_json_path(output_dir: Path, file_prefix: str, protocol: str, event: str) -> Path:
    return output_dir / f"{file_prefix}_{protocol_tag(protocol)}_qmdp_{event}.json"


def run_qmdp_case(
    args: Any,
    protocol: str,
    event: str,
    output_dir: Path,
) -> tuple[Path, float | None]:
    path = output_json_path(output_dir, args.file_prefix, protocol, event)
    if args.reuse_existing and path.is_file():
        print(f"{protocol} qmdp/{event}: reusing {path}")
        return path, None

    command = [
        *executable_command(args.executable),
        "--paper-assumptions" if protocol == "swap-asap" else "--homogeneous-links",
        "--protocol",
        protocol,
        "--event",
        event,
        "--p-gen",
        f"{args.p_gen:.17g}",
        "--p-swap",
        "1" if protocol == "swap-asap" else f"{args.p_swap:.17g}",
        "--w0",
        "1" if protocol == "swap-asap" else f"{args.w0:.17g}",
        "--t-coh",
        str(args.t_coh if protocol == "swap-asap" else args.reference_t_coh),
        "--json",
        "qmdp",
        "--compute-extremal",
        "--truncation",
        str(args.truncation),
    ]
    elapsed = run_command(
        command,
        stdout_path=path,
        status_label=f"{protocol} qmdp/{event}",
    )
    return path, elapsed


def run_protocol_outputs(
    args: Any,
    protocols: tuple[str, ...],
    output_dir: Path,
) -> list[dict[str, Any]]:
    if not args.no_build:
        command = build_command(args.executable)
        if command is not None:
            run_command(command, status_label=f"cabal build {args.executable}")

    timing_rows: list[dict[str, Any]] = []
    output_dir.mkdir(parents=True, exist_ok=True)
    for protocol in protocols:
        event_seconds: dict[str, float] = {}
        for event in ("pure", "mixed"):
            path, elapsed = run_qmdp_case(args, protocol, event, output_dir)
            if elapsed is not None and protocol in ("doubling", "left-to-right"):
                event_seconds[event] = elapsed
                timing_rows.append(
                    {
                        "method": "QBKAT",
                        "protocol": (
                            "doubling" if protocol == "doubling" else "sequential"
                        ),
                        "event": event,
                        "seconds": f"{elapsed:.6f}",
                        "details": str(path),
                    }
                )
        if len(event_seconds) == 2:
            timing_rows.append(
                {
                    "method": "QBKAT",
                    "protocol": (
                        "doubling" if protocol == "doubling" else "sequential"
                    ),
                    "event": "pure+mixed",
                    "seconds": f"{sum(event_seconds.values()):.6f}",
                    "details": "sum of the two QMDP event evaluations",
                }
            )
    return timing_rows


def load_protocol_series(output_dir: Path, file_prefix: str, protocol: str) -> ProtocolSeries:
    pure_path = output_json_path(output_dir, file_prefix, protocol, "pure")
    mixed_path = output_json_path(output_dir, file_prefix, protocol, "mixed")
    pure_series = load_extremal_series(pure_path)
    mixed_series = load_extremal_series(mixed_path)

    pure_min, pure_max = derive_pmf_series(pure_series)
    mixed_min, mixed_max = derive_pmf_series(mixed_series)
    if len(pure_min) != len(mixed_min):
        raise SystemExit(f"{protocol}: pure/mixed PMFs have different lengths.")

    pure_pmf = np.array(pure_max, dtype=float)
    mixed_pmf = np.array(mixed_max, dtype=float)
    pmf = pure_pmf + mixed_pmf
    cdf = np.cumsum(pmf)
    _, werner_min, werner_max = derive_average_werner_series(pure_series, mixed_series)
    werner = np.array(werner_max, dtype=float)
    extrema_gap = max(
        max_abs_diff(pure_min, pure_max),
        max_abs_diff(mixed_min, mixed_max),
        max_abs_diff(werner_min, werner_max),
    )

    total_mass = float(np.sum(pmf))
    pure_mass = float(np.sum(pure_pmf))
    return ProtocolSeries(
        protocol=protocol,
        pure_path=pure_path,
        mixed_path=mixed_path,
        pmf=pmf,
        cdf=cdf,
        werner=werner,
        pure_mass=pure_mass,
        total_mass=total_mass,
        tail=max(0.0, 1.0 - total_mass),
        extrema_gap=extrema_gap,
    )


def max_abs_diff(left: Any, right: Any) -> float:
    left_array = np.array(left, dtype=float)
    right_array = np.array(right, dtype=float)
    if left_array.shape != right_array.shape:
        return math.inf
    if left_array.size == 0:
        return 0.0
    return float(np.max(np.abs(left_array - right_array)))


def pick_sequence(mapping: dict[str, Any], keys: tuple[str, ...]) -> np.ndarray | None:
    for key in keys:
        if key in mapping and mapping[key] is not None:
            return np.array(mapping[key], dtype=float)
    return None


def pick_float(mapping: dict[str, Any], keys: tuple[str, ...]) -> float | None:
    for key in keys:
        if key in mapping and mapping[key] is not None:
            return float(mapping[key])
    return None


def normalize_reference_entry(protocol: str, source: Path, entry: Any) -> ReferenceSeries:
    if not isinstance(entry, dict):
        raise SystemExit(
            f"{source}: reference entry for {protocol!r} must be a dict. "
            "Expected keys include pmf, cdf, werner, and/or e_lambda."
        )

    pmf = pick_sequence(entry, PMF_KEYS)
    cdf = pick_sequence(entry, CDF_KEYS)
    if pmf is None and cdf is not None:
        pmf = np.diff(np.insert(cdf, 0, 0.0))
    if cdf is None and pmf is not None:
        cdf = np.cumsum(pmf)

    return ReferenceSeries(
        protocol=protocol,
        source=source,
        pmf=pmf,
        cdf=cdf,
        werner=pick_sequence(entry, WERNER_KEYS),
        final_lambda=pick_float(entry, FINAL_LAMBDA_KEYS),
    )


def load_reference_pickle(
    path: Path | None,
    protocols: tuple[str, ...],
    *,
    quiet_missing: bool = False,
) -> dict[str, ReferenceSeries]:
    if path is None:
        return {}
    if not path.exists():
        if not quiet_missing:
            print_reference_template(path, protocols)
        return {}

    with open(path, "rb") as handle:
        payload = pickle.load(handle)

    if isinstance(payload, dict) and isinstance(payload.get("protocols"), dict):
        payload = payload["protocols"]

    references: dict[str, ReferenceSeries] = {}
    if isinstance(payload, dict):
        if "protocol" in payload:
            protocol = str(payload["protocol"])
            references[protocol] = normalize_reference_entry(protocol, path, payload)
        else:
            for protocol in protocols:
                if protocol in payload:
                    references[protocol] = normalize_reference_entry(protocol, path, payload[protocol])
    elif isinstance(payload, (list, tuple)):
        for item in payload:
            if isinstance(item, dict) and "protocol" in item:
                protocol = str(item["protocol"])
                if protocol in protocols:
                    references[protocol] = normalize_reference_entry(protocol, path, item)
    else:
        raise SystemExit(f"{path}: unsupported reference pickle shape {type(payload).__name__}.")

    if references:
        loaded = ", ".join(sorted(references))
        print(f"Loaded reference data for {loaded} from {path}")
    else:
        print_reference_template(path, protocols)
    return references


def load_reference_pair_pickles(
    pmf_path: Path,
    werner_path: Path,
    protocols: tuple[str, ...],
) -> dict[str, ReferenceSeries]:
    if not pmf_path.exists() or not werner_path.exists():
        missing = [
            str(path)
            for path in (pmf_path, werner_path)
            if not path.exists()
        ]
        print(
            "Reference PMF/Werner pickle pair missing: "
            + ", ".join(missing)
            + ". Missing references are skipped."
        )
        return {}

    pmf = load_array_pickle(pmf_path)
    werner = load_array_pickle(werner_path)
    cdf = np.cumsum(pmf)
    references = {
        protocol: ReferenceSeries(
            protocol=protocol,
            source=pmf_path,
            pmf=pmf,
            cdf=cdf,
            werner=werner,
        )
        for protocol in protocols
    }
    loaded = ", ".join(protocols)
    print(f"Loaded PMF/Werner reference data for {loaded} from {pmf_path} and {werner_path}")
    return references


def load_array_pickle(path: Path) -> np.ndarray:
    with open(path, "rb") as handle:
        payload = pickle.load(handle)
    return np.array(payload, dtype=float)


def print_reference_template(path: Path, protocols: tuple[str, ...]) -> None:
    expected = ", ".join(protocols)
    print(
        f"Reference pickle not found or did not contain expected protocols at {path}.\n"
        f"Template shape: {{protocol: {{'pmf': [...], 'werner': [...]}}}} for {expected}.\n"
        "Optional keys: cdf, e_lambda/final_lambda. Missing references are skipped."
    )


def compare_reference(
    series: ProtocolSeries,
    reference: ReferenceSeries,
    *,
    atol: float,
    werner_pmf_threshold: float,
) -> dict[str, float | str]:
    row: dict[str, float | str] = {
        "protocol": series.protocol,
        "reference_source": str(reference.source),
    }
    if reference.pmf is not None:
        row["pmf_max_abs_diff"] = compare_arrays(
            series.pmf,
            reference.pmf,
            f"{series.protocol} PMF",
            atol,
        )
    if reference.cdf is not None:
        row["cdf_max_abs_diff"] = compare_arrays(
            series.cdf,
            reference.cdf,
            f"{series.protocol} CDF",
            atol,
        )
    if reference.werner is not None:
        row["werner_max_abs_diff"] = compare_conditional_werner(
            series.werner,
            reference.werner,
            series.pmf,
            reference.pmf,
            f"{series.protocol} Werner",
            atol,
            werner_pmf_threshold,
        )
        row["werner_pmf_threshold"] = werner_pmf_threshold
    if reference.final_lambda is not None:
        diff = series.pure_mass - reference.final_lambda
        if abs(diff) > atol:
            raise SystemExit(
                f"{series.protocol} final Lambda differs from reference by {diff:.12e}; "
                f"tolerance is {atol:.12e}."
            )
        row["final_lambda_diff"] = diff
    return row


def compare_arrays(model: np.ndarray, reference: np.ndarray, label: str, atol: float) -> float:
    common = min(len(model), len(reference))
    if common == 0:
        raise SystemExit(f"{label}: cannot compare empty arrays.")

    diff = float(np.max(np.abs(model[:common] - reference[:common])))
    if diff > atol:
        raise SystemExit(
            f"{label} max abs diff {diff:.12e} exceeds tolerance {atol:.12e}."
        )
    if len(model) != len(reference):
        print(
            f"{label}: compared first {common} points only "
            f"(model={len(model)}, reference={len(reference)})."
        )
    return diff


def compare_conditional_werner(
    model: np.ndarray,
    reference: np.ndarray,
    model_pmf: np.ndarray,
    reference_pmf: np.ndarray | None,
    label: str,
    atol: float,
    pmf_threshold: float,
) -> float:
    common = min(len(model), len(reference), len(model_pmf))
    if reference_pmf is not None:
        common = min(common, len(reference_pmf))
    if common == 0:
        raise SystemExit(f"{label}: cannot compare empty arrays.")

    supported = model_pmf[:common] >= pmf_threshold
    if reference_pmf is not None:
        supported &= reference_pmf[:common] >= pmf_threshold
    if not np.any(supported):
        raise SystemExit(
            f"{label}: no points meet Werner PMF support threshold {pmf_threshold:.12e}."
        )

    diff = float(
        np.max(np.abs(model[:common][supported] - reference[:common][supported]))
    )
    if diff > atol:
        raise SystemExit(
            f"{label} max abs diff {diff:.12e} exceeds tolerance {atol:.12e} "
            f"on PMF support >= {pmf_threshold:.12e}."
        )
    if len(model) != len(reference):
        print(
            f"{label}: compared supported points in the first {common} entries only "
            f"(model={len(model)}, reference={len(reference)})."
        )
    return diff


def validate_swap_asap(
    series: ProtocolSeries,
    *,
    p_gen: float,
    t_coh: int,
    atol: float,
    tail_tolerance: float,
    strict: bool,
) -> dict[str, float | str]:
    oracle = goodenough_lambda_4(p_gen, t_coh)
    diff = series.pure_mass - oracle
    tail_exhausted = series.tail <= tail_tolerance
    if strict and not tail_exhausted:
        raise SystemExit(
            f"swap-asap static tail {series.tail:.12e} exceeds tolerance "
            f"{tail_tolerance:.12e}; increase --truncation."
        )
    if (strict or tail_exhausted) and abs(diff) > atol:
        raise SystemExit(
            f"swap-asap E[Lambda_4] mismatch: model={series.pure_mass:.12e}, "
            f"Goodenough={oracle:.12e}, diff={diff:.12e}, tolerance={atol:.12e}."
        )
    return {
        "protocol": series.protocol,
        "reference_source": "Goodenough Appendix D.1 recursion, n=4",
        "goodenough_status": "matched" if tail_exhausted else "truncated-tail",
        "model_pure_mass": series.pure_mass,
        "reference_lambda": oracle,
        "final_lambda_diff": diff,
    }


def write_summary_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    keys: list[str] = []
    for row in rows:
        for key in row:
            if key not in keys:
                keys.append(key)

    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=keys)
        writer.writeheader()
        writer.writerows(rows)


def merge_timing_rows(
    path: Path,
    new_rows: list[dict[str, Any]],
) -> list[dict[str, Any]]:
    """Preserve unaffected timings when a plots-only run refreshes MC data."""
    if not path.is_file():
        return new_rows
    with open(path, encoding="utf-8", newline="") as handle:
        existing_rows = list(csv.DictReader(handle))
    replaced = {
        (str(row.get("method")), str(row.get("protocol")), str(row.get("event")))
        for row in new_rows
    }
    retained = [
        row
        for row in existing_rows
        if (
            str(row.get("method")),
            str(row.get("protocol")),
            str(row.get("event")),
        )
        not in replaced
    ]
    return [*retained, *new_rows]


def make_shared_bin_edges(
    series_by_protocol: dict[str, ProtocolSeries],
    references: dict[str, ReferenceSeries],
    bin_width: int,
) -> np.ndarray:
    """Return identical half-open time bins for every plotted dataset."""
    horizons: list[int] = []
    for protocol in plotted_protocols(series_by_protocol):
        series = series_by_protocol[protocol]
        horizons.append(min(len(series.pmf), len(series.werner)))
        reference = references.get(protocol)
        if reference is not None:
            if reference.pmf is not None:
                horizons.append(len(reference.pmf))
            if reference.werner is not None:
                horizons.append(len(reference.werner))
    if not horizons or min(horizons) < 1:
        raise SystemExit("Cannot construct shared bins from empty validation series.")

    horizon = min(horizons)
    edges = np.arange(0, horizon + 1, bin_width, dtype=np.int64)
    if len(edges) == 0 or edges[0] != 0:
        edges = np.insert(edges, 0, 0)
    if edges[-1] != horizon:
        edges = np.append(edges, horizon)
    return edges


def bin_distribution(
    pmf: np.ndarray,
    werner: np.ndarray,
    edges: np.ndarray,
) -> BinnedSeries:
    """Average PMF density and condition Werner values in shared time bins."""
    if len(edges) < 2 or edges[0] < 0 or np.any(np.diff(edges) <= 0):
        raise ValueError("bin edges must be strictly increasing and non-negative")
    if edges[-1] > len(pmf) or edges[-1] > len(werner):
        raise ValueError("bin edges extend past the supplied distribution")

    widths = np.diff(edges).astype(float)
    pmf_density = np.empty(len(widths), dtype=float)
    binned_werner = np.full(len(widths), np.nan, dtype=float)
    for index, (start, end) in enumerate(zip(edges[:-1], edges[1:])):
        bin_pmf = pmf[start:end]
        mass = float(np.sum(bin_pmf))
        pmf_density[index] = mass / widths[index]
        finite = np.isfinite(werner[start:end])
        finite_mass = float(np.sum(bin_pmf[finite]))
        if finite_mass > 0.0:
            binned_werner[index] = float(
                np.sum(bin_pmf[finite] * werner[start:end][finite]) / finite_mass
            )

    time_points = (edges[:-1] + edges[1:] - 1) / 2.0
    return BinnedSeries(
        time=time_points,
        pmf=pmf_density,
        werner=binned_werner,
        widths=widths,
    )


def bin_monte_carlo_samples(
    protocol: str,
    times: np.ndarray,
    werner: np.ndarray,
    edges: np.ndarray,
    shots: int,
    runtime_seconds: float,
) -> MonteCarloSeries:
    """Bin samples using the same edges and PMF-density convention as exact data."""
    if len(times) != shots or len(werner) != shots:
        raise ValueError("Monte Carlo sample arrays must contain exactly `shots` entries")

    valid = (times >= edges[0]) & (times < edges[-1])
    positions = np.searchsorted(edges, times[valid], side="right") - 1
    bin_count = np.bincount(positions, minlength=len(edges) - 1)
    werner_sum = np.bincount(
        positions,
        weights=werner[valid],
        minlength=len(edges) - 1,
    )
    widths = np.diff(edges).astype(float)
    pmf_density = bin_count.astype(float) / (shots * widths)
    binned_werner = np.divide(
        werner_sum,
        bin_count,
        out=np.full(len(bin_count), np.nan, dtype=float),
        where=bin_count > 0,
    )
    return MonteCarloSeries(
        protocol=protocol,
        time=(edges[:-1] + edges[1:] - 1) / 2.0,
        pmf=pmf_density,
        werner=binned_werner,
        widths=widths,
        sample_count=bin_count,
        shots=shots,
        runtime_seconds=runtime_seconds,
        coverage=float(np.sum(bin_count) / shots),
    )


def write_monte_carlo_csv(path: Path, series: MonteCarloSeries) -> None:
    edges_left = series.time - (series.widths - 1.0) / 2.0
    edges_right = edges_left + series.widths
    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(
            [
                "bin_start",
                "bin_end_exclusive",
                "bin_center",
                "pmf_per_timestep",
                "mean_werner",
                "sample_count",
            ]
        )
        writer.writerows(
            zip(
                edges_left.astype(int),
                edges_right.astype(int),
                series.time,
                series.pmf,
                series.werner,
                series.sample_count,
            )
        )


def run_monte_carlo_validation(
    args: Any,
    series_by_protocol: dict[str, ProtocolSeries],
    edges: np.ndarray,
    output_dir: Path,
) -> tuple[dict[str, MonteCarloSeries], list[dict[str, Any]]]:
    from scripts.validation.swap_scheme_monte_carlo import simulate, warm_up

    protocols = plotted_protocols(series_by_protocol)
    if not protocols:
        print("--include-mc: no doubling or sequential protocol is selected.")
        return {}, []

    print("[progress] Monte Carlo: compiling sampling kernels", flush=True)
    warm_up()
    print("[progress] Monte Carlo: sampling kernels ready", flush=True)

    mc_by_protocol: dict[str, MonteCarloSeries] = {}
    timing_rows: list[dict[str, Any]] = []
    summary: dict[str, Any] = {
        "parameters": {
            "shots_per_protocol": args.mc_shots,
            "links": MC_LINKS,
            "p_gen": args.p_gen,
            "p_swap": args.p_swap,
            "w0": args.w0,
            "t_coh_per_memory": args.reference_t_coh,
            "decay_factor": MC_DECAY_FACTOR,
            "seed": args.mc_seed,
            "bin_width": args.bin_width,
            "horizon_exclusive": int(edges[-1]),
        },
        "timing_note": "Numba compilation is warmed up and excluded.",
        "protocols": {},
    }

    output_dir.mkdir(parents=True, exist_ok=True)
    for protocol in protocols:
        scheme = "doubling" if protocol == "doubling" else "sequential"
        seed = args.mc_seed if scheme == "doubling" else args.mc_seed + 1
        started = time.perf_counter()
        times, werner = simulate(
            scheme,
            n_links=MC_LINKS,
            sample_size=args.mc_shots,
            p_gen=args.p_gen,
            p_swap=args.p_swap,
            w0=args.w0,
            t_coh=float(args.reference_t_coh),
            seed=seed,
            decay_factor=MC_DECAY_FACTOR,
        )
        elapsed = time.perf_counter() - started
        mc_series = bin_monte_carlo_samples(
            protocol,
            times,
            werner,
            edges,
            args.mc_shots,
            elapsed,
        )
        mc_by_protocol[protocol] = mc_series

        csv_path = (
            output_dir
            / f"{args.file_prefix}_{protocol_tag(protocol)}_mc_binned.csv"
        )
        write_monte_carlo_csv(csv_path, mc_series)
        protocol_name = "doubling" if protocol == "doubling" else "sequential"
        timing_rows.append(
            {
                "method": "Monte Carlo",
                "protocol": protocol_name,
                "event": "simulation",
                "seconds": f"{elapsed:.6f}",
                "details": (
                    f"{args.mc_shots} shots; Numba warm-up excluded; {csv_path}"
                ),
            }
        )
        summary["protocols"][protocol_name] = {
            "seed": seed,
            "runtime_seconds": elapsed,
            "coverage_at_horizon": mc_series.coverage,
            "mean_completion_time": float(np.mean(times)),
            "mean_werner": float(np.mean(werner)),
            "binned_csv": str(csv_path),
        }
        print(
            f"[progress] MC {protocol_name}: {args.mc_shots} shots finished "
            f"in {elapsed:.2f}s; coverage={mc_series.coverage:.9f}",
            flush=True,
        )

    summary_path = output_dir / "monte_carlo_summary.json"
    with open(summary_path, "w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)
        handle.write("\n")
    print(f"Monte Carlo summary: {summary_path}")
    return mc_by_protocol, timing_rows


def compare_monte_carlo(
    series: ProtocolSeries,
    mc_series: MonteCarloSeries,
    edges: np.ndarray,
) -> dict[str, float | int]:
    model = bin_distribution(series.pmf, series.werner, edges)
    model_mass = model.pmf * model.widths
    mc_mass = mc_series.pmf * mc_series.widths
    cdf_diff = np.cumsum(model_mass) - np.cumsum(mc_mass)

    supported = (
        (mc_series.sample_count > 0)
        & np.isfinite(model.werner)
        & np.isfinite(mc_series.werner)
    )
    if np.any(supported):
        werner_abs_diff = np.abs(
            model.werner[supported] - mc_series.werner[supported]
        )
        werner_weighted_mae = float(
            np.average(
                werner_abs_diff,
                weights=mc_series.sample_count[supported],
            )
        )
        werner_max_abs_diff = float(np.max(werner_abs_diff))
    else:
        werner_weighted_mae = math.nan
        werner_max_abs_diff = math.nan

    return {
        "mc_shots": mc_series.shots,
        "mc_runtime_seconds": mc_series.runtime_seconds,
        "mc_coverage": mc_series.coverage,
        "mc_cdf_max_abs_diff": float(np.max(np.abs(cdf_diff))),
        "mc_pmf_l1_diff": float(np.sum(np.abs(model_mass - mc_mass))),
        "mc_werner_weighted_mae": werner_weighted_mae,
        "mc_werner_max_abs_diff": werner_max_abs_diff,
        "mc_dkw_95_half_width": math.sqrt(
            math.log(2.0 / 0.05) / (2.0 * mc_series.shots)
        ),
    }


def plot_validation(
    plt: Any,
    figure_dir: Path,
    file_prefix: str,
    plot_profile_name: str,
    series_by_protocol: dict[str, ProtocolSeries],
    references: dict[str, ReferenceSeries],
    monte_carlo: dict[str, MonteCarloSeries],
    bin_edges: np.ndarray | None,
    skip_werner_legend: bool = False,
) -> dict[str, Path]:
    plot_profile = get_plot_profile(plot_profile_name)
    fig, (pmf_ax, werner_ax) = plt.subplots(
        1,
        2,
        sharex=True,
        figsize=(
            SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES,
            SWAP_COMPARISON_COMBINED_HEIGHT_INCHES,
        ),
        gridspec_kw={"width_ratios": (1.0, 1.0), "wspace": JOINT_PLOTS_WSPACE},
    )
    plot_validation_curves(
        pmf_ax,
        "pmf",
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
    )
    plot_validation_curves(
        werner_ax,
        "werner",
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
    )

    pmf_ax.set_ylabel("Probability")
    werner_ax.set_ylabel("Werner parameter")
    pmf_ax.set_xlabel(TIME_AXIS_LABEL)
    werner_ax.set_xlabel(TIME_AXIS_LABEL)
    pmf_ax.tick_params(axis="y", right=False, labelright=False)
    werner_ax.yaxis.set_label_position("right")
    werner_ax.yaxis.tick_right()
    werner_ax.tick_params(
        axis="y",
        left=False,
        labelleft=False,
        right=True,
        labelright=True,
    )
    pmf_ax.margins(x=0)
    werner_ax.margins(x=0)
    pmf_ax.set_ylim(bottom=0.0)
    # werner_ax.set_ylim(bottom=0.0)
    if bin_edges is None:
        pmf_ax.set_xlim(left=0)
    else:
        pmf_ax.set_xlim(0, int(bin_edges[-1]))
    style_axes(pmf_ax)
    style_axes(werner_ax)
    use_informative_y_ticks(pmf_ax)
    use_informative_y_ticks(werner_ax)

    handles, labels = pmf_ax.get_legend_handles_labels()
    if handles:
        fig.legend(
            handles,
            labels,
            frameon=False,
            loc="upper center",
            bbox_to_anchor=(0.5, SWAP_COMPARISON_JOINT_LEGEND_Y),
            ncol=len(handles),
            columnspacing=0.12 if monte_carlo else 0.8,
            handletextpad=0.14 if monte_carlo else 0.4,
            handlelength=0.8 if monte_carlo else 1.5,
            fontsize=8.5 if monte_carlo else 8,
        )

    fig.subplots_adjust(
        left=0.08,
        right=0.88,
        bottom=0.21,
        top=SWAP_COMPARISON_JOINT_TOP,
        wspace=JOINT_PLOTS_WSPACE,
    )
    hide_overlapping_inner_x_tick_label(fig, pmf_ax, werner_ax)

    figure_dir.mkdir(parents=True, exist_ok=True)
    combined_path = output_path(figure_dir, file_prefix, "validation", plot_profile)
    save_figure(fig, combined_path, tight_layout=False, bbox_inches=None)
    plt.close(fig)

    pmf_path = plot_pmf_validation(
        plt,
        figure_dir,
        file_prefix,
        plot_profile,
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
    )
    werner_path = plot_werner_validation(
        plt,
        figure_dir,
        file_prefix,
        plot_profile,
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
        skip_legend=skip_werner_legend,
    )
    return {
        "combined": combined_path,
        "pmf": pmf_path,
        "werner": werner_path,
    }


def plotted_protocols(series_by_protocol: dict[str, ProtocolSeries]) -> tuple[str, ...]:
    protocols = [
        protocol
        for protocol in ("doubling",)
        if protocol in series_by_protocol
    ]
    sequential = next(
        (
            protocol
            for protocol in ("left-to-right", "right-to-left")
            if protocol in series_by_protocol
        ),
        None,
    )
    if sequential is not None:
        protocols.append(sequential)
    return tuple(protocols)


def plot_validation_curves(
    ax: Any,
    attribute: str,
    series_by_protocol: dict[str, ProtocolSeries],
    references: dict[str, ReferenceSeries],
    monte_carlo: dict[str, MonteCarloSeries],
    bin_edges: np.ndarray | None,
) -> None:
    protocols = plotted_protocols(series_by_protocol)
    for protocol in protocols:
        color = PROTOCOL_COLORS[protocol]
        series = series_by_protocol[protocol]
        if bin_edges is None:
            values = getattr(series, attribute)
            start = WERNER_START_TIME if attribute == "werner" else 0
            time_points = np.arange(start, len(values))
            values = values[start:]
        else:
            binned = bin_distribution(series.pmf, series.werner, bin_edges)
            time_points = binned.time
            values = getattr(binned, attribute)
        ax.plot(
            time_points,
            values,
            color=color,
            linestyle=QBKAT_LINESTYLE,
            label=validation_label(protocol, "qbkat"),
            zorder=3,
        )

    # Draw references after QBKAT so their dashed curves remain visible over
    # QBKAT's nearly identical solid curves.
    for protocol in protocols:
        reference = references.get(protocol)
        if reference is None:
            continue
        color = REFERENCE_COLORS[protocol]
        values = getattr(reference, attribute)
        if values is None:
            continue
        if bin_edges is None:
            start = WERNER_START_TIME if attribute == "werner" else 0
            time_points = np.arange(start, len(values))
            values = values[start:]
        else:
            if reference.pmf is None:
                continue
            reference_werner = (
                reference.werner
                if reference.werner is not None
                else np.zeros_like(reference.pmf)
            )
            binned = bin_distribution(
                reference.pmf,
                reference_werner,
                bin_edges,
            )
            time_points = binned.time
            values = getattr(binned, attribute)
        ax.plot(
            time_points,
            values,
            color=color,
            linestyle=REFERENCE_LINESTYLE,
            linewidth=REFERENCE_LINEWIDTH,
            label=validation_label(protocol, "reference"),
            zorder=4,
        )

    # As in Li et al. Figure 4, show Monte Carlo estimates as marker-only
    # samples. Keep them above the deterministic curves so they remain visible
    # where the estimates agree, while low opacity leaves those curves legible.
    for protocol in protocols:
        mc_series = monte_carlo.get(protocol)
        if mc_series is None:
            continue
        values = getattr(mc_series, attribute)
        supported = np.isfinite(values)
        ax.plot(
            mc_series.time[supported],
            values[supported],
            color=MC_COLORS[protocol],
            marker=MC_MARKERS[protocol],
            linestyle="none",
            markersize=MC_MARKERSIZE,
            markeredgewidth=0.0,
            alpha=MC_ALPHA,
            label=validation_label(protocol, "mc"),
            zorder=5,
        )


def plot_pmf_curves(
    ax: Any,
    series_by_protocol: dict[str, ProtocolSeries],
    references: dict[str, ReferenceSeries],
    monte_carlo: dict[str, MonteCarloSeries],
    bin_edges: np.ndarray | None,
) -> None:
    plot_validation_curves(
        ax,
        "pmf",
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
    )


def plot_werner_curves(
    ax: Any,
    series_by_protocol: dict[str, ProtocolSeries],
    references: dict[str, ReferenceSeries],
    monte_carlo: dict[str, MonteCarloSeries],
    bin_edges: np.ndarray | None,
) -> None:
    plot_validation_curves(
        ax,
        "werner",
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
    )


def plot_pmf_validation(
    plt: Any,
    figure_dir: Path,
    file_prefix: str,
    plot_profile: Any,
    series_by_protocol: dict[str, ProtocolSeries],
    references: dict[str, ReferenceSeries],
    monte_carlo: dict[str, MonteCarloSeries],
    bin_edges: np.ndarray | None,
) -> Path:
    fig, ax = plt.subplots(
        figsize=(VALIDATION_LINE_WIDTH_INCHES, VALIDATION_HEIGHT_INCHES)
    )
    plot_pmf_curves(
        ax,
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
    )
    ax.set_xlabel(TIME_AXIS_LABEL)
    ax.set_ylabel("Probability")
    ax.set_ylim(bottom=0.0)
    if bin_edges is not None:
        ax.set_xlim(0, int(bin_edges[-1]))
    ax.legend(loc="best")
    style_axes(ax)
    use_informative_y_ticks(ax)
    path = output_path(figure_dir, file_prefix, "validation_pmf", plot_profile)
    save_figure(fig, path, tight_layout=True, bbox_inches=None)
    plt.close(fig)
    return path


def plot_werner_validation(
    plt: Any,
    figure_dir: Path,
    file_prefix: str,
    plot_profile: Any,
    series_by_protocol: dict[str, ProtocolSeries],
    references: dict[str, ReferenceSeries],
    monte_carlo: dict[str, MonteCarloSeries],
    bin_edges: np.ndarray | None,
    *,
    skip_legend: bool = False,
) -> Path:
    fig, ax = plt.subplots(
        figsize=(VALIDATION_LINE_WIDTH_INCHES, VALIDATION_HEIGHT_INCHES)
    )
    plot_werner_curves(
        ax,
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
    )
    ax.set_xlabel(TIME_AXIS_LABEL)
    ax.set_ylabel("Werner parameter")
    # ax.set_ylim(bottom=0.0)
    if bin_edges is not None:
        ax.set_xlim(0, int(bin_edges[-1]))
    if not skip_legend:
        ax.legend(loc="best")
    style_axes(ax)
    use_informative_y_ticks(ax)
    path = output_path(figure_dir, file_prefix, "validation_werner", plot_profile)
    save_figure(fig, path, tight_layout=True, bbox_inches=None)
    plt.close(fig)
    return path


def run_validation(args: Any) -> None:
    protocols = tuple(args.protocol or DEFAULT_PROTOCOLS)
    output_dir = Path(args.output_dir)
    figure_dir = Path(args.figure_dir)
    timing_rows: list[dict[str, Any]] = []

    if not args.plots_only:
        timing_rows = run_protocol_outputs(args, protocols, output_dir)
    elif not output_dir.is_dir():
        raise SystemExit(f"--plots-only requires an existing output directory: {output_dir}")

    series_by_protocol = {
        protocol: load_protocol_series(output_dir, args.file_prefix, protocol)
        for protocol in protocols
    }
    swap_asap_oracle = goodenough_lambda_4(args.p_gen, args.t_coh)

    li_references: dict[str, ReferenceSeries] = {}
    if "doubling" in protocols:
        li_references = load_reference_pickle(
            Path(args.li_reference),
            ("doubling",),
            quiet_missing=True,
        ) or load_reference_pair_pickles(
            Path(args.li_pmf_reference),
            Path(args.li_werner_reference),
            ("doubling",),
        )

    sequential_protocols = tuple(
        protocol
        for protocol in ("left-to-right", "right-to-left")
        if protocol in protocols
    )
    lacorte_references: dict[str, ReferenceSeries] = {}
    if sequential_protocols:
        lacorte_references = load_reference_pickle(
            Path(args.lacorte_reference),
            sequential_protocols,
            quiet_missing=True,
        )
    if "left-to-right" in protocols:
        lacorte_references.update(
            load_reference_pair_pickles(
                Path(args.lacorte_left_pmf_reference),
                Path(args.lacorte_left_werner_reference),
                ("left-to-right",),
            )
        )
    if "right-to-left" in protocols:
        lacorte_references.update(
            load_reference_pair_pickles(
                Path(args.lacorte_right_pmf_reference),
                Path(args.lacorte_right_werner_reference),
                ("right-to-left",),
            )
        )
    references = {**li_references, **lacorte_references}

    bin_edges: np.ndarray | None = None
    monte_carlo: dict[str, MonteCarloSeries] = {}
    if args.include_mc:
        bin_edges = make_shared_bin_edges(
            series_by_protocol,
            references,
            args.bin_width,
        )
        monte_carlo, mc_timing_rows = run_monte_carlo_validation(
            args,
            series_by_protocol,
            bin_edges,
            output_dir,
        )
        timing_rows.extend(mc_timing_rows)

    rows: list[dict[str, Any]] = []
    for protocol, series in series_by_protocol.items():
        base_row: dict[str, Any] = {
            "protocol": protocol,
            "pure_mass": f"{series.pure_mass:.15g}",
            "total_mass": f"{series.total_mass:.15g}",
            "tail": f"{series.tail:.15g}",
            "extrema_gap": f"{series.extrema_gap:.15g}",
            "pure_json": str(series.pure_path),
            "mixed_json": str(series.mixed_path),
        }
        if protocol == "swap-asap":
            validation_row = validate_swap_asap(
                series,
                p_gen=args.p_gen,
                t_coh=args.t_coh,
                atol=args.goodenough_atol,
                tail_tolerance=args.tail_tolerance,
                strict=args.strict_goodenough,
            )
            base_row.update(validation_row)
        if protocol in references:
            base_row.update(
                compare_reference(
                    series,
                    references[protocol],
                    atol=args.reference_atol,
                    werner_pmf_threshold=args.werner_pmf_threshold,
                )
            )
        if protocol in monte_carlo and bin_edges is not None:
            base_row.update(
                compare_monte_carlo(
                    series,
                    monte_carlo[protocol],
                    bin_edges,
                )
            )
        rows.append(base_row)

    output_dir.mkdir(parents=True, exist_ok=True)
    summary_path = output_dir / "validation_summary.csv"
    write_summary_csv(summary_path, rows)
    timings_path: Path | None = None
    if timing_rows:
        timings_path = output_dir / "validation_timings.csv"
        if args.plots_only:
            timing_rows = merge_timing_rows(timings_path, timing_rows)
        write_summary_csv(timings_path, timing_rows)

    plt = configure_matplotlib(args.plot_profile)
    figure_paths = plot_validation(
        plt,
        figure_dir,
        args.file_prefix,
        args.plot_profile,
        series_by_protocol,
        references,
        monte_carlo,
        bin_edges,
        skip_werner_legend=args.skip_werner_legend,
    )

    print("\n5-node swap-scheme validation")
    print(
        f"p_gen={args.p_gen:.12g}, p_swap={args.p_swap:.12g}, w0={args.w0:.12g}, "
        f"t_coh={args.t_coh}, reference_t_coh={args.reference_t_coh}, "
        f"truncation={args.truncation}"
    )
    if bin_edges is not None:
        print(
            f"Monte Carlo: {args.mc_shots} shots/protocol, "
            f"shared bin width={args.bin_width}, "
            f"horizon=[0,{int(bin_edges[-1])})"
        )
    print(f"Goodenough E[Lambda_4] = {swap_asap_oracle:.15g}")
    for row in rows:
        print(
            f"{row['protocol']:<15} pure_mass={row['pure_mass']} "
            f"tail={row['tail']}"
        )
    print(f"Summary CSV: {summary_path}")
    if timings_path is not None:
        print(f"Timing CSV: {timings_path}")
    print(f"Combined validation figure: {figure_paths['combined']}")
    print(f"PMF validation figure: {figure_paths['pmf']}")
    print(f"Werner validation figure: {figure_paths['werner']}")
