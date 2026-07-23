#!/usr/bin/env python3

from __future__ import annotations

import argparse
import csv
import math
from dataclasses import dataclass, replace
from pathlib import Path

import numpy as np

from scripts.analysis.swap_comparison.common import (
    DEFAULT_TRUNCATION,
    MDP_MODE,
    QMDP_MODE,
    MIXED_EVENT,
    PURE_EVENT,
    STATIC_EVENT,
    compute_secret_key_rate_from_split,
    load_coverage_budget,
    run_command,
    validate_extremal_json,
)
from scripts.plot.config import (
    DEFAULT_PROFILE,
    PLOT_SETTINGS,
    configure_matplotlib,
    get_plot_profile,
    output_path,
    save_figure,
    style_axes,
)


TARGET_PROTOCOL = "at-last"
BASELINE_PROTOCOLS = ("left-to-right", "right-to-left", "doubling")
PROTOCOLS = (TARGET_PROTOCOL, *BASELINE_PROTOCOLS)
AXES = ("p-gen", "multiplexing", "edge-skew", "t-coh", "p-swap", "w0")
AXIS_ALIASES = {
    "p_ge": "p-gen",
    "p-ge": "p-gen",
    "p_gen": "p-gen",
    "pgen": "p-gen",
    "skew": "edge-skew",
    "edge_skew": "edge-skew",
    "t_coh": "t-coh",
    "tcoh": "t-coh",
    "p_swap": "p-swap",
    "pswap": "p-swap",
    "w_0": "w0",
    "w-0": "w0",
}
DEFAULT_CONTOURS = (("p-gen", "multiplexing"),)
LOG_AXES = {"p-gen", "edge-skew", "t-coh"}
AXIS_LABELS = {
    "p-gen": r"Elementary generation probability $p_{\mathrm{gen}}$",
    "multiplexing": "Parallel attempts per elementary link",
    "edge-skew": r"Slow-link penalty $\eta$",
    "t-coh": r"Coherence time $t_{\mathrm{coh}}$",
    "p-swap": r"Swap success probability $p_{\mathrm{swap}}$",
    "w0": r"Initial Werner parameter $w_0$",
}
PROTOCOL_MARKERS = {
    "left-to-right": ">",
    "right-to-left": "<",
    "doubling": "D",
    "at-last": "*",
}
PROTOCOL_MARKER_LABELS = {
    "left-to-right": "L2R",
    "right-to-left": "R2L",
    "doubling": "doubling",
    "at-last": "at-last",
}
WINNER_MARKER_SIZE = 6.0
WINNER_MARKER_HALO_SIZE = 8.4


@dataclass(frozen=True)
class SweepPoint:
    p_gen: float
    multiplexing: int
    edge_skew: float
    t_coh: int
    p_swap: float
    w0: float


@dataclass(frozen=True)
class ContourSpec:
    x_axis: str
    y_axis: str


@dataclass(frozen=True)
class PointResult:
    point: SweepPoint
    skr_by_protocol: dict[str, float]
    best_baseline_protocol: str
    best_baseline_skr: float
    target_skr: float
    ratio: float
    advantage: float
    winner: str


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Sweep the configurable 5-node swap comparison and plot where "
            "at-last/simultaneous swapping beats sequential and doubling policies."
        )
    )
    budget_group = parser.add_mutually_exclusive_group()
    budget_group.add_argument(
        "--truncation",
        type=int,
        default=None,
        help=(
            "Extremal reachability budget for QMDP pure/mixed runs. "
            f"Defaults to {DEFAULT_TRUNCATION} when --coverage is not used."
        ),
    )
    budget_group.add_argument(
        "--coverage",
        type=float,
        default=None,
        help=(
            "Resolve a time-only MDP budget per sweep point/protocol until "
            "worst-scheduler CDF reaches this probability, then reuse the "
            "maximum resolved budget for QMDP pure/mixed runs."
        ),
    )
    parser.add_argument(
        "--contour",
        action="append",
        default=None,
        help=(
            "Contour axes as X,Y. Choices are p-gen, multiplexing, edge-skew, "
            "t-coh, p-swap, and w0. Aliases such as p_ge, p_swap, and w_0 "
            "are accepted. Can be passed multiple times."
        ),
    )
    parser.add_argument(
        "--p-gen-values",
        default="0.01,0.02,0.05",
        help="Comma-separated values for p-gen axes.",
    )
    parser.add_argument(
        "--multiplexing-values",
        default="1,2",
        help="Comma-separated integer values for multiplexing axes.",
    )
    parser.add_argument(
        "--edge-skew-values",
        default="1,2,5",
        help=(
            "Comma-separated slow-link penalties. The D-E generation "
            "probability is p-gen / edge-skew; 1 is homogeneous."
        ),
    )
    parser.add_argument(
        "--t-coh-values",
        default="1000,5000,20000",
        help="Comma-separated integer values for t-coh axes.",
    )
    parser.add_argument(
        "--p-swap-values",
        default="0.5,0.75,1.0",
        help="Comma-separated values for p-swap axes.",
    )
    parser.add_argument(
        "--w0-values",
        default="0.952,0.985,1.0",
        help="Comma-separated values for w0 axes.",
    )
    parser.add_argument("--fixed-p-gen", type=float, default=0.02)
    parser.add_argument("--fixed-multiplexing", type=int, default=1)
    parser.add_argument("--fixed-edge-skew", type=float, default=1.0)
    parser.add_argument("--fixed-t-coh", type=int, default=5000)
    parser.add_argument("--fixed-p-swap", "--p-swap", dest="fixed_p_swap", type=float, default=0.9)
    parser.add_argument("--fixed-w0", "--w0", dest="fixed_w0", type=float, default=0.95)
    parser.add_argument(
        "--output-dir",
        default="output/pswap-optimality",
        help="Directory for JSON dumps and CSV summaries.",
    )
    parser.add_argument(
        "--figure-dir",
        default="output/pswap-optimality",
        help="Directory for contour figures.",
    )
    parser.add_argument(
        "--file-prefix",
        default="pswap5_optimality",
        help="Prefix for generated JSON/CSV artifacts.",
    )
    parser.add_argument(
        "--figure-prefix",
        default="pswap5_optimality",
        help="Prefix for generated contour figures.",
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=DEFAULT_PROFILE,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--executable",
        default="quantP_compare_swap_5_multiplex",
        help="Cabal executable name.",
    )
    parser.add_argument(
        "--plots-only",
        "--plots_only",
        action="store_true",
        dest="plots_only",
        help="Skip Cabal runs and build contours from existing JSON dumps.",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Reuse existing JSON dumps for completed pure/mixed runs.",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip the initial cabal build step.",
    )
    parser.add_argument(
        "--mark-protocol-winners",
        action="store_true",
        help=(
            "Overlay high-contrast markers on contour points for the protocol "
            "with maximal SKR: triangle arrows for sequential, diamond for "
            "doubling, star for at-last."
        ),
    )
    return parser.parse_args()


def log(message: str = "") -> None:
    print(message, flush=True)


def parse_float_values(raw: str, flag: str) -> tuple[float, ...]:
    values = tuple(float(part.strip()) for part in raw.split(",") if part.strip())
    if not values:
        raise SystemExit(f"{flag} must contain at least one value.")
    return values


def parse_int_values(raw: str, flag: str) -> tuple[int, ...]:
    values = tuple(int(part.strip()) for part in raw.split(",") if part.strip())
    if not values:
        raise SystemExit(f"{flag} must contain at least one value.")
    return values


def validate_probability(name: str, value: float, *, allow_zero: bool = False) -> None:
    lower_ok = value >= 0 if allow_zero else value > 0
    if not lower_ok or value > 1:
        interval = "[0, 1]" if allow_zero else "(0, 1]"
        raise SystemExit(f"{name} must be in the interval {interval}.")


def validate_args(args) -> None:
    if not args.plots_only and args.coverage is None and args.truncation is None:
        args.truncation = DEFAULT_TRUNCATION
    if args.coverage is not None:
        validate_probability("--coverage", args.coverage)
    if args.truncation is not None and args.truncation < 0:
        raise SystemExit("--truncation must be non-negative.")
    validate_probability("--fixed-p-gen", args.fixed_p_gen)
    validate_probability("--fixed-p-swap", args.fixed_p_swap, allow_zero=True)
    validate_probability("--fixed-w0", args.fixed_w0, allow_zero=True)
    if args.fixed_multiplexing <= 0:
        raise SystemExit("--fixed-multiplexing must be positive.")
    if args.fixed_edge_skew < 1:
        raise SystemExit("--fixed-edge-skew must be at least 1.")
    if args.fixed_t_coh <= 0:
        raise SystemExit("--fixed-t-coh must be positive.")
    for value in axis_values("p-gen", args):
        validate_probability("--p-gen-values", value)
    for value in axis_values("edge-skew", args):
        if value < 1:
            raise SystemExit("--edge-skew-values entries must be at least 1.")
    for value in axis_values("multiplexing", args):
        if value <= 0:
            raise SystemExit("--multiplexing-values entries must be positive.")
    for value in axis_values("t-coh", args):
        if value <= 0:
            raise SystemExit("--t-coh-values entries must be positive.")
    for value in axis_values("p-swap", args):
        validate_probability("--p-swap-values", value, allow_zero=True)
    for value in axis_values("w0", args):
        validate_probability("--w0-values", value, allow_zero=True)


def normalize_axis(axis: str) -> str:
    normalized = axis.strip().lower().replace(" ", "-")
    return AXIS_ALIASES.get(normalized, normalized)


def parse_contours(raw_specs: list[str] | None) -> tuple[ContourSpec, ...]:
    if raw_specs is None:
        return tuple(ContourSpec(x_axis, y_axis) for x_axis, y_axis in DEFAULT_CONTOURS)

    specs = []
    for raw in raw_specs:
        parts = [normalize_axis(part) for part in raw.split(",")]
        if len(parts) != 2:
            raise SystemExit(f"Invalid --contour '{raw}'. Use X,Y.")
        x_axis, y_axis = parts
        if x_axis not in AXES or y_axis not in AXES:
            raise SystemExit(f"Invalid --contour '{raw}'. Available axes: {', '.join(AXES)}.")
        if x_axis == y_axis:
            raise SystemExit(f"Invalid --contour '{raw}'. Axes must be distinct.")
        specs.append(ContourSpec(x_axis, y_axis))
    return tuple(specs)


def axis_values(axis: str, args) -> tuple[float | int, ...]:
    if axis == "p-gen":
        return parse_float_values(args.p_gen_values, "--p-gen-values")
    if axis == "multiplexing":
        return parse_int_values(args.multiplexing_values, "--multiplexing-values")
    if axis == "edge-skew":
        return parse_float_values(args.edge_skew_values, "--edge-skew-values")
    if axis == "t-coh":
        return parse_int_values(args.t_coh_values, "--t-coh-values")
    if axis == "p-swap":
        return parse_float_values(args.p_swap_values, "--p-swap-values")
    if axis == "w0":
        return parse_float_values(args.w0_values, "--w0-values")
    raise AssertionError(f"Unhandled axis {axis}.")


def point_with_axis(point: SweepPoint, axis: str, value: float | int) -> SweepPoint:
    if axis == "p-gen":
        return replace(point, p_gen=float(value))
    if axis == "multiplexing":
        return replace(point, multiplexing=int(value))
    if axis == "edge-skew":
        return replace(point, edge_skew=float(value))
    if axis == "t-coh":
        return replace(point, t_coh=int(value))
    if axis == "p-swap":
        return replace(point, p_swap=float(value))
    if axis == "w0":
        return replace(point, w0=float(value))
    raise AssertionError(f"Unhandled axis {axis}.")


def fixed_point(args) -> SweepPoint:
    return SweepPoint(
        p_gen=args.fixed_p_gen,
        multiplexing=args.fixed_multiplexing,
        edge_skew=args.fixed_edge_skew,
        t_coh=args.fixed_t_coh,
        p_swap=args.fixed_p_swap,
        w0=args.fixed_w0,
    )


def has_fixed_quality(point: SweepPoint, p_swap: float, w0: float) -> bool:
    return math.isclose(point.p_swap, p_swap, rel_tol=0.0, abs_tol=1e-15) and math.isclose(
        point.w0,
        w0,
        rel_tol=0.0,
        abs_tol=1e-15,
    )


def value_tag(value: float | int) -> str:
    if isinstance(value, int):
        return str(value)
    return f"{value:.12g}".replace("-", "m").replace("+", "").replace(".", "p")


def scenario_tag(point: SweepPoint) -> str:
    return (
        f"p{value_tag(point.p_gen)}"
        f"_mux{point.multiplexing}"
        f"_skew{value_tag(point.edge_skew)}"
        f"_t{point.t_coh}"
        f"_pswap{value_tag(point.p_swap)}"
        f"_w{value_tag(point.w0)}"
    )


def legacy_scenario_tag(point: SweepPoint) -> str:
    return (
        f"p{value_tag(point.p_gen)}"
        f"_mux{point.multiplexing}"
        f"_skew{value_tag(point.edge_skew)}"
        f"_t{point.t_coh}"
    )


def output_json_path(output_dir: Path, file_prefix: str, point: SweepPoint, protocol: str, mode: str, event: str) -> Path:
    return output_dir / f"{file_prefix}_{scenario_tag(point)}_{protocol}_{mode}_{event}.json"


def legacy_output_json_path(
    output_dir: Path,
    file_prefix: str,
    point: SweepPoint,
    protocol: str,
    mode: str,
    event: str,
) -> Path:
    return output_dir / f"{file_prefix}_{legacy_scenario_tag(point)}_{protocol}_{mode}_{event}.json"


def existing_output_json_path(
    output_dir: Path,
    file_prefix: str,
    point: SweepPoint,
    protocol: str,
    mode: str,
    event: str,
    *,
    allow_legacy: bool = True,
) -> Path:
    path = output_json_path(output_dir, file_prefix, point, protocol, mode, event)
    require_coverage = mode == MDP_MODE and event == STATIC_EVENT
    valid, reason = validate_extremal_json(path, require_coverage=require_coverage)
    if valid:
        return path

    if not allow_legacy:
        raise SystemExit(f"Unusable existing JSON: {path} ({reason})")

    legacy_path = legacy_output_json_path(output_dir, file_prefix, point, protocol, mode, event)
    legacy_valid, legacy_reason = validate_extremal_json(
        legacy_path,
        require_coverage=require_coverage,
    )
    if legacy_valid:
        return legacy_path

    raise SystemExit(
        f"Unusable existing JSON: {path} ({reason}); "
        f"legacy candidate {legacy_path} ({legacy_reason})"
    )


def command_args_for_point(point: SweepPoint, args) -> list[str]:
    return [
        "--p-gen",
        f"{point.p_gen:.17g}",
        "--p-swap",
        f"{point.p_swap:.17g}",
        "--w0",
        f"{point.w0:.17g}",
        "--t-coh",
        str(point.t_coh),
        "--multiplexing",
        str(point.multiplexing),
        "--edge-skew",
        f"{point.edge_skew:.17g}",
    ]


def run_extremal_case(
    executable: str,
    protocol: str,
    mode: str,
    event: str,
    point: SweepPoint,
    args,
    budget_flag: str,
    budget_value: float | int,
    json_path: Path,
) -> float:
    command = [
        "cabal",
        "run",
        "-v0",
        executable,
        "--",
        "--protocol",
        protocol,
        "--event",
        event,
        *command_args_for_point(point, args),
        "--json",
        mode,
        "--compute-extremal",
        budget_flag,
        str(budget_value),
    ]
    status_label = f"{scenario_tag(point)} {protocol} {mode}/{event}"
    return run_command(command, stdout_path=json_path, status_label=status_label)


def resolve_budget(point: SweepPoint, output_dir: Path, args) -> int:
    if args.coverage is None:
        return args.truncation

    budgets = []
    for protocol in PROTOCOLS:
        target_path = output_json_path(
            output_dir,
            args.file_prefix,
            point,
            protocol,
            MDP_MODE,
            STATIC_EVENT,
        )
        if args.plots_only:
            json_path = existing_output_json_path(
                output_dir,
                args.file_prefix,
                point,
                protocol,
                MDP_MODE,
                STATIC_EVENT,
                allow_legacy=has_fixed_quality(point, args.fixed_p_swap, args.fixed_w0),
            )
        elif args.resume:
            try:
                json_path = existing_output_json_path(
                    output_dir,
                    args.file_prefix,
                    point,
                    protocol,
                    MDP_MODE,
                    STATIC_EVENT,
                    allow_legacy=has_fixed_quality(point, args.fixed_p_swap, args.fixed_w0),
                )
                log(f"{scenario_tag(point)} {protocol} coverage: reused {json_path}")
            except SystemExit as exc:
                json_path = target_path
                log(f"{exc}; rerunning {protocol} {MDP_MODE}/{STATIC_EVENT}")
                elapsed = run_extremal_case(
                    args.executable,
                    protocol,
                    MDP_MODE,
                    STATIC_EVENT,
                    point,
                    args,
                    "--coverage",
                    args.coverage,
                    json_path,
                )
                log(f"{scenario_tag(point)} {protocol} coverage: {elapsed:.2f}s -> {json_path}")
        else:
            json_path = target_path
            elapsed = run_extremal_case(
                args.executable,
                protocol,
                MDP_MODE,
                STATIC_EVENT,
                point,
                args,
                "--coverage",
                args.coverage,
                json_path,
            )
            log(f"{scenario_tag(point)} {protocol} coverage: {elapsed:.2f}s -> {json_path}")
        resolved_budget, coverage_value = load_coverage_budget(protocol, args.coverage, json_path)
        budgets.append(resolved_budget)
        log(
            f"{scenario_tag(point)} {protocol}: "
            f"coverage {coverage_value:.12g} at R={resolved_budget}"
        )
    return max(budgets)


def ensure_protocol_jsons(point: SweepPoint, protocol: str, output_dir: Path, args, budget: int) -> tuple[Path, Path]:
    paths = []
    for event in (PURE_EVENT, MIXED_EVENT):
        target_path = output_json_path(
            output_dir,
            args.file_prefix,
            point,
            protocol,
            QMDP_MODE,
            event,
        )
        reused = False
        if args.plots_only:
            json_path = existing_output_json_path(
                output_dir,
                args.file_prefix,
                point,
                protocol,
                QMDP_MODE,
                event,
                allow_legacy=has_fixed_quality(point, args.fixed_p_swap, args.fixed_w0),
            )
            paths.append(json_path)
            continue
        if args.resume:
            try:
                json_path = existing_output_json_path(
                    output_dir,
                    args.file_prefix,
                    point,
                    protocol,
                    QMDP_MODE,
                    event,
                    allow_legacy=has_fixed_quality(point, args.fixed_p_swap, args.fixed_w0),
                )
                reused = True
            except SystemExit as exc:
                json_path = target_path
                log(f"{exc}; rerunning {protocol} {QMDP_MODE}/{event}")
        else:
            json_path = target_path
        paths.append(json_path)
        if reused:
            log(f"{scenario_tag(point)} {protocol} {event}: reused {json_path}")
            continue
        elapsed = run_extremal_case(
            args.executable,
            protocol,
            QMDP_MODE,
            event,
            point,
            args,
            "--truncation",
            budget,
            json_path,
        )
        log(f"{scenario_tag(point)} {protocol} {event}: {elapsed:.2f}s -> {json_path}")
    return paths[0], paths[1]


def evaluate_point(point: SweepPoint, output_dir: Path, args) -> PointResult:
    budget = 0 if args.plots_only else resolve_budget(point, output_dir, args)
    skr_by_protocol = {}

    for protocol in PROTOCOLS:
        pure_path, mixed_path = ensure_protocol_jsons(point, protocol, output_dir, args, budget)
        skr_by_protocol[protocol] = compute_secret_key_rate_from_split(pure_path, mixed_path)
        log(f"{scenario_tag(point)} {protocol}: SKR={skr_by_protocol[protocol]:.12g}")

    best_baseline_protocol = max(BASELINE_PROTOCOLS, key=lambda name: skr_by_protocol[name])
    best_baseline_skr = skr_by_protocol[best_baseline_protocol]
    target_skr = skr_by_protocol[TARGET_PROTOCOL]
    ratio = target_skr / best_baseline_skr if best_baseline_skr > 0 else math.nan
    advantage = target_skr - best_baseline_skr
    winner = max(PROTOCOLS, key=lambda name: skr_by_protocol[name])
    return PointResult(
        point=point,
        skr_by_protocol=skr_by_protocol,
        best_baseline_protocol=best_baseline_protocol,
        best_baseline_skr=best_baseline_skr,
        target_skr=target_skr,
        ratio=ratio,
        advantage=advantage,
        winner=winner,
    )


def write_protocol_rows(path: Path, results: dict[SweepPoint, PointResult]) -> None:
    with open(path, "w", encoding="utf-8", newline="") as handle:
        fieldnames = (
            "scenario",
            "p_gen",
            "multiplexing",
            "edge_skew",
            "t_coh",
            "p_swap",
            "w0",
            "protocol",
            "skr",
        )
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for result in results.values():
            point = result.point
            for protocol, skr in result.skr_by_protocol.items():
                writer.writerow(
                    {
                        "scenario": scenario_tag(point),
                        "p_gen": f"{point.p_gen:.12g}",
                        "multiplexing": point.multiplexing,
                        "edge_skew": f"{point.edge_skew:.12g}",
                        "t_coh": point.t_coh,
                        "p_swap": f"{point.p_swap:.12g}",
                        "w0": f"{point.w0:.12g}",
                        "protocol": protocol,
                        "skr": f"{skr:.12g}",
                    }
                )


def write_point_rows(path: Path, results: dict[SweepPoint, PointResult]) -> None:
    with open(path, "w", encoding="utf-8", newline="") as handle:
        fieldnames = (
            "scenario",
            "p_gen",
            "multiplexing",
            "edge_skew",
            "t_coh",
            "p_swap",
            "w0",
            "target_skr",
            "best_baseline_protocol",
            "best_baseline_skr",
            "ratio",
            "advantage",
            "winner",
        )
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for result in results.values():
            point = result.point
            writer.writerow(
                {
                    "scenario": scenario_tag(point),
                    "p_gen": f"{point.p_gen:.12g}",
                    "multiplexing": point.multiplexing,
                    "edge_skew": f"{point.edge_skew:.12g}",
                    "t_coh": point.t_coh,
                    "p_swap": f"{point.p_swap:.12g}",
                    "w0": f"{point.w0:.12g}",
                    "target_skr": f"{result.target_skr:.12g}",
                    "best_baseline_protocol": result.best_baseline_protocol,
                    "best_baseline_skr": f"{result.best_baseline_skr:.12g}",
                    "ratio": f"{result.ratio:.12g}",
                    "advantage": f"{result.advantage:.12g}",
                    "winner": result.winner,
                }
            )


def contour_points(spec: ContourSpec, args) -> tuple[tuple[float | int, ...], tuple[float | int, ...], list[list[SweepPoint]]]:
    x_values = axis_values(spec.x_axis, args)
    y_values = axis_values(spec.y_axis, args)
    if len(x_values) < 2 or len(y_values) < 2:
        raise SystemExit(
            f"--contour {spec.x_axis},{spec.y_axis} requires at least two "
            f"values for each axis, but got {len(x_values)} x {len(y_values)}."
        )
    rows = []
    for y_value in y_values:
        row = []
        for x_value in x_values:
            point = fixed_point(args)
            point = point_with_axis(point, spec.x_axis, x_value)
            point = point_with_axis(point, spec.y_axis, y_value)
            row.append(point)
        rows.append(row)
    return x_values, y_values, rows


def plot_contour(
    plt,
    figure_dir: Path,
    args,
    spec: ContourSpec,
    x_values: tuple[float | int, ...],
    y_values: tuple[float | int, ...],
    grid: list[list[SweepPoint]],
    results: dict[SweepPoint, PointResult],
) -> Path:
    from matplotlib.colors import TwoSlopeNorm

    plot_profile = get_plot_profile(args.plot_profile)
    ratio = np.array([[results[point].ratio for point in row] for row in grid], dtype=float)
    advantage = np.array([[results[point].advantage for point in row] for row in grid], dtype=float)
    x = np.array(x_values, dtype=float)
    y = np.array(y_values, dtype=float)

    fig, ax = plt.subplots()
    finite_ratio = ratio[np.isfinite(ratio)]
    contour_kwargs = {"levels": 21, "cmap": "coolwarm"}
    if finite_ratio.size > 0:
        ratio_min = float(np.nanmin(finite_ratio))
        ratio_max = float(np.nanmax(finite_ratio))
        if ratio_min < 1.0 < ratio_max:
            contour_kwargs["norm"] = TwoSlopeNorm(vmin=ratio_min, vcenter=1.0, vmax=ratio_max)

    heatmap = ax.contourf(x, y, np.ma.masked_invalid(ratio), **contour_kwargs)
    finite_advantage = advantage[np.isfinite(advantage)]
    if finite_advantage.size > 0 and np.nanmin(finite_advantage) <= 0 <= np.nanmax(finite_advantage):
        boundary = ax.contour(x, y, advantage, levels=[0.0], colors="black", linewidths=1.2)
        if boundary.allsegs[0]:
            ax.plot([], [], color="black", linewidth=1.2, label="Equal SKR")

    if spec.x_axis in LOG_AXES:
        ax.set_xscale("log")
    if spec.y_axis in LOG_AXES:
        ax.set_yscale("log")

    ax.set_xlabel(AXIS_LABELS[spec.x_axis])
    ax.set_ylabel(AXIS_LABELS[spec.y_axis])
    ax.set_title("At-last optimality")
    ax.set_xticks(x)
    ax.set_yticks(y)
    if spec.x_axis == "multiplexing":
        ax.set_xticklabels([str(value) for value in x_values])
    if spec.y_axis == "multiplexing":
        ax.set_yticklabels([str(value) for value in y_values])

    if args.mark_protocol_winners:
        ax.margins(x=0.08, y=0.08)
        plot_protocol_winner_markers(ax, x_values, y_values, grid, results)

    style_axes(ax)
    fig.colorbar(
        heatmap,
        ax=ax,
        label=r"$\mathrm{SKR}_{\mathrm{at-last}} / \max \mathrm{SKR}_{\mathrm{baseline}}$",
    )
    add_external_legend(ax)
    suffix = f"{spec.x_axis}_vs_{spec.y_axis}_contour".replace("-", "_")
    figure_path = output_path(figure_dir, args.figure_prefix, suffix, plot_profile)
    save_figure(fig, figure_path)
    plt.close(fig)
    return figure_path


def plot_protocol_winner_markers(ax, x_values, y_values, grid, results):
    labelled_protocols = set()
    for row_index, y_value in enumerate(y_values):
        for column_index, x_value in enumerate(x_values):
            winner = results[grid[row_index][column_index]].winner
            marker = PROTOCOL_MARKERS[winner]
            label = None
            if winner not in labelled_protocols:
                label = PROTOCOL_MARKER_LABELS[winner]
                labelled_protocols.add(winner)
            ax.plot(
                [float(x_value)],
                [float(y_value)],
                marker=marker,
                linestyle="None",
                color="white",
                markerfacecolor="white",
                markeredgecolor="white",
                markersize=WINNER_MARKER_HALO_SIZE,
                markeredgewidth=1.0,
                clip_on=False,
                zorder=5,
            )
            ax.plot(
                [float(x_value)],
                [float(y_value)],
                marker=marker,
                linestyle="None",
                color="black",
                markerfacecolor="black",
                markeredgecolor="black",
                markersize=WINNER_MARKER_SIZE,
                markeredgewidth=0.8,
                label=label,
                clip_on=False,
                zorder=6,
            )


def add_external_legend(ax) -> None:
    handles, labels = ax.get_legend_handles_labels()
    if not handles:
        return
    ax.legend(
        handles,
        labels,
        frameon=False,
        loc="upper center",
        bbox_to_anchor=(0.5, -0.26),
        ncol=min(3, len(handles)),
        borderaxespad=0.0,
        columnspacing=1.1,
        handletextpad=0.5,
    )


def main() -> None:
    args = parse_args()
    validate_args(args)
    contour_specs = parse_contours(args.contour)
    output_dir = Path(args.output_dir)
    figure_dir = Path(args.figure_dir)
    if args.plots_only:
        if not output_dir.is_dir():
            raise SystemExit(f"--plots-only requires an existing --output-dir: {output_dir}")
        log("--plots-only: using existing JSONs; Cabal build/run steps are skipped.")
        if args.coverage is not None or args.truncation is not None:
            log("--plots-only: budget options are ignored.")
    else:
        output_dir.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)

    if not args.no_build and not args.plots_only:
        run_command(
            ["cabal", "build", args.executable],
            status_label=f"cabal build {args.executable}",
        )

    plt = configure_matplotlib(args.plot_profile)
    results: dict[SweepPoint, PointResult] = {}
    contour_grids = []

    for spec in contour_specs:
        x_values, y_values, grid = contour_points(spec, args)
        contour_grids.append((spec, x_values, y_values, grid))
        for row in grid:
            for point in row:
                if point not in results:
                    log()
                    log(f"Evaluating {scenario_tag(point)}")
                    results[point] = evaluate_point(point, output_dir, args)

    protocol_csv = output_dir / f"{args.file_prefix}_protocol_skr.csv"
    point_csv = output_dir / f"{args.file_prefix}_points.csv"
    write_protocol_rows(protocol_csv, results)
    write_point_rows(point_csv, results)
    log()
    log(f"Wrote protocol SKRs to {protocol_csv}")
    log(f"Wrote optimality summary to {point_csv}")

    for spec, x_values, y_values, grid in contour_grids:
        figure_path = plot_contour(plt, figure_dir, args, spec, x_values, y_values, grid, results)
        log(f"Saved contour figure to {figure_path}")


if __name__ == "__main__":
    main()
