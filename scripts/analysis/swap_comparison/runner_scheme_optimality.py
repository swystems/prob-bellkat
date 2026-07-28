#!/usr/bin/env python3

from __future__ import annotations

import argparse
import csv
import math
import os
import sys
import time
from dataclasses import dataclass
from pathlib import Path

from scripts.analysis.swap_comparison.common import (
    MDP_MODE,
    QMDP_MODE,
    MIXED_EVENT,
    PURE_EVENT,
    STATIC_EVENT,
    build_command,
    compute_secret_key_rate_from_split,
    edge_generation_scaling_factor,
    executable_command,
    format_duration,
    generation_scaling_factor,
    load_coverage_budget,
    reference_p_ge_from_scaling_factor,
    run_command,
    validate_extremal_json,
)
from scripts.plot.config import (
    DEFAULT_PROFILE,
    JOINT_PLOTS_HSPACE,
    OPTIMALITY_COMBINED_HEIGHT_INCHES,
    OPTIMALITY_COMBINED_LINE_WIDTH_INCHES,
    OPTIMALITY_HEIGHT_INCHES,
    OPTIMALITY_LINE_WIDTH_INCHES,
    PLOT_SETTINGS,
    configure_matplotlib,
    get_plot_profile,
    output_path,
    save_figure,
)
from scripts.plot.contour import RATIO_CONTOUR_LEVELS, draw_ratio_contour, thinned_ticks


BASELINE_PROTOCOL = "swap-asap"
DOUBLING_PROTOCOL = "doubling"
SEQUENTIAL_PROTOCOLS = ("left-to-right", "right-to-left")
PROTOCOLS = (BASELINE_PROTOCOL, DOUBLING_PROTOCOL, *SEQUENTIAL_PROTOCOLS)
PROTOCOL_PLOT_LABELS = {
    "left-to-right": "L2R",
    "right-to-left": "R2L",
}
FILE_PREFIX = "swap_scheme_optimality"
FIGURE_PREFIX = "swap_scheme_optimality"
DEFAULT_OUTPUT_DIR = Path("output/swap-scheme-optimality")
DEFAULT_OPTIMALITY_TRUNCATION = 5000
DOUBLING_EXPERIMENT = "doubling-asap"
SEQUENTIAL_EXPERIMENT = "sequential-asap"
EXPERIMENTS = (DOUBLING_EXPERIMENT, SEQUENTIAL_EXPERIMENT)
DEFAULT_GENERATION_SCALING_VALUES_A = "8,32,128,512,1024"
DEFAULT_P_SWAP_VALUES_A = "0.25,0.5,0.75,1"
DEFAULT_GENERATION_SCALING_VALUES_B = "8,32,128,512,1024"
DEFAULT_EDGE_SKEW_VALUES_B = "1,2,4,8,16,32"
EVALUATION_A_EDGE_SKEW = 1.0
EVALUATION_A_W0 = 0.955
EVALUATION_B_W0 = 0.955
LOG_AXES = {"generation-scaling", "edge-skew"}
AXIS_LABELS = {
    "generation-scaling": r"$p_{\mathrm{ge}}$ scaling factor",
    "p-swap": r"$p_{\mathrm{sw}}$",
    "edge-skew": r"$p_{\mathrm{ge}}^{ZE}$ scaling factor",
}


@dataclass(frozen=True)
class RatioJob:
    experiment: str
    name: str
    x_axis: str
    y_axis: str
    x_values_attr: str
    x_values_flag: str
    y_values_attr: str
    y_values_flag: str
    fixed_axes: tuple[tuple[str, float], ...]
    cmap: str
    numerator_protocols: tuple[str, ...]
    numerator_label: str
    caption: str
    contour_levels: int = RATIO_CONTOUR_LEVELS


@dataclass(frozen=True)
class SchemePoint:
    p_gen: float | None
    edge_skew: float | None
    t_coh: int | None
    p_swap: float | None
    w0: float | None


@dataclass(frozen=True)
class PointResult:
    point: SchemePoint
    skr_by_protocol: dict[str, float]


@dataclass(frozen=True)
class RatioResult:
    point: SchemePoint
    ratio: float
    numerator_protocol: str
    numerator_skr: float
    baseline_skr: float


DEFAULT_JOBS = (
    RatioJob(
        experiment=DOUBLING_EXPERIMENT,
        name="doubling_over_swap_asap",
        x_axis="generation-scaling",
        y_axis="p-swap",
        x_values_attr="generation_scaling_values_a",
        x_values_flag="--generation-scaling-values-a",
        y_values_attr="p_swap_values_a",
        y_values_flag="--p-sw-values-a",
        fixed_axes=(("edge-skew", EVALUATION_A_EDGE_SKEW), ("w0", EVALUATION_A_W0)),
        cmap="RdBu",
        numerator_protocols=(DOUBLING_PROTOCOL,),
        numerator_label="doubling",
        caption=r"$\mathrm{SKR}(\mathrm{doubling}/\mathrm{asap})$",
        contour_levels=301,
    ),
    RatioJob(
        experiment=SEQUENTIAL_EXPERIMENT,
        name="sequential_over_swap_asap",
        x_axis="generation-scaling",
        y_axis="edge-skew",
        x_values_attr="generation_scaling_values_b",
        x_values_flag="--generation-scaling-values-b",
        y_values_attr="edge_skew_values_b",
        y_values_flag="--edge-skew-values-b",
        fixed_axes=(("w0", EVALUATION_B_W0),),
        cmap="BrBG",
        numerator_protocols=SEQUENTIAL_PROTOCOLS,
        numerator_label="sequential",
        caption=r"$\mathrm{SKR}(\mathrm{sequential}/\mathrm{asap})$",
    ),
)

def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Run a compact topology-aware swap-scheme optimality analysis. "
            "Evaluation (a), when requested, plots doubling/swap-asap over "
            "generation scaling and p_swap with homogeneous edges at fixed w0. "
            "Evaluation (b) plots the best sequential "
            "order/swap-asap over generation scaling and right-edge skew at fixed w0."
        )
    )
    budget_group = parser.add_mutually_exclusive_group()
    budget_group.add_argument(
        "--truncation",
        type=int,
        default=DEFAULT_OPTIMALITY_TRUNCATION,
        help="QMDP pure/mixed truncation budget.",
    )
    budget_group.add_argument(
        "--coverage",
        type=float,
        default=None,
        help=(
            "Resolve an MDP/static budget per point/protocol until the "
            "worst-scheduler CDF reaches this coverage, then reuse the maximum "
            "budget for QMDP pure/mixed runs."
        ),
    )
    parser.add_argument(
        "--generation-scaling-values-a",
        dest="generation_scaling_values_a",
        default=None,
        help="Comma-separated generation scaling factors for doubling vs. swap-asap.",
    )
    parser.add_argument(
        "--p-sw-values-a",
        "--p-swap-values-a",
        dest="p_swap_values_a",
        default=None,
        help="Comma-separated swap success probabilities for doubling vs. swap-asap.",
    )
    parser.add_argument(
        "--generation-scaling-values-b",
        dest="generation_scaling_values_b",
        default=None,
        help="Comma-separated generation scaling factors for sequential vs. swap-asap.",
    )
    parser.add_argument(
        "--edge-skew-values-b",
        default=None,
        help="Comma-separated rightmost-link skew penalties for sequential vs. swap-asap.",
    )
    parser.add_argument("--t-coh", "--fixed-t-coh", dest="fixed_t_coh", type=int, default=None, help="Fixed coherence time; omitted values use the executable default.")
    parser.add_argument("--p-swap", "--fixed-p-swap", dest="fixed_p_swap", type=float, default=None, help="Fixed swap probability; omitted values use the executable default.")
    parser.add_argument(
        "--experiment",
        action="append",
        choices=EXPERIMENTS,
        dest="experiments",
        help=(
            "Experiment to run. Pass multiple times for multiple experiments. "
            "Defaults to sequential-asap unless only experiment (a) values are supplied."
        ),
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help="Root directory for JSON dumps, CSVs, figures, and report.",
    )
    parser.add_argument(
        "--markdown",
        type=Path,
        default=DEFAULT_OUTPUT_DIR / "swap-scheme-optimality.md",
        help="Markdown report path.",
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=DEFAULT_PROFILE,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--executable",
        default="quantP_compare_swap_parameters",
        help="Cabal executable name, or path to an already-built executable.",
    )
    parser.add_argument("--plots-only", action="store_true", help="Reuse existing JSONs and only rewrite CSVs/plots/report.")
    parser.add_argument(
        "--joint-plots",
        action="store_true",
        help="Also combine the optimality plots into one figure with a shared p_ge axis.",
    )
    parser.add_argument("--resume", action="store_true", help="Reuse valid existing JSONs and run only missing cases.")
    parser.add_argument("--no-build", action="store_true", help="Skip the initial cabal build step.")
    parser.add_argument(
        "--smoke-test",
        action="store_true",
        help="Run a tiny truncation-1 sweep under an output smoke directory.",
    )
    args = parser.parse_args()
    configure_experiments(args)
    if args.smoke_test:
        if args.plots_only:
            parser.error("--smoke-test cannot be combined with --plots-only.")
        default_markdown = DEFAULT_OUTPUT_DIR / "swap-scheme-optimality.md"
        args.coverage = None
        args.truncation = 1
        if DOUBLING_EXPERIMENT in args.selected_experiments:
            args.generation_scaling_values_a = smoke_value_text(
                args.generation_scaling_values_a,
                "--generation-scaling-values-a",
            )
            args.p_swap_values_a = smoke_value_text(args.p_swap_values_a, "--p-sw-values-a")
        if SEQUENTIAL_EXPERIMENT in args.selected_experiments:
            args.generation_scaling_values_b = smoke_value_text(
                args.generation_scaling_values_b,
                "--generation-scaling-values-b",
            )
            args.edge_skew_values_b = smoke_value_text(args.edge_skew_values_b, "--edge-skew-values-b")
        args.output_dir = args.output_dir / "smoke"
        if args.markdown == default_markdown:
            args.markdown = args.output_dir / "swap-scheme-optimality.md"
    return args


def configure_experiments(args) -> None:
    explicit_a = ratio_job_requested(DEFAULT_JOBS[0], args)
    explicit_b = ratio_job_requested(DEFAULT_JOBS[1], args)
    if args.experiments:
        selected = tuple(dict.fromkeys(args.experiments))
    elif explicit_a and not explicit_b:
        selected = (DOUBLING_EXPERIMENT,)
    elif explicit_b and not explicit_a:
        selected = (SEQUENTIAL_EXPERIMENT,)
    elif explicit_a and explicit_b:
        selected = (DOUBLING_EXPERIMENT, SEQUENTIAL_EXPERIMENT)
    else:
        selected = (SEQUENTIAL_EXPERIMENT,)

    args.selected_experiments = selected
    if DOUBLING_EXPERIMENT in selected:
        if args.generation_scaling_values_a is None:
            args.generation_scaling_values_a = DEFAULT_GENERATION_SCALING_VALUES_A
        if args.p_swap_values_a is None:
            args.p_swap_values_a = DEFAULT_P_SWAP_VALUES_A
    if SEQUENTIAL_EXPERIMENT in selected:
        if args.generation_scaling_values_b is None:
            args.generation_scaling_values_b = DEFAULT_GENERATION_SCALING_VALUES_B
        if args.edge_skew_values_b is None:
            args.edge_skew_values_b = DEFAULT_EDGE_SKEW_VALUES_B


def smoke_value_text(axis_values_text: str | None, flag: str) -> str:
    if axis_values_text is None:
        raise SystemExit(f"--smoke-test requires at least one value for {flag}.")
    for part in axis_values_text.split(","):
        value = part.strip()
        if value:
            return value
    raise SystemExit(f"--smoke-test requires at least one value for {flag}.")


def log(message: str = "") -> None:
    print(message, flush=True)


def parse_float_values(raw: str, flag: str) -> tuple[float, ...]:
    values = tuple(float(part.strip()) for part in raw.split(",") if part.strip())
    if not values:
        raise SystemExit(f"{flag} must contain at least one value.")
    return values


def validate_probability(flag: str, value: float, *, allow_zero: bool = False) -> None:
    lower_ok = value >= 0 if allow_zero else value > 0
    if not lower_ok or value > 1:
        interval = "[0, 1]" if allow_zero else "(0, 1]"
        raise SystemExit(f"{flag} must be in the interval {interval}.")


def validate_args(args) -> None:
    if args.coverage is not None:
        validate_probability("--coverage", args.coverage)
    if args.truncation is not None and args.truncation < 0:
        raise SystemExit("--truncation must be non-negative.")
    if args.fixed_p_swap is not None:
        validate_probability("--p-swap", args.fixed_p_swap, allow_zero=True)
    if args.fixed_t_coh is not None and args.fixed_t_coh <= 0:
        raise SystemExit("--t-coh must be positive.")
    for job in enabled_jobs(args):
        for axis in (job.x_axis, job.y_axis):
            for value in axis_values(job, axis, args):
                validate_axis_value(axis, value, axis_flag(job, axis))


def validate_axis_value(axis: str, value: float, flag: str) -> None:
    if axis == "generation-scaling":
        reference_p_ge = reference_p_ge_from_scaling_factor(value)
        if value <= 0 or reference_p_ge > 1:
            maximum = 1 / reference_p_ge_from_scaling_factor(1)
            raise SystemExit(f"{flag} entries must be in the interval (0, {maximum:.6g}].")
    elif axis == "p-swap":
        validate_probability(flag, value, allow_zero=True)
    elif axis == "edge-skew":
        if value < 1:
            raise SystemExit(f"{flag} entries must be at least 1.")
    else:
        raise AssertionError(f"Unexpected axis: {axis}")


def axis_flag(job: RatioJob, axis: str) -> str:
    if axis == job.x_axis:
        return job.x_values_flag
    if axis == job.y_axis:
        return job.y_values_flag
    raise AssertionError(f"Unexpected axis {axis} for job {job.name}")


def axis_values(job: RatioJob, axis: str, args) -> tuple[float, ...]:
    if axis == job.x_axis:
        return parse_float_values(getattr(args, job.x_values_attr), job.x_values_flag)
    if axis == job.y_axis:
        return parse_float_values(getattr(args, job.y_values_attr), job.y_values_flag)
    raise AssertionError(f"Unexpected axis {axis} for job {job.name}")


def ratio_job_requested(job: RatioJob, args) -> bool:
    return getattr(args, job.x_values_attr) is not None or getattr(args, job.y_values_attr) is not None


def ratio_job_enabled(job: RatioJob, args) -> bool:
    return job.experiment in args.selected_experiments


def enabled_jobs(args) -> tuple[RatioJob, ...]:
    jobs = tuple(job for job in DEFAULT_JOBS if ratio_job_enabled(job, args))
    if not jobs:
        raise SystemExit("No optimality evaluation is enabled.")
    return jobs


def fixed_values(args) -> dict[str, float | int | None]:
    return {
        "p-gen": None,
        "w0": None,
        "edge-skew": None,
        "t-coh": args.fixed_t_coh,
        "p-swap": args.fixed_p_swap,
    }


def point_from_values(values: dict[str, float | int | None]) -> SchemePoint:
    return SchemePoint(
        p_gen=float(values["p-gen"]) if values["p-gen"] is not None else None,
        edge_skew=float(values["edge-skew"]) if values["edge-skew"] is not None else None,
        t_coh=int(values["t-coh"]) if values["t-coh"] is not None else None,
        p_swap=float(values["p-swap"]) if values["p-swap"] is not None else None,
        w0=float(values["w0"]) if values["w0"] is not None else None,
    )


def point_for_job(job: RatioJob, x_value: float, y_value: float, args) -> SchemePoint:
    values = fixed_values(args)
    for axis, value in job.fixed_axes:
        values[axis] = value
    for axis, value in ((job.x_axis, x_value), (job.y_axis, y_value)):
        if axis == "generation-scaling":
            values["p-gen"] = reference_p_ge_from_scaling_factor(value)
        else:
            values[axis] = value
    return point_from_values(values)


def protocols_for_job(job: RatioJob) -> tuple[str, ...]:
    return (BASELINE_PROTOCOL, *job.numerator_protocols)


def all_point_protocols(args) -> list[tuple[SchemePoint, tuple[str, ...]]]:
    points: list[SchemePoint] = []
    protocols_by_point: dict[SchemePoint, set[str]] = {}
    for job in enabled_jobs(args):
        for y_value in axis_values(job, job.y_axis, args):
            for x_value in axis_values(job, job.x_axis, args):
                point = point_for_job(job, x_value, y_value, args)
                if point not in protocols_by_point:
                    protocols_by_point[point] = set()
                    points.append(point)
                protocols_by_point[point].update(protocols_for_job(job))
    return [
        (point, tuple(protocol for protocol in PROTOCOLS if protocol in protocols_by_point[point]))
        for point in points
    ]


def command_args_for_point(point: SchemePoint) -> list[str]:
    args = []
    if point.p_gen is not None:
        args.extend(("--p-gen", f"{point.p_gen:.17g}"))
    if point.p_swap is not None:
        args.extend(("--p-swap", f"{point.p_swap:.17g}"))
    if point.w0 is not None:
        args.extend(("--w0", f"{point.w0:.17g}"))
    if point.t_coh is not None:
        args.extend(("--t-coh", str(point.t_coh)))
    if point.edge_skew is not None:
        args.extend(("--edge-skew", f"{point.edge_skew:.17g}"))
    return args


def value_tag(value: float | int) -> str:
    if isinstance(value, int):
        return str(value)
    return f"{value:.12g}".replace("-", "m").replace("+", "").replace(".", "p")


def optional_value_tag(value: float | int | None) -> str:
    if value is None:
        return "default"
    return value_tag(value)


def value_text(value: float | int | None) -> str:
    if value is None:
        return "default"
    return f"{value:.12g}"


def fixed_value_text(value: float | int | None) -> str:
    if value is None:
        return "example default"
    return f"{value:g}"


def scenario_tag(point: SchemePoint) -> str:
    return (
        f"p{optional_value_tag(point.p_gen)}"
        f"_skew{optional_value_tag(point.edge_skew)}"
        f"_t{optional_value_tag(point.t_coh)}"
        f"_pswap{optional_value_tag(point.p_swap)}"
        f"_w{optional_value_tag(point.w0)}"
    )


def json_path(data_dir: Path, point: SchemePoint, protocol: str, mode: str, event: str) -> Path:
    return data_dir / f"{FILE_PREFIX}_{scenario_tag(point)}_{protocol}_{mode}_{event}.json"


def existing_json_path(data_dir: Path, point: SchemePoint, protocol: str, mode: str, event: str) -> Path:
    path = json_path(data_dir, point, protocol, mode, event)
    require_coverage = mode == MDP_MODE and event == STATIC_EVENT
    valid, reason = validate_extremal_json(path, require_coverage=require_coverage)
    if valid:
        return path
    raise SystemExit(f"Unusable existing JSON: {path} ({reason})")


def run_extremal_case(
    executable: str,
    protocol: str,
    mode: str,
    event: str,
    point: SchemePoint,
    budget_flag: str,
    budget_value: float | int,
    target_path: Path,
) -> float:
    command = [
        *executable_command(executable),
        "--protocol",
        protocol,
        "--event",
        event,
        *command_args_for_point(point),
        "--json",
        mode,
        "--compute-extremal",
        budget_flag,
        str(budget_value),
    ]
    status_label = f"{scenario_tag(point)} {protocol} {mode}/{event}"
    return run_command(command, stdout_path=target_path, status_label=status_label)


def resolve_budget(point: SchemePoint, protocols: tuple[str, ...], data_dir: Path, args) -> int:
    if args.coverage is None:
        return args.truncation

    budgets = []
    for protocol in protocols:
        target_path = json_path(data_dir, point, protocol, MDP_MODE, STATIC_EVENT)
        if args.plots_only:
            path = existing_json_path(data_dir, point, protocol, MDP_MODE, STATIC_EVENT)
        elif args.resume:
            try:
                path = existing_json_path(data_dir, point, protocol, MDP_MODE, STATIC_EVENT)
                log(f"{scenario_tag(point)} {protocol} coverage: reused {path}")
            except SystemExit as exc:
                log(f"{exc}; rerunning {protocol} {MDP_MODE}/{STATIC_EVENT}")
                path = target_path
                elapsed = run_extremal_case(
                    args.executable,
                    protocol,
                    MDP_MODE,
                    STATIC_EVENT,
                    point,
                    "--coverage",
                    args.coverage,
                    path,
                )
                log(f"{scenario_tag(point)} {protocol} coverage: {elapsed:.2f}s -> {path}")
        else:
            path = target_path
            elapsed = run_extremal_case(
                args.executable,
                protocol,
                MDP_MODE,
                STATIC_EVENT,
                point,
                "--coverage",
                args.coverage,
                path,
            )
            log(f"{scenario_tag(point)} {protocol} coverage: {elapsed:.2f}s -> {path}")
        resolved_budget, coverage_value = load_coverage_budget(protocol, args.coverage, path)
        budgets.append(resolved_budget)
        log(f"{scenario_tag(point)} {protocol}: coverage {coverage_value:.12g} at R={resolved_budget}")
    return max(budgets)


def ensure_protocol_jsons(
    point: SchemePoint,
    protocol: str,
    data_dir: Path,
    args,
    budget: int,
) -> tuple[Path, Path]:
    paths = []
    for event in (PURE_EVENT, MIXED_EVENT):
        target_path = json_path(data_dir, point, protocol, QMDP_MODE, event)
        reused = False
        if args.plots_only:
            paths.append(existing_json_path(data_dir, point, protocol, QMDP_MODE, event))
            continue
        if args.resume:
            try:
                paths.append(existing_json_path(data_dir, point, protocol, QMDP_MODE, event))
                reused = True
            except SystemExit as exc:
                log(f"{exc}; rerunning {protocol} {QMDP_MODE}/{event}")
                paths.append(target_path)
        else:
            paths.append(target_path)
        if reused:
            log(f"{scenario_tag(point)} {protocol} {event}: reused {paths[-1]}")
            continue
        elapsed = run_extremal_case(
            args.executable,
            protocol,
            QMDP_MODE,
            event,
            point,
            "--truncation",
            budget,
            paths[-1],
        )
        log(f"{scenario_tag(point)} {protocol} {event}: {elapsed:.2f}s -> {paths[-1]}")
    return paths[0], paths[1]


def evaluate_point(
    point: SchemePoint,
    protocols: tuple[str, ...],
    data_dir: Path,
    args,
    *,
    index: int,
    total: int,
) -> PointResult:
    budget = 0 if args.plots_only else resolve_budget(point, protocols, data_dir, args)
    skr_by_protocol = {}
    for protocol_index, protocol in enumerate(protocols, start=1):
        log(f"[progress] point {index}/{total}; protocol {protocol_index}/{len(protocols)}: {protocol}")
        pure_path, mixed_path = ensure_protocol_jsons(point, protocol, data_dir, args, budget)
        skr_by_protocol[protocol] = compute_secret_key_rate_from_split(pure_path, mixed_path)
        log(f"{scenario_tag(point)} {protocol}: SKR={skr_by_protocol[protocol]:.12g}")
    return PointResult(point=point, skr_by_protocol=skr_by_protocol)


def ratio_result(job: RatioJob, point_result: PointResult) -> RatioResult:
    baseline_skr = point_result.skr_by_protocol[BASELINE_PROTOCOL]
    # TODO: add to the summary what was the protocol chosen for numerator
    numerator_protocol = max(
        job.numerator_protocols,
        key=lambda protocol: point_result.skr_by_protocol[protocol],
    )
    numerator_skr = point_result.skr_by_protocol[numerator_protocol]
    ratio = numerator_skr / baseline_skr if baseline_skr > 0 else math.nan
    return RatioResult(
        point=point_result.point,
        ratio=ratio,
        numerator_protocol=numerator_protocol,
        numerator_skr=numerator_skr,
        baseline_skr=baseline_skr,
    )


def ratio_grid(job: RatioJob, results: dict[SchemePoint, PointResult], args):
    x_values = axis_values(job, job.x_axis, args)
    y_values = axis_values(job, job.y_axis, args)
    grid = [
        [
            ratio_result(job, results[point_for_job(job, x_value, y_value, args)])
            for x_value in x_values
        ]
        for y_value in y_values
    ]
    return x_values, y_values, grid


def axis_label(axis: str) -> str:
    return AXIS_LABELS[axis]


def plotted_axis_value(axis: str, value: float | int | None) -> float:
    numeric_value = require_axis_number(axis, value)
    if axis == "edge-skew":
        return edge_generation_scaling_factor(numeric_value)
    return numeric_value


def plotted_axis_tick_label(axis: str, value: float | int | None) -> str:
    numeric_value = require_axis_number(axis, value)
    if axis == "generation-scaling":
        return f"{numeric_value:g}"
    if axis == "edge-skew":
        denominator = round(numeric_value)
        return r"$1$" if denominator == 1 else rf"$1/{denominator}$"
    return f"{numeric_value:g}"


def plotted_values_text(axis: str, raw_values: str, flag: str) -> str:
    return ",".join(
        f"{plotted_axis_value(axis, value):g}"
        for value in parse_float_values(raw_values, flag)
    )


def require_axis_number(axis: str, value: float | int | None) -> float:
    if value is None:
        raise AssertionError(f"Axis {axis} unexpectedly used an example default value.")
    return float(value)


def annotate_best_protocol(
    ax,
    x_values: list[float],
    y_values: list[float],
    protocol_grid: list[list[str]],
    *,
    log_x: bool,
    log_y: bool,
) -> None:
    """Mark which policy supplies a pointwise maximum in a ratio plot."""
    protocols = tuple(
        protocol
        for protocol in SEQUENTIAL_PROTOCOLS
        if any(protocol in row for row in protocol_grid)
    )
    if len(protocols) == 2:
        winner_codes = [
            [protocols.index(protocol) for protocol in row]
            for row in protocol_grid
        ]
        ax.contour(
            x_values,
            y_values,
            winner_codes,
            levels=[0.5],
            colors="#303030",
            linewidths=0.45,
            linestyles="--",
        )

    for protocol in protocols:
        coordinates = [
            (x_values[x_index], y_values[y_index])
            for y_index, row in enumerate(protocol_grid)
            for x_index, winner in enumerate(row)
            if winner == protocol
        ]
        transformed_x = [math.log(value) if log_x else value for value, _ in coordinates]
        transformed_y = [math.log(value) if log_y else value for _, value in coordinates]
        label_x = sum(transformed_x) / len(transformed_x)
        label_y = sum(transformed_y) / len(transformed_y)
        if log_x:
            label_x = math.exp(label_x)
        if log_y:
            label_y = math.exp(label_y)
        ax.text(
            label_x,
            label_y,
            PROTOCOL_PLOT_LABELS[protocol],
            ha="center",
            va="center",
            fontsize=5.5,
            fontweight="bold",
            color="#202020",
            bbox={"facecolor": "white", "alpha": 0.72, "edgecolor": "none", "pad": 0.7},
            zorder=4,
        )


def draw_ratio(fig, ax, job: RatioJob, results: dict[SchemePoint, PointResult], args, *, show_xlabel: bool) -> None:
    x_values, y_values, grid = ratio_grid(job, results, args)
    plotted_x = [plotted_axis_value(job.x_axis, value) for value in x_values]
    plotted_y = [plotted_axis_value(job.y_axis, value) for value in y_values]
    x_ticklabels = [plotted_axis_tick_label(job.x_axis, value) for value in x_values]
    y_ticklabels = [plotted_axis_tick_label(job.y_axis, value) for value in y_values]
    plotted_grid = [[entry.ratio for entry in row] for row in grid]
    protocol_grid = [[entry.numerator_protocol for entry in row] for row in grid]
    if len(plotted_x) > 1 and plotted_x[0] > plotted_x[-1]:
        plotted_x.reverse()
        x_ticklabels.reverse()
        plotted_grid = [list(reversed(row)) for row in plotted_grid]
        protocol_grid = [list(reversed(row)) for row in protocol_grid]
    if len(plotted_y) > 1 and plotted_y[0] > plotted_y[-1]:
        plotted_y.reverse()
        y_ticklabels.reverse()
        plotted_grid.reverse()
        protocol_grid.reverse()
    x_ticks, x_ticklabels = thinned_ticks(plotted_x, x_ticklabels, 5)
    y_ticks, y_ticklabels = thinned_ticks(plotted_y, y_ticklabels, 4)
    draw_ratio_contour(
        fig,
        ax,
        plotted_x,
        plotted_y,
        plotted_grid,
        cmap=job.cmap,
        colorbar_label=job.caption,
        xlabel=axis_label(job.x_axis),
        ylabel=axis_label(job.y_axis),
        log_x=job.x_axis in LOG_AXES,
        log_y=job.y_axis in LOG_AXES,
        show_xlabel=show_xlabel,
        x_ticks=x_ticks,
        y_ticks=y_ticks,
        x_ticklabels=x_ticklabels,
        y_ticklabels=y_ticklabels,
        levels=job.contour_levels,
    )
    if len(job.numerator_protocols) > 1:
        annotate_best_protocol(
            ax,
            plotted_x,
            plotted_y,
            protocol_grid,
            log_x=job.x_axis in LOG_AXES,
            log_y=job.y_axis in LOG_AXES,
        )


def plot_ratio(plt, figure_dir: Path, job: RatioJob, results: dict[SchemePoint, PointResult], args) -> Path:
    fig, ax = plt.subplots(
        figsize=(OPTIMALITY_LINE_WIDTH_INCHES, OPTIMALITY_HEIGHT_INCHES)
    )
    draw_ratio(fig, ax, job, results, args, show_xlabel=True)

    plot_profile = get_plot_profile(args.plot_profile)
    figure_path = output_path(figure_dir, FIGURE_PREFIX, job.name, plot_profile)
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    return figure_path


def plot_joint_ratios(plt, figure_dir: Path, results: dict[SchemePoint, PointResult], args) -> Path:
    jobs = enabled_jobs(args)
    plot_profile = get_plot_profile(args.plot_profile)
    fig, axes = plt.subplots(
        len(jobs),
        1,
        sharex=True,
        figsize=(
            OPTIMALITY_COMBINED_LINE_WIDTH_INCHES,
            OPTIMALITY_COMBINED_HEIGHT_INCHES,
        ),
        gridspec_kw={"hspace": JOINT_PLOTS_HSPACE},
    )
    if len(jobs) == 1:
        axes = (axes,)
    for index, (ax, job) in enumerate(zip(axes, jobs)):
        draw_ratio(
            fig,
            ax,
            job,
            results,
            args,
            show_xlabel=index == len(jobs) - 1,
        )
    fig.align_ylabels(axes)

    figure_path = output_path(figure_dir, FIGURE_PREFIX, "joint", plot_profile)
    save_figure(fig, figure_path, tight_layout=False, bbox_inches=None)
    plt.close(fig)
    return figure_path


def write_protocol_csv(path: Path, results: dict[SchemePoint, PointResult]) -> None:
    with open(path, "w", encoding="utf-8", newline="") as handle:
        fieldnames = (
            "scenario",
            "generation_scaling",
            "p_ge",
            "w0",
            "edge_skew",
            "t_coh",
            "p_swap",
            "protocol",
            "skr",
        )
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for point_result in results.values():
            point = point_result.point
            for protocol, skr in point_result.skr_by_protocol.items():
                writer.writerow(
                    {
                        "scenario": scenario_tag(point),
                        "generation_scaling": value_text(
                            generation_scaling_factor(point.p_gen)
                            if point.p_gen is not None
                            else None
                        ),
                        "p_ge": value_text(point.p_gen),
                        "w0": value_text(point.w0),
                        "edge_skew": value_text(point.edge_skew),
                        "t_coh": value_text(point.t_coh),
                        "p_swap": value_text(point.p_swap),
                        "protocol": protocol,
                        "skr": f"{skr:.12g}",
                    }
                )


def write_ratio_csv(path: Path, results: dict[SchemePoint, PointResult], args) -> None:
    with open(path, "w", encoding="utf-8", newline="") as handle:
        fieldnames = (
            "job",
            "scenario",
            "generation_scaling",
            "p_ge",
            "w0",
            "edge_skew",
            "p_swap",
            "numerator",
            "numerator_skr",
            "swap_asap_skr",
            "ratio",
        )
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for job in enabled_jobs(args):
            for y_value in axis_values(job, job.y_axis, args):
                for x_value in axis_values(job, job.x_axis, args):
                    point = point_for_job(job, x_value, y_value, args)
                    ratio = ratio_result(job, results[point])
                    writer.writerow(
                        {
                            "job": job.name,
                            "scenario": scenario_tag(point),
                            "generation_scaling": value_text(
                                generation_scaling_factor(point.p_gen)
                                if point.p_gen is not None
                                else None
                            ),
                            "p_ge": value_text(point.p_gen),
                            "w0": value_text(point.w0),
                            "edge_skew": value_text(point.edge_skew),
                            "p_swap": value_text(point.p_swap),
                            "numerator": ratio.numerator_protocol,
                            "numerator_skr": f"{ratio.numerator_skr:.12g}",
                            "swap_asap_skr": f"{ratio.baseline_skr:.12g}",
                            "ratio": f"{ratio.ratio:.12g}",
                        }
                    )


def relative_link(from_path: Path, to_path: Path) -> str:
    return os.path.relpath(to_path, start=from_path.parent)


def write_report(
    markdown_path: Path,
    args,
    protocol_csv: Path,
    ratio_csv: Path,
    figures: list[tuple[str, str, Path]],
    results: dict[SchemePoint, PointResult],
) -> None:
    markdown_path.parent.mkdir(parents=True, exist_ok=True)
    budget_text = (
        "plots-only"
        if args.plots_only
        else (f"coverage={args.coverage:g}" if args.coverage is not None else f"truncation={args.truncation}")
    )
    jobs = enabled_jobs(args)
    configuration_lines = []
    if DEFAULT_JOBS[0] in jobs:
        configuration_lines.append(
            "- evaluation (a), doubling vs. swap-asap: "
            f"`generation_scaling_eta={args.generation_scaling_values_a}`, "
            f"`p_swap={args.p_swap_values_a}`, `w0={EVALUATION_A_W0:g}`, "
            f"`right_edge_scaling_eta_ZE={edge_generation_scaling_factor(EVALUATION_A_EDGE_SKEW):g}`"
        )
    else:
        configuration_lines.append("- evaluation (a), doubling vs. swap-asap: skipped")
    if DEFAULT_JOBS[1] in jobs:
        configuration_lines.append(
            "- evaluation (b), best sequential vs. swap-asap: "
            f"`generation_scaling_eta={args.generation_scaling_values_b}`, "
            f"`right_edge_scaling_eta_ZE={plotted_values_text('edge-skew', args.edge_skew_values_b, '--edge-skew-values-b')}`, "
            f"`w0={EVALUATION_B_W0:g}`"
        )
    lines = [
        "# Swap Scheme Optimality",
        "",
        "Generated by `scripts.analysis.swap_comparison.runner_scheme_optimality`.",
        "",
        "## Configuration",
        "",
        *configuration_lines,
        (
            f"- hardware overrides: `t_coh={fixed_value_text(args.fixed_t_coh)}, "
            f"p_swap={fixed_value_text(args.fixed_p_swap)}`"
        ),
        f"- budget: `{budget_text}`",
        f"- protocols: `{', '.join(PROTOCOLS)}`",
        f"- command: `{' '.join(sys.argv)}`",
        "",
        "## Data",
        "",
        f"- Protocol SKR CSV: [{protocol_csv.name}]({relative_link(markdown_path, protocol_csv)})",
        f"- Ratio CSV: [{ratio_csv.name}]({relative_link(markdown_path, ratio_csv)})",
        "",
        "## Figures",
        "",
    ]
    for heading, alt_text, figure_path in figures:
        lines.extend(
            [
                f"### {heading}",
                "",
                f"![{alt_text}]({relative_link(markdown_path, figure_path)})",
                "",
            ]
        )

    lines.extend(["## Ratio Summary", ""])
    for job in jobs:
        ratios = []
        for y_value in axis_values(job, job.y_axis, args):
            for x_value in axis_values(job, job.x_axis, args):
                point = point_for_job(job, x_value, y_value, args)
                ratios.append(ratio_result(job, results[point]))
        finite = [entry.ratio for entry in ratios if math.isfinite(entry.ratio)]
        if finite:
            best = max(ratios, key=lambda entry: entry.ratio)
            lines.append(
                f"- `{job.name}`: min={min(finite):.6g}, max={max(finite):.6g}; "
                f"best point `{scenario_tag(best.point)}` via `{best.numerator_protocol}`."
            )
    lines.append("")
    markdown_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    args = parse_args()
    validate_args(args)
    data_dir = args.output_dir / "data"
    figure_dir = args.output_dir / "figures"
    if args.plots_only:
        if not data_dir.is_dir():
            raise SystemExit(f"--plots-only requires existing data directory: {data_dir}")
        log("--plots-only: using existing JSONs; Cabal build/run steps are skipped.")
    else:
        data_dir.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)

    if not args.no_build and not args.plots_only:
        command = build_command(args.executable)
        if command is not None:
            run_command(command, status_label=f"cabal build {args.executable}")

    point_protocols = all_point_protocols(args)
    total_protocol_cases = sum(len(protocols) for _, protocols in point_protocols)
    started = time.perf_counter()
    results = {}
    log(f"[progress] starting {len(point_protocols)} point(s), {total_protocol_cases} protocol case(s)")
    for index, (point, protocols) in enumerate(point_protocols, start=1):
        log()
        elapsed = time.perf_counter() - started
        log(f"[progress] point {index}/{len(point_protocols)} after {format_duration(elapsed)}: {scenario_tag(point)}")
        results[point] = evaluate_point(
            point,
            protocols,
            data_dir,
            args,
            index=index,
            total=len(point_protocols),
        )

    protocol_csv = args.output_dir / f"{FILE_PREFIX}_protocol_skr.csv"
    ratio_csv = args.output_dir / f"{FILE_PREFIX}_ratios.csv"
    write_protocol_csv(protocol_csv, results)
    write_ratio_csv(ratio_csv, results, args)
    log()
    log(f"Wrote protocol SKRs to {protocol_csv}")
    log(f"Wrote ratios to {ratio_csv}")

    plt = configure_matplotlib(args.plot_profile)
    figures = []
    if args.smoke_test:
        log("Smoke test: skipped contour figures for the one-point grid.")
    else:
        for job in enabled_jobs(args):
            figure_path = plot_ratio(plt, figure_dir, job, results, args)
            figures.append((f"{job.numerator_label} over swap-asap", job.name, figure_path))
            log(f"Saved {job.name} figure to {figure_path}")
        if args.joint_plots:
            figure_path = plot_joint_ratios(plt, figure_dir, results, args)
            figures.append(("Joint optimality plots", "joint optimality plots", figure_path))
            log(f"Saved joint optimality figure to {figure_path}")

    write_report(args.markdown, args, protocol_csv, ratio_csv, figures, results)
    log(f"Wrote markdown report to {args.markdown}")


if __name__ == "__main__":
    main()
