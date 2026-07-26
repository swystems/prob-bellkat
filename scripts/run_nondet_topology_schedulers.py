#!/usr/bin/env python3

import argparse
import csv
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

import numpy as np

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from scripts.analysis.swap_comparison.common import (
    assert_extrema_coincide,
    bin_time_series,
    build_command,
    compute_secret_key_rate_from_split,
    derive_full_pmf_series_from_split,
    executable_command,
    format_duration,
    run_command,
    validate_extremal_json,
)
from scripts.plot.config import (
    DEFAULT_PROFILE,
    JOINT_PLOTS_WSPACE,
    NONDET_HEIGHT_INCHES,
    NONDET_LINE_WIDTH_INCHES,
    PLOT_SETTINGS,
    SWAP_COMPARISON_COMBINED_HEIGHT_INCHES,
    SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES,
    TIME_AXIS_LABEL,
    get_plot_profile,
    hide_overlapping_inner_x_tick_label,
    output_path,
    save_figure,
)
from scripts.plot.plot_extremal import (
    configure_matplotlib,
    derive_average_werner_series,
    derive_pmf_series,
    load_extremal_payload,
    load_extremal_series,
    style_axes,
)
from scripts.run_nondet_topology_goals import GOAL_BY_NAME, goal_legend_handles
from scripts.run_nondet_topology_protocols import (
    PROTOCOLS,
    PROTOCOL_BY_NAME,
    draw_joint_bands as draw_nondeterministic_cdf,
    protocol_band_handles,
)
from scripts.utils.utils import get_mean_waiting_time, get_mean_werner


MDP_MODE = "mdp"
QMDP_MODE = "qmdp"
STATIC_EVENT = "static"
PURE_EVENT = "pure"
MIXED_EVENT = "mixed"
INDIVIDUAL_GOAL_NAMES = ("a-c", "b-d")
ALL_GOAL_NAMES = (*INDIVIDUAL_GOAL_NAMES, "either")
PRIORITY_NAMES = INDIVIDUAL_GOAL_NAMES
DEFAULT_REFERENCE_DIR = Path("output/nondet-topology-protocols")
DEFAULT_OUTPUT_DIR = Path("output/nondet-topology-schedulers")
DEFAULT_EXECUTABLE = "quantP_compare_nondet_schedulers"
DEFAULT_VERIFICATION_ATOL = 1e-10
DEFAULT_WERNER_BINNING = 10
DEFAULT_T_COH = 1440000
LINE_ALPHA = 0.82


@dataclass(frozen=True)
class Priority:
    name: str
    label: str


PRIORITIES = (
    Priority("a-c", r"prioritize $A\sim C$"),
    Priority("b-d", r"prioritize $B\sim D$"),
)
PRIORITY_BY_NAME = {priority.name: priority for priority in PRIORITIES}


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Evaluate implementable ordered schedulers for the nondeterministic "
            "butterfly topology, certify their deadline CDFs against the "
            "nondeterministic bounds, and derive fixed-policy Werner/SKR metrics."
        )
    )
    parser.add_argument(
        "--protocol",
        action="append",
        choices=tuple(PROTOCOL_BY_NAME),
        help="Swapping direction. Can be repeated; defaults to L-to-R and R-to-L.",
    )
    parser.add_argument(
        "--priority",
        action="append",
        choices=PRIORITY_NAMES,
        help="Ordered resource priority. Can be repeated; defaults to both priorities.",
    )
    parser.add_argument(
        "--objective",
        "--goal",
        dest="objectives",
        action="append",
        choices=ALL_GOAL_NAMES,
        help="Goal to evaluate. Can be repeated; defaults to all three static goals.",
    )
    parser.add_argument(
        "--truncation",
        type=int,
        default=None,
        help=(
            "Analysis horizon. By default, infer the common horizon from the "
            "existing nondeterministic protocol JSON files."
        ),
    )
    parser.add_argument(
        "--plot-truncation",
        type=int,
        default=None,
        help="Display figures only through this time without changing analysis.",
    )
    parser.add_argument(
        "--quality-truncation",
        type=int,
        default=None,
        help=(
            "Separate QMDP horizon for pure/mixed, Werner, and SKR analysis. "
            "Defaults to the full static --truncation horizon; set a smaller "
            "value only for quick exploratory runs."
        ),
    )
    parser.add_argument(
        "--quality-plot-truncation",
        type=int,
        default=None,
        help="Display the Werner panel only through this time.",
    )
    parser.add_argument(
        "--reference-dir",
        type=Path,
        default=DEFAULT_REFERENCE_DIR,
        help="Directory containing nondeterministic protocol JSON files.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help="Directory for ordered-policy JSON and CSV outputs.",
    )
    parser.add_argument(
        "--figure-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help="Directory for Werner and joint figures.",
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=DEFAULT_PROFILE,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--executable",
        default=DEFAULT_EXECUTABLE,
        help="Cabal executable name, or path to an already-built executable.",
    )
    parser.add_argument(
        "--p-gen-override",
        type=float,
        default=None,
        help="Override all elementary generation probabilities.",
    )
    parser.add_argument(
        "--p-swap",
        type=float,
        default=0.5,
        help="Swap success probability. Defaults to 0.5.",
    )
    parser.add_argument(
        "--w0-override",
        type=float,
        default=None,
        help="Override all elementary Werner parameters.",
    )
    parser.add_argument(
        "--t-coh",
        type=int,
        default=DEFAULT_T_COH,
        help=(
            "Memory coherence time in L0/c units. Defaults to "
            f"{DEFAULT_T_COH}, matching the paper hardware configuration."
        ),
    )
    parser.add_argument(
        "--verification-atol",
        type=float,
        default=DEFAULT_VERIFICATION_ATOL,
        help=(
            "Absolute tolerance for pointwise CDF certification. Defaults to "
            f"{DEFAULT_VERIFICATION_ATOL:g}."
        ),
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=2,
        help="Independent ordered-policy cases to run concurrently. Defaults to 2.",
    )
    parser.add_argument(
        "--werner-binning",
        type=int,
        default=DEFAULT_WERNER_BINNING,
        help=(
            "Average consecutive supported Werner samples in bins of this size "
            f"for plotting. Defaults to {DEFAULT_WERNER_BINNING}; use 1 for raw samples."
        ),
    )
    parser.add_argument(
        "--joint-protocols-cdf-werner",
        action="store_true",
        help=(
            "Also plot the existing nondeterministic protocol CDF and the "
            "goal-prioritized fixed-policy Werner curves side by side."
        ),
    )
    parser.add_argument(
        "--no-shade",
        "--no-shades",
        dest="no_shades",
        action="store_true",
        help="Draw only nondeterministic CDF boundaries in the joint figure.",
    )
    parser.add_argument(
        "--static-only",
        action="store_true",
        help="Run/certify static CDFs but skip QMDP, Werner, and SKR analysis.",
    )
    parser.add_argument(
        "--skip-union-quality",
        action="store_true",
        help=(
            "Do not run pure/mixed QMDP analyses for the union objective. "
            "This does not affect static CDF certification or the individual "
            "A-C/B-D Werner plot; it only omits aggregate union Werner/SKR metrics."
        ),
    )
    parser.add_argument(
        "--plots-only",
        action="store_true",
        help="Load existing ordered-policy and nondeterministic JSON files.",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help=(
            "Reuse valid ordered-policy JSON files already present and recompute "
            "only missing or incompatible cases."
        ),
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip the initial Cabal build step.",
    )
    return parser.parse_args()


def validate_probability(name, value):
    if value is not None and not 0.0 <= value <= 1.0:
        raise SystemExit(f"{name} must be in the interval [0, 1].")


def validate_args(args):
    if args.truncation is not None and args.truncation < 0:
        raise SystemExit("--truncation must be non-negative.")
    if args.plot_truncation is not None and args.plot_truncation <= 0:
        raise SystemExit("--plot-truncation must be positive.")
    if args.quality_truncation is not None and args.quality_truncation < 0:
        raise SystemExit("--quality-truncation must be non-negative.")
    if (
        args.quality_plot_truncation is not None
        and args.quality_plot_truncation <= 0
    ):
        raise SystemExit("--quality-plot-truncation must be positive.")
    if args.verification_atol < 0:
        raise SystemExit("--verification-atol must be non-negative.")
    if args.jobs < 1:
        raise SystemExit("--jobs must be positive.")
    if args.werner_binning < 1:
        raise SystemExit("--werner-binning must be positive.")
    validate_probability("--p-gen-override", args.p_gen_override)
    validate_probability("--p-swap", args.p_swap)
    validate_probability("--w0-override", args.w0_override)
    if args.t_coh <= 0:
        raise SystemExit("--t-coh must be positive.")


def unique_selected(values, defaults, lookup):
    names = list(dict.fromkeys(values or defaults))
    return [lookup[name] for name in names]


def selected_protocols(args):
    return unique_selected(
        args.protocol,
        [protocol.name for protocol in PROTOCOLS],
        PROTOCOL_BY_NAME,
    )


def selected_priorities(args):
    return unique_selected(
        args.priority,
        [priority.name for priority in PRIORITIES],
        PRIORITY_BY_NAME,
    )


def selected_goals(args):
    names = list(dict.fromkeys(args.objectives or ALL_GOAL_NAMES))
    return [GOAL_BY_NAME[name] for name in names]


def scenario_args(args):
    flags = ["--p-swap", str(args.p_swap), "--t-coh", str(args.t_coh)]
    if args.p_gen_override is not None:
        flags.extend(["--p-gen-override", str(args.p_gen_override)])
    if args.w0_override is not None:
        flags.extend(["--w0-override", str(args.w0_override)])
    return flags


def ordered_json_path(output_dir, protocol, priority, goal, mode, event):
    return output_dir / (
        f"nondet_topology_schedulers_{protocol.name}_{priority.name}_"
        f"{goal.name}_{mode}_{event}.json"
    )


def nondeterministic_json_path(reference_dir, protocol, goal):
    return reference_dir / (
        f"nondet_topology_protocols_{protocol.name}_{goal.name}_mdp_static.json"
    )


def require_extremal_json(path, description):
    ok, reason = validate_extremal_json(path, require_coverage=False)
    if not ok:
        raise SystemExit(f"Missing {description}: {path} ({reason}).")
    return path


def infer_truncation(reference_dir, protocols, goals):
    budgets = []
    for protocol in protocols:
        for goal in goals:
            path = require_extremal_json(
                nondeterministic_json_path(reference_dir, protocol, goal),
                f"nondeterministic reference for {protocol.name}/{goal.name}",
            )
            payload = load_extremal_payload(path)
            budget = payload.get("resolved_budget")
            if not isinstance(budget, int):
                raise SystemExit(f"Reference JSON has no integer resolved_budget: {path}")
            budgets.append(budget)
    truncation = min(budgets)
    if len(set(budgets)) > 1:
        print(
            "Reference horizons differ; using their common pointwise prefix "
            f"through R={truncation}."
        )
    else:
        print(f"Using reference horizon R={truncation}.")
    return truncation


def analysis_command(args, protocol, priority, goal, mode, event, truncation):
    return [
        *executable_command(args.executable),
        "--protocol",
        protocol.name,
        "--priority",
        priority.name,
        "--objective",
        goal.name,
        "--event",
        event,
        *scenario_args(args),
        "--json",
        mode,
        "--compute-extremal",
        "--truncation",
        str(truncation),
    ]


def obtain_result(
    args,
    protocol,
    priority,
    goal,
    mode,
    event,
    truncation,
):
    path = ordered_json_path(
        args.output_dir,
        protocol,
        priority,
        goal,
        mode,
        event,
    )
    description = (
        f"{protocol.name}/{priority.name}/{goal.name} {mode}/{event}"
    )
    if args.plots_only or args.resume:
        try:
            require_extremal_json(path, f"ordered-policy result {description}")
            resolved_budget = load_extremal_payload(path).get("resolved_budget")
            if resolved_budget != truncation:
                raise SystemExit(
                    f"Existing {description} has R={resolved_budget}, expected "
                    f"R={truncation}: {path}"
                )
        except SystemExit as exc:
            if args.plots_only:
                raise
            print(f"{description}: cannot resume ({exc}); recomputing")
        else:
            print(f"{description}: resumed {path}")
            return path, None

    elapsed = run_command(
        analysis_command(
            args,
            protocol,
            priority,
            goal,
            mode,
            event,
            truncation,
        ),
        stdout_path=path,
        status_label=description,
    )
    print(f"{description}: {elapsed:.2f}s -> {path}")
    return path, elapsed


def obtain_static_case(args, case, truncation):
    protocol, priority, goal = case
    path, elapsed = obtain_result(
        args,
        protocol,
        priority,
        goal,
        MDP_MODE,
        STATIC_EVENT,
        truncation,
    )
    return protocol, priority, goal, path, elapsed


def obtain_quality_case(args, case):
    protocol, priority, goal = case
    pure_path, pure_elapsed = obtain_result(
        args,
        protocol,
        priority,
        goal,
        QMDP_MODE,
        PURE_EVENT,
        args.quality_truncation,
    )
    mixed_path, mixed_elapsed = obtain_result(
        args,
        protocol,
        priority,
        goal,
        QMDP_MODE,
        MIXED_EVENT,
        args.quality_truncation,
    )
    return (
        protocol,
        priority,
        goal,
        pure_path,
        pure_elapsed,
        mixed_path,
        mixed_elapsed,
    )


def maximum_absolute_difference(left, right, description):
    if len(left) != len(right):
        raise SystemExit(
            f"{description} has mismatched lengths: left={len(left)}, right={len(right)}"
        )
    if not left:
        return 0.0, 0
    differences = [abs(a - b) for a, b in zip(left, right)]
    max_difference = max(differences)
    return max_difference, differences.index(max_difference)


def assert_pointwise_close(left, right, description, atol):
    max_difference, max_time = maximum_absolute_difference(left, right, description)
    if max_difference > atol:
        raise SystemExit(
            f"{description} differs at t={max_time}: "
            f"left={left[max_time]:.17g}, right={right[max_time]:.17g}, "
            f"abs_diff={max_difference:.3g}, atol={atol:.3g}"
        )
    return max_difference, max_time


def expected_bound(priority, goal):
    if priority.name == goal.name:
        return "max"
    return "min"


def certify_static_cdf(
    args,
    protocol,
    priority,
    goal,
    ordered_path,
    truncation,
):
    ordered = load_extremal_series(ordered_path)
    ordered_min = ordered["cdf_min"]
    ordered_max = ordered["cdf_max"]
    required_length = truncation + 1
    if len(ordered_min) != required_length:
        raise SystemExit(
            f"Ordered CDF has length {len(ordered_min)}, expected {required_length}: "
            f"{ordered_path}"
        )

    deterministic_diff, deterministic_time = assert_pointwise_close(
        ordered_min,
        ordered_max,
        f"{protocol.name}/{priority.name}/{goal.name} deterministic CDF",
        args.verification_atol,
    )

    reference_path = require_extremal_json(
        nondeterministic_json_path(args.reference_dir, protocol, goal),
        f"nondeterministic reference for {protocol.name}/{goal.name}",
    )
    reference = load_extremal_series(reference_path)

    if goal.name in INDIVIDUAL_GOAL_NAMES:
        bound = expected_bound(priority, goal)
        reference_curve = reference[f"cdf_{bound}"][:required_length]
        extremal_diff, extremal_time = assert_pointwise_close(
            ordered_max,
            reference_curve,
            (
                f"{protocol.name}/{priority.name}/{goal.name} fixed CDF "
                f"vs nondeterministic {bound} bound"
            ),
            args.verification_atol,
        )
        status = f"matches_{bound}"
    else:
        lower = reference["cdf_min"][:required_length]
        upper = reference["cdf_max"][:required_length]
        extremal_diff = 0.0
        extremal_time = 0
        for time, (value, low, high) in enumerate(zip(ordered_max, lower, upper)):
            violation = max(low - value, value - high, 0.0)
            if violation > extremal_diff:
                extremal_diff = violation
                extremal_time = time
        if extremal_diff > args.verification_atol:
            raise SystemExit(
                f"{protocol.name}/{priority.name}/{goal.name} fixed CDF leaves "
                f"the nondeterministic band at t={extremal_time}: "
                f"violation={extremal_diff:.3g}"
            )
        status = "within_bounds"

    print(
        f"certified {protocol.name}/{priority.name}/{goal.name}: {status}, "
        f"deterministic max |Δ|={deterministic_diff:.3g}, "
        f"reference max |Δ|={extremal_diff:.3g}"
    )
    return {
        "protocol": protocol.name,
        "priority": priority.name,
        "goal": goal.name,
        "status": status,
        "resolved_budget": truncation,
        "cdf_final": f"{ordered_max[-1]:.15g}",
        "deterministic_max_abs_diff": f"{deterministic_diff:.15g}",
        "deterministic_max_diff_time": deterministic_time,
        "reference_max_abs_diff": f"{extremal_diff:.15g}",
        "reference_max_diff_time": extremal_time,
        "ordered_json_path": str(ordered_path),
        "reference_json_path": str(reference_path),
    }


def write_csv(path, rows, fieldnames):
    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def quality_metrics(pure_path, mixed_path):
    _, pmf = derive_full_pmf_series_from_split(pure_path, mixed_path)
    pure_series = load_extremal_series(pure_path)
    mixed_series = load_extremal_series(mixed_path)
    _, _, werner = derive_average_werner_series(pure_series, mixed_series)
    pmf_array = np.asarray(pmf, dtype=float)
    werner_array = np.asarray(werner, dtype=float)
    return {
        "skr": compute_secret_key_rate_from_split(pure_path, mixed_path),
        "coverage": float(np.sum(pmf_array)),
        "mean_werner": get_mean_werner(pmf_array, werner_array),
        "mean_waiting_time": get_mean_waiting_time(pmf_array),
    }


def certify_static_quality_split(
    static_path,
    pure_path,
    mixed_path,
    description,
    atol,
):
    static_series = load_extremal_series(static_path)
    pure_series = load_extremal_series(pure_path)
    mixed_series = load_extremal_series(mixed_path)
    pure_pmf_min, pure_pmf_max = derive_pmf_series(pure_series)
    mixed_pmf_min, mixed_pmf_max = derive_pmf_series(mixed_series)
    static_pmf_min, static_pmf_max = derive_pmf_series(static_series)
    quality_length = len(pure_pmf_max)
    if len(mixed_pmf_max) != quality_length:
        raise SystemExit(
            f"{description} pure/mixed PMFs have different horizons."
        )
    if len(static_pmf_max) < quality_length:
        raise SystemExit(
            f"{description} static PMF is shorter than the quality PMFs."
        )

    reconstructed_min = [
        pure + mixed
        for pure, mixed in zip(pure_pmf_min, mixed_pmf_min)
    ]
    reconstructed_max = [
        pure + mixed
        for pure, mixed in zip(pure_pmf_max, mixed_pmf_max)
    ]
    assert_pointwise_close(
        reconstructed_min,
        reconstructed_max,
        f"{description} deterministic pure+mixed PMF",
        atol,
    )
    max_difference, _ = assert_pointwise_close(
        static_pmf_max[:quality_length],
        reconstructed_max,
        f"{description} static PMF prefix vs pure+mixed",
        atol,
    )
    return max_difference


def supported_werner_series(pure_path, mixed_path, bin_size):
    pure_series = load_extremal_series(pure_path)
    mixed_series = load_extremal_series(mixed_path)
    t, _, werner = derive_average_werner_series(pure_series, mixed_series)
    _, pmf = derive_full_pmf_series_from_split(pure_path, mixed_path)
    supported = [
        (time, value)
        for time, value, probability in zip(t, werner, pmf)
        if time > 0 and probability > 0.0
    ]
    if not supported:
        return [], []
    supported_t, supported_w = zip(*supported)
    if bin_size == 1:
        return list(supported_t), list(supported_w)
    return bin_time_series(supported_t, supported_w, bin_size)


def goal_priority_quality_paths(quality_paths, protocols):
    selected = []
    for protocol in protocols:
        for goal_name in INDIVIDUAL_GOAL_NAMES:
            key = (protocol.name, goal_name, goal_name)
            if key in quality_paths:
                pure_path, mixed_path = quality_paths[key]
                selected.append(
                    (protocol, GOAL_BY_NAME[goal_name], pure_path, mixed_path)
                )
    return selected


def draw_werner(
    ax,
    werner_paths,
    *,
    bin_size,
    plot_truncation=None,
    show_y_axis_on_right=False,
):
    for protocol, goal, pure_path, mixed_path in werner_paths:
        t, values = supported_werner_series(pure_path, mixed_path, bin_size)
        if not t:
            continue
        ax.plot(
            t,
            values,
            color=goal.color,
            alpha=LINE_ALPHA,
            linestyle=protocol.linestyle,
            linewidth=1.0,
        )

    ax.set_xlabel(TIME_AXIS_LABEL)
    ax.set_ylabel("Werner parameter")
    ax.set_ylim(0.0, 1.0)
    ax.margins(x=0)
    ax.set_xlim(left=0)
    if plot_truncation is not None:
        ax.set_xlim(0, plot_truncation)
    if show_y_axis_on_right:
        ax.yaxis.set_label_position("right")
        ax.yaxis.tick_right()
        ax.tick_params(
            axis="y",
            which="both",
            left=False,
            labelleft=False,
            right=True,
            labelright=True,
        )
    style_axes(ax)

    goals = [
        GOAL_BY_NAME[name]
        for name in INDIVIDUAL_GOAL_NAMES
        if any(goal.name == name for _, goal, _, _ in werner_paths)
    ]
    protocols = list(
        dict.fromkeys(protocol for protocol, _, _, _ in werner_paths)
    )
    if goals:
        goal_legend = ax.legend(
            handles=goal_legend_handles(goals),
            frameon=False,
            loc="upper left",
        )
        ax.add_artist(goal_legend)
    if protocols:
        ax.legend(
            handles=protocol_band_handles(
                protocols,
                lambda _protocol: "#777777",
                no_shades=True,
            ),
            frameon=False,
            loc="lower right",
        )


def plot_werner(
    plt,
    figure_dir,
    werner_paths,
    plot_profile,
    *,
    bin_size,
    plot_truncation=None,
):
    fig, ax = plt.subplots(
        figsize=(NONDET_LINE_WIDTH_INCHES, NONDET_HEIGHT_INCHES)
    )
    draw_werner(
        ax,
        werner_paths,
        bin_size=bin_size,
        plot_truncation=plot_truncation,
    )
    figure_path = output_path(
        figure_dir,
        "nondet_topology_schedulers",
        "werner",
        plot_profile,
    )
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    print(f"Saved ordered-scheduler Werner figure to {figure_path}")
    return figure_path


def nondeterministic_paths_by_goal(reference_dir, protocols, goals):
    return {
        goal.name: [
            (
                protocol,
                require_extremal_json(
                    nondeterministic_json_path(reference_dir, protocol, goal),
                    f"nondeterministic reference for {protocol.name}/{goal.name}",
                ),
            )
            for protocol in protocols
        ]
        for goal in goals
    }


def plot_joint_protocol_cdf_werner(
    plt,
    figure_dir,
    reference_dir,
    protocols,
    werner_paths,
    plot_profile,
    *,
    bin_size,
    no_shades=False,
    plot_truncation=None,
    quality_plot_truncation=None,
):
    goals = [GOAL_BY_NAME[name] for name in INDIVIDUAL_GOAL_NAMES]
    paths_by_goal = nondeterministic_paths_by_goal(
        reference_dir,
        protocols,
        goals,
    )
    fig, (cdf_ax, werner_ax) = plt.subplots(
        1,
        2,
        figsize=(
            SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES,
            SWAP_COMPARISON_COMBINED_HEIGHT_INCHES,
        ),
        gridspec_kw={"width_ratios": (1.0, 1.0), "wspace": JOINT_PLOTS_WSPACE},
    )
    draw_nondeterministic_cdf(
        cdf_ax,
        goals,
        paths_by_goal,
        "cdf",
        no_shades=no_shades,
        no_y_axis_label=False,
        no_y_ticks=False,
        plot_truncation=plot_truncation,
    )
    draw_werner(
        werner_ax,
        werner_paths,
        bin_size=bin_size,
        plot_truncation=quality_plot_truncation,
        show_y_axis_on_right=True,
    )
    fig.subplots_adjust(
        left=0.10,
        right=0.90,
        bottom=0.21,
        top=0.98,
        wspace=JOINT_PLOTS_WSPACE,
    )
    hide_overlapping_inner_x_tick_label(fig, cdf_ax, werner_ax)

    figure_path = output_path(
        figure_dir,
        "nondet_topology_protocols",
        "cdf_werner_side_by_side",
        plot_profile,
    )
    save_figure(fig, figure_path, tight_layout=False, bbox_inches=None)
    plt.close(fig)
    print(f"Saved protocol CDF/Werner joint figure to {figure_path}")
    return figure_path


def jain_index(values):
    denominator = len(values) * sum(value * value for value in values)
    if denominator == 0.0:
        return 0.0
    return sum(values) ** 2 / denominator


def conditional_latency_quantile(cdf, quantile):
    coverage = cdf[-1]
    if coverage <= 0.0:
        return None
    threshold = quantile * coverage
    return next(
        time
        for time, probability in enumerate(cdf)
        if probability >= threshold
    )


def aggregate_throughput(cdf):
    pmf = np.diff(np.asarray(cdf, dtype=float), prepend=0.0)
    mean_waiting_time = get_mean_waiting_time(pmf)
    if not np.isfinite(mean_waiting_time) or mean_waiting_time <= 0.0:
        return 0.0
    return 1.0 / mean_waiting_time


def fairness_rows(
    args,
    protocols,
    priorities,
    static_paths,
    truncation,
):
    rows = []
    for protocol in protocols:
        goal_series = {
            goal_name: load_extremal_series(
                require_extremal_json(
                    nondeterministic_json_path(
                        args.reference_dir,
                        protocol,
                        GOAL_BY_NAME[goal_name],
                    ),
                    f"nondeterministic reference for {protocol.name}/{goal_name}",
                )
            )
            for goal_name in INDIVIDUAL_GOAL_NAMES
        }
        for priority in priorities:
            probabilities = {}
            individual_curves = {}
            for goal_name in INDIVIDUAL_GOAL_NAMES:
                goal = GOAL_BY_NAME[goal_name]
                bound = expected_bound(priority, goal)
                curve = goal_series[goal_name][f"cdf_{bound}"]
                if truncation >= len(curve):
                    raise SystemExit(
                        f"Fairness horizon R={truncation} exceeds {protocol.name}/"
                        f"{goal_name} reference horizon {len(curve) - 1}."
                    )
                probabilities[goal_name] = curve[truncation]
                individual_curves[goal_name] = curve[: truncation + 1]

            either_path = static_paths.get(
                (protocol.name, priority.name, "either")
            )
            if either_path is not None:
                aggregate_curve = load_extremal_series(either_path)["cdf_max"]
            else:
                aggregate_curve = None

            rows.append(
                {
                    "protocol": protocol.name,
                    "priority": priority.name,
                    "resolved_budget": truncation,
                    "p_a_c": f"{probabilities['a-c']:.15g}",
                    "p_b_d": f"{probabilities['b-d']:.15g}",
                    "p_either": (
                        f"{aggregate_curve[-1]:.15g}"
                        if aggregate_curve is not None
                        else ""
                    ),
                    "aggregate_throughput": (
                        f"{aggregate_throughput(aggregate_curve):.15g}"
                        if aggregate_curve is not None
                        else ""
                    ),
                    "a_c_latency_p95_given_success": (
                        conditional_latency_quantile(
                            individual_curves["a-c"],
                            0.95,
                        )
                    ),
                    "a_c_latency_p99_given_success": (
                        conditional_latency_quantile(
                            individual_curves["a-c"],
                            0.99,
                        )
                    ),
                    "b_d_latency_p95_given_success": (
                        conditional_latency_quantile(
                            individual_curves["b-d"],
                            0.95,
                        )
                    ),
                    "b_d_latency_p99_given_success": (
                        conditional_latency_quantile(
                            individual_curves["b-d"],
                            0.99,
                        )
                    ),
                    "either_latency_p95_given_success": (
                        conditional_latency_quantile(aggregate_curve, 0.95)
                        if aggregate_curve is not None
                        else ""
                    ),
                    "either_latency_p99_given_success": (
                        conditional_latency_quantile(aggregate_curve, 0.99)
                        if aggregate_curve is not None
                        else ""
                    ),
                    "jain_index": f"{jain_index(list(probabilities.values())):.15g}",
                }
            )
    return rows


def print_fairness(rows):
    print("\nDeadline-success fairness:")
    print(
        f"{'Protocol':<15} {'Priority':<9} {'P(A-C)':<12} "
        f"{'P(B-D)':<12} {'P(either)':<12} {'Jain':<10}"
    )
    print("-" * 77)
    for row in rows:
        p_either = (
            f"{float(row['p_either']):<12.6f}"
            if row["p_either"] != ""
            else f"{'n/a':<12}"
        )
        print(
            f"{row['protocol']:<15} {row['priority']:<9} "
            f"{float(row['p_a_c']):<12.6f} {float(row['p_b_d']):<12.6f} "
            f"{p_either}"
            f"{float(row['jain_index']):<10.3f}"
        )


def main():
    args = parse_args()
    validate_args(args)
    protocols = selected_protocols(args)
    priorities = selected_priorities(args)
    goals = selected_goals(args)

    if args.plots_only:
        if not args.output_dir.is_dir():
            raise SystemExit(
                f"--plots-only requires an existing --output-dir: {args.output_dir}"
            )
    else:
        args.output_dir.mkdir(parents=True, exist_ok=True)
    args.figure_dir.mkdir(parents=True, exist_ok=True)

    if not args.reference_dir.is_dir():
        raise SystemExit(f"Reference directory does not exist: {args.reference_dir}")

    if not args.no_build and not args.plots_only:
        command = build_command(args.executable)
        if command is not None:
            run_command(
                command,
                status_label=f"cabal build {args.executable}",
            )

    truncation = args.truncation
    if truncation is None:
        truncation = infer_truncation(args.reference_dir, protocols, goals)
    if args.quality_truncation is None:
        args.quality_truncation = truncation

    timing_rows = []
    certification_rows = []
    static_paths = {}
    static_cases = [
        (protocol, priority, goal)
        for protocol in protocols
        for priority in priorities
        for goal in goals
    ]
    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        static_results = executor.map(
            lambda case: obtain_static_case(args, case, truncation),
            static_cases,
        )
        for protocol, priority, goal, path, elapsed in static_results:
            static_paths[(protocol.name, priority.name, goal.name)] = path
            certification_rows.append(
                certify_static_cdf(
                    args,
                    protocol,
                    priority,
                    goal,
                    path,
                    truncation,
                )
            )
            if elapsed is not None:
                timing_rows.append(
                    {
                        "protocol": protocol.name,
                        "priority": priority.name,
                        "goal": goal.name,
                        "mode": MDP_MODE,
                        "event": STATIC_EVENT,
                        "seconds": f"{elapsed:.6f}",
                        "json_path": str(path),
                    }
                )

    write_csv(
        args.output_dir / "cdf_certification.csv",
        certification_rows,
        (
            "protocol",
            "priority",
            "goal",
            "status",
            "resolved_budget",
            "cdf_final",
            "deterministic_max_abs_diff",
            "deterministic_max_diff_time",
            "reference_max_abs_diff",
            "reference_max_diff_time",
            "ordered_json_path",
            "reference_json_path",
        ),
    )

    fairness = fairness_rows(
        args,
        protocols,
        priorities,
        static_paths,
        truncation,
    )
    write_csv(
        args.output_dir / "deadline_fairness.csv",
        fairness,
        (
            "protocol",
            "priority",
            "resolved_budget",
            "p_a_c",
            "p_b_d",
            "p_either",
            "aggregate_throughput",
            "a_c_latency_p95_given_success",
            "a_c_latency_p99_given_success",
            "b_d_latency_p95_given_success",
            "b_d_latency_p99_given_success",
            "either_latency_p95_given_success",
            "either_latency_p99_given_success",
            "jain_index",
        ),
    )
    print_fairness(fairness)

    quality_paths = {}
    quality_rows = []
    if not args.static_only:
        quality_goals = [
            goal
            for goal in goals
            if not (args.skip_union_quality and goal.name == "either")
        ]
        if args.skip_union_quality and any(
            goal.name == "either" for goal in goals
        ):
            print(
                "Skipping pure/mixed QMDP analyses for the union objective; "
                "static union CDF certification is retained."
            )
        quality_cases = [
            (protocol, priority, goal)
            for protocol in protocols
            for priority in priorities
            for goal in quality_goals
        ]
        with ThreadPoolExecutor(max_workers=args.jobs) as executor:
            quality_results = executor.map(
                lambda case: obtain_quality_case(args, case),
                quality_cases,
            )
            for (
                protocol,
                priority,
                goal,
                pure_path,
                pure_elapsed,
                mixed_path,
                mixed_elapsed,
            ) in quality_results:
                assert_extrema_coincide(
                        f"{protocol.name}/{priority.name}/{goal.name}",
                        QMDP_MODE,
                        PURE_EVENT,
                        pure_path,
                )
                assert_extrema_coincide(
                        f"{protocol.name}/{priority.name}/{goal.name}",
                        QMDP_MODE,
                        MIXED_EVENT,
                        mixed_path,
                )
                split_difference = certify_static_quality_split(
                    static_paths[(protocol.name, priority.name, goal.name)],
                    pure_path,
                    mixed_path,
                    f"{protocol.name}/{priority.name}/{goal.name}",
                    args.verification_atol,
                )
                metrics = quality_metrics(pure_path, mixed_path)
                quality_paths[
                    (protocol.name, priority.name, goal.name)
                ] = (pure_path, mixed_path)
                quality_rows.append(
                    {
                        "protocol": protocol.name,
                        "priority": priority.name,
                        "goal": goal.name,
                        "resolved_budget": args.quality_truncation,
                        "split_status": "exact",
                        "split_max_abs_diff": f"{split_difference:.15g}",
                        "coverage": f"{metrics['coverage']:.15g}",
                        "mean_waiting_time": f"{metrics['mean_waiting_time']:.15g}",
                        "mean_werner": f"{metrics['mean_werner']:.15g}",
                        "skr": f"{metrics['skr']:.15g}",
                        "pure_json_path": str(pure_path),
                        "mixed_json_path": str(mixed_path),
                    }
                )
                for mode, event, elapsed, path in (
                    (QMDP_MODE, PURE_EVENT, pure_elapsed, pure_path),
                    (QMDP_MODE, MIXED_EVENT, mixed_elapsed, mixed_path),
                ):
                    if elapsed is not None:
                        timing_rows.append(
                            {
                                "protocol": protocol.name,
                                "priority": priority.name,
                                "goal": goal.name,
                                "mode": mode,
                                "event": event,
                                "seconds": f"{elapsed:.6f}",
                                "json_path": str(path),
                            }
                        )

        write_csv(
            args.output_dir / "quality_skr_summary.csv",
            quality_rows,
            (
                "protocol",
                "priority",
                "goal",
                "resolved_budget",
                "split_status",
                "split_max_abs_diff",
                "coverage",
                "mean_waiting_time",
                "mean_werner",
                "skr",
                "pure_json_path",
                "mixed_json_path",
            ),
        )

        aggregate_quality = {
            (row["protocol"], row["priority"]): row
            for row in quality_rows
            if row["goal"] == "either"
        }
        policy_metric_rows = []
        for deadline_row in fairness:
            key = (deadline_row["protocol"], deadline_row["priority"])
            quality_row = aggregate_quality.get(key)
            if quality_row is None:
                continue
            policy_metric_rows.append(
                {
                    **deadline_row,
                    "quality_resolved_budget": quality_row["resolved_budget"],
                    "aggregate_mean_werner": quality_row["mean_werner"],
                    "aggregate_skr": quality_row["skr"],
                }
            )
        if policy_metric_rows:
            write_csv(
                args.output_dir / "policy_metrics_summary.csv",
                policy_metric_rows,
                (
                    "protocol",
                    "priority",
                    "resolved_budget",
                    "p_a_c",
                    "p_b_d",
                    "p_either",
                    "aggregate_throughput",
                    "a_c_latency_p95_given_success",
                    "a_c_latency_p99_given_success",
                    "b_d_latency_p95_given_success",
                    "b_d_latency_p99_given_success",
                    "either_latency_p95_given_success",
                    "either_latency_p99_given_success",
                    "jain_index",
                    "quality_resolved_budget",
                    "aggregate_mean_werner",
                    "aggregate_skr",
                ),
            )

        plt = configure_matplotlib(args.plot_profile)
        plot_profile = get_plot_profile(args.plot_profile)
        werner_paths = goal_priority_quality_paths(quality_paths, protocols)
        if werner_paths:
            plot_werner(
                plt,
                args.figure_dir,
                werner_paths,
                plot_profile,
                bin_size=args.werner_binning,
                plot_truncation=args.quality_plot_truncation,
            )
            if args.joint_protocols_cdf_werner:
                plot_joint_protocol_cdf_werner(
                    plt,
                    args.figure_dir,
                    args.reference_dir,
                    protocols,
                    werner_paths,
                    plot_profile,
                    bin_size=args.werner_binning,
                    no_shades=args.no_shades,
                    plot_truncation=args.plot_truncation,
                    quality_plot_truncation=args.quality_plot_truncation,
                )

        print("\nFixed-policy quality and secret-key rates:")
        print(
            f"{'Protocol':<15} {'Priority':<9} {'Goal':<6} "
            f"{'Mean w':<12} {'SKR':<14}"
        )
        print("-" * 62)
        for row in quality_rows:
            print(
                f"{row['protocol']:<15} {row['priority']:<9} {row['goal']:<6} "
                f"{float(row['mean_werner']):<12.6f} "
                f"{float(row['skr']):<14.6g}"
            )

    if timing_rows:
        write_csv(
            args.output_dir / "timings.csv",
            timing_rows,
            (
                "protocol",
                "priority",
                "goal",
                "mode",
                "event",
                "seconds",
                "json_path",
            ),
        )
    elif args.plots_only:
        print("--plots-only: timings.csv was not rewritten.")


if __name__ == "__main__":
    main()
