#!/usr/bin/env python3

import argparse
import csv
import sys
from dataclasses import dataclass
from pathlib import Path

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from scripts.analysis.swap_comparison.common import (
    build_command,
    executable_command,
    format_duration,
    run_command,
    validate_extremal_json,
)
from scripts.plot.config import (
    DEFAULT_PROFILE,
    JOINT_PLOTS_WSPACE,
    LINE_WIDTH_INCHES,
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
    load_extremal_payload,
    load_extremal_series,
    style_axes,
)
from scripts.run_nondet_topology_goals import (
    GOAL_BY_NAME,
    MAX_BOUNDARY_LINEWIDTH,
    MIN_BOUNDARY_LINEWIDTH,
    band_series,
    configure_probability_y_axis,
    coverage_budget,
    coverage_status,
    draw_joint_bands as draw_goal_joint_bands,
    existing_goal_json_path,
    goal_legend_handles,
    resolved_budget,
    selected_goals,
    validate_probability,
)


MDP_MODE = "mdp"
STATIC_EVENT = "static"
DEFAULT_TRUNCATION = 100
LINE_ALPHA = 0.82
BAND_ALPHA = 0.14
JOINT_LAYOUTS = ("stacked", "side-by-side", "both")
NONDET_STACKED_HSPACE = 0.025


@dataclass(frozen=True)
class Protocol:
    name: str
    label: str
    color: str
    linestyle: str
    hatch: str


PROTOCOLS = (
    Protocol("left-to-right", "L2R", "#005AB5", "-", ""),
    Protocol("right-to-left", "R2L", "#DC3220", "--", "."),
)
PROTOCOL_BY_NAME = {protocol.name: protocol for protocol in PROTOCOLS}


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Compare left-to-right and right-to-left swapping on the "
            "non-deterministic butterfly topology for goals G1, G2, and G3. "
            "Both protocols use the fixed loop condition alpha = A~C or B~D."
        )
    )
    parser.add_argument(
        "--protocol",
        dest="protocols",
        action="append",
        choices=tuple(PROTOCOL_BY_NAME),
        help="Protocol to evaluate. Can be passed more than once; defaults to both.",
    )
    parser.add_argument(
        "--event",
        "--goal",
        dest="events",
        action="append",
        choices=tuple(GOAL_BY_NAME),
        help="Goal event to evaluate. Can be passed more than once; defaults to G1, G2, and G3.",
    )
    budget_group = parser.add_mutually_exclusive_group()
    budget_group.add_argument(
        "--truncation",
        type=int,
        default=DEFAULT_TRUNCATION,
        help=f"Extremal reachability truncation budget. Defaults to {DEFAULT_TRUNCATION}.",
    )
    budget_group.add_argument(
        "--coverage",
        type=float,
        default=None,
        help=(
            "Resolve the horizon for each protocol using --coverage-event and "
            "then evaluate every protocol/goal pair at the maximum resolved horizon."
        ),
    )
    parser.add_argument(
        "--coverage-event",
        choices=tuple(GOAL_BY_NAME),
        default="either",
        help="Goal used to resolve coverage horizons. Defaults to G3 (either).",
    )
    parser.add_argument(
        "--output-dir",
        default="output/nondet-topology-protocols",
        help="Directory for JSON dumps and CSV summaries.",
    )
    parser.add_argument(
        "--figure-dir",
        default="output/nondet-topology-protocols",
        help="Directory for per-goal and joint CDF protocol-comparison figures.",
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=DEFAULT_PROFILE,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--executable",
        default="quantP_compare_nondet_protocols",
        help="Cabal executable name, or path to an already-built executable.",
    )
    parser.add_argument(
        "--p-gen-override",
        type=float,
        default=None,
        help="Override all elementary generation probabilities, useful for smoke tests.",
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
        default=720000,
        help="Memory coherence time in L0/c units. Defaults to 720000.",
    )
    parser.add_argument(
        "--plots-only",
        action="store_true",
        help="Skip Cabal runs and regenerate figures from existing JSON dumps.",
    )
    parser.add_argument(
        "--no-shade",
        "--no-shades",
        dest="no_shades",
        action="store_true",
        help="Plot only the lower and upper CDF boundaries, without shaded bands.",
    )
    parser.add_argument(
        "--protocol-legend-only",
        action="store_true",
        help="Show only the protocol legend in the joint CDF comparison.",
    )
    parser.add_argument(
        "--joint-goals-dir",
        "--stacked-goals-dir",
        dest="joint_goals_dir",
        type=Path,
        default=None,
        help=(
            "Directory containing existing adapted-loop goal JSON files. "
            "When supplied, also create the joint evaluation figure selected "
            "by --joint-layout."
        ),
    )
    parser.add_argument(
        "--joint-layout",
        choices=JOINT_LAYOUTS,
        default="stacked",
        help=(
            "Layout for the joint goal/protocol evaluation figure. Defaults "
            "to stacked; both writes separate stacked and side-by-side PDFs."
        ),
    )
    parser.add_argument(
        "--plot-truncation",
        type=int,
        default=None,
        help=(
            "Display plots only through this time, without changing the "
            "computed model horizon. Joint panels use it as their x-axis "
            "maximum."
        ),
    )
    parser.add_argument(
        "--no-y-axis-label",
        action="store_true",
        help="Omit the y-axis label from the CDF comparison figures.",
    )
    parser.add_argument(
        "--no-y-ticks",
        action="store_true",
        help="Omit y-axis ticks and tick labels from the CDF comparison figures.",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip the initial cabal build step.",
    )
    return parser.parse_args()


def validate_args(args):
    if args.coverage is not None:
        validate_probability("--coverage", args.coverage, strict_lower=True)
    elif args.truncation < 0:
        raise SystemExit("--truncation must be a non-negative integer.")
    validate_probability("--p-gen-override", args.p_gen_override)
    validate_probability("--p-swap", args.p_swap)
    validate_probability("--w0-override", args.w0_override)
    if args.t_coh <= 0:
        raise SystemExit("--t-coh must be positive.")
    if args.plot_truncation is not None and args.plot_truncation <= 0:
        raise SystemExit("--plot-truncation must be a positive integer.")


def selected_protocols(args):
    names = args.protocols or [protocol.name for protocol in PROTOCOLS]
    names = list(dict.fromkeys(names))
    return [PROTOCOL_BY_NAME[name] for name in names]


def scenario_args(args):
    flags = ["--p-swap", str(args.p_swap), "--t-coh", str(args.t_coh)]
    if args.p_gen_override is not None:
        flags.extend(["--p-gen-override", str(args.p_gen_override)])
    if args.w0_override is not None:
        flags.extend(["--w0-override", str(args.w0_override)])
    return flags


def result_json_path(output_dir, protocol, goal):
    return output_dir / (
        f"nondet_topology_protocols_{protocol.name}_{goal.name}_"
        f"{MDP_MODE}_{STATIC_EVENT}.json"
    )


def coverage_json_path(output_dir, protocol, goal):
    return output_dir / (
        f"nondet_topology_protocols_coverage_{protocol.name}_{goal.name}_"
        f"{MDP_MODE}_{STATIC_EVENT}.json"
    )


def existing_result_json_path(output_dir, protocol, goal):
    path = result_json_path(output_dir, protocol, goal)
    ok, reason = validate_extremal_json(path, require_coverage=False)
    if not ok:
        raise SystemExit(
            f"Missing existing JSON for {protocol.name}/{goal.name}: {path} ({reason}). "
            "Run without --plots-only first, or adjust --output-dir."
        )
    return path


def analysis_command(args, protocol, goal, budget_flag, budget_value):
    return [
        *executable_command(args.executable),
        "--protocol",
        protocol.name,
        "--event",
        goal.name,
        *scenario_args(args),
        "--json",
        MDP_MODE,
        "--compute-extremal",
        budget_flag,
        str(budget_value),
    ]


def run_result(args, protocol, goal, output_dir, truncation):
    path = result_json_path(output_dir, protocol, goal)
    status = f"{protocol.name}/{goal.name} {MDP_MODE}/{STATIC_EVENT}"
    elapsed = run_command(
        analysis_command(args, protocol, goal, "--truncation", truncation),
        stdout_path=path,
        status_label=status,
    )
    return path, elapsed


def run_coverage(args, protocol, goal, output_dir):
    path = coverage_json_path(output_dir, protocol, goal)
    status = f"coverage {protocol.name}/{goal.name} {MDP_MODE}/{STATIC_EVENT}"
    elapsed = run_command(
        analysis_command(args, protocol, goal, "--coverage", args.coverage),
        stdout_path=path,
        status_label=status,
    )
    return path, elapsed


def fill_protocol_band(ax, t, lower, upper, color, protocol):
    from matplotlib.colors import to_rgba

    return ax.fill_between(
        t,
        lower,
        upper,
        facecolor="none" if protocol.hatch else to_rgba(color, BAND_ALPHA),
        edgecolor=color if protocol.hatch else "none",
        hatch=protocol.hatch or None,
        hatch_linewidth=0.35,
        linestyle=protocol.linestyle,
        linewidth=0.25 if protocol.hatch else 0,
    )


def protocol_band_handles(protocols, color_for, no_shades=False):
    from matplotlib.colors import to_rgba
    from matplotlib.lines import Line2D
    from matplotlib.patches import Patch

    if no_shades:
        return [
            Line2D(
                [0],
                [0],
                color=color_for(protocol),
                linestyle=protocol.linestyle,
                linewidth=1.5,
                label=protocol.label,
            )
            for protocol in protocols
        ]

    return [
        Patch(
            facecolor=(
                "none"
                if protocol.hatch
                else to_rgba(color_for(protocol), BAND_ALPHA)
            ),
            edgecolor=color_for(protocol),
            hatch=protocol.hatch or None,
            linestyle=protocol.linestyle,
            linewidth=MAX_BOUNDARY_LINEWIDTH,
            label=protocol.label,
        )
        for protocol in protocols
    ]


def plot_goal_bands(
    plt,
    figure_dir,
    goal,
    protocol_paths,
    plot_kind,
    plot_profile,
    no_shades=False,
    no_y_axis_label=False,
    no_y_ticks=False,
    plot_truncation=None,
):
    fig, ax = plt.subplots(
        figsize=(NONDET_LINE_WIDTH_INCHES, NONDET_HEIGHT_INCHES)
    )
    for protocol, json_path in protocol_paths:
        series = load_extremal_series(json_path)
        t, lower, upper = band_series(series, plot_kind)
        if not no_shades:
            fill_protocol_band(ax, t, lower, upper, protocol.color, protocol)
        ax.plot(
            t,
            lower,
            color=protocol.color,
            alpha=LINE_ALPHA,
            linestyle=protocol.linestyle,
            linewidth=MIN_BOUNDARY_LINEWIDTH,
        )
        ax.plot(
            t,
            upper,
            color=protocol.color,
            alpha=LINE_ALPHA,
            linestyle=protocol.linestyle,
            linewidth=MAX_BOUNDARY_LINEWIDTH,
        )

    ax.set_xlabel(TIME_AXIS_LABEL)
    configure_probability_y_axis(
        ax,
        plot_kind,
        no_y_axis_label=no_y_axis_label,
        no_y_ticks=no_y_ticks,
    )

    style_axes(ax)
    if plot_truncation is not None:
        ax.set_xlim(0, plot_truncation)
    ax.legend(
        handles=protocol_band_handles(
            [protocol for protocol, _ in protocol_paths],
            lambda protocol: protocol.color,
            no_shades=no_shades,
        ),
        frameon=False,
        loc="best",
        title=goal.label,
    )

    figure_path = output_path(
        figure_dir,
        f"nondet_topology_protocols_{goal.name}",
        f"{plot_kind}_bands",
        plot_profile,
    )
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    print(f"Saved {goal.name} protocol {plot_kind.upper()} comparison to {figure_path}")
    return figure_path


def plot_joint_bands(
    plt,
    figure_dir,
    goals,
    paths_by_goal,
    plot_kind,
    plot_profile,
    no_shades=False,
    protocol_legend_only=False,
    no_y_axis_label=False,
    no_y_ticks=False,
    plot_truncation=None,
):
    fig, ax = plt.subplots(
        figsize=(NONDET_LINE_WIDTH_INCHES, NONDET_HEIGHT_INCHES)
    )
    draw_joint_bands(
        ax,
        goals,
        paths_by_goal,
        plot_kind,
        no_shades=no_shades,
        protocol_legend_only=protocol_legend_only,
        no_y_axis_label=no_y_axis_label,
        no_y_ticks=no_y_ticks,
        plot_truncation=plot_truncation,
    )

    figure_path = output_path(
        figure_dir,
        "nondet_topology_protocols",
        f"{plot_kind}_bands",
        plot_profile,
    )
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    print(f"Saved joint protocol/goal {plot_kind.upper()} comparison to {figure_path}")
    return figure_path


def draw_joint_bands(
    ax,
    goals,
    paths_by_goal,
    plot_kind,
    *,
    no_shades=False,
    protocol_legend_only=False,
    no_y_axis_label=False,
    no_y_ticks=False,
    show_x_axis=True,
    plot_truncation=None,
):

    for goal in goals:
        for protocol, json_path in paths_by_goal[goal.name]:
            series = load_extremal_series(json_path)
            t, lower, upper = band_series(series, plot_kind)
            if not no_shades:
                fill_protocol_band(ax, t, lower, upper, goal.color, protocol)
            ax.plot(
                t,
                lower,
                color=goal.color,
                alpha=LINE_ALPHA,
                linestyle=protocol.linestyle,
                linewidth=MIN_BOUNDARY_LINEWIDTH,
            )
            ax.plot(
                t,
                upper,
                color=goal.color,
                alpha=LINE_ALPHA,
                linestyle=protocol.linestyle,
                linewidth=MAX_BOUNDARY_LINEWIDTH,
            )

    ax.set_xlabel(TIME_AXIS_LABEL if show_x_axis else "")
    if not show_x_axis:
        ax.tick_params(axis="x", which="both", bottom=False, labelbottom=False)
    configure_probability_y_axis(
        ax,
        plot_kind,
        no_y_axis_label=no_y_axis_label,
        no_y_ticks=no_y_ticks,
    )

    style_axes(ax)
    if plot_truncation is not None:
        ax.set_xlim(0, plot_truncation)
    protocol_handles = protocol_band_handles(
        [protocol for protocol, _ in paths_by_goal[goals[0].name]],
        lambda _protocol: "#777777",
        no_shades=no_shades,
    )
    if not protocol_legend_only:
        goal_legend = ax.legend(
            handles=goal_legend_handles(goals),
            frameon=False,
            loc="upper left",
        )
        ax.add_artist(goal_legend)
    ax.legend(
        handles=protocol_handles,
        frameon=False,
        loc="best" if protocol_legend_only else "lower right",
    )


def add_shared_cdf_y_label(fig, *, no_y_axis_label, x):
    if not no_y_axis_label:
        fig.supylabel("Cumulative probability", x=x, fontsize=8)


def hide_stacked_inner_y_ticks(fig, upper_ax, lower_ax):
    """Hide the two endpoint ticks that meet between stacked panels."""
    fig.canvas.draw()
    upper_ticks = upper_ax.yaxis.get_major_ticks()
    lower_ticks = lower_ax.yaxis.get_major_ticks()
    if upper_ticks:
        upper_ticks[0].label1.set_visible(False)
        upper_ticks[0].tick1line.set_visible(False)
    if lower_ticks:
        lower_ticks[-1].label1.set_visible(False)
        lower_ticks[-1].tick1line.set_visible(False)


def plot_stacked_evaluation_bands(
    plt,
    figure_dir,
    goal_paths,
    goals,
    paths_by_goal,
    plot_profile,
    *,
    no_shades=False,
    no_y_axis_label=False,
    no_y_ticks=False,
    plot_truncation=None,
):
    fig, (goal_ax, protocol_ax) = plt.subplots(
        2,
        1,
        sharex=True,
        figsize=(LINE_WIDTH_INCHES, 2 * NONDET_HEIGHT_INCHES),
    )
    draw_goal_joint_bands(
        goal_ax,
        goal_paths,
        "cdf",
        no_shades=no_shades,
        no_y_axis_label=True,
        no_y_ticks=no_y_ticks,
        show_x_axis=False,
        plot_truncation=plot_truncation,
    )
    draw_joint_bands(
        protocol_ax,
        goals,
        paths_by_goal,
        "cdf",
        no_shades=no_shades,
        protocol_legend_only=True,
        no_y_axis_label=True,
        no_y_ticks=no_y_ticks,
        plot_truncation=plot_truncation,
    )
    add_shared_cdf_y_label(fig, no_y_axis_label=no_y_axis_label, x=0.025)
    if not no_y_ticks:
        hide_stacked_inner_y_ticks(fig, goal_ax, protocol_ax)
    fig.subplots_adjust(
        left=0.16,
        right=0.98,
        bottom=0.17,
        top=0.98,
        hspace=NONDET_STACKED_HSPACE,
    )

    figure_path = output_path(
        figure_dir,
        "nondet_topology_evaluation",
        "cdf_bands",
        plot_profile,
    )
    save_figure(fig, figure_path, tight_layout=False, bbox_inches=None)
    plt.close(fig)
    print(f"Saved stacked nondeterministic evaluation figure to {figure_path}")
    return figure_path


def plot_side_by_side_evaluation_bands(
    plt,
    figure_dir,
    goal_paths,
    goals,
    paths_by_goal,
    plot_profile,
    *,
    no_shades=False,
    no_y_axis_label=False,
    no_y_ticks=False,
    plot_truncation=None,
):
    fig, (goal_ax, protocol_ax) = plt.subplots(
        1,
        2,
        sharey=True,
        figsize=(
            SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES,
            SWAP_COMPARISON_COMBINED_HEIGHT_INCHES,
        ),
        gridspec_kw={"width_ratios": (1.0, 1.0), "wspace": JOINT_PLOTS_WSPACE},
    )
    draw_goal_joint_bands(
        goal_ax,
        goal_paths,
        "cdf",
        no_shades=no_shades,
        no_y_axis_label=no_y_axis_label,
        no_y_ticks=no_y_ticks,
        plot_truncation=plot_truncation,
    )
    draw_joint_bands(
        protocol_ax,
        goals,
        paths_by_goal,
        "cdf",
        no_shades=no_shades,
        protocol_legend_only=True,
        no_y_axis_label=True,
        no_y_ticks=no_y_ticks,
        plot_truncation=plot_truncation,
    )
    protocol_ax.tick_params(
        axis="y",
        which="both",
        left=False,
        labelleft=False,
    )
    fig.subplots_adjust(
        left=0.10,
        right=0.98,
        bottom=0.21,
        top=0.98,
        wspace=JOINT_PLOTS_WSPACE,
    )
    hide_overlapping_inner_x_tick_label(fig, goal_ax, protocol_ax)

    figure_path = output_path(
        figure_dir,
        "nondet_topology_evaluation",
        "cdf_bands_side_by_side",
        plot_profile,
    )
    save_figure(fig, figure_path, tight_layout=False, bbox_inches=None)
    plt.close(fig)
    print(f"Saved side-by-side nondeterministic evaluation figure to {figure_path}")
    return figure_path


def summarize_result(protocol, goal, json_path, elapsed):
    payload = load_extremal_payload(json_path)
    series = payload["series"]
    cdf_min = series["cdf_min"]
    cdf_max = series["cdf_max"]
    status, value = coverage_status(payload)
    return {
        "protocol": protocol.name,
        "protocol_label": protocol.label,
        "goal": goal.name,
        "goal_label": goal.label.replace("$", ""),
        "resolved_budget": resolved_budget(payload),
        "cdf_min_final": f"{cdf_min[-1]:.12g}" if cdf_min else "",
        "cdf_max_final": f"{cdf_max[-1]:.12g}" if cdf_max else "",
        "coverage_status": status,
        "coverage_value": value,
        "seconds": "" if elapsed is None else f"{elapsed:.6f}",
        "json_path": str(json_path),
    }


def write_summary(path, rows):
    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=(
                "protocol",
                "protocol_label",
                "goal",
                "goal_label",
                "resolved_budget",
                "cdf_min_final",
                "cdf_max_final",
                "coverage_status",
                "coverage_value",
                "seconds",
                "json_path",
            ),
        )
        writer.writeheader()
        writer.writerows(rows)


def write_coverage_summary(path, rows):
    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=(
                "protocol",
                "coverage_event",
                "target",
                "resolved_budget",
                "coverage_status",
                "coverage_value",
                "seconds",
                "json_path",
            ),
        )
        writer.writeheader()
        writer.writerows(rows)


def print_summary(rows):
    print("\nExtremal reachability summary:")
    print(f"{'Protocol':<15} {'Goal':<6} {'R':<7} {'CDF min':<14} {'CDF max':<14} {'Time':<10}")
    print("-" * 83)
    for row in rows:
        seconds = row["seconds"]
        duration = format_duration(float(seconds)) if seconds else "loaded"
        print(
            f"{row['protocol']:<15} {row['goal']:<6} {row['resolved_budget']:<7} "
            f"{row['cdf_min_final']:<14} {row['cdf_max_final']:<14} {duration:<10}"
        )


def resolve_coverage(args, protocols, output_dir):
    coverage_goal = GOAL_BY_NAME[args.coverage_event]
    rows = []
    budgets = []
    for protocol in protocols:
        json_path, elapsed = run_coverage(args, protocol, coverage_goal, output_dir)
        budget, value = coverage_budget(coverage_goal, args.coverage, json_path)
        budgets.append(budget)
        rows.append(
            {
                "protocol": protocol.name,
                "coverage_event": coverage_goal.name,
                "target": f"{args.coverage:.12g}",
                "resolved_budget": str(budget),
                "coverage_status": "reached",
                "coverage_value": value,
                "seconds": f"{elapsed:.6f}",
                "json_path": str(json_path),
            }
        )
        print(
            f"coverage {protocol.name}/{coverage_goal.name}: "
            f"R={budget}, value={value}, {elapsed:.2f}s -> {json_path}"
        )

    write_coverage_summary(
        output_dir / "nondet_topology_protocols_coverage_summary.csv",
        rows,
    )
    truncation = max(budgets)
    print(f"Using shared maximum coverage horizon R={truncation}.")
    return truncation


def main():
    args = parse_args()
    validate_args(args)
    protocols = selected_protocols(args)
    goals = selected_goals(args)
    output_dir = Path(args.output_dir)
    figure_dir = Path(args.figure_dir)
    plot_profile = get_plot_profile(args.plot_profile)

    if args.plots_only:
        if not output_dir.is_dir():
            raise SystemExit(f"--plots-only requires an existing --output-dir: {output_dir}")
    else:
        output_dir.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)

    if not args.no_build and not args.plots_only:
        command = build_command(args.executable)
        if command is not None:
            run_command(command, status_label=f"cabal build {args.executable}")

    truncation = args.truncation
    if args.coverage is not None and args.plots_only:
        print("--plots-only: --coverage/--coverage-event are ignored; loading existing result JSON.")
    elif args.coverage is not None:
        truncation = resolve_coverage(args, protocols, output_dir)

    plt = configure_matplotlib(args.plot_profile)
    paths_by_goal = {goal.name: [] for goal in goals}
    rows = []
    for protocol in protocols:
        for goal in goals:
            if args.plots_only:
                json_path = existing_result_json_path(output_dir, protocol, goal)
                elapsed = None
                print(f"{protocol.name}/{goal.name}: loaded {json_path}")
            else:
                json_path, elapsed = run_result(
                    args,
                    protocol,
                    goal,
                    output_dir,
                    truncation,
                )
                print(f"{protocol.name}/{goal.name}: {elapsed:.2f}s -> {json_path}")

            paths_by_goal[goal.name].append((protocol, json_path))
            rows.append(summarize_result(protocol, goal, json_path, elapsed))

    plot_kinds = ("cdf",)
    for goal in goals:
        for plot_kind in plot_kinds:
            plot_goal_bands(
                plt,
                figure_dir,
                goal,
                paths_by_goal[goal.name],
                plot_kind,
                plot_profile,
                no_shades=args.no_shades,
                no_y_axis_label=args.no_y_axis_label,
                no_y_ticks=args.no_y_ticks,
                plot_truncation=args.plot_truncation,
            )

    for plot_kind in plot_kinds:
        plot_joint_bands(
            plt,
            figure_dir,
            goals,
            paths_by_goal,
            plot_kind,
            plot_profile,
            no_shades=args.no_shades,
            protocol_legend_only=args.protocol_legend_only,
            no_y_axis_label=args.no_y_axis_label,
            no_y_ticks=args.no_y_ticks,
            plot_truncation=args.plot_truncation,
        )

    if args.joint_goals_dir is not None:
        goal_paths = [
            (goal, existing_goal_json_path(args.joint_goals_dir, goal))
            for goal in goals
        ]
        if args.joint_layout in ("stacked", "both"):
            plot_stacked_evaluation_bands(
                plt,
                figure_dir,
                goal_paths,
                goals,
                paths_by_goal,
                plot_profile,
                no_shades=args.no_shades,
                no_y_axis_label=args.no_y_axis_label,
                no_y_ticks=args.no_y_ticks,
                plot_truncation=args.plot_truncation,
            )
        if args.joint_layout in ("side-by-side", "both"):
            plot_side_by_side_evaluation_bands(
                plt,
                figure_dir,
                goal_paths,
                goals,
                paths_by_goal,
                plot_profile,
                no_shades=args.no_shades,
                no_y_axis_label=args.no_y_axis_label,
                no_y_ticks=args.no_y_ticks,
                plot_truncation=args.plot_truncation,
            )

    write_summary(output_dir / "nondet_topology_protocols_summary.csv", rows)
    print_summary(rows)


if __name__ == "__main__":
    main()
