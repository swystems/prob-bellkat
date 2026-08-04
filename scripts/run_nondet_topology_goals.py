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
    NONDET_HEIGHT_INCHES,
    NONDET_LINE_WIDTH_INCHES,
    PLOT_SETTINGS,
    TIME_AXIS_LABEL,
    get_plot_profile,
    output_path,
    save_figure,
)
from scripts.plot.plot_extremal import (
    configure_matplotlib,
    derive_plot_series,
    load_extremal_payload,
    load_extremal_series,
    plot_cdf,
    plot_pmf,
    style_axes,
)


MDP_MODE = "mdp"
STATIC_EVENT = "static"
DEFAULT_TRUNCATION = 100
LINE_ALPHA = 0.82
BAND_ALPHA = 0.14
MIN_BOUNDARY_LINEWIDTH = 0.65
MAX_BOUNDARY_LINEWIDTH = 1.5
PLOT_KINDS = ("pmf", "cdf", "both")
CDF_Y_TICKS = (0.0, 0.25, 0.5, 0.75, 1.0)


@dataclass(frozen=True)
class Goal:
    name: str
    label: str
    color: str


GOALS = (
    Goal("a-c", r"$A\simeq C$", "#005AB5"),
    Goal("b-d", r"$B\simeq D$", "#DC3220"),
    Goal(
        "either",
        r"$A\simeq C\ \mathrm{or}\ B\simeq D$",
        "#111111",
    ),
)
GOAL_BY_NAME = {goal.name: goal for goal in GOALS}


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Run the nondeterministic topology protocol against several static "
            "goal objectives and plot the extremal reachability bands."
        )
    )
    parser.add_argument(
        "--event",
        "--goal",
        dest="events",
        action="append",
        choices=tuple(GOAL_BY_NAME),
        help="Static objective event to evaluate. Can be passed more than once; defaults to all events.",
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
            "Resolve --coverage-event until the worst-scheduler CDF reaches this "
            "coverage, then evaluate all selected --event objectives at that horizon. "
            "With --adapt-loop-test, resolve coverage separately for each selected event."
        ),
    )
    parser.add_argument(
        "--coverage-event",
        choices=tuple(GOAL_BY_NAME),
        default="either",
        help="Static objective event used only to choose the coverage horizon. Defaults to either.",
    )
    parser.add_argument(
        "--adapt-loop-test",
        action="store_true",
        help=(
            "Use each selected event as the protocol loop exit condition. "
            "With --coverage, each event gets its own resolved horizon."
        ),
    )
    parser.add_argument(
        "--output-dir",
        default="output/nondet-topology-goals",
        help="Directory for JSON dumps and CSV summaries.",
    )
    parser.add_argument(
        "--figure-dir",
        default="output/nondet-topology-goals",
        help="Directory for per-goal and joint figures.",
    )
    parser.add_argument(
        "--plot-kind",
        choices=PLOT_KINDS,
        default="both",
        help="Plot PMF bands, CDF bands, or both in the joint figure.",
    )
    parser.add_argument(
        "--plot-truncation",
        type=int,
        default=None,
        help=(
            "Display the joint plots only through this time, without changing "
            "the computed model horizon."
        ),
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=DEFAULT_PROFILE,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--executable",
        default="quantP_compare_nondet_goals",
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
        help="Plot only the lower and upper boundaries, without shaded bands.",
    )
    parser.add_argument(
        "--no-legend",
        action="store_true",
        help="Omit the legend from the joint PMF/CDF band figures.",
    )
    parser.add_argument(
        "--no-y-axis-label",
        action="store_true",
        help="Omit the y-axis label from the joint PMF/CDF band figures.",
    )
    parser.add_argument(
        "--no-y-ticks",
        action="store_true",
        help="Omit y-axis ticks and tick labels from the joint PMF/CDF band figures.",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip the initial cabal build step.",
    )
    return parser.parse_args()


def validate_probability(name, value, *, strict_lower=False):
    if value is None:
        return
    lower_ok = value > 0.0 if strict_lower else value >= 0.0
    if not lower_ok or value > 1.0:
        interval = "(0, 1]" if strict_lower else "[0, 1]"
        raise SystemExit(f"{name} must be in the interval {interval}.")


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


def selected_goals(args):
    names = args.events or [goal.name for goal in GOALS]
    names = list(dict.fromkeys(names))
    return [GOAL_BY_NAME[name] for name in names]


def scenario_args(args):
    flags = ["--p-swap", str(args.p_swap), "--t-coh", str(args.t_coh)]
    if args.adapt_loop_test:
        flags.append("--adapt-loop-test")
    if args.p_gen_override is not None:
        flags.extend(["--p-gen-override", str(args.p_gen_override)])
    if args.w0_override is not None:
        flags.extend(["--w0-override", str(args.w0_override)])
    return flags


def truncation_args(truncation):
    return ["--truncation", str(truncation)]


def coverage_args(coverage):
    return ["--coverage", str(coverage)]


def goal_json_path(output_dir, goal):
    return output_dir / f"nondet_topology_goals_{goal.name}_{MDP_MODE}_{STATIC_EVENT}.json"


def coverage_json_path(output_dir, goal):
    return output_dir / f"nondet_topology_goals_coverage_{goal.name}_{MDP_MODE}_{STATIC_EVENT}.json"


def existing_goal_json_path(output_dir, goal):
    path = goal_json_path(output_dir, goal)
    ok, reason = validate_extremal_json(path, require_coverage=False)
    if not ok:
        raise SystemExit(
            f"Missing existing JSON for {goal.name}: {path} ({reason}). "
            "Run without --plots-only first, or adjust --output-dir."
        )
    return path


def run_goal(args, goal, output_dir, truncation):
    json_path = goal_json_path(output_dir, goal)
    command = [
        *executable_command(args.executable),
        "--event",
        goal.name,
        *scenario_args(args),
        "--json",
        MDP_MODE,
        "--compute-extremal",
        *truncation_args(truncation),
    ]
    status = f"{goal.name} {MDP_MODE}/{STATIC_EVENT}"
    elapsed = run_command(command, stdout_path=json_path, status_label=status)
    return json_path, elapsed


def run_goal_coverage(args, goal, output_dir):
    json_path = goal_json_path(output_dir, goal)
    command = [
        *executable_command(args.executable),
        "--event",
        goal.name,
        *scenario_args(args),
        "--json",
        MDP_MODE,
        "--compute-extremal",
        *coverage_args(args.coverage),
    ]
    status = f"{goal.name} {MDP_MODE}/{STATIC_EVENT} adaptive coverage"
    elapsed = run_command(command, stdout_path=json_path, status_label=status)
    return json_path, elapsed


def run_coverage(args, goal, output_dir):
    json_path = coverage_json_path(output_dir, goal)
    command = [
        *executable_command(args.executable),
        "--event",
        goal.name,
        *scenario_args(args),
        "--json",
        MDP_MODE,
        "--compute-extremal",
        *coverage_args(args.coverage),
    ]
    status = f"coverage {goal.name} {MDP_MODE}/{STATIC_EVENT}"
    elapsed = run_command(command, stdout_path=json_path, status_label=status)
    return json_path, elapsed


def coverage_budget(goal, coverage, json_path):
    payload = load_extremal_payload(json_path)
    status, value = coverage_status(payload)
    if status != "reached":
        raise SystemExit(
            f"Coverage event {goal.name!r} did not reach {coverage}: "
            f"status={status or 'missing'}, value={value or 'missing'}."
        )
    budget = payload.get("resolved_budget")
    if not isinstance(budget, int):
        raise SystemExit(f"Coverage JSON does not contain an integer resolved_budget: {json_path}")
    return budget, value


def plot_goal_extremal(plt, figure_dir, goal, json_path, plot_profile):
    series = load_extremal_series(json_path)
    t, pmf_min, pmf_max, cdf_min, cdf_max = derive_plot_series(series)
    file_stem = f"nondet_topology_goals_{goal.name}"
    figure_size = (NONDET_LINE_WIDTH_INCHES, NONDET_HEIGHT_INCHES)
    plot_pmf(
        plt,
        figure_dir,
        file_stem,
        t,
        pmf_min,
        pmf_max,
        plot_profile,
        figure_size=figure_size,
        bbox_inches=None,
    )
    plot_cdf(
        plt,
        figure_dir,
        file_stem,
        t,
        cdf_min,
        cdf_max,
        plot_profile,
        figure_size=figure_size,
        bbox_inches=None,
    )


def band_series(series, plot_kind):
    t, pmf_min, pmf_max, cdf_min, cdf_max = derive_plot_series(series)
    if plot_kind == "cdf":
        return t, cdf_min, cdf_max
    return t, pmf_min, pmf_max


def goal_legend_handles(goals):
    from matplotlib.lines import Line2D

    return [
        Line2D(
            [0],
            [0],
            color=goal.color,
            alpha=LINE_ALPHA,
            linestyle="-",
            linewidth=1.0,
            label=goal.label,
        )
        for goal in goals
    ]


def configure_probability_y_axis(
    ax,
    plot_kind,
    *,
    no_y_axis_label=False,
    no_y_ticks=False,
):
    ax.set_ylim(bottom=0.0)
    if plot_kind == "cdf":
        ax.set_ylim(0.0, 1.0)
        ax.set_yticks(CDF_Y_TICKS)
        label = "Cumulative probability"
    else:
        label = "Probability"

    ax.set_ylabel("" if no_y_axis_label else label)
    if no_y_ticks:
        ax.tick_params(axis="y", which="both", left=False, labelleft=False)


def plot_joint_bands(
    plt,
    figure_dir,
    goal_paths,
    plot_kind,
    plot_profile,
    *,
    no_shades=False,
    no_legend=False,
    no_y_axis_label=False,
    no_y_ticks=False,
    plot_truncation=None,
):
    fig, ax = plt.subplots(
        figsize=(NONDET_LINE_WIDTH_INCHES, NONDET_HEIGHT_INCHES)
    )
    draw_joint_bands(
        ax,
        goal_paths,
        plot_kind,
        no_shades=no_shades,
        no_legend=no_legend,
        no_y_axis_label=no_y_axis_label,
        no_y_ticks=no_y_ticks,
        plot_truncation=plot_truncation,
    )

    figure_path = output_path(
        figure_dir,
        "nondet_topology_goals",
        f"{plot_kind}_bands",
        plot_profile,
    )
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    print(f"Saved joint {plot_kind.upper()} band figure to {figure_path}")
    return figure_path


def draw_joint_bands(
    ax,
    goal_paths,
    plot_kind,
    *,
    no_shades=False,
    no_legend=False,
    no_y_axis_label=False,
    no_y_ticks=False,
    show_x_axis=True,
    plot_truncation=None,
):
    for goal, json_path in goal_paths:
        series = load_extremal_series(json_path)
        t, lower, upper = band_series(series, plot_kind)
        if not no_shades:
            ax.fill_between(
                t,
                lower,
                upper,
                color=goal.color,
                alpha=BAND_ALPHA,
                linewidth=0,
                label=goal.label,
            )
        ax.plot(
            t,
            lower,
            color=goal.color,
            alpha=LINE_ALPHA,
            linestyle="-",
            linewidth=MIN_BOUNDARY_LINEWIDTH,
        )
        ax.plot(
            t,
            upper,
            color=goal.color,
            alpha=LINE_ALPHA,
            linestyle="-",
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
    ax.margins(x=0)
    if plot_truncation is not None:
        ax.set_xlim(0, plot_truncation)
    if not no_legend:
        ax.legend(
            handles=goal_legend_handles([goal for goal, _ in goal_paths]),
            frameon=False,
            loc="best",
        )


def resolved_budget(payload):
    value = payload.get("resolved_budget")
    return "" if value is None else str(value)


def coverage_status(payload):
    status = payload.get("coverage_status")
    if not isinstance(status, dict):
        return "", ""
    return str(status.get("status", "")), str(status.get("value", ""))


def summarize_goal(goal, json_path, elapsed):
    payload = load_extremal_payload(json_path)
    series = payload["series"]
    cdf_min = series["cdf_min"]
    cdf_max = series["cdf_max"]
    status, value = coverage_status(payload)
    return {
        "goal": goal.name,
        "label": goal.label.replace("$", ""),
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
                "goal",
                "label",
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
    if isinstance(rows, dict):
        rows = [rows]
    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=(
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
    print(f"{'Goal':<6} {'R':<7} {'CDF min':<14} {'CDF max':<14} {'Time':<10}")
    print("-" * 57)
    for row in rows:
        seconds = row["seconds"]
        duration = format_duration(float(seconds)) if seconds else "loaded"
        print(
            f"{row['goal']:<6} {row['resolved_budget']:<7} "
            f"{row['cdf_min_final']:<14} {row['cdf_max_final']:<14} {duration:<10}"
        )


def main():
    args = parse_args()
    validate_args(args)
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

    plt = configure_matplotlib(args.plot_profile)
    truncation = args.truncation
    coverage_rows = []
    if args.coverage is not None and args.adapt_loop_test and not args.plots_only:
        print(
            "--adapt-loop-test: resolving coverage separately for each selected event; "
            "--coverage-event is ignored."
        )
    elif args.coverage is not None and not args.plots_only:
        coverage_goal = GOAL_BY_NAME[args.coverage_event]
        coverage_path, coverage_elapsed = run_coverage(args, coverage_goal, output_dir)
        truncation, coverage_value = coverage_budget(coverage_goal, args.coverage, coverage_path)
        write_coverage_summary(
            output_dir / "nondet_topology_goals_coverage_summary.csv",
            {
                "coverage_event": coverage_goal.name,
                "target": f"{args.coverage:.12g}",
                "resolved_budget": str(truncation),
                "coverage_status": "reached",
                "coverage_value": coverage_value,
                "seconds": f"{coverage_elapsed:.6f}",
                "json_path": str(coverage_path),
            },
        )
        print(
            f"coverage {coverage_goal.name} {MDP_MODE}/{STATIC_EVENT}: "
            f"R={truncation}, value={coverage_value}, {coverage_elapsed:.2f}s -> {coverage_path}"
        )
    elif args.coverage is not None:
        print("--plots-only: --coverage/--coverage-event are ignored; loading existing event JSON.")

    goal_paths = []
    rows = []
    for goal in goals:
        if args.plots_only:
            json_path = existing_goal_json_path(output_dir, goal)
            elapsed = None
            print(f"{goal.name} {MDP_MODE}/{STATIC_EVENT}: loaded {json_path}")
        elif args.coverage is not None and args.adapt_loop_test:
            json_path, elapsed = run_goal_coverage(args, goal, output_dir)
            event_truncation, coverage_value = coverage_budget(goal, args.coverage, json_path)
            coverage_rows.append(
                {
                    "coverage_event": goal.name,
                    "target": f"{args.coverage:.12g}",
                    "resolved_budget": str(event_truncation),
                    "coverage_status": "reached",
                    "coverage_value": coverage_value,
                    "seconds": f"{elapsed:.6f}",
                    "json_path": str(json_path),
                }
            )
            print(
                f"{goal.name} {MDP_MODE}/{STATIC_EVENT}: "
                f"R={event_truncation}, value={coverage_value}, {elapsed:.2f}s -> {json_path}"
            )
        else:
            json_path, elapsed = run_goal(args, goal, output_dir, truncation)
            print(f"{goal.name} {MDP_MODE}/{STATIC_EVENT}: {elapsed:.2f}s -> {json_path}")

        plot_goal_extremal(plt, figure_dir, goal, json_path, plot_profile)
        goal_paths.append((goal, json_path))
        rows.append(summarize_goal(goal, json_path, elapsed))

    if coverage_rows:
        write_coverage_summary(
            output_dir / "nondet_topology_goals_coverage_summary.csv",
            coverage_rows,
        )

    if args.plot_kind == "both":
        plot_joint_bands(
            plt,
            figure_dir,
            goal_paths,
            "pmf",
            plot_profile,
            no_shades=args.no_shades,
            no_legend=args.no_legend,
            no_y_axis_label=args.no_y_axis_label,
            no_y_ticks=args.no_y_ticks,
            plot_truncation=args.plot_truncation,
        )
        plot_joint_bands(
            plt,
            figure_dir,
            goal_paths,
            "cdf",
            plot_profile,
            no_shades=args.no_shades,
            no_legend=args.no_legend,
            no_y_axis_label=args.no_y_axis_label,
            no_y_ticks=args.no_y_ticks,
            plot_truncation=args.plot_truncation,
        )
    else:
        plot_joint_bands(
            plt,
            figure_dir,
            goal_paths,
            args.plot_kind,
            plot_profile,
            no_shades=args.no_shades,
            no_legend=args.no_legend,
            no_y_axis_label=args.no_y_axis_label,
            no_y_ticks=args.no_y_ticks,
            plot_truncation=args.plot_truncation,
        )

    write_summary(output_dir / "nondet_topology_goals_summary.csv", rows)
    print_summary(rows)


if __name__ == "__main__":
    main()
