#!/usr/bin/env python3

import argparse
import csv
import sys
from dataclasses import dataclass
from pathlib import Path

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from scripts.plot.config import (
    DEFAULT_PROFILE,
    LINE_WIDTH_INCHES,
    PLOT_SETTINGS,
    SWAP_COMPARISON_HEIGHT_INCHES,
    TEXT_WIDTH_INCHES,
    TIME_AXIS_LABEL,
    configure_matplotlib,
    get_plot_profile,
    output_path,
    save_figure,
)
from scripts.plot.plot_extremal import load_extremal_series
from scripts.run_nondet_topology_goals import (
    GOAL_BY_NAME,
    MAX_BOUNDARY_LINEWIDTH,
    MIN_BOUNDARY_LINEWIDTH,
)
from scripts.run_nondet_topology_protocols import PROTOCOLS
from scripts.run_nondet_topology_schedulers import (
    INDIVIDUAL_GOAL_NAMES,
    PRIORITIES,
    jain_index,
)


DEFAULT_INPUT_DIR = Path("output/nondet-topology-schedulers-cluster-1")
DEFAULT_OUTPUT_DIR = DEFAULT_INPUT_DIR
QUALITY_SUMMARY = "quality_skr_summary.csv"
POLICY_SUMMARY = "ordered_policy_metrics.csv"
LINE_ALPHA = 0.88
POLICY_X = (0.0, 1.0, 2.4, 3.4)
GOAL_MARKERS = {"a-c": "o", "b-d": "s"}


@dataclass(frozen=True)
class Policy:
    protocol_name: str
    protocol_label: str
    linestyle: str
    priority_name: str
    priority_index: int
    x: float

    @property
    def label(self):
        order = "L" if self.protocol_name == "left-to-right" else "R"
        return rf"$\mathrm{{{order}}}_{self.priority_index}$"


@dataclass(frozen=True)
class Metric:
    name: str
    column: str
    label: str
    compact_title: str
    scale: float = 1.0
    lower_bound: float | None = None
    upper_bound: float | None = None


METRICS = {
    "skr": Metric(
        "skr",
        "skr",
        r"SKR ($10^{-6}\,t_{\mathrm{unit}}^{-1}$)",
        r"SKR ($10^{-6}$)",
        scale=1e6,
        lower_bound=0.0,
    ),
    "mean-werner": Metric(
        "mean-werner",
        "mean_werner",
        r"$\overline{w}_R$",
        r"$\overline{w}_R$",
    ),
    "jain-skr": Metric(
        "jain-skr",
        "",
        r"$J_{\mathrm{SKR}}$",
        r"$J_{\mathrm{SKR}}$",
        lower_bound=0.5,
        upper_bound=1.02,
    ),
}


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Plot the ordered-policy CDF, Werner, SKR, and SKR-fairness "
            "results of the nondeterministic butterfly experiment."
        )
    )
    parser.add_argument(
        "--input-dir",
        type=Path,
        default=DEFAULT_INPUT_DIR,
        help=(
            "Directory containing ordered static JSON files and "
            f"{QUALITY_SUMMARY}. Defaults to {DEFAULT_INPUT_DIR}."
        ),
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help=f"Directory for figures and {POLICY_SUMMARY}.",
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=DEFAULT_PROFILE,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--plot-truncation",
        type=int,
        default=None,
        help="Display the CDF only through this time.",
    )
    return parser.parse_args()


def policies():
    result = []
    for protocol in PROTOCOLS:
        for priority_index, priority in enumerate(PRIORITIES, start=1):
            result.append(
                Policy(
                    protocol_name=protocol.name,
                    protocol_label=protocol.label,
                    linestyle=protocol.linestyle,
                    priority_name=priority.name,
                    priority_index=priority_index,
                    x=POLICY_X[len(result)],
                )
            )
    return result


def load_quality_metrics(input_dir):
    path = input_dir / QUALITY_SUMMARY
    if not path.is_file():
        raise SystemExit(f"Missing ordered-policy quality summary: {path}")

    rows = {}
    with open(path, encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            goal_name = row["goal"]
            if goal_name not in INDIVIDUAL_GOAL_NAMES:
                continue
            key = (row["protocol"], row["priority"], goal_name)
            if key in rows:
                raise SystemExit(f"Duplicate quality row for {key}: {path}")
            rows[key] = {
                "mean_werner": float(row["mean_werner"]),
                "skr": float(row["skr"]),
            }

    expected = {
        (policy.protocol_name, policy.priority_name, goal_name)
        for policy in policies()
        for goal_name in INDIVIDUAL_GOAL_NAMES
    }
    missing = sorted(expected - set(rows))
    if missing:
        raise SystemExit(f"Missing ordered-policy quality rows: {missing}")
    return rows


def policy_metric_rows(policy_list, quality):
    rows = []
    for policy in policy_list:
        prefix = (policy.protocol_name, policy.priority_name)
        a_c = quality[(*prefix, "a-c")]
        b_d = quality[(*prefix, "b-d")]
        rows.append(
            {
                "protocol": policy.protocol_name,
                "priority": policy.priority_name,
                "policy_label": policy.label.replace("$", ""),
                "a_c_mean_werner": a_c["mean_werner"],
                "b_d_mean_werner": b_d["mean_werner"],
                "a_c_skr": a_c["skr"],
                "b_d_skr": b_d["skr"],
                "jain_skr": jain_index([a_c["skr"], b_d["skr"]]),
            }
        )
    return rows


def write_policy_summary(path, rows):
    fieldnames = (
        "protocol",
        "priority",
        "policy_label",
        "a_c_mean_werner",
        "b_d_mean_werner",
        "a_c_skr",
        "b_d_skr",
        "jain_skr",
    )
    with open(path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def static_json_path(input_dir, policy, goal_name):
    return input_dir / (
        "nondet_topology_schedulers_"
        f"{policy.protocol_name}_{policy.priority_name}_{goal_name}_"
        "mdp_static.json"
    )


def require_static_cdf(input_dir, policy, goal_name):
    path = static_json_path(input_dir, policy, goal_name)
    if not path.is_file():
        raise SystemExit(f"Missing ordered-policy CDF JSON: {path}")
    series = load_extremal_series(path)
    cdf_min = series["cdf_min"]
    cdf_max = series["cdf_max"]
    if len(cdf_min) != len(cdf_max):
        raise SystemExit(f"CDF extrema have different lengths: {path}")
    maximum_difference = max(
        (abs(lower - upper) for lower, upper in zip(cdf_min, cdf_max)),
        default=0.0,
    )
    if maximum_difference > 1e-10:
        raise SystemExit(
            f"Ordered policy is not deterministic in {path}: "
            f"max |CDF_min-CDF_max|={maximum_difference:.3g}"
        )
    return cdf_max


def goal_handles():
    from matplotlib.lines import Line2D

    return [
        Line2D(
            [0],
            [0],
            color=GOAL_BY_NAME[goal_name].color,
            marker=GOAL_MARKERS[goal_name],
            markeredgewidth=0,
            linewidth=1.2,
            label=GOAL_BY_NAME[goal_name].label,
        )
        for goal_name in INDIVIDUAL_GOAL_NAMES
    ]


def order_handles():
    from matplotlib.lines import Line2D

    return [
        Line2D(
            [0],
            [0],
            color="#666666",
            linestyle=protocol.linestyle,
            linewidth=1.2,
            label=protocol.label,
        )
        for protocol in PROTOCOLS
    ]


def draw_ordered_cdfs(
    ax,
    input_dir,
    policy_list,
    *,
    plot_truncation=None,
    compact=False,
):
    for goal_name in INDIVIDUAL_GOAL_NAMES:
        goal = GOAL_BY_NAME[goal_name]
        for policy in policy_list:
            cdf = require_static_cdf(input_dir, policy, goal_name)
            last_time = (
                min(plot_truncation, len(cdf) - 1)
                if plot_truncation is not None
                else len(cdf) - 1
            )
            linewidth = (
                MAX_BOUNDARY_LINEWIDTH
                if policy.priority_name == goal_name
                else MIN_BOUNDARY_LINEWIDTH
            )
            ax.plot(
                range(last_time + 1),
                cdf[: last_time + 1],
                color=goal.color,
                alpha=LINE_ALPHA,
                linestyle=policy.linestyle,
                linewidth=linewidth,
            )

    ax.set_xlabel(TIME_AXIS_LABEL)
    ax.set_ylabel("Cumulative probability")
    ax.set_xlim(left=0)
    ax.set_ylim(0.0, 1.0)
    ax.set_yticks((0.0, 0.25, 0.5, 0.75, 1.0))
    ax.grid(True, which="major", linestyle=":", linewidth=0.35, alpha=0.45)
    ax.margins(x=0)
    from matplotlib.ticker import ScalarFormatter

    time_formatter = ScalarFormatter(useMathText=True)
    time_formatter.set_powerlimits((0, 0))
    time_formatter.set_useOffset(False)
    ax.xaxis.set_major_formatter(time_formatter)

    goal_legend = ax.legend(
        handles=goal_handles(),
        frameon=False,
        loc="upper left",
        fontsize=7.0,
        handlelength=1.6,
        borderaxespad=0.25,
    )
    ax.add_artist(goal_legend)
    ax.legend(
        handles=order_handles(),
        frameon=False,
        loc="lower right",
        fontsize=7.0,
        handlelength=1.6,
        borderaxespad=0.25,
    )


def values_for_policy(policy, quality, metric):
    prefix = (policy.protocol_name, policy.priority_name)
    return {
        goal_name: quality[(*prefix, goal_name)][metric.column] * metric.scale
        for goal_name in INDIVIDUAL_GOAL_NAMES
    }


def metric_limits(metric, all_values):
    if metric.lower_bound is not None:
        lower = metric.lower_bound
    else:
        span = max(all_values) - min(all_values)
        padding = max(0.08 * span, 1e-5)
        lower = min(all_values) - padding

    if metric.upper_bound is not None:
        upper = metric.upper_bound
    else:
        span = max(all_values) - min(all_values)
        padding = max(0.08 * span, 1e-5)
        upper = max(all_values) + padding
    return lower, upper


def draw_metric(
    ax,
    policy_list,
    quality,
    metric,
    *,
    compact=False,
    show_goal_legend=False,
):
    all_values = []
    if metric.name == "jain-skr":
        for policy in policy_list:
            values = values_for_policy(policy, quality, METRICS["skr"])
            fairness = jain_index(list(values.values()))
            all_values.append(fairness)
            priority_color = GOAL_BY_NAME[policy.priority_name].color
            ax.scatter(
                policy.x,
                fairness,
                s=16 if compact else 23,
                color=priority_color,
                edgecolor="white",
                linewidth=0.35,
                zorder=3,
            )
    else:
        for policy in policy_list:
            values = values_for_policy(policy, quality, metric)
            all_values.extend(values.values())
            ax.plot(
                [policy.x, policy.x],
                [values["a-c"], values["b-d"]],
                color="#666666",
                alpha=0.9,
                linestyle=policy.linestyle,
                linewidth=1.1,
                zorder=1,
            )
            for goal_name in INDIVIDUAL_GOAL_NAMES:
                ax.scatter(
                    policy.x,
                    values[goal_name],
                    s=15 if compact else 22,
                    marker=GOAL_MARKERS[goal_name],
                    color=GOAL_BY_NAME[goal_name].color,
                    edgecolor="white",
                    linewidth=0.3,
                    zorder=3,
                )

    lower, upper = metric_limits(metric, all_values)
    ax.set_ylim(lower, upper)
    ax.set_xlim(-0.45, 3.85)
    ax.set_xticks(
        [policy.x for policy in policy_list],
        [policy.label for policy in policy_list],
    )
    ax.set_ylabel("" if compact else metric.label)
    ax.set_title(metric.compact_title if compact else "", pad=1.5)
    ax.grid(True, axis="y", linestyle=":", linewidth=0.35, alpha=0.45)
    ax.tick_params(axis="both", labelsize=6.5 if compact else 7.0)
    if compact:
        from matplotlib.ticker import MaxNLocator

        ax.yaxis.set_major_locator(MaxNLocator(nbins=3, min_n_ticks=3))
    if metric.name == "jain-skr":
        ax.axhline(
            1.0,
            color="#777777",
            linestyle=":",
            linewidth=0.6,
            zorder=0,
        )
    if show_goal_legend and metric.name != "jain-skr":
        ax.legend(
            handles=goal_handles(),
            frameon=False,
            loc="upper center",
            bbox_to_anchor=(0.5, 1.18),
            ncol=2,
            fontsize=7.0,
            handlelength=1.2,
            borderaxespad=0,
            columnspacing=1.0,
        )


def plot_ordered_cdfs(
    plt,
    output_dir,
    input_dir,
    policy_list,
    plot_profile,
    *,
    plot_truncation=None,
):
    fig, ax = plt.subplots(
        figsize=(LINE_WIDTH_INCHES, SWAP_COMPARISON_HEIGHT_INCHES)
    )
    draw_ordered_cdfs(
        ax,
        input_dir,
        policy_list,
        plot_truncation=plot_truncation,
    )
    path = output_path(
        output_dir,
        "nondet_topology_ordered",
        "cdf",
        plot_profile,
    )
    save_figure(fig, path, bbox_inches=None)
    plt.close(fig)
    print(f"Saved ordered-policy CDF figure to {path}")
    return path


def plot_metric(
    plt,
    output_dir,
    policy_list,
    quality,
    metric,
    plot_profile,
):
    fig, ax = plt.subplots(
        figsize=(LINE_WIDTH_INCHES, SWAP_COMPARISON_HEIGHT_INCHES)
    )
    draw_metric(
        ax,
        policy_list,
        quality,
        metric,
        show_goal_legend=True,
    )
    ax.set_xlabel("Ordered policy")
    fig.subplots_adjust(
        left=0.17,
        right=0.99,
        bottom=0.25,
        top=0.82 if metric.name != "jain-skr" else 0.96,
    )
    path = output_path(
        output_dir,
        "nondet_topology_ordered",
        metric.name.replace("-", "_"),
        plot_profile,
    )
    save_figure(fig, path, tight_layout=False, bbox_inches=None)
    plt.close(fig)
    print(f"Saved ordered-policy {metric.name} figure to {path}")
    return path


def plot_metric_group(
    plt,
    output_dir,
    policy_list,
    quality,
    metric_names,
    suffix,
    plot_profile,
):
    fig, axes = plt.subplots(
        1,
        len(metric_names),
        figsize=(LINE_WIDTH_INCHES, SWAP_COMPARISON_HEIGHT_INCHES),
    )
    if len(metric_names) == 1:
        axes = [axes]
    for ax, metric_name in zip(axes, metric_names):
        draw_metric(
            ax,
            policy_list,
            quality,
            METRICS[metric_name],
            compact=True,
        )
    fig.subplots_adjust(
        left=0.11,
        right=0.99,
        bottom=0.23,
        top=0.87,
        wspace=0.72 if len(metric_names) == 3 else 0.45,
    )
    path = output_path(
        output_dir,
        "nondet_topology_ordered",
        suffix,
        plot_profile,
    )
    save_figure(fig, path, tight_layout=False, bbox_inches=None)
    plt.close(fig)
    print(f"Saved ordered-policy metric group to {path}")
    return path


def plot_cdf_metric_group(
    plt,
    output_dir,
    input_dir,
    policy_list,
    quality,
    metric_names,
    suffix,
    plot_profile,
    *,
    plot_truncation=None,
):
    figure = plt.figure(
        figsize=(
            TEXT_WIDTH_INCHES,
            SWAP_COMPARISON_HEIGHT_INCHES,
        )
    )
    metric_count = len(metric_names)
    grid = figure.add_gridspec(
        1,
        metric_count + 1,
        width_ratios=(float(metric_count), *(1.0 for _ in metric_names)),
    )
    cdf_ax = figure.add_subplot(grid[0, 0])
    draw_ordered_cdfs(
        cdf_ax,
        input_dir,
        policy_list,
        plot_truncation=plot_truncation,
        compact=True,
    )
    for index, metric_name in enumerate(metric_names, start=1):
        metric_ax = figure.add_subplot(grid[0, index])
        draw_metric(
            metric_ax,
            policy_list,
            quality,
            METRICS[metric_name],
            compact=True,
        )
    figure.subplots_adjust(
        left=0.09,
        right=0.99,
        bottom=0.22,
        top=0.88,
        wspace=0.65 if metric_count == 3 else 0.48,
    )
    path = output_path(
        output_dir,
        "nondet_topology_ordered",
        suffix,
        plot_profile,
    )
    save_figure(figure, path, tight_layout=False, bbox_inches=None)
    plt.close(figure)
    print(f"Saved ordered-policy CDF/metric figure to {path}")
    return path


def main():
    args = parse_args()
    if args.plot_truncation is not None and args.plot_truncation <= 0:
        raise SystemExit("--plot-truncation must be positive.")
    if not args.input_dir.is_dir():
        raise SystemExit(f"Input directory does not exist: {args.input_dir}")
    args.output_dir.mkdir(parents=True, exist_ok=True)

    policy_list = policies()
    quality = load_quality_metrics(args.input_dir)
    summary_rows = policy_metric_rows(policy_list, quality)
    write_policy_summary(args.output_dir / POLICY_SUMMARY, summary_rows)

    plt = configure_matplotlib(args.plot_profile)
    plot_profile = get_plot_profile(args.plot_profile)
    plot_ordered_cdfs(
        plt,
        args.output_dir,
        args.input_dir,
        policy_list,
        plot_profile,
        plot_truncation=args.plot_truncation,
    )
    for metric in METRICS.values():
        plot_metric(
            plt,
            args.output_dir,
            policy_list,
            quality,
            metric,
            plot_profile,
        )

    plot_metric_group(
        plt,
        args.output_dir,
        policy_list,
        quality,
        ("skr", "mean-werner", "jain-skr"),
        "metrics",
        plot_profile,
    )
    plot_cdf_metric_group(
        plt,
        args.output_dir,
        args.input_dir,
        policy_list,
        quality,
        ("skr", "mean-werner", "jain-skr"),
        "cdf_metrics",
        plot_profile,
        plot_truncation=args.plot_truncation,
    )
    plot_cdf_metric_group(
        plt,
        args.output_dir,
        args.input_dir,
        policy_list,
        quality,
        ("skr", "jain-skr"),
        "cdf_skr_jain",
        plot_profile,
        plot_truncation=args.plot_truncation,
    )
    plot_cdf_metric_group(
        plt,
        args.output_dir,
        args.input_dir,
        policy_list,
        quality,
        ("skr",),
        "cdf_skr",
        plot_profile,
        plot_truncation=args.plot_truncation,
    )


if __name__ == "__main__":
    main()
