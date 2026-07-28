#!/usr/bin/env python3

import argparse
import json
from pathlib import Path

from scripts.plot.config import (
    DEFAULT_PROFILE,
    PLOT_SETTINGS,
    TIME_AXIS_LABEL,
    configure_matplotlib,
    get_plot_profile,
    output_path,
    save_figure,
    style_axes,
)

# To discuss:
# PMF/CDF data are discrete and are often represented with step plots
# We currently use plot() for visual consistency and may revisit a step-based rendering later

COLOR_MIN = "#1b4f72"
COLOR_MAX = "#b35a00"
BAND_COLOR = "0.88"
LINE_ALPHA = 0.78

PMF_MIN_LABEL = r"$\Pr_{\min}(X=t)$"
PMF_MAX_LABEL = r"$\Pr_{\max}(X=t)$"
CDF_MIN_LABEL = r"$\Pr_{\min}(X\leq t)$"
CDF_MAX_LABEL = r"$\Pr_{\max}(X\leq t)$"
WERNER_MIN_LABEL = r"$W_{\min}(t)$"
WERNER_MAX_LABEL = r"$W_{\max}(t)$"
FIDELITY_MIN_LABEL = r"$F_{\min}(t)$"
FIDELITY_MAX_LABEL = r"$F_{\max}(t)$"


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Plot extremal PMF/CDF curves from one BellKAT extremal JSON dump, "
            "or derive the average Werner parameter from separate pure and mixed dumps."
        )
    )
    parser.add_argument(
        "input_json",
        nargs="?",
        help="Path to a single JSON dump produced by the mdp/qmdp command",
    )
    parser.add_argument(
        "--pure-json",
        help="Path to the extremal JSON dump computed with the pure event target",
    )
    parser.add_argument(
        "--mixed-json",
        help="Path to the extremal JSON dump computed with the mixed event target",
    )
    parser.add_argument(
        "--output-dir",
        default=None,
        help=(
            "Directory where output figures will be written "
            "(defaults to the selected plot profile output directory)"
        ),
    )
    parser.add_argument(
        "--file-stem",
        default=None,
        help="Base name used for output files; defaults to the input stem",
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=DEFAULT_PROFILE,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--plot-fidelity",
        action="store_true",
        dest="plot_fidelity",
        help=(
            "When plotting from --pure-json/--mixed-json, also write an average "
            "fidelity figure derived from the Werner parameter."
        ),
    )
    return parser.parse_args()


def load_extremal_payload(path):
    with open(path, "r", encoding="utf-8") as handle:
        payload = json.load(handle)

    if "extremal" in payload:
        return payload["extremal"]


def load_extremal_series(path):
    payload = load_extremal_payload(path)
    if "series" not in payload:
        raise SystemExit("Could not find an extremal series payload in the provided JSON file.")

    return payload["series"]



def pmf_from_cdf(cdf):
    if not cdf:
        return []

    pmf = [cdf[0]]
    pmf.extend(curr - prev for prev, curr in zip(cdf, cdf[1:]))
    return pmf


def derive_pmf_series(series):
    cdf_min = series["cdf_min"]
    cdf_max = series["cdf_max"]

    if len(cdf_min) != len(cdf_max):
        raise SystemExit("cdf_min and cdf_max must have the same length.")

    pmf_min = pmf_from_cdf(cdf_min)
    pmf_max = pmf_from_cdf(cdf_max)
    return pmf_min, pmf_max


def derive_plot_series(series):
    cdf_min = series["cdf_min"]
    cdf_max = series["cdf_max"]
    pmf_min, pmf_max = derive_pmf_series(series)
    t = list(range(len(cdf_min)))
    return t, pmf_min, pmf_max, cdf_min, cdf_max


def derive_average_werner_series(pure_series, mixed_series):
    pure_pmf_min, pure_pmf_max = derive_pmf_series(pure_series)
    mixed_pmf_min, mixed_pmf_max = derive_pmf_series(mixed_series)

    if len(pure_pmf_min) != len(mixed_pmf_min) or len(pure_pmf_max) != len(mixed_pmf_max):
        raise SystemExit(
            "Pure and mixed extremal series must have the same resolved budget. "
            "Please rerun both commands with the same --truncation value."
        )

    t = list(range(len(pure_pmf_min)))
    werner_min = average_werner_series(pure_pmf_min, mixed_pmf_min)
    werner_max = average_werner_series(pure_pmf_max, mixed_pmf_max)
    return t, werner_min, werner_max


def average_werner_series(pmf_pure, pmf_mixed):
    return [
        pure / total if total > 0 else 0.0
        for pure, total in (
            (pure, pure + mixed)
            for pure, mixed in zip(pmf_pure, pmf_mixed)
        )
    ]


def werner_to_fid(werner):
    return (1.0 + 3.0 * werner) / 4.0


def plot_pmf(
    plt,
    output_dir,
    file_stem,
    t,
    pmf_min,
    pmf_max,
    plot_profile,
    *,
    figure_size=None,
    bbox_inches="tight",
):
    fig, ax = plt.subplots(figsize=figure_size)

    ax.plot(
        t,
        pmf_min,
        color=COLOR_MIN,
        alpha=LINE_ALPHA,
        linestyle="-",
        label=PMF_MIN_LABEL,
    )
    ax.plot(
        t,
        pmf_max,
        color=COLOR_MAX,
        alpha=LINE_ALPHA,
        linestyle="--",
        label=PMF_MAX_LABEL,
    )

    ax.set_xlabel(TIME_AXIS_LABEL)
    ax.set_ylabel("Probability")
    ax.set_xlim(min(t), max(t))
    ax.set_ylim(bottom=0.0)
    style_axes(ax)
    ax.legend(frameon=False, loc="best")

    save_figure(
        fig,
        output_path(output_dir, file_stem, "pmf", plot_profile),
        bbox_inches=bbox_inches,
    )
    plt.close(fig)


def plot_cdf(
    plt,
    output_dir,
    file_stem,
    t,
    cdf_min,
    cdf_max,
    plot_profile,
    *,
    figure_size=None,
    bbox_inches="tight",
):
    fig, ax = plt.subplots(figsize=figure_size)

    ax.fill_between(
        t,
        cdf_min,
        cdf_max,
        color=BAND_COLOR,
        alpha=1.0,
        linewidth=0,
    )
    ax.plot(
        t,
        cdf_min,
        color=COLOR_MIN,
        alpha=LINE_ALPHA,
        linestyle="-",
        label=CDF_MIN_LABEL,
    )
    ax.plot(
        t,
        cdf_max,
        color=COLOR_MAX,
        alpha=LINE_ALPHA,
        linestyle="--",
        label=CDF_MAX_LABEL,
    )

    ax.set_xlabel(TIME_AXIS_LABEL)
    ax.set_ylabel("Cumulative probability")
    ax.set_xlim(min(t), max(t))
    ax.set_ylim(0.0, 1.0)
    style_axes(ax)
    ax.legend(frameon=False, loc="lower right")

    save_figure(
        fig,
        output_path(output_dir, file_stem, "cdf", plot_profile),
        bbox_inches=bbox_inches,
    )
    plt.close(fig)


def plot_average_quantity(
    plt,
    output_dir,
    file_stem,
    t,
    values_min,
    values_max,
    plot_profile,
    *,
    suffix,
    ylabel,
    min_label,
    max_label,
    ylim=(0.0, 1.0),
):
    fig, ax = plt.subplots()

    ax.fill_between(
        t,
        values_min,
        values_max,
        color=BAND_COLOR,
        alpha=1.0,
        linewidth=0,
    )
    ax.plot(
        t,
        values_min,
        color=COLOR_MIN,
        alpha=LINE_ALPHA,
        linestyle="-",
        label=min_label,
    )
    ax.plot(
        t,
        values_max,
        color=COLOR_MAX,
        alpha=LINE_ALPHA,
        linestyle="--",
        label=max_label,
    )

    ax.set_xlabel(TIME_AXIS_LABEL)
    ax.set_ylabel(ylabel)
    ax.set_xlim(min(t), max(t))
    ax.set_ylim(*ylim)
    style_axes(ax)
    ax.legend(frameon=False, loc="best")

    save_figure(fig, output_path(output_dir, file_stem, suffix, plot_profile))
    plt.close(fig)


def plot_average_werner(plt, output_dir, file_stem, t, werner_min, werner_max, plot_profile):
    plot_average_quantity(
        plt,
        output_dir,
        file_stem,
        t,
        werner_min,
        werner_max,
        plot_profile,
        suffix="average_werner",
        ylabel="Werner parameter",
        min_label=WERNER_MIN_LABEL,
        max_label=WERNER_MAX_LABEL,
    )


def plot_average_fidelity(plt, output_dir, file_stem, t, werner_min, werner_max, plot_profile):
    fidelity_min = [werner_to_fid(werner) for werner in werner_min]
    fidelity_max = [werner_to_fid(werner) for werner in werner_max]
    plot_average_quantity(
        plt,
        output_dir,
        file_stem,
        t,
        fidelity_min,
        fidelity_max,
        plot_profile,
        suffix="average_fidelity",
        ylabel="Average fidelity",
        min_label=FIDELITY_MIN_LABEL,
        max_label=FIDELITY_MAX_LABEL,
        ylim=(0.25, 1.0),
    )


def resolve_output_stem(args):
    if args.file_stem:
        return args.file_stem

    if args.input_json:
        return Path(args.input_json).stem or "extremal"

    pure_stem = Path(args.pure_json).stem
    mixed_stem = Path(args.mixed_json).stem
    common_prefix = common_stem_prefix(pure_stem, mixed_stem)
    return common_prefix or "extremal"


def common_stem_prefix(left, right):
    prefix_chars = []
    for left_char, right_char in zip(left, right):
        if left_char != right_char:
            break
        prefix_chars.append(left_char)

    return "".join(prefix_chars).rstrip("_-. ")


def validate_args(args):
    paired_mode = args.pure_json is not None or args.mixed_json is not None

    if paired_mode:
        if not (args.pure_json and args.mixed_json):
            raise SystemExit("Please provide both --pure-json and --mixed-json.")
        if args.input_json is not None:
            raise SystemExit(
                "Use either a single input JSON or the --pure-json/--mixed-json pair, not both."
            )
        return "average_werner"

    if args.input_json is None:
        raise SystemExit(
            "Please provide either input_json for PMF/CDF plots, "
            "or both --pure-json and --mixed-json for the Werner plot."
        )
    if args.plot_fidelity:
        raise SystemExit("--plot-fidelity requires --pure-json and --mixed-json.")

    return "extremal"


def main():
    args = parse_args()
    mode = validate_args(args)
    plot_profile = get_plot_profile(args.plot_profile)
    output_dir = Path(args.output_dir or plot_profile.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    file_stem = resolve_output_stem(args)
    plt = configure_matplotlib(args.plot_profile)

    if mode == "average_werner":
        pure_series = load_extremal_series(args.pure_json)
        mixed_series = load_extremal_series(args.mixed_json)
        t, werner_min, werner_max = derive_average_werner_series(pure_series, mixed_series)
        plot_average_werner(plt, output_dir, file_stem, t, werner_min, werner_max, plot_profile)
        if args.plot_fidelity:
            plot_average_fidelity(
                plt,
                output_dir,
                file_stem,
                t,
                werner_min,
                werner_max,
                plot_profile,
            )
        return

    series = load_extremal_series(args.input_json)
    t, pmf_min, pmf_max, cdf_min, cdf_max = derive_plot_series(series)
    plot_pmf(plt, output_dir, file_stem, t, pmf_min, pmf_max, plot_profile)
    plot_cdf(plt, output_dir, file_stem, t, cdf_min, cdf_max, plot_profile)


if __name__ == "__main__":
    main()
