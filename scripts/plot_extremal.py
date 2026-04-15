#!/usr/bin/env python3

import argparse
import json
from pathlib import Path


FIGURE_SIZE = (3.5, 2.3)

# To discuss: 
# PMF/CDF data are discrete and are often represented with step plots
# We currently use plot() for visual consistency and may revisit a step-based rendering later

COLOR_MIN = "#1b4f72"
COLOR_MAX = "#b35a00"
BAND_COLOR = "0.88"

PMF_MIN_LABEL = r"$\Pr_{\min}(X=t)$"
PMF_MAX_LABEL = r"$\Pr_{\max}(X=t)$"
CDF_MIN_LABEL = r"$\Pr_{\min}(X\leq t)$"
CDF_MAX_LABEL = r"$\Pr_{\max}(X\leq t)$"
WERNER_MIN_LABEL = r"$W_{\min}(t)$"
WERNER_MAX_LABEL = r"$W_{\max}(t)$"


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
        default=".",
        help=(
            "Directory where output figures will be written "
            "(PMF/CDF for single-input mode, average Werner in paired mode)"
        ),
    )
    parser.add_argument(
        "--file-stem",
        default=None,
        help="Base name used for output files; defaults to the input stem",
    )
    return parser.parse_args()


def load_extremal_payload(path):
    with open(path, "r", encoding="utf-8") as handle:
        payload = json.load(handle)

    if "extremal" in payload:
        payload = payload["extremal"]

    if "series" not in payload:
        raise SystemExit("Could not find an extremal series payload in the provided JSON file.")

    return payload["series"]


def load_extremal_series(path):
    return load_extremal_payload(path)


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


def configure_matplotlib():
    try:
        import matplotlib.pyplot as plt
    except ImportError as exc:
        raise SystemExit(
            "matplotlib is required to use this script. Install it with `pip install matplotlib`."
        ) from exc

    plt.rcParams.update(
        {
            "figure.figsize": FIGURE_SIZE,
            "font.size": 9,
            "font.family": "serif",
            "font.serif": [
                "Times New Roman",
                "Times",
                "Nimbus Roman",
                "STIX Two Text",
                "DejaVu Serif",
            ],
            "mathtext.fontset": "stix",
            "axes.labelsize": 9,
            "axes.titlesize": 9,
            "axes.linewidth": 0.6,
            "legend.fontsize": 8,
            "legend.handlelength": 2.4,
            "xtick.labelsize": 8,
            "ytick.labelsize": 8,
            "xtick.major.width": 0.6,
            "ytick.major.width": 0.6,
            "xtick.major.size": 3.0,
            "ytick.major.size": 3.0,
            "lines.linewidth": 1.6,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
        }
    )
    return plt


def style_axes(ax):
    # IEEE guidance favors only coordinate axes or at most major grid lines.
    ax.grid(True, which="major", linestyle=":", linewidth=0.35, alpha=0.45)


def plot_pmf(plt, output_dir, file_stem, t, pmf_min, pmf_max):
    fig, ax = plt.subplots()

    ax.plot(
        t,
        pmf_min,
        color=COLOR_MIN,
        linestyle="-",
        label=PMF_MIN_LABEL,
    )
    ax.plot(
        t,
        pmf_max,
        color=COLOR_MAX,
        linestyle="--",
        label=PMF_MAX_LABEL,
    )

    ax.set_xlabel(r"$t$")
    ax.set_ylabel("Probability")
    ax.set_xlim(min(t), max(t))
    style_axes(ax)
    ax.legend(frameon=False, loc="best")

    fig.tight_layout(pad=0.25)
    fig.savefig(output_dir / f"{file_stem}_pmf.pdf", bbox_inches="tight")
    plt.close(fig)


def plot_cdf(plt, output_dir, file_stem, t, cdf_min, cdf_max):
    fig, ax = plt.subplots()

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
        linestyle="-",
        label=CDF_MIN_LABEL,
    )
    ax.plot(
        t,
        cdf_max,
        color=COLOR_MAX,
        linestyle="--",
        label=CDF_MAX_LABEL,
    )

    ax.set_xlabel(r"$t$")
    ax.set_ylabel("Cumulative probability")
    ax.set_xlim(min(t), max(t))
    ax.set_ylim(0.0, 1.0)
    style_axes(ax)
    ax.legend(frameon=False, loc="lower right")

    fig.tight_layout(pad=0.25)
    fig.savefig(output_dir / f"{file_stem}_cdf.pdf", bbox_inches="tight")
    plt.close(fig)


def plot_average_werner(plt, output_dir, file_stem, t, werner_min, werner_max):
    fig, ax = plt.subplots()

    ax.fill_between(
        t,
        werner_min,
        werner_max,
        color=BAND_COLOR,
        alpha=1.0,
        linewidth=0,
    )
    ax.plot(
        t,
        werner_min,
        color=COLOR_MIN,
        linestyle="-",
        label=WERNER_MIN_LABEL,
    )
    ax.plot(
        t,
        werner_max,
        color=COLOR_MAX,
        linestyle="--",
        label=WERNER_MAX_LABEL,
    )

    ax.set_xlabel(r"$t$")
    ax.set_ylabel("Average Werner parameter")
    ax.set_xlim(min(t), max(t))
    ax.set_ylim(0.0, 1.0)
    style_axes(ax)
    ax.legend(frameon=False, loc="best")

    fig.tight_layout(pad=0.25)
    fig.savefig(output_dir / f"{file_stem}_average_werner.pdf", bbox_inches="tight")
    plt.close(fig)


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

    return "extremal"


def main():
    args = parse_args()
    mode = validate_args(args)
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    file_stem = resolve_output_stem(args)
    plt = configure_matplotlib()

    if mode == "average_werner":
        pure_series = load_extremal_series(args.pure_json)
        mixed_series = load_extremal_series(args.mixed_json)
        t, werner_min, werner_max = derive_average_werner_series(pure_series, mixed_series)
        plot_average_werner(plt, output_dir, file_stem, t, werner_min, werner_max)
        return

    series = load_extremal_series(args.input_json)
    t, pmf_min, pmf_max, cdf_min, cdf_max = derive_plot_series(series)
    plot_pmf(plt, output_dir, file_stem, t, pmf_min, pmf_max)
    plot_cdf(plt, output_dir, file_stem, t, cdf_min, cdf_max)


if __name__ == "__main__":
    main()
