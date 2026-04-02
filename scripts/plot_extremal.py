#!/usr/bin/env python3

import argparse
import json
from pathlib import Path


# One-column paper figure; your original width was already good.
FIGURE_SIZE = (3.5, 2.3)

# Color-blind-friendlier than blue/red, and still distinguishable in grayscale
# once paired with different line styles.
COLOR_MIN = "#1b4f72"   # dark blue
COLOR_MAX = "#b35a00"   # dark orange/brown
BAND_COLOR = "0.88"     # neutral light gray

# Replace X with the random variable symbol used in the paper.
PMF_MIN_LABEL = r"$\Pr_{\min}(X=t)$"
PMF_MAX_LABEL = r"$\Pr_{\max}(X=t)$"
CDF_MIN_LABEL = r"$\Pr_{\min}(X\leq t)$"
CDF_MAX_LABEL = r"$\Pr_{\max}(X\leq t)$"


def parse_args():
    parser = argparse.ArgumentParser(
        description="Plot extremal PMF/CDF curves from a BellKAT extremal JSON dump."
    )
    parser.add_argument("input_json", help="Path to the JSON dump produced by the mdp command")
    parser.add_argument(
        "--output-dir",
        default=".",
        help="Directory where extremal_pmf.pdf and extremal_cdf.pdf will be written",
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


def pmf_from_cdf(cdf):
    if not cdf:
        return []

    pmf = [cdf[0]]
    pmf.extend(curr - prev for prev, curr in zip(cdf, cdf[1:]))
    return pmf


def derive_plot_series(series):
    cdf_min = series["cdf_min"]
    cdf_max = series["cdf_max"]

    if len(cdf_min) != len(cdf_max):
        raise SystemExit("cdf_min and cdf_max must have the same length.")

    t = list(range(len(cdf_min)))
    pmf_min = pmf_from_cdf(cdf_min)
    pmf_max = pmf_from_cdf(cdf_max)
    return t, pmf_min, pmf_max, cdf_min, cdf_max


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

    ax.step(
        t,
        pmf_min,
        where="post",
        color=COLOR_MIN,
        linestyle="-",
        label=PMF_MIN_LABEL,
    )
    ax.step(
        t,
        pmf_max,
        where="post",
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
        step="post",
        color=BAND_COLOR,
        alpha=1.0,
        linewidth=0,
    )
    ax.step(
        t,
        cdf_min,
        where="post",
        color=COLOR_MIN,
        linestyle="-",
        label=CDF_MIN_LABEL,
    )
    ax.step(
        t,
        cdf_max,
        where="post",
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


def main():
    args = parse_args()
    series = load_extremal_payload(args.input_json)
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    file_stem = Path(args.input_json).stem or "extremal"

    t, pmf_min, pmf_max, cdf_min, cdf_max = derive_plot_series(series)

    plt = configure_matplotlib()
    plot_pmf(plt, output_dir, file_stem, t, pmf_min, pmf_max)
    plot_cdf(plt, output_dir, file_stem, t, cdf_min, cdf_max)


if __name__ == "__main__":
    main()
