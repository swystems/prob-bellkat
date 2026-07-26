from __future__ import annotations

import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path


# IEEE conference papers are two-column; keep the default paper figure to a
# single-column width so labels remain readable after placement.
LINE_WIDTH_INCHES = 3.5
TEXT_WIDTH_INCHES = 7.16

VALIDATION_LINE_WIDTH_INCHES = 3.58/1.5
VALIDATION_TEXT_WIDTH_INCHES = 3.58/1.5
SWAP_COMPARISON_LINE_WIDTH_INCHES = 3.58/1.5
SWAP_COMPARISON_HEIGHT_INCHES = 1.75
VALIDATION_HEIGHT_INCHES = 1.75

VALIDATION_COMBINED_LINE_WIDTH_INCHES = 3.58
VALIDATION_COMBINED_TEXT_WIDTH_INCHES = 7.16
VALIDATION_COMBINED_HEIGHT_INCHES = 3.5

SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES = 7.16/1.5
SWAP_COMPARISON_COMBINED_HEIGHT_INCHES = 1.75
SWAP_COMPARISON_DETAIL_INSET_BOUNDS = (0.34, 0.27, 0.64, 0.65)
SWAP_COMPARISON_JOINT_LEGEND_Y = 0.98
SWAP_COMPARISON_JOINT_TOP = 0.82
SWAP_COMPARISON_BINNED_LINE_WIDTH = 1.1

OPTIMALITY_LINE_WIDTH_INCHES = 3.58/1.5
OPTIMALITY_HEIGHT_INCHES = 1.75/1.5
OPTIMALITY_COMBINED_LINE_WIDTH_INCHES = 3.58
OPTIMALITY_COMBINED_HEIGHT_INCHES = 3.5

NONDET_LINE_WIDTH_INCHES = 3.58/1.5
NONDET_HEIGHT_INCHES = 1.75/1.5
DEFAULT_PROFILE = "paper"
TIME_AXIS_LABEL = r"Time ($t_{\mathrm{unit}}$)"
SCIENTIFIC_NOTATION_POWER_LIMITS = (-2, 5)
MOVE_SCIENTIFIC_NOTATION_TO_LABELS = True
JOINT_PLOTS_HSPACE = 0.08
JOINT_PLOTS_WSPACE = 0.03

PLOT_SETTINGS = {
    "normal": {
        "dpi": 150,
        "figsize_distribution": (10, 6),
        "format": "png",
        "output_dir": "figures-normal",
    },
    "paper": {
        "dpi": 300,
        "figsize_distribution": (LINE_WIDTH_INCHES, LINE_WIDTH_INCHES * 0.62),
        "format": "pdf",
        "output_dir": "figures-paper",
    },
}


@dataclass(frozen=True)
class PlotProfile:
    name: str
    dpi: int
    figure_size: tuple[float, float]
    file_format: str
    output_dir: str
    use_tex: bool


def get_plot_profile(profile: str = DEFAULT_PROFILE) -> PlotProfile:
    if profile not in PLOT_SETTINGS:
        available = ", ".join(sorted(PLOT_SETTINGS))
        raise ValueError(f"Unknown plot profile '{profile}'. Available profiles: {available}")

    settings = PLOT_SETTINGS[profile]
    return PlotProfile(
        name=profile,
        dpi=int(settings["dpi"]),
        figure_size=tuple(settings["figsize_distribution"]),
        file_format=str(settings["format"]),
        output_dir=str(settings["output_dir"]),
        use_tex=profile == "paper",
    )


def output_path(output_dir: Path, file_stem: str, suffix: str, profile: PlotProfile) -> Path:
    return output_dir / f"{file_stem}_{suffix}.{profile.file_format}"


def configure_matplotlib(profile: str = DEFAULT_PROFILE):
    configure_plot_cache()
    try:
        import matplotlib.pyplot as plt
    except ImportError as exc:
        raise SystemExit(
            "matplotlib is required to use this script. Install it with `pip install matplotlib`."
        ) from exc

    plot_profile = get_plot_profile(profile)
    use_tex = plot_profile.use_tex and latex_is_usable()
    plt.rcParams.update(
        {
            "figure.figsize": plot_profile.figure_size,
            "figure.dpi": plot_profile.dpi,
            "savefig.dpi": plot_profile.dpi,
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
            "axes.labelsize": 8,
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
            "axes.formatter.limits": SCIENTIFIC_NOTATION_POWER_LIMITS,
            "axes.formatter.use_mathtext": True,
            "axes.formatter.useoffset": False,
            "text.usetex": use_tex,
            "text.latex.preamble": r"\usepackage{amsmath}",
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
            "svg.fonttype": "none",
        }
    )
    return plt


def configure_plot_cache() -> None:
    cache_root = Path(os.getenv("TMPDIR", "/tmp")) / "bellkat-plot-cache"
    cache_root.mkdir(parents=True, exist_ok=True)
    os.environ.setdefault("MPLCONFIGDIR", str(cache_root / "matplotlib"))
    os.environ.setdefault("XDG_CACHE_HOME", str(cache_root / "xdg"))


def latex_is_usable() -> bool:
    if shutil.which("latex") is None:
        return False
    if shutil.which("kpsewhich") is None:
        return False

    result = subprocess.run(
        ["kpsewhich", "ptmr7t.tfm"],
        text=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return result.returncode == 0


def style_axes(ax):
    configure_tick_formatter(ax)
    ax.grid(True, which="major", linestyle=":", linewidth=0.35, alpha=0.45)


def use_informative_y_ticks(ax, *, nbins=5):
    """Request a compact but informative number of automatic y-axis ticks."""
    from matplotlib.ticker import MaxNLocator

    ax.yaxis.set_major_locator(MaxNLocator(nbins=nbins, min_n_ticks=4))


def hide_overlapping_inner_x_tick_label(fig, left_ax, right_ax, padding_points=2.0):
    """Hide the left panel's last x label only when the two panels collide."""
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    left_labels = [label for label in left_ax.get_xticklabels() if label.get_visible()]
    right_labels = [label for label in right_ax.get_xticklabels() if label.get_visible()]
    if not left_labels or not right_labels:
        return

    left_label = max(left_labels, key=lambda label: label.get_window_extent(renderer).x1)
    right_label = min(right_labels, key=lambda label: label.get_window_extent(renderer).x0)
    padding_pixels = padding_points * fig.dpi / 72.0
    if (
        left_label.get_window_extent(renderer).x1 + padding_pixels
        > right_label.get_window_extent(renderer).x0
    ):
        left_label.set_visible(False)


def configure_tick_formatter(ax):
    from matplotlib.ticker import ScalarFormatter

    for axis in (ax.xaxis, ax.yaxis):
        formatter = ScalarFormatter(useMathText=True)
        formatter.set_powerlimits(SCIENTIFIC_NOTATION_POWER_LIMITS)
        formatter.set_useOffset(False)
        axis.set_major_formatter(formatter)


def save_figure(fig, path, *, tight_layout=True, tight_pad=0.25, bbox_inches="tight"):
    if tight_layout:
        fig.tight_layout(pad=tight_pad)

    if MOVE_SCIENTIFIC_NOTATION_TO_LABELS:
        move_scientific_notation_to_labels(fig)
        if tight_layout:
            fig.tight_layout(pad=tight_pad)
        move_scientific_notation_to_labels(fig)

    fig.savefig(path, bbox_inches=bbox_inches)


def move_scientific_notation_to_labels(fig):
    fig.canvas.draw()
    for ax in fig.axes:
        move_axis_scientific_notation_to_label(ax, "x")
        move_axis_scientific_notation_to_label(ax, "y")


def move_axis_scientific_notation_to_label(ax, axis_name):
    if axis_name == "x":
        axis = ax.xaxis
        get_label = ax.get_xlabel
        set_label = ax.set_xlabel
        base_label_attr = "_bellkat_base_xlabel"
    else:
        axis = ax.yaxis
        get_label = ax.get_ylabel
        set_label = ax.set_ylabel
        base_label_attr = "_bellkat_base_ylabel"

    if not hasattr(ax, base_label_attr):
        setattr(ax, base_label_attr, get_label())

    base_label = getattr(ax, base_label_attr)
    exponent = int(getattr(axis.get_major_formatter(), "orderOfMagnitude", 0) or 0)
    axis.get_offset_text().set_visible(False)

    if not base_label:
        return

    suffix = f" ($10^{{{exponent}}}$)" if exponent else ""
    set_label(f"{base_label}{suffix}")
