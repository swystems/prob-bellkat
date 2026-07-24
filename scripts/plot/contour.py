from __future__ import annotations

import math

import numpy as np

from scripts.plot.config import style_axes


RATIO_CONTOUR_LEVELS = 101
RATIO_AXIS_LABEL_SIZE = 6
RATIO_TICK_LABEL_SIZE = 6
RATIO_COLORBAR_LABEL_SIZE = 6
RATIO_COLORBAR_TICK_LABEL_SIZE = 5.5
RATIO_COLORBAR_MAX_INTERVALS = 4


def tick_label(value: float | int) -> str:
    return f"{float(value):.12g}"


def thinned_ticks(values, labels, maximum: int):
    """Select readable ticks while retaining both endpoints."""
    if len(values) <= maximum:
        return values, labels
    step = math.ceil((len(values) - 1) / (maximum - 1))
    indices = list(range(0, len(values), step))
    if indices[-1] != len(values) - 1:
        indices.append(len(values) - 1)
    return [values[index] for index in indices], [labels[index] for index in indices]


def draw_ratio_contour(
    fig,
    ax,
    x_values,
    y_values,
    ratios,
    *,
    cmap: str,
    colorbar_label: str,
    xlabel: str,
    ylabel: str,
    log_x: bool = False,
    log_y: bool = False,
    show_xlabel: bool = True,
    levels: int = RATIO_CONTOUR_LEVELS,
    x_ticks=None,
    y_ticks=None,
    x_ticklabels=None,
    y_ticklabels=None,
):
    """Draw a consistently styled ratio contour centered on equality when possible."""
    from matplotlib.colors import TwoSlopeNorm
    from matplotlib.ticker import MaxNLocator, NullLocator

    x = np.asarray(x_values, dtype=float)
    y = np.asarray(y_values, dtype=float)
    ratio = np.asarray(ratios, dtype=float)

    finite_ratio = ratio[np.isfinite(ratio)]
    contour_kwargs = {"levels": levels, "cmap": cmap}
    if finite_ratio.size > 0:
        ratio_min = float(np.nanmin(finite_ratio))
        ratio_max = float(np.nanmax(finite_ratio))
        if ratio_min < 1.0 < ratio_max:
            contour_kwargs["norm"] = TwoSlopeNorm(
                vmin=ratio_min,
                vcenter=1.0,
                vmax=ratio_max,
            )

    heatmap = ax.contourf(
        x,
        y,
        np.ma.masked_invalid(ratio),
        **contour_kwargs,
    )

    if log_x:
        ax.set_xscale("log")
        ax.xaxis.set_minor_locator(NullLocator())
    if log_y:
        ax.set_yscale("log")
        ax.yaxis.set_minor_locator(NullLocator())

    ax.set_xlabel(xlabel if show_xlabel else "")
    ax.set_ylabel(ylabel)
    ax.set_xticks(x if x_ticks is None else x_ticks)
    ax.set_yticks(y if y_ticks is None else y_ticks)
    style_axes(ax)
    ax.xaxis.label.set_size(RATIO_AXIS_LABEL_SIZE)
    ax.yaxis.label.set_size(RATIO_AXIS_LABEL_SIZE)
    ax.tick_params(axis="both", labelsize=RATIO_TICK_LABEL_SIZE)
    ax.set_xticklabels(
        x_ticklabels if x_ticklabels is not None else [tick_label(value) for value in x],
        rotation=0,
        ha="center",
    )
    ax.set_yticklabels(
        y_ticklabels if y_ticklabels is not None else [tick_label(value) for value in y]
    )
    colorbar = fig.colorbar(heatmap, ax=ax, pad=0.02)
    colorbar.locator = MaxNLocator(nbins=RATIO_COLORBAR_MAX_INTERVALS)
    colorbar.update_ticks()
    colorbar.set_label(colorbar_label, size=RATIO_COLORBAR_LABEL_SIZE)
    colorbar.ax.tick_params(labelsize=RATIO_COLORBAR_TICK_LABEL_SIZE)
    return colorbar
