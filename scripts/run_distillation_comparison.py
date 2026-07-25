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

if __package__ is None or __package__ == "":
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from scripts.analysis.swap_comparison.common import (
    MIXED_EVENT,
    QMDP_MODE,
    PURE_EVENT,
    STATIC_EVENT,
    build_command,
    compute_secret_key_rate_from_split,
    executable_command,
    format_duration,
    load_extremal_payload,
    load_extremal_series,
    reference_p_ge_from_scaling_factor,
    run_command,
)
from scripts.plot.config import (
    DEFAULT_PROFILE,
    OPTIMALITY_HEIGHT_INCHES,
    OPTIMALITY_LINE_WIDTH_INCHES,
    PLOT_SETTINGS,
    configure_matplotlib,
    get_plot_profile,
    output_path,
    save_figure,
)
from scripts.plot.contour import draw_ratio_contour, thinned_ticks


PROTOCOLS = ("swap", "dist-swap", "swap-dist")
BASELINE_PROTOCOL = "swap"
DISTILL_PROTOCOLS = ("dist-swap", "swap-dist")
PROTOCOL_PLOT_LABELS = {
    "dist-swap": "D-S",
    "swap-dist": "S-D",
}
# Keep the new data namespace separate from cached results for the previous
# X-Y-only experiment.
FILE_PREFIX = "distillation_order_comparison"
FIGURE_PREFIX = "distillation_comparison"
DEFAULT_OUTPUT_DIR = Path("output/distillation-comparison")
DEFAULT_TRUNCATION = 5000
DEFAULT_GENERATION_SCALING = 128.0
DEFAULT_UNIFORM_W0_VALUES = "0.925,0.94,0.955,0.97,0.985,1.0"
DEFAULT_T_COH_VALUES = "14400,57600,230400,921600,3686400"
DEFAULT_P_SWAP = 0.5
MINIMUM_COVERAGE = 0.99


@dataclass(frozen=True)
class DistillationPoint:
    p_ge: float
    uniform_w0: float
    p_swap: float | None
    t_coh: int | None


@dataclass(frozen=True)
class PointResult:
    point: DistillationPoint
    skr_by_protocol: dict[str, float]
    coverage_by_protocol: dict[str, float]


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Compare swap-only with distill-swap and swap-distill on A-X-Y-C. "
            "The contour plots swap-only over the best distillation ordering "
            "and marks the D-S/S-D winner boundary."
        )
    )
    parser.add_argument("--truncation", type=int, default=DEFAULT_TRUNCATION)
    parser.add_argument(
        "--generation-scaling",
        type=float,
        default=DEFAULT_GENERATION_SCALING,
        help="Fixed generation scaling factor.",
    )
    parser.add_argument(
        "--uniform-w0-values",
        default=DEFAULT_UNIFORM_W0_VALUES,
        help="Comma-separated Werner parameters applied identically to every link.",
    )
    parser.add_argument(
        "--t-coh-values",
        default=DEFAULT_T_COH_VALUES,
        help="Comma-separated coherence times applied identically to every memory.",
    )
    parser.add_argument("--p-swap", type=float, default=DEFAULT_P_SWAP)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument(
        "--markdown",
        type=Path,
        default=DEFAULT_OUTPUT_DIR / "distillation-comparison.md",
    )
    parser.add_argument("--plot-profile", choices=tuple(PLOT_SETTINGS), default=DEFAULT_PROFILE)
    parser.add_argument("--executable", default="quantP_compare_distillation")
    parser.add_argument("--plots-only", action="store_true")
    cache_group = parser.add_mutually_exclusive_group()
    cache_group.add_argument(
        "--resume",
        dest="reuse_existing",
        action="store_true",
        default=True,
        help="Reuse valid results at the requested truncation (default).",
    )
    cache_group.add_argument(
        "--force",
        dest="reuse_existing",
        action="store_false",
        help="Recompute every result even when a matching cached JSON exists.",
    )
    parser.add_argument("--no-build", action="store_true")
    parser.add_argument("--smoke-test", action="store_true")
    args = parser.parse_args()
    default_markdown = DEFAULT_OUTPUT_DIR / "distillation-comparison.md"
    use_output_dir_for_markdown = args.markdown == default_markdown
    if args.smoke_test:
        if args.plots_only:
            parser.error("--smoke-test cannot be combined with --plots-only.")
        args.truncation = 1
        args.uniform_w0_values = first_value(
            args.uniform_w0_values,
            "--uniform-w0-values",
        )
        args.t_coh_values = first_value(
            args.t_coh_values,
            "--t-coh-values",
        )
        args.output_dir = args.output_dir / "smoke"
    if use_output_dir_for_markdown:
        args.markdown = args.output_dir / "distillation-comparison.md"
    return args


def first_value(raw_values: str, flag: str) -> str:
    for raw_value in raw_values.split(","):
        value = raw_value.strip()
        if value:
            return value
    raise SystemExit(f"{flag} must contain at least one value.")


def parse_float_values(raw_values: str, flag: str) -> tuple[float, ...]:
    values = tuple(float(value.strip()) for value in raw_values.split(",") if value.strip())
    if not values:
        raise SystemExit(f"{flag} must contain at least one value.")
    return values


def parse_int_values(raw_values: str, flag: str) -> tuple[int, ...]:
    try:
        values = tuple(
            int(value.strip())
            for value in raw_values.split(",")
            if value.strip()
        )
    except ValueError as exc:
        raise SystemExit(f"{flag} entries must be integers.") from exc
    if not values:
        raise SystemExit(f"{flag} must contain at least one value.")
    return values


def validate_probability(flag: str, value: float, *, allow_zero: bool = False) -> None:
    lower_ok = value >= 0 if allow_zero else value > 0
    if not lower_ok or value > 1:
        interval = "[0, 1]" if allow_zero else "(0, 1]"
        raise SystemExit(f"{flag} must be in the interval {interval}.")


def validate_args(args) -> None:
    if args.truncation < 0:
        raise SystemExit("--truncation must be non-negative.")
    reference_p_ge = reference_p_ge_from_scaling_factor(args.generation_scaling)
    if args.generation_scaling <= 0 or reference_p_ge > 1:
        maximum = 1 / reference_p_ge_from_scaling_factor(1)
        raise SystemExit(
            f"--generation-scaling must be in the interval (0, {maximum:.6g}]."
        )
    for value in uniform_w0_values(args):
        validate_probability("--uniform-w0-values", value, allow_zero=True)
    for value in t_coh_values(args):
        if value <= 0:
            raise SystemExit("--t-coh-values entries must be positive.")
    validate_probability("--p-swap", args.p_swap, allow_zero=True)


def uniform_w0_values(args) -> tuple[float, ...]:
    return parse_float_values(args.uniform_w0_values, "--uniform-w0-values")


def t_coh_values(args) -> tuple[int, ...]:
    return parse_int_values(args.t_coh_values, "--t-coh-values")


def all_points(args) -> list[DistillationPoint]:
    return [
        DistillationPoint(
            p_ge=reference_p_ge_from_scaling_factor(args.generation_scaling),
            uniform_w0=uniform_w0,
            p_swap=args.p_swap,
            t_coh=t_coh,
        )
        for t_coh in t_coh_values(args)
        for uniform_w0 in uniform_w0_values(args)
    ]


def value_tag(value: float | int | None) -> str:
    if value is None:
        return "default"
    if isinstance(value, int):
        return str(value)
    return f"{value:.12g}".replace("-", "m").replace("+", "").replace(".", "p")


def value_text(value: float | int | None) -> str:
    if value is None:
        return "default"
    return f"{value:.12g}"


def scenario_tag(point: DistillationPoint) -> str:
    return (
        f"p{value_tag(point.p_ge)}"
        f"_uw{value_tag(point.uniform_w0)}"
        f"_pswap{value_tag(point.p_swap)}"
        f"_t{value_tag(point.t_coh)}"
    )


def command_args_for_point(point: DistillationPoint) -> list[str]:
    command = [
        "--p-ge",
        f"{point.p_ge:.17g}",
        "--uniform-w0",
        f"{point.uniform_w0:.17g}",
    ]
    if point.p_swap is not None:
        command.extend(("--p-swap", f"{point.p_swap:.17g}"))
    if point.t_coh is not None:
        command.extend(("--t-coh", str(point.t_coh)))
    return command


def json_path(data_dir: Path, point: DistillationPoint, protocol: str, event: str) -> Path:
    return data_dir / f"{FILE_PREFIX}_{scenario_tag(point)}_{protocol}_{QMDP_MODE}_{event}.json"


def existing_json_path(
    data_dir: Path,
    point: DistillationPoint,
    protocol: str,
    event: str,
    expected_truncation: int,
) -> Path:
    path = json_path(data_dir, point, protocol, event)
    if not path.is_file():
        raise SystemExit(f"Unusable existing JSON: {path} (file does not exist)")
    if path.stat().st_size == 0:
        raise SystemExit(f"Unusable existing JSON: {path} (file is empty)")
    try:
        payload = load_extremal_payload(path)
    except (OSError, UnicodeError, ValueError) as exc:
        raise SystemExit(f"Unusable existing JSON: {path} (cannot parse JSON: {exc})") from exc
    if not isinstance(payload, dict):
        raise SystemExit(f"Unusable existing JSON: {path} (missing extremal object)")
    if not isinstance(payload.get("series"), dict):
        raise SystemExit(f"Unusable existing JSON: {path} (missing extremal.series object)")

    resolved_budget = payload.get("resolved_budget")
    try:
        actual_truncation = int(resolved_budget)
    except (TypeError, ValueError) as exc:
        raise SystemExit(
            f"Unusable existing JSON: {path} "
            f"(invalid truncation {resolved_budget!r})"
        ) from exc
    if actual_truncation != expected_truncation:
        raise SystemExit(
            f"Unusable existing JSON: {path} "
            f"(truncation {resolved_budget}, expected {expected_truncation})"
        )
    return path


def build_if_needed(args) -> None:
    if args.no_build or args.plots_only or args.build_performed:
        return
    command = build_command(args.executable)
    if command is not None:
        run_command(command, status_label=f"cabal build {args.executable}")
    args.build_performed = True


def ensure_protocol_jsons(
    point: DistillationPoint,
    protocol: str,
    data_dir: Path,
    args,
) -> tuple[Path, Path, Path]:
    paths = []
    for event in (STATIC_EVENT, PURE_EVENT, MIXED_EVENT):
        target_path = json_path(data_dir, point, protocol, event)
        reused = False
        if args.plots_only or args.reuse_existing:
            try:
                paths.append(
                    existing_json_path(
                        data_dir,
                        point,
                        protocol,
                        event,
                        args.truncation,
                    )
                )
                reused = True
            except SystemExit as exc:
                if args.plots_only:
                    raise
                print(f"{exc}; rerunning {protocol} {event}", flush=True)
                paths.append(target_path)
        else:
            paths.append(target_path)
        if reused:
            print(f"{scenario_tag(point)} {protocol} {event}: reused {paths[-1]}", flush=True)
            continue
        build_if_needed(args)
        command = [
            *executable_command(args.executable),
            "--protocol",
            protocol,
            "--event",
            event,
            *command_args_for_point(point),
            "--json",
            QMDP_MODE,
            "--compute-extremal",
            "--truncation",
            str(args.truncation),
        ]
        status_label = f"{scenario_tag(point)} {protocol} {event}"
        elapsed = run_command(command, stdout_path=paths[-1], status_label=status_label)
        args.executed_cases += 1
        print(f"{status_label}: {elapsed:.2f}s -> {paths[-1]}", flush=True)
    return paths[0], paths[1], paths[2]


def evaluate_point(
    point: DistillationPoint,
    data_dir: Path,
    args,
    *,
    index: int,
    total: int,
) -> PointResult:
    skr_by_protocol = {}
    coverage_by_protocol = {}
    for protocol_index, protocol in enumerate(PROTOCOLS, start=1):
        print(
            f"[progress] point {index}/{total}; protocol {protocol_index}/{len(PROTOCOLS)}: {protocol}",
            flush=True,
        )
        static_path, pure_path, mixed_path = ensure_protocol_jsons(point, protocol, data_dir, args)
        skr_by_protocol[protocol] = compute_secret_key_rate_from_split(pure_path, mixed_path)
        static_series = load_extremal_series(static_path)
        coverage_by_protocol[protocol] = static_series["cdf_min"][-1]
        print(
            f"{scenario_tag(point)} {protocol}: "
            f"SKR={skr_by_protocol[protocol]:.12g}, "
            f"coverage={coverage_by_protocol[protocol]:.12g}",
            flush=True,
        )
    return PointResult(
        point=point,
        skr_by_protocol=skr_by_protocol,
        coverage_by_protocol=coverage_by_protocol,
    )


def best_distillation_protocol(result: PointResult) -> str:
    return max(
        DISTILL_PROTOCOLS,
        key=lambda protocol: result.skr_by_protocol[protocol],
    )


def swap_over_best_distillation(result: PointResult) -> float:
    swap_skr = result.skr_by_protocol[BASELINE_PROTOCOL]
    best_protocol = best_distillation_protocol(result)
    best_distillation_skr = result.skr_by_protocol[best_protocol]
    return swap_skr / best_distillation_skr if best_distillation_skr > 0 else math.nan


def minimum_coverage_by_protocol(
    results: dict[DistillationPoint, PointResult],
) -> dict[str, float]:
    return {
        protocol: min(result.coverage_by_protocol[protocol] for result in results.values())
        for protocol in PROTOCOLS
    }


def validate_minimum_coverage(results: dict[DistillationPoint, PointResult]) -> None:
    minimums = minimum_coverage_by_protocol(results)
    summary = ", ".join(f"{protocol}={value:.12g}" for protocol, value in minimums.items())
    print(f"Minimum completion coverage at R: {summary}", flush=True)
    failures = {
        protocol: value
        for protocol, value in minimums.items()
        if value < MINIMUM_COVERAGE
    }
    if failures:
        detail = ", ".join(
            f"{protocol}={value:.12g}" for protocol, value in failures.items()
        )
        raise SystemExit(
            f"Completion coverage is below {MINIMUM_COVERAGE:g}: {detail}. "
            "Increase --truncation and resume."
        )


def write_csv(path: Path, results: dict[DistillationPoint, PointResult]) -> None:
    with open(path, "w", encoding="utf-8", newline="") as handle:
        fieldnames = (
            "scenario",
            "generation_scaling",
            "p_ge",
            "uniform_w0",
            "p_swap",
            "t_coh",
            "swap_skr",
            "swap_coverage_at_truncation",
            "dist_swap_skr",
            "dist_swap_coverage_at_truncation",
            "swap_dist_skr",
            "swap_dist_coverage_at_truncation",
            "best_distillation_protocol",
            "best_distillation_skr",
            "swap_over_best_distillation",
            "dist_swap_over_swap",
            "swap_dist_over_swap",
        )
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for point, result in results.items():
            swap_skr = result.skr_by_protocol[BASELINE_PROTOCOL]
            dist_swap_skr = result.skr_by_protocol["dist-swap"]
            swap_dist_skr = result.skr_by_protocol["swap-dist"]
            best_protocol = best_distillation_protocol(result)
            best_distillation_skr = result.skr_by_protocol[best_protocol]
            writer.writerow(
                {
                    "scenario": scenario_tag(point),
                    "generation_scaling": f"{point.p_ge / reference_p_ge_from_scaling_factor(1):.12g}",
                    "p_ge": value_text(point.p_ge),
                    "uniform_w0": value_text(point.uniform_w0),
                    "p_swap": value_text(point.p_swap),
                    "t_coh": value_text(point.t_coh),
                    "swap_skr": f"{swap_skr:.12g}",
                    "swap_coverage_at_truncation": f"{result.coverage_by_protocol[BASELINE_PROTOCOL]:.12g}",
                    "dist_swap_skr": f"{dist_swap_skr:.12g}",
                    "dist_swap_coverage_at_truncation": f"{result.coverage_by_protocol['dist-swap']:.12g}",
                    "swap_dist_skr": f"{swap_dist_skr:.12g}",
                    "swap_dist_coverage_at_truncation": f"{result.coverage_by_protocol['swap-dist']:.12g}",
                    "best_distillation_protocol": best_protocol,
                    "best_distillation_skr": f"{best_distillation_skr:.12g}",
                    "swap_over_best_distillation": (
                        f"{swap_skr / best_distillation_skr:.12g}"
                        if best_distillation_skr > 0
                        else "nan"
                    ),
                    "dist_swap_over_swap": (
                        f"{dist_swap_skr / swap_skr:.12g}"
                        if swap_skr > 0
                        else "nan"
                    ),
                    "swap_dist_over_swap": (
                        f"{swap_dist_skr / swap_skr:.12g}"
                        if swap_skr > 0
                        else "nan"
                    ),
                }
            )


def annotate_best_distillation(
    ax,
    x_values: list[float],
    y_values: list[float],
    protocol_grid: list[list[str]],
) -> None:
    """Mark the pointwise D-S/S-D maximum, matching the sequential-order plot."""
    plotted_protocols = tuple(
        protocol
        for protocol in DISTILL_PROTOCOLS
        if any(protocol in row for row in protocol_grid)
    )
    if len(plotted_protocols) == 2:
        winner_codes = [
            [plotted_protocols.index(protocol) for protocol in row]
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

    for protocol in plotted_protocols:
        coordinates = [
            (x_values[x_index], y_values[y_index])
            for y_index, row in enumerate(protocol_grid)
            for x_index, winner in enumerate(row)
            if winner == protocol
        ]
        label_x = math.exp(
            sum(math.log(x_value) for x_value, _ in coordinates)
            / len(coordinates)
        )
        label_y = sum(y_value for _, y_value in coordinates) / len(coordinates)
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


def plot_ratio(
    plt,
    figure_dir: Path,
    results: dict[DistillationPoint, PointResult],
    args,
) -> Path:
    raw_x_values = list(t_coh_values(args))
    time_exponent = int(math.floor(math.log10(max(raw_x_values))))
    time_scale = 10**time_exponent
    x_values = [value / time_scale for value in raw_x_values]
    all_x_ticklabels = [f"{value:.4g}" for value in x_values]
    x_ticks, x_ticklabels = thinned_ticks(x_values, all_x_ticklabels, 3)
    y_values = list(uniform_w0_values(args))
    all_y_ticklabels = [f"{value:g}" for value in y_values]
    y_ticks, y_ticklabels = thinned_ticks(y_values, all_y_ticklabels, 4)
    result_grid = [
        [
            results[
                DistillationPoint(
                    p_ge=reference_p_ge_from_scaling_factor(args.generation_scaling),
                    uniform_w0=y_value,
                    p_swap=args.p_swap,
                    t_coh=x_value,
                )
            ]
            for x_value in raw_x_values
        ]
        for y_value in y_values
    ]
    ratio = [
        [swap_over_best_distillation(result) for result in row]
        for row in result_grid
    ]
    protocol_grid = [
        [best_distillation_protocol(result) for result in row]
        for row in result_grid
    ]

    fig, ax = plt.subplots(
        figsize=(OPTIMALITY_LINE_WIDTH_INCHES, OPTIMALITY_HEIGHT_INCHES)
    )
    draw_ratio_contour(
        fig,
        ax,
        x_values,
        y_values,
        ratio,
        cmap="PiYG",
        colorbar_label=r"$\mathrm{SKR}(\mathrm{wo.\ dist.}/\mathrm{w.\ dist.})$",
        xlabel=(
            r"$t_{\mathrm{coh}}$ ($t_{\mathrm{unit}}$)"
            rf" ($10^{{{time_exponent}}}$)"
        ),
        ylabel=r"$w_0$",
        log_x=True,
        x_ticks=x_ticks,
        y_ticks=y_ticks,
        y_ticklabels=y_ticklabels,
        x_ticklabels=x_ticklabels,
    )
    annotate_best_distillation(ax, x_values, y_values, protocol_grid)

    plot_profile = get_plot_profile(args.plot_profile)
    figure_path = output_path(
        figure_dir,
        FIGURE_PREFIX,
        "swap_over_best_distillation",
        plot_profile,
    )
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    return figure_path


def relative_link(from_path: Path, to_path: Path) -> str:
    return os.path.relpath(to_path, start=from_path.parent)


def write_report(
    markdown_path: Path,
    args,
    csv_path: Path,
    figure_path: Path | None,
    results: dict[DistillationPoint, PointResult],
) -> None:
    markdown_path.parent.mkdir(parents=True, exist_ok=True)
    minimums = minimum_coverage_by_protocol(results)
    lines = [
        "# Distillation Comparison",
        "",
        "Generated by `scripts/run_distillation_comparison.py`.",
        "",
        "## Configuration",
        "",
        f"- `generation_scaling_eta={args.generation_scaling:g}`",
        f"- `uniform_w0={args.uniform_w0_values}`",
        f"- `t_coh={args.t_coh_values}`",
        f"- `p_swap={value_text(args.p_swap)}`",
        f"- `truncation={args.truncation}`",
        f"- required minimum completion coverage: `{MINIMUM_COVERAGE}`",
        "- observed minimum completion coverage: "
        + ", ".join(f"`{protocol}={value:.12g}`" for protocol, value in minimums.items()),
        "- `swap`: swap raw `A-X` and `X-Y` at `X`, then swap at `Y`",
        "- `dist-swap` (D-S): distill `A-X` and `X-Y`, then swap at `X` and `Y`",
        "- `swap-dist` (S-D): swap twice at `X`, distill the two `A-Y` pairs, then swap at `Y`",
        f"- command: `{' '.join(sys.argv)}`",
        "",
        "## Data",
        "",
        f"- SKR CSV: [{csv_path.name}]({relative_link(markdown_path, csv_path)})",
        "",
    ]
    if figure_path is not None:
        lines.extend(
            [
                "## Figure",
                "",
                f"![without distillation over best with distillation]({relative_link(markdown_path, figure_path)})",
                "",
            ]
        )
    markdown_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    args = parse_args()
    validate_args(args)
    data_dir = args.output_dir / "data"
    figure_dir = args.output_dir / "figures"
    if args.plots_only:
        if not data_dir.is_dir():
            raise SystemExit(f"--plots-only requires existing data directory: {data_dir}")
    else:
        data_dir.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)

    args.build_performed = False
    args.executed_cases = 0
    points = all_points(args)
    started = time.perf_counter()
    results = {}
    print(f"[progress] starting {len(points)} point(s), {len(PROTOCOLS)} protocols per point", flush=True)
    for index, point in enumerate(points, start=1):
        elapsed = time.perf_counter() - started
        print(f"[progress] point {index}/{len(points)} after {format_duration(elapsed)}: {scenario_tag(point)}", flush=True)
        results[point] = evaluate_point(point, data_dir, args, index=index, total=len(points))

    if args.reuse_existing and not args.plots_only and args.executed_cases == 0:
        print("All simulation results were reused; Cabal build and execution were skipped.", flush=True)

    csv_path = args.output_dir / f"{FILE_PREFIX}_skr.csv"
    write_csv(csv_path, results)
    print(f"Wrote SKRs to {csv_path}", flush=True)
    if not args.smoke_test:
        validate_minimum_coverage(results)

    figure_path = None
    if args.smoke_test:
        print("Smoke test: skipped contour figure for the one-point grid.", flush=True)
    else:
        plt = configure_matplotlib(args.plot_profile)
        figure_path = plot_ratio(plt, figure_dir, results, args)
        print(f"Saved distillation comparison figure to {figure_path}", flush=True)

    write_report(args.markdown, args, csv_path, figure_path, results)
    print(f"Wrote markdown report to {args.markdown}", flush=True)


if __name__ == "__main__":
    main()
