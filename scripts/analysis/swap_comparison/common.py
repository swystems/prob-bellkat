import argparse
import csv
import math
import os
import subprocess
import time
from dataclasses import dataclass, replace
from pathlib import Path

import numpy as np

from scripts.plot.config import (
    DEFAULT_PROFILE,
    PLOT_SETTINGS,
    SWAP_COMPARISON_COMBINED_HEIGHT_INCHES,
    SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES,
    SWAP_COMPARISON_BINNED_LINE_WIDTH,
    SWAP_COMPARISON_DETAIL_INSET_BOUNDS,
    SWAP_COMPARISON_HEIGHT_INCHES,
    SWAP_COMPARISON_JOINT_LEGEND_Y,
    SWAP_COMPARISON_JOINT_TOP,
    SWAP_COMPARISON_LINE_WIDTH_INCHES,
    TIME_AXIS_LABEL,
    JOINT_PLOTS_WSPACE,
    get_plot_profile,
    hide_overlapping_inner_x_tick_label,
    output_path,
    save_figure,
)
from scripts.plot.plot_extremal import (
    configure_matplotlib,
    derive_average_werner_series,
    derive_plot_series,
    derive_pmf_series,
    load_extremal_payload,
    load_extremal_series,
    style_axes,
    werner_to_fid,
)
from scripts.utils.utils import secret_key_rate


DEFAULT_TRUNCATION = 100
MDP_MODE = "mdp"
QMDP_MODE = "qmdp"
STATIC_EVENT = "static"
PURE_EVENT = "pure"
MIXED_EVENT = "mixed"
COLORS = ("#cddb87","#ee7833", "#7c0006", "#cc9cff", "#01b56c")
LINE_ALPHA = 0.65
PROTOCOL_LEGEND_LABELS = {
    "swap-asap": "asap",
    "left-to-right": "L2R",
    "right-to-left": "R2L",
}

PMF_ASSERTION_TOLERANCE = 1e-4
PMF_PLOT_KIND = "pmf"
CDF_PLOT_KIND = "cdf"
BOTH_PLOT_KIND = "both"
PLOT_KINDS = (PMF_PLOT_KIND, CDF_PLOT_KIND, BOTH_PLOT_KIND)

# Physical generation baseline for a 50 km link in Common.NetworkConfig.
PHYSICAL_P_GE_50_KM = 9.187e-4


def generation_scaling_factor(reference_p_ge: float) -> float:
    """Express a 50 km reference probability relative to its physical baseline."""
    return reference_p_ge / PHYSICAL_P_GE_50_KM


def reference_p_ge_from_scaling_factor(scaling_factor: float) -> float:
    """Convert a generation scaling factor to the 50 km reference probability."""
    return scaling_factor * PHYSICAL_P_GE_50_KM


def edge_generation_scaling_factor(edge_skew: float) -> float:
    """Convert the internal divisor for the slow edge to a multiplicative factor."""
    return 1.0 / edge_skew


def detail_range(value):
    try:
        start_text, end_text = (part.strip() for part in value.split(","))
        start = int(start_text)
        end = int(end_text)
    except (TypeError, ValueError):
        raise argparse.ArgumentTypeError(
            "detail range must have the form T1,T2 with integer timesteps"
        ) from None
    if start < 0 or end <= start:
        raise argparse.ArgumentTypeError(
            "detail range must satisfy 0 <= T1 < T2"
        )
    return start, end


@dataclass(frozen=True)
class ComparisonConfig:
    description: str
    default_protocols: tuple[str, ...]
    executable: str
    output_dir: str
    figure_dir: str
    file_prefix: str
    figure_prefix: str
    default_truncation: int = DEFAULT_TRUNCATION
    colors: tuple[str, ...] = COLORS
    plot_profile: str = DEFAULT_PROFILE


def parse_args(config):
    parser = argparse.ArgumentParser(description=config.description)
    parser.add_argument(
        "--protocol",
        action="append",
        choices=config.default_protocols,
        help="Protocol to run. Can be passed multiple times. Defaults to all protocols.",
    )
    budget_group = parser.add_mutually_exclusive_group()
    budget_group.add_argument(
        "--truncation",
        type=int,
        default=None,
        help=(
            "Extremal reachability budget passed to --truncation. "
            f"Defaults to {config.default_truncation} when --coverage is not used."
        ),
    )
    budget_group.add_argument(
        "--coverage",
        type=float,
        default=None,
        help=(
            "First run each selected protocol in MDP/static mode until the "
            "worst-scheduler CDF reaches this probability, then rerun the full "
            "comparison with --truncation set to the maximum resolved budget."
        ),
    )
    parser.add_argument(
        "--output-dir",
        default=config.output_dir,
        help="Directory for JSON dumps and timing summary.",
    )
    parser.add_argument(
        "--figure-dir",
        default=config.figure_dir,
        help="Directory for combined PMF/CDF and Werner figures.",
    )
    parser.add_argument(
        "--plot-kind",
        choices=PLOT_KINDS,
        default=BOTH_PLOT_KIND,
        help=(
            "Plot the combined MDP reachability figure as a PMF, CDF, or both. "
            "Use 'both' to produce both figures."
        ),
    )
    parser.add_argument(
        "--plot-profile",
        choices=tuple(PLOT_SETTINGS),
        default=config.plot_profile,
        help="Plot styling profile.",
    )
    parser.add_argument(
        "--executable",
        default=config.executable,
        help="Cabal executable name, or path to an already-built executable.",
    )
    parser.add_argument(
        "--validate-deterministic-extrema",
        action="store_true",
        help="Assert that min and max extremal series coincide for every run.",
    )
    parser.add_argument(
        "--validate-static-split",
        action="store_true",
        help=(
            "Assert that the MDP/static PMF equals the sum of the QMDP pure "
            "and mixed PMFs. This runs the extra MDP/static case unless "
            "--plots-only is enabled, in which case the existing static JSON "
            "is loaded from --output-dir."
        ),
    )
    parser.add_argument(
        "--plot-fidelity",
        "--plot_fidelity",
        action="store_true",
        dest="plot_fidelity",
        help="Also plot average fidelity, derived from the average Werner parameter.",
    )
    parser.add_argument(
        "--joint-plots",
        "--joint_plots",
        action="store_true",
        dest="joint_plots",
        help=(
            "Also plot PMF and Werner, or PMF and fidelity when --plot-fidelity "
            "is enabled, as two side-by-side panels sharing the x-axis."
        ),
    )
    parser.add_argument(
        "--show-detail",
        type=detail_range,
        metavar="T1,T2",
        help=(
            "Add an inset to the joint PMF panel showing timesteps T1 through T2. "
            "Requires --joint-plots."
        ),
    )
    parser.add_argument(
        "--show-detail-link",
        action="store_true",
        help=(
            "Mark the source region and connect it to the --show-detail inset. "
            "Requires --show-detail."
        ),
    )
    parser.add_argument(
        "--show-skr-legend",
        action="store_true",
        help="Append each protocol's secret key rate, in scientific notation, to plot legends.",
    )
    parser.add_argument(
        "--skip-werner-legend",
        action="store_true",
        help="Hide the legend only in the standalone Werner plot.",
    )
    parser.add_argument(
        "--binning",
        type=int,
        default=10,
        metavar="BIN",
        help=(
            "Average each consecutive BIN-point time window before plotting the "
            "overlaid dashed curves. Defaults to 10."
        ),
    )
    binning_group = parser.add_mutually_exclusive_group()
    binning_group.add_argument(
        "--only-binned-plots",
        "--only_binned_plots",
        action="store_true",
        dest="only_binned_plots",
        help="Plot only the dashed, binned curves.",
    )
    binning_group.add_argument(
        "--only-non-binned",
        "--only_non_binned",
        action="store_true",
        dest="only_non_binned",
        help="Plot only the original non-binned curves.",
    )
    parser.add_argument(
        "--plots-only",
        "--plots_only",
        action="store_true",
        dest="plots_only",
        help=(
            "Skip all Cabal runs and generate plots from existing JSON dumps "
            "in --output-dir, using the selected protocols and plotting flags."
        ),
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip the initial cabal build step.",
    )
    parser.add_argument(
        "--smoke-test",
        action="store_true",
        help="Run one tiny protocol case with truncation 1 under a smoke output directory.",
    )
    args = parser.parse_args()
    if args.binning < 1:
        parser.error("--binning must be a positive integer.")
    if args.show_detail is not None and not args.joint_plots:
        parser.error("--show-detail requires --joint-plots.")
    if args.show_detail_link and args.show_detail is None:
        parser.error("--show-detail-link requires --show-detail.")
    if args.smoke_test:
        if args.plots_only:
            parser.error("--smoke-test cannot be combined with --plots-only.")
        smoke_protocol = "doubling" if "doubling" in config.default_protocols else config.default_protocols[0]
        if not args.protocol:
            args.protocol = [smoke_protocol]
        args.coverage = None
        args.truncation = 1
        args.output_dir = str(Path(args.output_dir) / "smoke")
        args.figure_dir = str(Path(args.figure_dir) / "smoke")
    return args


def format_duration(seconds):
    seconds = int(seconds)
    hours, remainder = divmod(seconds, 3600)
    minutes, seconds = divmod(remainder, 60)
    if hours:
        return f"{hours}h{minutes:02d}m{seconds:02d}s"
    if minutes:
        return f"{minutes}m{seconds:02d}s"
    return f"{seconds}s"


def validate_extremal_json(path, *, require_coverage=False):
    path = Path(path)
    if not path.is_file():
        return False, "file does not exist"
    if path.stat().st_size == 0:
        return False, "file is empty"

    try:
        payload = load_extremal_payload(path)
    except (OSError, UnicodeError, ValueError) as exc:
        return False, f"cannot parse JSON: {exc}"

    if not isinstance(payload, dict):
        return False, "missing extremal object"
    if not isinstance(payload.get("series"), dict):
        return False, "missing extremal.series object"
    if payload.get("resolved_budget") is None:
        return False, "missing extremal.resolved_budget"
    if require_coverage and not isinstance(payload.get("coverage_status"), dict):
        return False, "missing extremal.coverage_status object"
    return True, None


def run_command(command, stdout_path=None, status_label=None, heartbeat_seconds=30):
    started = time.perf_counter()
    stdout_handle = None
    stdout_target = subprocess.PIPE
    final_stdout_path = None
    temporary_stdout_path = None
    if stdout_path is not None:
        final_stdout_path = Path(stdout_path)
        temporary_stdout_path = final_stdout_path.with_name(
            f".{final_stdout_path.name}.tmp-{os.getpid()}"
        )
        stdout_handle = open(temporary_stdout_path, "w", encoding="utf-8")
        stdout_target = stdout_handle

    process = subprocess.Popen(
        command,
        text=True,
        stdout=stdout_target,
        stderr=subprocess.PIPE,
    )
    stderr = ""
    try:
        while True:
            try:
                _, stderr = process.communicate(timeout=heartbeat_seconds)
                break
            except subprocess.TimeoutExpired:
                if status_label:
                    elapsed = time.perf_counter() - started
                    print(
                        f"[progress] {status_label}: still running after "
                        f"{format_duration(elapsed)}",
                        flush=True,
                    )
    finally:
        if stdout_handle is not None:
            stdout_handle.close()

    elapsed = time.perf_counter() - started

    if process.returncode != 0:
        if temporary_stdout_path is not None:
            temporary_stdout_path.unlink(missing_ok=True)
        command_text = " ".join(command)
        raise SystemExit(
            f"Command failed after {elapsed:.2f}s: {command_text}\n{stderr}"
        )

    if temporary_stdout_path is not None:
        temporary_stdout_path.replace(final_stdout_path)

    if status_label:
        print(
            f"[progress] {status_label}: finished in {format_duration(elapsed)}",
            flush=True,
        )

    return elapsed


def executable_command(executable):
    path = Path(executable)
    is_path = path.is_absolute() or os.sep in executable or (
        os.altsep is not None and os.altsep in executable
    )
    if is_path or path.is_file():
        return [str(path)]
    return ["cabal", "run", "-v0", executable, "--"]


def build_command(executable):
    path = Path(executable)
    is_path = path.is_absolute() or os.sep in executable or (
        os.altsep is not None and os.altsep in executable
    )
    if is_path or path.is_file():
        return None
    return ["cabal", "build", executable]


def output_json_path(output_dir, file_prefix, protocol, mode, event):
    return output_dir / f"{file_prefix}_{protocol}_{mode}_{event}.json"


def output_coverage_json_path(output_dir, file_prefix, protocol):
    return output_dir / f"{file_prefix}_{protocol}_{MDP_MODE}_{STATIC_EVENT}_coverage.json"


def existing_json_path(output_dir, file_prefix, protocol, mode, event):
    path = output_json_path(output_dir, file_prefix, protocol, mode, event)
    if not path.is_file():
        raise SystemExit(
            f"Missing existing JSON for {protocol} {mode}/{event}: {path}\n"
            "Run the comparison without --plots-only first, or adjust "
            "--output-dir/--protocol to match the available dumps."
        )
    return path


def run_extremal_case(
    executable,
    protocol,
    mode,
    event,
    budget_flag,
    budget_value,
    output_path,
):
    command = [
        *executable_command(executable),
        "--protocol",
        protocol,
        "--event",
        event,
        "--json",
        mode,
        "--compute-extremal",
        budget_flag,
        str(budget_value),
    ]
    status_label = f"{protocol} {mode}/{event}"
    elapsed = run_command(command, stdout_path=output_path, status_label=status_label)
    return output_path, elapsed


def run_case(executable, file_prefix, protocol, mode, event, truncation, output_dir):
    return run_extremal_case(
        executable,
        protocol,
        mode,
        event,
        "--truncation",
        truncation,
        output_json_path(output_dir, file_prefix, protocol, mode, event),
    )


def run_coverage_case(executable, file_prefix, protocol, coverage, output_dir):
    return run_extremal_case(
        executable,
        protocol,
        MDP_MODE,
        STATIC_EVENT,
        "--coverage",
        coverage,
        output_coverage_json_path(output_dir, file_prefix, protocol),
    )


def write_summary(summary_path, rows):
    with open(summary_path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=("protocol", "mode", "event", "seconds", "json_path"),
        )
        writer.writeheader()
        writer.writerows(rows)


def write_coverage_summary(summary_path, rows):
    with open(summary_path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=(
                "protocol",
                "coverage",
                "resolved_budget",
                "coverage_value",
                "seconds",
                "json_path",
            ),
        )
        writer.writeheader()
        writer.writerows(rows)


def validate_truncation(truncation):
    if truncation < 0:
        raise SystemExit("--truncation must be a non-negative integer.")


def validate_coverage(coverage):
    if not 0.0 < coverage <= 1.0:
        raise SystemExit("--coverage must be a probability in the interval (0, 1].")


def validate_budget_args(args):
    if args.coverage is not None:
        validate_coverage(args.coverage)
    elif args.truncation is not None:
        validate_truncation(args.truncation)


def load_coverage_budget(protocol, coverage, json_path):
    payload = load_extremal_payload(json_path)
    if payload is None:
        raise SystemExit(f"{json_path} does not contain an extremal payload.")

    resolved_budget = payload.get("resolved_budget")
    coverage_status = payload.get("coverage_status")

    if resolved_budget is None:
        raise SystemExit(f"{json_path} does not contain resolved_budget.")
    if not isinstance(coverage_status, dict):
        raise SystemExit(f"{json_path} does not contain a coverage_status object.")

    cached_target = coverage_status.get("target")
    if not isinstance(cached_target, (int, float)):
        raise SystemExit(
            f"{protocol} cached coverage result {json_path} does not contain a numeric "
            "coverage_status.target; refusing to reuse it."
        )
    if not math.isclose(float(cached_target), float(coverage), rel_tol=0.0, abs_tol=1e-12):
        raise SystemExit(
            f"{protocol} cached coverage target mismatch in {json_path}: "
            f"requested {coverage:g}, cached {cached_target:g}. "
            "Remove or regenerate the cached coverage result before resuming."
        )

    status = coverage_status.get("status")
    coverage_value = coverage_status.get("value")
    if status != "reached":
        raise SystemExit(
            f"{protocol} did not reach coverage {coverage:g}; "
            f"status={status}, R={resolved_budget}, value={coverage_value}."
        )

    status_budget = coverage_status.get("budget")
    if status_budget is not None and int(status_budget) != int(resolved_budget):
        raise SystemExit(
            f"{protocol} coverage budget mismatch: "
            f"resolved_budget={resolved_budget}, coverage_status.budget={status_budget}."
        )

    return int(resolved_budget), coverage_value


def resolve_truncation(args, protocols, output_dir, config):
    if args.coverage is None:
        truncation = (
            args.truncation
            if args.truncation is not None
            else config.default_truncation
        )
        validate_truncation(truncation)
        return truncation

    validate_coverage(args.coverage)
    coverage_rows = []
    coverage_budgets = {}

    for protocol in protocols:
        json_path, elapsed = run_coverage_case(
            args.executable,
            config.file_prefix,
            protocol,
            args.coverage,
            output_dir,
        )
        resolved_budget, coverage_value = load_coverage_budget(
            protocol,
            args.coverage,
            json_path,
        )
        coverage_budgets[protocol] = resolved_budget
        coverage_rows.append(
            {
                "protocol": protocol,
                "coverage": f"{args.coverage:.12g}",
                "resolved_budget": resolved_budget,
                "coverage_value": f"{coverage_value:.12g}",
                "seconds": f"{elapsed:.6f}",
                "json_path": str(json_path),
            }
        )
        print(
            f"{protocol} {MDP_MODE}/{STATIC_EVENT} coverage {args.coverage:g}: "
            f"R={resolved_budget}, value={coverage_value:.12g}, "
            f"{elapsed:.2f}s -> {json_path}"
        )

    write_coverage_summary(output_dir / "coverage_budgets.csv", coverage_rows)
    truncation = max(coverage_budgets.values())
    print(f"Using max coverage budget R={truncation} for the full comparison.")
    return truncation


def assert_close_series(left, right, description):
    if len(left) != len(right):
        raise SystemExit(
            f"{description} has mismatched lengths: left={len(left)}, right={len(right)}"
        )

    for t, (left_value, right_value) in enumerate(zip(left, right)):
        if not math.isclose(
            left_value,
            right_value,
            rel_tol=0.0,
            abs_tol=PMF_ASSERTION_TOLERANCE,
        ):
            raise SystemExit(
                f"{description} differs at t={t}: "
                f"left={left_value:.17g}, right={right_value:.17g}"
            )


def is_close_series(left, right):
    if len(left) != len(right):
        return False

    return all(
        math.isclose(
            left_value,
            right_value,
            rel_tol=0.0,
            abs_tol=PMF_ASSERTION_TOLERANCE,
        )
        for left_value, right_value in zip(left, right)
    )


def assert_split_bounds(lower, value, upper, description):
    if len(lower) != len(value) or len(value) != len(upper):
        raise SystemExit(
            f"{description} has mismatched lengths: "
            f"lower={len(lower)}, value={len(value)}, upper={len(upper)}"
        )

    for t, (lower_value, value_value, upper_value) in enumerate(zip(lower, value, upper)):
        if value_value + PMF_ASSERTION_TOLERANCE < lower_value:
            raise SystemExit(
                f"{description} is below its split lower bound at t={t}: "
                f"value={value_value:.17g}, lower={lower_value:.17g}"
            )
        if value_value - PMF_ASSERTION_TOLERANCE > upper_value:
            raise SystemExit(
                f"{description} is above its split upper bound at t={t}: "
                f"value={value_value:.17g}, upper={upper_value:.17g}"
            )


def assert_extrema_coincide(protocol, mode, event, json_path):
    series = load_extremal_series(json_path)
    assert_close_series(
        series["cdf_min"],
        series["cdf_max"],
        f"{protocol} {mode}/{event} CDF min/max",
    )
    pmf_min, pmf_max = derive_pmf_series(series)
    assert_close_series(
        pmf_min,
        pmf_max,
        f"{protocol} {mode}/{event} PMF min/max",
    )


def assert_static_pmf_equals_pure_plus_mixed(protocol, static_path, pure_path, mixed_path):
    static_pmf_min, static_pmf_max = derive_pmf_series(load_extremal_series(static_path))
    pure_pmf_min, pure_pmf_max = derive_pmf_series(load_extremal_series(pure_path))
    mixed_pmf_min, mixed_pmf_max = derive_pmf_series(load_extremal_series(mixed_path))

    reconstructed_min = [
        pure_prob + mixed_prob
        for pure_prob, mixed_prob in zip(pure_pmf_min, mixed_pmf_min)
    ]
    reconstructed_max = [
        pure_prob + mixed_prob
        for pure_prob, mixed_prob in zip(pure_pmf_max, mixed_pmf_max)
    ]

    deterministic = all(
        is_close_series(pmf_min, pmf_max)
        for pmf_min, pmf_max in (
            (static_pmf_min, static_pmf_max),
            (pure_pmf_min, pure_pmf_max),
            (mixed_pmf_min, mixed_pmf_max),
        )
    )

    if deterministic:
        assert_close_series(
            static_pmf_max,
            reconstructed_max,
            f"{protocol} PMF static != pure + mixed",
        )
        return "exact"

    assert_split_bounds(
        reconstructed_min,
        static_pmf_min,
        reconstructed_max,
        f"{protocol} PMF min static",
    )
    assert_split_bounds(
        reconstructed_min,
        static_pmf_max,
        reconstructed_max,
        f"{protocol} PMF max static",
    )
    return "bounds"


def cdf_from_pmf(pmf):
    cdf = []
    total = 0.0
    for probability in pmf:
        total += probability
        cdf.append(total)
    return cdf


def sum_pmf_series(left, right, description):
    if len(left) != len(right):
        raise SystemExit(
            f"{description} has mismatched lengths: left={len(left)}, right={len(right)}"
        )

    return [
        left_probability + right_probability
        for left_probability, right_probability in zip(left, right)
    ]


def derive_full_pmf_series_from_split(pure_path, mixed_path):
    pure_pmf_min, pure_pmf_max = derive_pmf_series(load_extremal_series(pure_path))
    mixed_pmf_min, mixed_pmf_max = derive_pmf_series(load_extremal_series(mixed_path))

    full_pmf_min = sum_pmf_series(
        pure_pmf_min,
        mixed_pmf_min,
        "pure/mixed PMF min",
    )
    full_pmf_max = sum_pmf_series(
        pure_pmf_max,
        mixed_pmf_max,
        "pure/mixed PMF max",
    )
    return full_pmf_min, full_pmf_max


def derive_full_series_from_split(pure_path, mixed_path):
    full_pmf_min, full_pmf_max = derive_full_pmf_series_from_split(pure_path, mixed_path)
    return {
        "cdf_min": cdf_from_pmf(full_pmf_min),
        "cdf_max": cdf_from_pmf(full_pmf_max),
    }


def compute_secret_key_rate_from_split(pure_path, mixed_path):
    _, static_pmf = derive_full_pmf_series_from_split(pure_path, mixed_path)
    pure_series = load_extremal_series(pure_path)
    mixed_series = load_extremal_series(mixed_path)
    _, _, werner = derive_average_werner_series(pure_series, mixed_series)

    if len(static_pmf) != len(werner):
        raise SystemExit(
            "Static PMF and Werner series must have matching lengths to compute SKR."
        )

    return secret_key_rate(np.array(static_pmf), np.array(werner))


def protocol_legend_label(protocol, skr_by_protocol, show_skr):
    label = PROTOCOL_LEGEND_LABELS.get(protocol, protocol)
    if not show_skr:
        return f"QBKAT {label}"
    skr = skr_by_protocol.get(protocol)
    if skr is None:
        return label
    return f"{label} SKR={skr:.2e}"


def bin_time_series(t, values, bin_size):
    t_array = np.asarray(t, dtype=float)
    values_array = np.asarray(values, dtype=float)
    if t_array.shape != values_array.shape:
        raise ValueError("Time and value series must have matching shapes for binning.")
    if bin_size < 1:
        raise ValueError("bin_size must be a positive integer.")

    binned_t = []
    binned_values = []
    for start in range(0, len(t_array), bin_size):
        stop = min(start + bin_size, len(t_array))
        binned_t.append(float(np.mean(t_array[start:stop])))
        binned_values.append(float(np.mean(values_array[start:stop])))
    return binned_t, binned_values


def plot_time_series(
    ax,
    t,
    values,
    *,
    color,
    label,
    bin_size,
    only_binned,
    only_non_binned,
):
    variants = time_series_variants(
        t,
        values,
        bin_size=bin_size,
        only_binned=only_binned,
        only_non_binned=only_non_binned,
    )
    for index, (
        variant_t,
        variant_values,
        alpha,
        linestyle,
        zorder,
    ) in enumerate(variants):
        ax.plot(
            variant_t,
            variant_values,
            color=color,
            alpha=alpha,
            linestyle=linestyle,
            linewidth=(
                SWAP_COMPARISON_BINNED_LINE_WIDTH
                if linestyle == "--"
                else None
            ),
            label=label if index == len(variants) - 1 else "_nolegend_",
            zorder=zorder,
        )


def time_series_variants(
    t,
    values,
    *,
    bin_size,
    only_binned,
    only_non_binned,
):
    plot_non_binned = not only_binned
    plot_binned = not only_non_binned
    variants = []
    if plot_non_binned:
        variants.append((t, values, LINE_ALPHA, "-", 2))
    if plot_binned:
        binned_t, binned_values = bin_time_series(t, values, bin_size)
        variants.append((binned_t, binned_values, 1.0, "--", 3))
    return variants


def add_pmf_detail_inset(
    pmf_ax,
    plot_data,
    detail,
    *,
    bin_size,
    only_binned,
    only_non_binned,
    show_link=False,
):
    from matplotlib.ticker import FixedLocator, FuncFormatter, MaxNLocator

    start, end = detail
    inset_ax = pmf_ax.inset_axes(SWAP_COMPARISON_DETAIL_INSET_BOUNDS)
    inset_values = []

    for t, values, color in plot_data:
        for variant_t, variant_values, alpha, linestyle, zorder in time_series_variants(
            t,
            values,
            bin_size=bin_size,
            only_binned=only_binned,
            only_non_binned=only_non_binned,
        ):
            variant_t = np.asarray(variant_t, dtype=float)
            variant_values = np.asarray(variant_values, dtype=float)
            mask = (variant_t >= start) & (variant_t <= end)
            if not np.any(mask):
                continue
            detail_t = variant_t[mask]
            detail_values = variant_values[mask]
            inset_ax.plot(
                detail_t,
                detail_values,
                color=color,
                alpha=alpha,
                linestyle=linestyle,
                linewidth=(
                    SWAP_COMPARISON_BINNED_LINE_WIDTH
                    if linestyle == "--"
                    else None
                ),
                zorder=zorder,
            )
            inset_values.extend(detail_values.tolist())

    if not inset_values:
        raise SystemExit(
            f"--show-detail range {start},{end} does not overlap the plotted PMF data."
        )

    value_min = min(inset_values)
    value_max = max(inset_values)
    value_span = value_max - value_min
    padding = (
        0.08 * value_span
        if value_span > 0.0
        else max(abs(value_max) * 0.08, 1e-15)
    )
    inset_ax.set_xlim(start, end)
    inset_ax.set_ylim(max(0.0, value_min - padding), value_max + padding)
    style_axes(inset_ax)

    inset_ax.xaxis.set_major_locator(FixedLocator((start, end)))
    inset_ax.xaxis.set_major_formatter(
        FuncFormatter(lambda value, _position: f"{value:g}")
    )
    inset_ax.yaxis.set_major_locator(MaxNLocator(3))
    inset_ax.yaxis.set_major_formatter(
        FuncFormatter(lambda value, _position: f"{value:.2g}")
    )
    inset_ax.tick_params(axis="both", labelsize=5.5, length=2.0, pad=1.0)
    inset_tick_labels = inset_ax.get_xticklabels()
    inset_tick_labels[0].set_ha("left")
    inset_tick_labels[-1].set_ha("right")
    inset_ax.set_facecolor("white")
    inset_ax.patch.set_alpha(0.96)
    if show_link:
        inset_indicator = pmf_ax.indicate_inset_zoom(
            inset_ax,
            edgecolor="0.65",
            linewidth=0.5,
            alpha=0.4,
            zorder=4,
        )
        connectors = getattr(inset_indicator, "connectors", None)
        if connectors is None:
            _, connectors = inset_indicator
        for connector in connectors:
            connector.set_visible(True)


def plot_combined_reachability(
    plt,
    figure_dir,
    protocol_series,
    plot_kind,
    config,
    *,
    skr_by_protocol=None,
    show_skr=False,
    bin_size=10,
    only_binned=False,
    only_non_binned=False,
):
    fig, ax = plt.subplots(
        figsize=(SWAP_COMPARISON_LINE_WIDTH_INCHES, SWAP_COMPARISON_HEIGHT_INCHES)
    )
    plot_profile = get_plot_profile(config.plot_profile)
    skr_by_protocol = skr_by_protocol or {}

    for index, (protocol, series) in enumerate(protocol_series):
        if plot_kind == CDF_PLOT_KIND:
            t, _, _, _, reachability = derive_plot_series(series)
        else:
            _, reachability = derive_pmf_series(series)
            t = list(range(len(reachability)))
        color = config.colors[index % len(config.colors)]

        plot_time_series(
            ax,
            t,
            reachability,
            color=color,
            label=protocol_legend_label(protocol, skr_by_protocol, show_skr),
            bin_size=bin_size,
            only_binned=only_binned,
            only_non_binned=only_non_binned,
        )

    ax.set_xlabel(TIME_AXIS_LABEL)
    if plot_kind == CDF_PLOT_KIND:
        ax.set_ylabel("Cumulative probability")
        # ax.set_ylim(0.0, 1.0)
    else:
        ax.set_ylabel("Probability")
    style_axes(ax)
    ax.legend(frameon=False, loc="best", ncol=1 if show_skr else 2)

    figure_path = output_path(figure_dir, config.figure_prefix, f"mdp_{plot_kind}s", plot_profile)
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    print(f"Saved {plot_kind.upper()}s figure to {figure_path}")


def plot_combined_quality(
    plt,
    figure_dir,
    protocol_paths,
    config,
    *,
    quality,
    skr_by_protocol=None,
    show_skr=False,
    skip_legend=False,
    bin_size=10,
    only_binned=False,
    only_non_binned=False,
):
    fig, ax = plt.subplots(
        figsize=(SWAP_COMPARISON_LINE_WIDTH_INCHES, SWAP_COMPARISON_HEIGHT_INCHES)
    )
    plot_profile = get_plot_profile(config.plot_profile)
    skr_by_protocol = skr_by_protocol or {}

    for index, (protocol, pure_path, mixed_path) in enumerate(protocol_paths):
        pure_series = load_extremal_series(pure_path)
        mixed_series = load_extremal_series(mixed_path)
        t, _, werner = derive_average_werner_series(
            pure_series,
            mixed_series,
        )
        color = config.colors[index % len(config.colors)]

        filtered = [
            (t_i, w)
            for t_i, w in zip(t, werner)
            if t_i > 0 and w > 0.0
        ]
        if not filtered:
            continue

        t, werner = zip(*filtered)
        if quality == "fidelity":
            values = [werner_to_fid(w) for w in werner]
        else:
            values = werner

        plot_time_series(
            ax,
            t,
            values,
            color=color,
            label=protocol_legend_label(protocol, skr_by_protocol, show_skr),
            bin_size=bin_size,
            only_binned=only_binned,
            only_non_binned=only_non_binned,
        )

    ax.set_xlabel(TIME_AXIS_LABEL)
    if quality == "fidelity":
        ax.set_ylabel(r"Fidelity")
        # ax.set_ylim(0.25, 1.0)
        suffix = "qmdp_fids"
    else:
        ax.set_ylabel(r"Werner parameter")
        # ax.set_ylim(0.0, 1.0)
        suffix = "qmdp_ws"

    style_axes(ax)
    if not skip_legend and ax.get_legend_handles_labels()[0]:
        ax.legend(frameon=False, loc="best", ncol=1 if show_skr else 2)

    figure_path = output_path(figure_dir, config.figure_prefix, suffix, plot_profile)
    save_figure(fig, figure_path, bbox_inches=None)
    plt.close(fig)
    print(f"Saved {quality} figure to {figure_path}")


def plot_joint_pmf_quality(
    plt,
    figure_dir,
    reachability_protocol_series,
    protocol_paths,
    config,
    *,
    quality,
    skr_by_protocol=None,
    show_skr=False,
    bin_size=10,
    only_binned=False,
    only_non_binned=False,
    show_detail=None,
    show_detail_link=False,
):
    plot_profile = get_plot_profile(config.plot_profile)
    skr_by_protocol = skr_by_protocol or {}
    fig, (pmf_ax, quality_ax) = plt.subplots(
        1,
        2,
        sharex=True,
        figsize=(
            SWAP_COMPARISON_COMBINED_LINE_WIDTH_INCHES,
            SWAP_COMPARISON_COMBINED_HEIGHT_INCHES,
        ),
        gridspec_kw={"width_ratios": (1.0, 1.0), "wspace": JOINT_PLOTS_WSPACE},
    )

    pmf_plot_data = []
    for index, (protocol, series) in enumerate(reachability_protocol_series):
        _, pmf = derive_pmf_series(series)
        t = list(range(len(pmf)))
        color = config.colors[index % len(config.colors)]
        pmf_plot_data.append((t, pmf, color))
        plot_time_series(
            pmf_ax,
            t,
            pmf,
            color=color,
            label=protocol_legend_label(protocol, skr_by_protocol, show_skr),
            bin_size=bin_size,
            only_binned=only_binned,
            only_non_binned=only_non_binned,
        )

    if show_detail is not None:
        add_pmf_detail_inset(
            pmf_ax,
            pmf_plot_data,
            show_detail,
            bin_size=bin_size,
            only_binned=only_binned,
            only_non_binned=only_non_binned,
            show_link=show_detail_link,
        )

    for index, (protocol, pure_path, mixed_path) in enumerate(protocol_paths):
        pure_series = load_extremal_series(pure_path)
        mixed_series = load_extremal_series(mixed_path)
        t, _, werner = derive_average_werner_series(
            pure_series,
            mixed_series,
        )
        color = config.colors[index % len(config.colors)]

        filtered = [
            (t_i, w)
            for t_i, w in zip(t, werner)
            if t_i > 0 and w > 0.0
        ]
        if not filtered:
            continue

        t, werner = zip(*filtered)
        if quality == "fidelity":
            values = [werner_to_fid(w) for w in werner]
        else:
            values = werner

        plot_time_series(
            quality_ax,
            t,
            values,
            color=color,
            label=protocol_legend_label(protocol, skr_by_protocol, show_skr),
            bin_size=bin_size,
            only_binned=only_binned,
            only_non_binned=only_non_binned,
        )

    pmf_ax.set_xlabel(TIME_AXIS_LABEL)
    pmf_ax.set_ylabel("Probability")
    if quality == "fidelity":
        quality_ax.set_ylabel(r"Fidelity")
        suffix = "pmfs_fids"
    else:
        quality_ax.set_ylabel(r"Werner parameter")
        # quality_ax.set_ylim(0.0, 1.0)
        suffix = "pmfs_ws"
    quality_ax.set_xlabel(TIME_AXIS_LABEL)
    quality_ax.yaxis.set_label_position("right")
    quality_ax.yaxis.tick_right()
    quality_ax.tick_params(
        axis="y",
        left=False,
        labelleft=False,
        right=True,
        labelright=True,
    )
    pmf_ax.margins(x=0)
    quality_ax.margins(x=0)
    pmf_ax.set_xlim(left=0)

    style_axes(pmf_ax)
    style_axes(quality_ax)

    handles, labels = pmf_ax.get_legend_handles_labels()
    if handles:
        fig.legend(
            handles,
            labels,
            frameon=False,
            loc="upper center",
            bbox_to_anchor=(0.5, SWAP_COMPARISON_JOINT_LEGEND_Y),
            ncol=len(handles),
            columnspacing=0.8,
            handletextpad=0.4,
            handlelength=1.5,
            fontsize=8,
        )

    fig.subplots_adjust(
        left=0.08,
        right=0.88,
        bottom=0.21,
        top=SWAP_COMPARISON_JOINT_TOP,
        wspace=JOINT_PLOTS_WSPACE,
    )
    hide_overlapping_inner_x_tick_label(fig, pmf_ax, quality_ax)
    figure_path = output_path(figure_dir, config.figure_prefix, suffix, plot_profile)
    save_figure(fig, figure_path, tight_layout=False, bbox_inches=None)
    plt.close(fig)
    print(f"Saved joint PMF/{quality} figure to {figure_path}")


def run_comparison(config):
    args = parse_args(config)
    validate_budget_args(args)

    if not args.no_build and not args.plots_only:
        command = build_command(args.executable)
        if command is not None:
            run_command(
                command,
                status_label=f"cabal build {args.executable}",
            )

    protocols = args.protocol or list(config.default_protocols)
    output_dir = Path(args.output_dir)
    figure_dir = Path(args.figure_dir)
    if args.plots_only:
        if not output_dir.is_dir():
            raise SystemExit(f"--plots-only requires an existing --output-dir: {output_dir}")
        if args.coverage is not None or args.truncation is not None:
            print("--plots-only: using existing JSONs; budget options are ignored.")
    else:
        output_dir.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)
    config = replace(config, plot_profile=args.plot_profile)
    plt = configure_matplotlib(args.plot_profile)
    truncation = None
    if not args.plots_only:
        truncation = resolve_truncation(args, protocols, output_dir, config)

    rows = []
    reachability_protocol_series = []
    werner_protocol_paths = []
    skr_rows = []

    for protocol in protocols:
        if args.plots_only:
            pure_path = existing_json_path(
                output_dir,
                config.file_prefix,
                protocol,
                QMDP_MODE,
                PURE_EVENT,
            )
            mixed_path = existing_json_path(
                output_dir,
                config.file_prefix,
                protocol,
                QMDP_MODE,
                MIXED_EVENT,
            )
        else:
            pure_path, pure_elapsed = run_case(
                args.executable,
                config.file_prefix,
                protocol,
                QMDP_MODE,
                PURE_EVENT,
                truncation,
                output_dir,
            )
            mixed_path, mixed_elapsed = run_case(
                args.executable,
                config.file_prefix,
                protocol,
                QMDP_MODE,
                MIXED_EVENT,
                truncation,
                output_dir,
            )

        if args.validate_deterministic_extrema:
            assert_extrema_coincide(protocol, QMDP_MODE, PURE_EVENT, pure_path)
            assert_extrema_coincide(protocol, QMDP_MODE, MIXED_EVENT, mixed_path)
            print(f"{protocol}: QMDP pure/mixed min/max extrema coincide")

        if args.validate_static_split:
            if args.plots_only:
                mdp_static_path = existing_json_path(
                    output_dir,
                    config.file_prefix,
                    protocol,
                    MDP_MODE,
                    STATIC_EVENT,
                )
            else:
                mdp_static_path, mdp_static_elapsed = run_case(
                    args.executable,
                    config.file_prefix,
                    protocol,
                    MDP_MODE,
                    STATIC_EVENT,
                    truncation,
                    output_dir,
                )
            if args.validate_deterministic_extrema:
                assert_extrema_coincide(protocol, MDP_MODE, STATIC_EVENT, mdp_static_path)
            split_check = assert_static_pmf_equals_pure_plus_mixed(
                protocol,
                mdp_static_path,
                pure_path,
                mixed_path,
            )
            if args.plots_only:
                print(f"{protocol} {MDP_MODE}/{STATIC_EVENT}: loaded {mdp_static_path}")
            else:
                rows.append(
                    {
                        "protocol": protocol,
                        "mode": MDP_MODE,
                        "event": STATIC_EVENT,
                        "seconds": f"{mdp_static_elapsed:.6f}",
                        "json_path": str(mdp_static_path),
                    }
                )
                print(
                    f"{protocol} {MDP_MODE}/{STATIC_EVENT}: "
                    f"{mdp_static_elapsed:.2f}s -> {mdp_static_path}"
                )
            if split_check == "exact":
                print(f"{protocol}: static PMF = pure PMF + mixed PMF")
            else:
                print(f"{protocol}: static PMF lies within pure/mixed split bounds")

        full_reachability_series = derive_full_series_from_split(pure_path, mixed_path)
        skr = compute_secret_key_rate_from_split(pure_path, mixed_path)

        reachability_protocol_series.append((protocol, full_reachability_series))
        werner_protocol_paths.append((protocol, pure_path, mixed_path))
        skr_rows.append({"protocol": protocol, "skr": skr})
        if args.plots_only:
            print(f"{protocol} {QMDP_MODE}/{PURE_EVENT}: loaded {pure_path}")
            print(f"{protocol} {QMDP_MODE}/{MIXED_EVENT}: loaded {mixed_path}")
        else:
            rows.extend(
                [
                    {
                        "protocol": protocol,
                        "mode": QMDP_MODE,
                        "event": PURE_EVENT,
                        "seconds": f"{pure_elapsed:.6f}",
                        "json_path": str(pure_path),
                    },
                    {
                        "protocol": protocol,
                        "mode": QMDP_MODE,
                        "event": MIXED_EVENT,
                        "seconds": f"{mixed_elapsed:.6f}",
                        "json_path": str(mixed_path),
                    },
                ]
            )
            print(f"{protocol} {QMDP_MODE}/{PURE_EVENT}: {pure_elapsed:.2f}s -> {pure_path}")
            print(f"{protocol} {QMDP_MODE}/{MIXED_EVENT}: {mixed_elapsed:.2f}s -> {mixed_path}")

    if args.plots_only:
        print("--plots-only: timings.csv was not rewritten.")
    else:
        write_summary(output_dir / "timings.csv", rows)

    skr_by_protocol = {row["protocol"]: row["skr"] for row in skr_rows}

    if args.plot_kind == BOTH_PLOT_KIND:
        plot_combined_reachability(
            plt,
            figure_dir,
            reachability_protocol_series,
            PMF_PLOT_KIND,
            config,
            skr_by_protocol=skr_by_protocol,
            show_skr=args.show_skr_legend,
            bin_size=args.binning,
            only_binned=args.only_binned_plots,
            only_non_binned=args.only_non_binned,
        )
        plot_combined_reachability(
            plt,
            figure_dir,
            reachability_protocol_series,
            CDF_PLOT_KIND,
            config,
            skr_by_protocol=skr_by_protocol,
            show_skr=args.show_skr_legend,
            bin_size=args.binning,
            only_binned=args.only_binned_plots,
            only_non_binned=args.only_non_binned,
        )
    else:
        plot_combined_reachability(
            plt,
            figure_dir,
            reachability_protocol_series,
            args.plot_kind,
            config,
            skr_by_protocol=skr_by_protocol,
            show_skr=args.show_skr_legend,
            bin_size=args.binning,
            only_binned=args.only_binned_plots,
            only_non_binned=args.only_non_binned,
        )

    plot_combined_quality(
        plt,
        figure_dir,
        werner_protocol_paths,
        config,
        quality="werner",
        skr_by_protocol=skr_by_protocol,
        show_skr=args.show_skr_legend,
        skip_legend=args.skip_werner_legend,
        bin_size=args.binning,
        only_binned=args.only_binned_plots,
        only_non_binned=args.only_non_binned,
    )
    if args.plot_fidelity:
        plot_combined_quality(
            plt,
            figure_dir,
            werner_protocol_paths,
            config,
            quality="fidelity",
            skr_by_protocol=skr_by_protocol,
            show_skr=args.show_skr_legend,
            bin_size=args.binning,
            only_binned=args.only_binned_plots,
            only_non_binned=args.only_non_binned,
        )
    if args.joint_plots:
        joint_quality = "fidelity" if args.plot_fidelity else "werner"
        plot_joint_pmf_quality(
            plt,
            figure_dir,
            reachability_protocol_series,
            werner_protocol_paths,
            config,
            quality=joint_quality,
            skr_by_protocol=skr_by_protocol,
            show_skr=args.show_skr_legend,
            bin_size=args.binning,
            only_binned=args.only_binned_plots,
            only_non_binned=args.only_non_binned,
            show_detail=args.show_detail,
            show_detail_link=args.show_detail_link,
        )

    print("\nSecret key rates:")
    print(f"{'Protocol':<15} {'SKR':<18}")
    print("-" * 33)
    for row in skr_rows:
        print(f"{row['protocol']:<15} {row['skr']:<18.12g}")
