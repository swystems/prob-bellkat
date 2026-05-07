#!/usr/bin/env python3

import argparse
import csv
import math
import subprocess
import time
from pathlib import Path

import numpy as np

from plot_extremal import (
    configure_matplotlib,
    derive_average_werner_series,
    derive_plot_series,
    derive_pmf_series,
    load_extremal_payload,
    load_extremal_series,
    style_axes,
)
from utils import secret_key_rate


DEFAULT_PROTOCOLS = ("asap", "seq1", "seq2", "sim")
DEFAULT_TRUNCATION = 100
MDP_MODE = "mdp"
QMDP_MODE = "qmdp"
STATIC_EVENT = "static"
PURE_EVENT = "pure"
MIXED_EVENT = "mixed"
# COLORS = ("#d7b225", "#df493d", "#260a75", "#ef83f5", "#000000")
COLORS = ("#cddb87", "#ee7833", "#01b56c", "#cc9cff", "#7c0006")

PMF_ASSERTION_TOLERANCE = 1e-4
PMF_PLOT_KIND = "pmf"
CDF_PLOT_KIND = "cdf"
BOTH_PLOT_KIND = "both"
PLOT_KINDS = (PMF_PLOT_KIND, CDF_PLOT_KIND, BOTH_PLOT_KIND)
CDF_BAND_ALPHA = 0.01


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Run the P-swap comparison protocols through the MDP and QMDP "
            "pipelines, then plot their extremal reachability and Werner curves together."
        )
    )
    parser.add_argument(
        "--protocol",
        action="append",
        choices=DEFAULT_PROTOCOLS,
        help="Protocol to run. Can be passed multiple times. Defaults to all protocols.",
    )
    budget_group = parser.add_mutually_exclusive_group()
    budget_group.add_argument(
        "--truncation",
        type=int,
        default=None,
        help=(
            "Extremal reachability budget passed to --truncation. "
            f"Defaults to {DEFAULT_TRUNCATION} when --coverage is not used."
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
        default="output/pswap-comparison",
        help="Directory for JSON dumps and timing summary.",
    )
    parser.add_argument(
        "--figure-dir",
        default="output/pswap-comparison",
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
        "--executable",
        default="quantP_compare_swap",
        help="Cabal executable name.",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip the initial cabal build step.",
    )
    return parser.parse_args()


def run_command(command, stdout_path=None):
    started = time.perf_counter()
    if stdout_path is None:
        result = subprocess.run(command, text=True, capture_output=True)
    else:
        with open(stdout_path, "w", encoding="utf-8") as stdout_handle:
            result = subprocess.run(
                command,
                text=True,
                stdout=stdout_handle,
                stderr=subprocess.PIPE,
            )
    elapsed = time.perf_counter() - started

    if result.returncode != 0:
        command_text = " ".join(command)
        raise SystemExit(
            f"Command failed after {elapsed:.2f}s: {command_text}\n{result.stderr}"
        )

    return elapsed


def output_json_path(output_dir, protocol, mode, event):
    return output_dir / f"pswap_{protocol}_{mode}_{event}.json"


def output_coverage_json_path(output_dir, protocol):
    return output_dir / f"pswap_{protocol}_{MDP_MODE}_{STATIC_EVENT}_coverage.json"


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
        "cabal",
        "run",
        "-v0",
        executable,
        "--",
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
    elapsed = run_command(command, stdout_path=output_path)
    return output_path, elapsed


def run_case(executable, protocol, mode, event, truncation, output_dir):
    return run_extremal_case(
        executable,
        protocol,
        mode,
        event,
        "--truncation",
        truncation,
        output_json_path(output_dir, protocol, mode, event),
    )


def run_coverage_case(executable, protocol, coverage, output_dir):
    return run_extremal_case(
        executable,
        protocol,
        MDP_MODE,
        STATIC_EVENT,
        "--coverage",
        coverage,
        output_coverage_json_path(output_dir, protocol),
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
    resolved_budget = payload.get("resolved_budget")
    coverage_status = payload.get("coverage_status")

    if resolved_budget is None:
        raise SystemExit(f"{json_path} does not contain resolved_budget.")
    if not isinstance(coverage_status, dict):
        raise SystemExit(f"{json_path} does not contain a coverage_status object.")

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


def resolve_truncation(args, protocols, output_dir):
    if args.coverage is None:
        truncation = (
            args.truncation
            if args.truncation is not None
            else DEFAULT_TRUNCATION
        )
        validate_truncation(truncation)
        return truncation

    validate_coverage(args.coverage)
    coverage_rows = []
    coverage_budgets = {}

    for protocol in protocols:
        json_path, elapsed = run_coverage_case(
            args.executable,
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
            static_pmf_min,
            reconstructed_min,
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


def compute_secret_key_rates(static_path, pure_path, mixed_path):
    static_pmf_min, static_pmf_max = derive_pmf_series(load_extremal_series(static_path))
    pure_series = load_extremal_series(pure_path)
    mixed_series = load_extremal_series(mixed_path)
    _, werner_min, werner_max = derive_average_werner_series(pure_series, mixed_series)

    if len(static_pmf_min) != len(werner_min) or len(static_pmf_max) != len(werner_max):
        raise SystemExit(
            "Static PMF and Werner series must have matching lengths to compute SKR."
        )

    return {
        "min": secret_key_rate(np.array(static_pmf_min), np.array(werner_min)),
        "max": secret_key_rate(np.array(static_pmf_max), np.array(werner_max)),
    }


def protocol_extremum_label(protocol, extremum):
    return f"{protocol} {extremum}"


def plot_combined_reachability(plt, figure_dir, protocol_paths, plot_kind):
    fig, ax = plt.subplots()

    for index, (protocol, json_path) in enumerate(protocol_paths):
        series = load_extremal_series(json_path)
        if plot_kind == CDF_PLOT_KIND:
            t, _, _, reachability_min, reachability_max = derive_plot_series(series)
        else:
            reachability_min, reachability_max = derive_pmf_series(series)
            t = list(range(len(reachability_min)))
        color = COLORS[index % len(COLORS)]

        if plot_kind == CDF_PLOT_KIND:
            ax.fill_between(
                t,
                reachability_min,
                reachability_max,
                color=color,
                alpha=CDF_BAND_ALPHA,
                linewidth=0,
            )
        ax.plot(
            t,
            reachability_min,
            color=color,
            linestyle="-",
            label=protocol_extremum_label(protocol, "min"),
        )
        ax.plot(
            t,
            reachability_max,
            color=color,
            linestyle="--",
            label=protocol_extremum_label(protocol, "max"),
        )

    ax.set_xlabel(r"$t$")
    if plot_kind == CDF_PLOT_KIND:
        ax.set_ylabel("Cumulative probability")
        ax.set_ylim(0.0, 1.0)
    else:
        ax.set_ylabel("Probability")
    ax.set_title(MDP_MODE.upper())
    style_axes(ax)
    ax.legend(frameon=False, loc="best", ncol=2)

    fig.tight_layout(pad=0.25)
    figure_path = figure_dir / f"pswap_mdp_{plot_kind}s.pdf"
    fig.savefig(figure_path, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved {plot_kind.upper()}s figure to {figure_path}")


def plot_combined_werners(plt, figure_dir, protocol_paths):
    fig, ax = plt.subplots()

    for index, (protocol, pure_path, mixed_path) in enumerate(protocol_paths):
        pure_series = load_extremal_series(pure_path)
        mixed_series = load_extremal_series(mixed_path)
        t, werner_min, werner_max = derive_average_werner_series(
            pure_series,
            mixed_series,
        )
        color = COLORS[index % len(COLORS)]
        
        # Skip the initial points where werner is 0 as they clutter the plot
        t, werner_min, werner_max = zip(*[
            (t_i, w_min, w_max)
            for t_i, w_min, w_max in zip(t, werner_min, werner_max)
            if w_min > 0.0 or w_max > 0.0
        ])
        ax.plot(
            t,
            werner_min,
            color=color,
            linestyle="-",
            label=protocol_extremum_label(protocol, "min"),
        )
        ax.plot(
            t,
            werner_max,
            color=color,
            linestyle="--",
            label=protocol_extremum_label(protocol, "max"),
        )

    ax.set_xlabel(r"$t$")
    ax.set_ylabel("Average Werner parameter")
    # ax.set_ylim(0.0, 1.0)
    ax.set_title(QMDP_MODE.upper())
    style_axes(ax)
    ax.legend(frameon=False, loc="best", ncol=2)

    fig.tight_layout(pad=0.25)
    fig.savefig(figure_dir / "pswap_qmdp_ws.pdf", bbox_inches="tight")
    plt.close(fig)
    print(f"Saved Werner figure to {figure_dir / 'pswap_qmdp_ws.pdf'}")


def main():
    args = parse_args()
    validate_budget_args(args)

    if not args.no_build:
        run_command(["cabal", "build", args.executable])

    protocols = args.protocol or list(DEFAULT_PROTOCOLS)
    output_dir = Path(args.output_dir)
    figure_dir = Path(args.figure_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)
    plt = configure_matplotlib()
    plt.rcParams.update({"figure.figsize": (5.2, 3.2), "text.usetex": False})
    truncation = resolve_truncation(args, protocols, output_dir)

    rows = []
    pmf_protocol_paths = []
    werner_protocol_paths = []
    skr_rows = []

    for protocol in protocols:
        mdp_static_path, mdp_static_elapsed = run_case(
            args.executable,
            protocol,
            MDP_MODE,
            STATIC_EVENT,
            truncation,
            output_dir,
        )
        # qmdp_static_path, qmdp_static_elapsed = run_case(
        #     args.executable,
        #     protocol,
        #     QMDP_MODE,
        #     STATIC_EVENT,
        #     truncation,
        #     output_dir,
        # )
        pure_path, pure_elapsed = run_case(
            args.executable,
            protocol,
            QMDP_MODE,
            PURE_EVENT,
            truncation,
            output_dir,
        )
        mixed_path, mixed_elapsed = run_case(
            args.executable,
            protocol,
            QMDP_MODE,
            MIXED_EVENT,
            truncation,
            output_dir,
        )

        split_check = assert_static_pmf_equals_pure_plus_mixed(
            protocol,
            mdp_static_path,
            pure_path,
            mixed_path,
        )
        skr = compute_secret_key_rates(mdp_static_path, pure_path, mixed_path)

        pmf_protocol_paths.append((protocol, mdp_static_path))
        werner_protocol_paths.append((protocol, pure_path, mixed_path))
        skr_rows.append({"protocol": protocol, **skr})
        rows.extend(
            [
                {
                    "protocol": protocol,
                    "mode": MDP_MODE,
                    "event": STATIC_EVENT,
                    "seconds": f"{mdp_static_elapsed:.6f}",
                    "json_path": str(mdp_static_path),
                },
                # {
                #     "protocol": protocol,
                #     "mode": QMDP_MODE,
                #     "event": STATIC_EVENT,
                #     "seconds": f"{qmdp_static_elapsed:.6f}",
                #     "json_path": str(qmdp_static_path),
                # },
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
        print(f"{protocol} {MDP_MODE}/{STATIC_EVENT}: {mdp_static_elapsed:.2f}s -> {mdp_static_path}")
        # print(f"{protocol} {QMDP_MODE}/{STATIC_EVENT}: {qmdp_static_elapsed:.2f}s -> {qmdp_static_path}")
        print(f"{protocol} {QMDP_MODE}/{PURE_EVENT}: {pure_elapsed:.2f}s -> {pure_path}")
        print(f"{protocol} {QMDP_MODE}/{MIXED_EVENT}: {mixed_elapsed:.2f}s -> {mixed_path}")
        if split_check == "exact":
            print(f"{protocol}: QMDP static PMF = pure PMF + mixed PMF")
        else:
            print(f"{protocol}: QMDP PMF split bounds hold for static/pure/mixed extrema")

    write_summary(output_dir / "timings.csv", rows)

    if args.plot_kind == BOTH_PLOT_KIND:
        plot_combined_reachability(plt, figure_dir, pmf_protocol_paths, PMF_PLOT_KIND)
        plot_combined_reachability(plt, figure_dir, pmf_protocol_paths, CDF_PLOT_KIND)
    else:
        plot_combined_reachability(plt, figure_dir, pmf_protocol_paths, args.plot_kind)

    plot_combined_werners(plt, figure_dir, werner_protocol_paths)

    print("\nSecret key rates:")
    print(f"{'Protocol':<15} {'Min SKR':<18} {'Max SKR':<18}")
    print("-" * 51)
    for row in skr_rows:
        print(f"{row['protocol']:<15} {row['min']:<18.12g} {row['max']:<18.12g}")


if __name__ == "__main__":
    main()
