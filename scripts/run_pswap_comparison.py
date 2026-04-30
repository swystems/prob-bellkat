#!/usr/bin/env python3

import argparse
import csv
import subprocess
import time
from pathlib import Path

from plot_extremal import (
    configure_matplotlib,
    derive_pmf_series,
    load_extremal_series,
    style_axes,
)


DEFAULT_PROTOCOLS = ("asap", "seq1", "seq2", "sim")
DEFAULT_MODE = "mdp"
COLORS = ("#d7b225","#df493d","#260a75","#ef83f5")

def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Run the P-swap comparison protocols through the mdp pipeline "
            "and plot their extremal PMFs together."
        )
    )
    parser.add_argument(
        "--protocol",
        action="append",
        choices=DEFAULT_PROTOCOLS,
        help="Protocol to run. Can be passed multiple times. Defaults to all protocols.",
    )
    parser.add_argument(
        "--truncation",
        type=int,
        default=100,
        help="Extremal reachability budget passed to --truncation.",
    )
    parser.add_argument(
        "--output-dir",
        default="output/pswap_comparison",
        help="Directory for JSON dumps and timing summary.",
    )
    parser.add_argument(
        "--figure-dir",
        default="output/figures",
        help="Directory for combined PMF figures.",
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


def run_case(executable, protocol, mode, truncation, output_dir):
    output_path = output_dir / f"pswap_{protocol}_{mode}.json"
    command = [
        "cabal",
        "exec",
        "-v0",
        executable,
        "--",
        "--protocol",
        protocol,
        "--json",
        mode,
        "--compute-extremal",
        "--truncation",
        str(truncation),
    ]
    elapsed = run_command(command, stdout_path=output_path)
    return output_path, elapsed


def write_summary(summary_path, rows):
    with open(summary_path, "w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=("protocol", "mode", "seconds", "json_path"),
        )
        writer.writeheader()
        writer.writerows(rows)


def plot_combined_pmfs(plt, figure_dir, protocol_paths):
    fig, ax = plt.subplots()

    for index, (protocol, json_path) in enumerate(protocol_paths):
        series = load_extremal_series(json_path)
        pmf_min, pmf_max = derive_pmf_series(series)
        t = list(range(len(pmf_min)))
        color = COLORS[index % len(COLORS)]

        ax.plot(
            t,
            pmf_min,
            color=color,
            linestyle="-",
            label=f"{protocol} min",
        )
        ax.plot(
            t,
            pmf_max,
            color=color,
            linestyle="--",
            label=f"{protocol} max",
        )

    ax.set_xlabel(r"$t$")
    ax.set_ylabel("Probability")
    ax.set_title(DEFAULT_MODE.upper())
    style_axes(ax)
    ax.legend(frameon=False, loc="best", ncol=2)

    fig.tight_layout(pad=0.25)
    fig.savefig(figure_dir / "pswap_mdp_pmfs.pdf", bbox_inches="tight")
    plt.close(fig)
    print(f"Saved PMFs figure to {figure_dir / 'pswap_mdp_pmfs.pdf'}")


def main():
    args = parse_args()
    protocols = args.protocol or list(DEFAULT_PROTOCOLS)
    output_dir = Path(args.output_dir)
    figure_dir = Path(args.figure_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)
    plt = configure_matplotlib()
    plt.rcParams.update({"figure.figsize": (5.2, 3.2)})

    if not args.no_build:
        run_command(["cabal", "build", args.executable])

    rows = []
    protocol_paths = []

    for protocol in protocols:
        json_path, elapsed = run_case(
            args.executable,
            protocol,
            DEFAULT_MODE,
            args.truncation,
            output_dir,
        )
        protocol_paths.append((protocol, json_path))
        rows.append(
            {
                "protocol": protocol,
                "mode": DEFAULT_MODE,
                "seconds": f"{elapsed:.6f}",
                "json_path": str(json_path),
            }
        )
        print(f"{protocol} {DEFAULT_MODE}: {elapsed:.2f}s -> {json_path}")

    write_summary(output_dir / "timings.csv", rows)

    plot_combined_pmfs(plt, figure_dir, protocol_paths)


if __name__ == "__main__":
    main()
