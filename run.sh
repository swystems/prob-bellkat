#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: ./run.sh [--cluster]

Without arguments, run in the background on macOS using caffeinate and nohup.

  --cluster  Run in the foreground for a cluster batch scheduler. The scheduler
             owns the process and captures its output, so nohup and caffeinate
             are not used.
  -h, --help Show this help.
EOF
}

run_mode="local"
while (($# > 0)); do
  case "$1" in
    --cluster)
      run_mode="cluster"
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown argument: %s\n\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

# Set PLOTS_ONLY=1 to regenerate every evaluation plot from existing data.
# The experiment-specific switches can still override the aggregate setting.
PLOTS_ONLY="${PLOTS_ONLY:-0}"
DISTILLATION_PLOTS_ONLY="${DISTILLATION_PLOTS_ONLY:-$PLOTS_ONLY}"
OPTIMALITY_PLOTS_ONLY="${OPTIMALITY_PLOTS_ONLY:-$PLOTS_ONLY}"
COMPARISON_PLOTS_ONLY="${COMPARISON_PLOTS_ONLY:-$PLOTS_ONLY}"
SCHEDULER_PLOTS_ONLY="${SCHEDULER_PLOTS_ONLY:-$PLOTS_ONLY}"
VALIDATION_PLOTS_ONLY="${VALIDATION_PLOTS_ONLY:-$PLOTS_ONLY}"
NONDET_PROTOCOLS_PLOTS_ONLY="${NONDET_PROTOCOLS_PLOTS_ONLY:-$PLOTS_ONLY}"

LOG_FILE="${LOG_FILE:-run-13.log}"
if [[ "$run_mode" == "cluster" ]]; then
  RUN_TMPDIR="${RUN_TMPDIR:-${SLURM_TMPDIR:-${TMPDIR:-/tmp}}}"
else
  RUN_TMPDIR="${RUN_TMPDIR:-/private/tmp}"
fi

TRUNCATION="${TRUNCATION:-2000}"
GENERATION_SCALING="${GENERATION_SCALING:-128}"
UNIFORM_W0_VALUES="${UNIFORM_W0_VALUES:-0.925,0.94,0.955,0.97,0.985,1.0}"
T_COH_VALUES="${T_COH_VALUES:-900,3600,14400,57600,230400,921600,3686400}"
P_SWAP="${P_SWAP:-0.5}"
OPTIMALITY_SCALING_VALUES="${OPTIMALITY_SCALING_VALUES:-4,16,64,256,1024}"
OPTIMALITY_P_SWAP_VALUES="${OPTIMALITY_P_SWAP_VALUES:-0.25,0.5,0.75,1}"
EDGE_SKEW_VALUES="${EDGE_SKEW_VALUES:-1,2,4,8,16,32}"
COVERAGE="${COVERAGE:-0.99}"
DETAIL_RANGE="${DETAIL_RANGE:-8267,8277}"
PLOT_PROFILE="${PLOT_PROFILE:-paper}"
OPTIMALITY_OUTPUT_DIR="${OPTIMALITY_OUTPUT_DIR:-output/swap-scheme-optimality-surf-7}"
NONDET_PROTOCOLS_OUTPUT_DIR="${NONDET_PROTOCOLS_OUTPUT_DIR:-output/nondet-topology-protocols}"
NONDET_GOALS_DIR="${NONDET_GOALS_DIR:-output/nondet-topology-goals-adapt-loop}"
SCHEDULER_TRUNCATION="${SCHEDULER_TRUNCATION:-8000}" # 101347
SCHEDULER_QUALITY_TRUNCATION="${SCHEDULER_QUALITY_TRUNCATION:-$SCHEDULER_TRUNCATION}"
SCHEDULER_T_COH="${SCHEDULER_T_COH:-1440000}"
SCHEDULER_JOBS="${SCHEDULER_JOBS:-1}"
SCHEDULER_FIGURE_DIR="${SCHEDULER_FIGURE_DIR:-../qbkat-overleaf/plots}"
SCHEDULER_SKIP_UNION_QUALITY="${SCHEDULER_SKIP_UNION_QUALITY:-1}"
VALIDATION_MC_SHOTS="${VALIDATION_MC_SHOTS:-10000000}"
VALIDATION_BIN_WIDTH="${VALIDATION_BIN_WIDTH:-100}"

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$repo_dir"

if [[ -x "$repo_dir/.venv/bin/python" ]]; then
  PYTHON_BIN="${PYTHON_BIN:-$repo_dir/.venv/bin/python}"
else
  PYTHON_BIN="${PYTHON_BIN:-python3}"
fi

distillation=(
  "$PYTHON_BIN" scripts/run_distillation_comparison.py
  --truncation "$TRUNCATION"
  --generation-scaling "$GENERATION_SCALING"
  --uniform-w0-values "$UNIFORM_W0_VALUES"
  --t-coh-values "$T_COH_VALUES"
  --p-swap "$P_SWAP"
  --plot-profile "$PLOT_PROFILE"
)

optimality=(
  "$PYTHON_BIN" -m scripts.run_swap_scheme_optimality
  --experiment doubling-asap
  --experiment sequential-asap
  --generation-scaling-values-a "$OPTIMALITY_SCALING_VALUES"
  --p-sw-values-a "$OPTIMALITY_P_SWAP_VALUES"
  --generation-scaling-values-b "$OPTIMALITY_SCALING_VALUES"
  --edge-skew-values-b "$EDGE_SKEW_VALUES"
  --output-dir "$OPTIMALITY_OUTPUT_DIR"
  --markdown "$OPTIMALITY_OUTPUT_DIR/swap-scheme-optimality.md"
  --plot-profile "$PLOT_PROFILE"
)

comparison=(
  "$PYTHON_BIN" -m scripts.run_swap_scheme_comparison
  --joint-plots
  --show-detail "$DETAIL_RANGE"
  --show-detail-link
)

schedulers=(
  "$PYTHON_BIN" scripts/run_nondet_topology_schedulers.py
  --truncation "$SCHEDULER_TRUNCATION"
  --plot-truncation "$SCHEDULER_TRUNCATION"
  --quality-truncation "$SCHEDULER_QUALITY_TRUNCATION"
  --quality-plot-truncation "$SCHEDULER_QUALITY_TRUNCATION"
  --t-coh "$SCHEDULER_T_COH"
  --jobs "$SCHEDULER_JOBS"
  --joint-protocols-cdf-werner
  --no-shade
  --figure-dir "$SCHEDULER_FIGURE_DIR"
)

nondet_protocols=(
  "$PYTHON_BIN" scripts/run_nondet_topology_protocols.py
  --output-dir "$NONDET_PROTOCOLS_OUTPUT_DIR"
  --figure-dir "$NONDET_PROTOCOLS_OUTPUT_DIR"
  --joint-goals-dir "$NONDET_GOALS_DIR"
  --joint-layout side-by-side
  --plot-profile "$PLOT_PROFILE"
  --no-shade
)

validation=(
  "$PYTHON_BIN" scripts/run_swap_scheme_validation.py
  --protocol doubling
  --protocol left-to-right
  --include-mc
  --mc-shots "$VALIDATION_MC_SHOTS"
  --bin-width "$VALIDATION_BIN_WIDTH"
  --plot-profile "$PLOT_PROFILE"
)

if [[ "$DISTILLATION_PLOTS_ONLY" == 1 ]]; then
  distillation+=(--plots-only)
else
  distillation+=(--resume)
fi

if [[ "$OPTIMALITY_PLOTS_ONLY" == 1 ]]; then
  optimality+=(--plots-only)
else
  optimality+=(--coverage "$COVERAGE" --resume)
fi

if [[ "$COMPARISON_PLOTS_ONLY" == 1 ]]; then
  comparison+=(--plots-only)
else
  comparison+=(--coverage "$COVERAGE")
fi

if [[ "$SCHEDULER_PLOTS_ONLY" == 1 ]]; then
  schedulers+=(--plots-only)
else
  schedulers+=(--resume)
fi

if [[ "$VALIDATION_PLOTS_ONLY" == 1 ]]; then
  validation+=(--plots-only)
fi

if [[ "$NONDET_PROTOCOLS_PLOTS_ONLY" == 1 ]]; then
  nondet_protocols+=(--plots-only)
else
  nondet_protocols+=(--coverage "$COVERAGE")
fi

if [[ "$SCHEDULER_SKIP_UNION_QUALITY" == 1 ]]; then
  schedulers+=(--skip-union-quality)
fi

quote_command() {
  local quoted
  printf -v quoted '%q ' "$@"
  printf '%s' "${quoted% }"
}

pipeline=""
pipeline+="$(quote_command "${schedulers[@]}")"
pipeline+=" && $(quote_command "${nondet_protocols[@]}")"
pipeline+=" && $(quote_command "${validation[@]}")"
pipeline+=" && $(quote_command "${distillation[@]}")"
pipeline+=" && $(quote_command "${comparison[@]}")"
pipeline+=" && $(quote_command "${optimality[@]}")"

if [[ "$run_mode" == "cluster" ]]; then
  printf 'Starting cluster run in the foreground; temporary directory: %s\n' "$RUN_TMPDIR"
  exec env \
    PYTHONUNBUFFERED=1 TMPDIR="$RUN_TMPDIR" TMP="$RUN_TMPDIR" TEMP="$RUN_TMPDIR" \
    nix develop -c bash -lc "$pipeline"
fi

nohup caffeinate -dims env \
  PYTHONUNBUFFERED=1 TMPDIR="$RUN_TMPDIR" TMP="$RUN_TMPDIR" TEMP="$RUN_TMPDIR" \
  nix develop -c bash -lc "$pipeline" >"$LOG_FILE" 2>&1 &

pid=$!
printf 'Started run as PID %s; logging to %s/%s\n' "$pid" "$repo_dir" "$LOG_FILE"
