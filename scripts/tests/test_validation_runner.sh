#!/bin/bash

set -euo pipefail
set -x

mode="${1:-star}"
output_dir="output/validation"
figure_dir="$output_dir/figures"
swap_example="quantum-examples/validation/P_oneswap.hs"
distill_example="quantum-examples/validation/P_onedist.hs"
distswap_example="quantum-examples/validation/P_distswap.hs"
star_example="quantum-examples/validation/P_star.hs"

mkdir -p "$output_dir" "$figure_dir"

plotting_enabled=true
if ! PYTHONDONTWRITEBYTECODE=1 python3 -c "import matplotlib.pyplot" >/dev/null 2>&1; then
	plotting_enabled=false
	echo "matplotlib is not available; skipping plots."
fi

run_extremal() {
	local executable="$1"
	local mdp_mode="$2"
	local truncation="$3"
	local suffix=""
	shift 3
	if [ "$#" -gt 0 ] && [[ "$1" != --* ]]; then
		suffix="$1"
		shift
	fi
	local extra_args=("$@")

	cabal -v0 run "$executable" -- "${extra_args[@]}" --json "$mdp_mode" --compute-extremal --truncation "$truncation" > "$output_dir/${executable}${suffix}.json"
}

replace_event() {
	local example_file="$1"
	local pattern="$2"
	local replacement="$3"

	perl -0pi -e "s|$pattern|$replacement|g" "$example_file"
}

restore_examples() {
	replace_event "$swap_example" 'let ev[[:space:]]*=[[:space:]]*"A" =~\? "C"' 'let ev  = "A" ~~? "C"' || true
	replace_event "$swap_example" 'let ev[[:space:]]*=[[:space:]]*"A" -~\? "C"' 'let ev  = "A" ~~? "C"' || true
	replace_event "$distill_example" 'hasMixedSubset' 'hasPureSubset' || true
	replace_event "$distswap_example" 'hasMixedSubset' 'hasPureSubset' || true
	replace_event "$star_example" 'hasSubset \["B" ~ "C"\]' 'hasSubset ["A" ~ "C"]' || true
	replace_event "$star_example" 'hasPureSubset \["A" ~ "C"]' 'hasSubset ["A" ~ "C"]' || true
	replace_event "$star_example" 'hasMixedSubset \["A" ~ "C"]' 'hasSubset ["A" ~ "C"]' || true
	replace_event "$star_example" 'hasPureSubset \["B" ~ "C"]' 'hasSubset ["A" ~ "C"]' || true
	replace_event "$star_example" 'hasMixedSubset \["B" ~ "C"]' 'hasSubset ["A" ~ "C"]' || true
}

trap restore_examples EXIT

plot_static() {
	local stem="$1"

	if [ "$plotting_enabled" = false ]; then
		return
	fi
	python3 scripts/plot_extremal.py "$output_dir/${stem}.json" --output-dir "$figure_dir"
}

plot_pure_mixed() {
	local stem="$1"

	if [ "$plotting_enabled" = false ]; then
		return
	fi
	python3 scripts/plot_extremal.py --pure-json "$output_dir/${stem}_pure.json" --mixed-json "$output_dir/${stem}_mixed.json" --output-dir "$figure_dir"
}

run_pure_mixed_case() {
	local executable="$1"
	local example_file="$2"
	local truncation="$3"

	# assume the example starts with hasPureSubset
	run_extremal "$executable" qmdp "$truncation" _pure
	replace_event "$example_file" 'hasPureSubset' 'hasMixedSubset'

	run_extremal "$executable" qmdp "$truncation" _mixed
	replace_event "$example_file" 'hasMixedSubset' 'hasPureSubset'

	plot_pure_mixed "$executable"
}

run_swap() {
	local example_file="$swap_example"
	local executable="quantP_oneswap"
	local truncation=49

	# assume it starts with "let ev = \"A\" ~~? \"C\""
	run_extremal "$executable" mdp "$truncation"

	replace_event "$example_file" 'let ev[[:space:]]*=[[:space:]]*"A" ~~\? "C"' 'let ev  = "A" =~? "C"'
	run_extremal "$executable" qmdp "$truncation" _mixed

	replace_event "$example_file" 'let ev[[:space:]]*=[[:space:]]*"A" =~\? "C"' 'let ev  = "A" -~? "C"'
	run_extremal "$executable" qmdp "$truncation" _pure

	replace_event "$example_file" 'let ev[[:space:]]*=[[:space:]]*"A" -~\? "C"' 'let ev  = "A" ~~? "C"'

	plot_static "$executable"
	plot_pure_mixed "$executable"
}

run_distill() {
	run_pure_mixed_case quantP_onedist "$distill_example" 34
}

run_distill_swap() {
	run_pure_mixed_case quantP_distswap "$distswap_example" 199
}

run_star() {
	local example_file="$star_example"
	local executable="quantP_star"
	local truncation=54

	# assume it starts with "hasSubset ["A" ~ "C"]"
	run_extremal "$executable" mdp "$truncation" _AC
	replace_event "$example_file" 'hasSubset \["A" ~ "C"\]' 'hasSubset ["B" ~ "C"]'

	run_extremal "$executable" mdp "$truncation" _BC
	replace_event "$example_file" 'hasSubset \["B" ~ "C"\]' 'hasPureSubset ["A" ~ "C"]'

	plot_static "${executable}_AC"
	plot_static "${executable}_BC"

	run_extremal "$executable" qmdp "$truncation" _AC_pure
	replace_event "$example_file" 'hasPureSubset \["A" ~ "C"]' 'hasMixedSubset ["A" ~ "C"]'

	run_extremal "$executable" qmdp "$truncation" _AC_mixed
	replace_event "$example_file" 'hasMixedSubset \["A" ~ "C"]' 'hasPureSubset ["B" ~ "C"]'

	run_extremal "$executable" qmdp "$truncation" _BC_pure
	replace_event "$example_file" 'hasPureSubset \["B" ~ "C"]' 'hasMixedSubset ["B" ~ "C"]'

	run_extremal "$executable" qmdp "$truncation" _BC_mixed
	replace_event "$example_file" 'hasMixedSubset \["B" ~ "C"]' 'hasSubset ["A" ~ "C"]'

	plot_pure_mixed "${executable}_AC"
	plot_pure_mixed "${executable}_BC"

}

run_swapasap() {
	local executable="quantP_swapasap"
	local label
	local p_gen
	local t_coh
	local truncation

	while read -r label p_gen t_coh truncation; do
		[ -n "$label" ] || continue

		run_extremal "$executable" mdp "$truncation" "_${label}" --event=static --p-gen="$p_gen" --t-coh="$t_coh"
		run_extremal "$executable" qmdp "$truncation" "_${label}_pure" --event=pure --p-gen="$p_gen" --t-coh="$t_coh"
		run_extremal "$executable" qmdp "$truncation" "_${label}_mixed" --event=mixed --p-gen="$p_gen" --t-coh="$t_coh"

		plot_static "${executable}_${label}"
		plot_pure_mixed "${executable}_${label}"
	done <<'EOF'
p020_t1000 0.2 1000 200
p010_t1000 0.1 1000 300
p005_t5000 0.05 5000 600
p0001_t50000 0.001 50000 25000
EOF
}

case "$mode" in
	swap)
		run_swap
		;;
	distill)
		run_distill
		;;
	distill-swap)
		run_distill_swap
		;;
	star)
		run_star
		;;
	swapasap|swap-asap)
		run_swapasap
		;;
	all)
		run_swap
		run_distill
		run_distill_swap
		run_star
		run_swapasap
		;;
	*)
		echo "Usage: $0 [swap|distill|distill-swap|star|swapasap|all]"
		exit 1
		;;
esac

set +x
