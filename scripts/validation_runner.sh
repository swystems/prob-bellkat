#!/bin/bash

set -x

mode="${1:-all}"

run_swap() {
	local example_file="quantum-examples/P_oneswap.hs"

	# assume it starts with "let ev = \"A\" ~~? \"C\""
	cabal build; cabal run quantP_oneswap -- --json mdp --compute-extremal --truncation 49 > output/quantP_oneswap.json

	sed -i '' 's/let ev[[:space:]]*=[[:space:]]*"A" ~~? "C"/let ev  = "A" =~? "C"/' "$example_file"
	cabal build; cabal run quantP_oneswap -- --json qmdp --compute-extremal --truncation 49 > output/quantP_oneswap_mixed.json

	sed -i '' 's/let ev[[:space:]]*=[[:space:]]*"A" =~? "C"/let ev  = "A" -~? "C"/' "$example_file"
	cabal build; cabal run quantP_oneswap -- --json qmdp --compute-extremal --truncation 49 > output/quantP_oneswap_pure.json

	sed -i '' 's/let ev[[:space:]]*=[[:space:]]*"A" -~? "C"/let ev  = "A" ~~? "C"/' "$example_file"

	python3 scripts/plot_extremal.py output/quantP_oneswap.json --output-dir output/figures
	python3 scripts/plot_extremal.py --pure-json output/quantP_oneswap_pure.json --mixed-json output/quantP_oneswap_mixed.json --output-dir output/figures
}

run_distill() {
	local example_file="quantum-examples/P_onedist.hs"

	# assume it starts with "hasPureSubset"
	cabal build; cabal run quantP_onedist -- --json qmdp --compute-extremal --truncation 34 > output/quantP_onedist_pure.json
	sed -i '' 's/hasPureSubset/hasMixedSubset/g' "$example_file"

	cabal build; cabal run quantP_onedist -- --json qmdp --compute-extremal --truncation 34 > output/quantP_onedist_mixed.json
	sed -i '' 's/hasMixedSubset/hasPureSubset/g' "$example_file"

	python3 scripts/plot_extremal.py --pure-json output/quantP_onedist_pure.json --mixed-json output/quantP_onedist_mixed.json --output-dir output/figures
}

case "$mode" in
	swap)
		run_swap
		;;
	distill)
		run_distill
		;;
	all)
		run_swap
		run_distill
		;;
	*)
		echo "Usage: $0 [swap|distill|all]"
		exit 1
		;;
esac

set +x