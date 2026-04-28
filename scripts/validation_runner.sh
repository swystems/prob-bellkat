#!/bin/bash

set -x

mode="${1:-star}"

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

run_distill_swap() {
	local example_file="quantum-examples/P_distswap.hs"

	# assume it starts with "hasPureSubset"
	cabal build; cabal run quantP_distswap -- --json qmdp --compute-extremal --truncation 199 > output/quantP_distswap_pure.json
	sed -i '' 's/hasPureSubset/hasMixedSubset/g' "$example_file"

	cabal build; cabal run quantP_distswap -- --json qmdp --compute-extremal --truncation 199 > output/quantP_distswap_mixed.json
	sed -i '' 's/hasMixedSubset/hasPureSubset/g' "$example_file"

	python3 scripts/plot_extremal.py --pure-json output/quantP_distswap_pure.json --mixed-json output/quantP_distswap_mixed.json --output-dir output/figures
}

run_star() {
	local example_file="quantum-examples/P5_Star.hs"

	# assume it starts with "hasSubset ["A" ~ "C"]"
	cabal build; cabal run quantP5_Star -- --json mdp --compute-extremal --truncation 54 > output/quantP5_Star_AC.json
	sed -i '' 's/hasSubset \["A" ~ "C"\]/hasSubset ["B" ~ "C"]/g' "$example_file"

	cabal build; cabal run quantP5_Star -- --json mdp --compute-extremal --truncation 54 > output/quantP5_Star_BC.json
	sed -i '' 's/hasSubset \["B" ~ "C"\]/hasPureSubset ["A" ~ "C"]/g' "$example_file"

	python3 scripts/plot_extremal.py output/quantP5_Star_AC.json --output-dir output/figures
	python3 scripts/plot_extremal.py output/quantP5_Star_BC.json --output-dir output/figures

	cabal build; cabal run quantP5_Star -- --json qmdp --compute-extremal --truncation 54 > output/quantP5_Star_AC_pure.json
	sed -i '' 's/hasPureSubset \["A" ~ "C"]/hasMixedSubset ["A" ~ "C"]/g' "$example_file"

	cabal build; cabal run quantP5_Star -- --json qmdp --compute-extremal --truncation 54 > output/quantP5_Star_AC_mixed.json
	sed -i '' 's/hasMixedSubset \["A" ~ "C"]/hasPureSubset ["B" ~ "C"]/g' "$example_file"

	cabal build; cabal run quantP5_Star -- --json qmdp --compute-extremal --truncation 54 > output/quantP5_Star_BC_pure.json
	sed -i '' 's/hasPureSubset \["B" ~ "C"]/hasMixedSubset ["B" ~ "C"]/g' "$example_file"

	cabal build; cabal run quantP5_Star -- --json qmdp --compute-extremal --truncation 54 > output/quantP5_Star_BC_mixed.json
	sed -i '' 's/hasMixedSubset \["B" ~ "C"]/hasSubset \["A" ~ "C"]/g' "$example_file"

	python3 scripts/plot_extremal.py --pure-json output/quantP5_Star_AC_pure.json --mixed-json output/quantP5_Star_AC_mixed.json --output-dir output/figures
	python3 scripts/plot_extremal.py --pure-json output/quantP5_Star_BC_pure.json --mixed-json output/quantP5_Star_BC_mixed.json --output-dir output/figures

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
	all)
		run_swap
		run_distill
		run_distill_swap
		run_star
		;;
	*)
		echo "Usage: $0 [swap|distill|distill-swap|star|all]"
		exit 1
		;;
esac

set +x
