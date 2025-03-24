output/probabilistic-examples/%.prob: output/probabilistic-examples/%.json
	cabal -O2 run prob$(basename $(notdir $<)) -- \
		--json probability \
		>$@ <$<

output/probabilistic-examples/%.json: probabilistic-examples/%.hs
	mkdir -p $(dir $@)
	cabal -O2 build prob$(basename $(notdir $<))
	cabal -O2 run prob$(basename $(notdir $<)) -- \
		+RTS --machine-readable -t -RTS --json run \
		>$@ \
		2>$@.stderr

output/examples/%.json: examples/%.hs
	mkdir -p $(dir $@)
	cabal -O2 build $(basename $(notdir $<))
	cabal -O2 run $(basename $(notdir $<)) -- \
		+RTS --machine-readable -t -RTS \
		>$@ \
		2>$@.stderr

output/probabilistic-examples/%.txt: probabilistic-examples/%.hs
	mkdir -p $(dir $@)
	cabal -O2 build prob$(basename $(notdir $<))
	cabal -O2 run prob$(basename $(notdir $<)) -- \
		+RTS --machine-readable -t -RTS run \
		>$@ \
		2>$@.stderr
