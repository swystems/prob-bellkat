output/%.prob: output/%.json
	cabal -O2 run prob$(basename $(notdir $<)) -- \
		--json probability \
		>$@ <$<

output/%.json: %.hs
	mkdir -p $(dir $@)
	cabal -O2 build prob$(basename $(notdir $<))
	cabal -O2 run prob$(basename $(notdir $<)) -- \
		+RTS --machine-readable -t -RTS --json run \
		>$@ \
		2>$@.stderr

output/%.txt: %.hs
	mkdir -p $(dir $@)
	cabal -O2 build prob$(basename $(notdir $<))
	cabal -O2 run prob$(basename $(notdir $<)) -- \
		+RTS --machine-readable -t -RTS run \
		>$@ \
		2>$@.stderr
