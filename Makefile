MODE = cabal

ifeq ($(MODE),cabal)
	build_and_run = cabal -O2 build $(1) && cabal -O2 run $(1) --
else ifeq ($(MODE),docker)
	build_and_run = docker run --rm -i pbkat:latest $(1)
endif

clean:
	rm -rv output

output/probabilistic-examples/%.prob: output/probabilistic-examples/%.json
	$(call build_and_run,prob$(basename $(notdir $<))) \
		--json probability \
		>$@ <$<

output/probabilistic-examples/%.json: probabilistic-examples/%.hs
	mkdir -p $(dir $@)
	$(call build_and_run,prob$(basename $(notdir $<))) \
		+RTS --machine-readable -t -RTS --json run \
		>$@ \
		2>$@.stderr

output/examples/%.json: examples/%.hs
	mkdir -p $(dir $@)
	$(call build_and_run,$(basename $(notdir $<))) \
		+RTS --machine-readable -t -RTS \
		>$@ \
		2>$@.stderr

output/probabilistic-examples/%.txt: probabilistic-examples/%.hs
	mkdir -p $(dir $@)
	$(call build_and_run,$(basename $(notdir $<))) \
		+RTS --machine-readable -t -RTS run \
		>$@ \
		2>$@.stderr

.PHONY: test
test:

README.pdf: README.md
	pandoc --pdf-engine=lualatex --output $@ $<
