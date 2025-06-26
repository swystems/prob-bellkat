MODE = cabal

ifeq ($(MODE),cabal)
	build_and_run = cabal -O2 build $(1) && cabal -O2 run $(1) --
else ifeq ($(MODE),docker)
	build_and_run = docker run --rm -i pbkat:latest $(1)
else ifeq ($(MODE),direct)
	build_and_run = $(1)
endif

PROBABILISTIC_PROTOS = \
	Pa \
	Pa1 \
	Pag \
	P5_1_I_ordered \
	P5_1_I_parallel \
	P5_1_II_ordered \
	P5_1_II_parallel \
	P5_1_II_ordered_three \
	P5_1_II_parallel_three \
	P5_1_III_one \
	P5_1_III_two \
	P5_1_IV \
	P5_3_pompili \
	P5_3_coopmans_outer \
	P5_3_coopmans_inner \
	P5_3_coopmans_mixed

clean:
	rm -rv output

all-prob: $(PROBABILISTIC_PROTOS:%=output/probabilistic-examples/%.prob) 

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
	$(call build_and_run,prob$(basename $(notdir $<))) \
		+RTS --machine-readable -t -RTS run \
		>$@ \
		2>$@.stderr

.PHONY: test
test:

README.pdf: README.md
	pandoc --pdf-engine=lualatex --output $@ $<
