MODE = cabal

ifeq ($(MODE),cabal)
	build_and_run = cabal build $(1) && cabal run $(1) --
else ifeq ($(MODE),stack)
	build_and_run = stack build && stack run $(1) --
else ifeq ($(MODE),docker)
	build_and_run = docker run --rm -i pbkat:latest $(1)
else ifeq ($(MODE),direct)
	build_and_run = $(1)
endif

QUANTUM_PROTOS = \
	Pa \
	Pd \
	Pd_parallel \
	P5_3_pompili \
	P5_ASAP \
	P5_Li \
	P5_Star

clean:
	rm -rv output

all-quant: $(QUANTUM_PROTOS:%=output/quantum-examples/%.quant) 

output/quantum-examples/%.quant: output/quantum-examples/%.json
	$(call build_and_run,quant$(basename $(notdir $<))) \
		--json probability \
		>$@ <$<

output/quantum-examples/%.json: quantum-examples/%.hs
	mkdir -p $(dir $@)
	$(call build_and_run,quant$(basename $(notdir $<))) \
		+RTS --machine-readable -t -RTS --json run \
		>$@ \
		2>$@.stderr

output/examples/%.json: examples/%.hs
	mkdir -p $(dir $@)
	$(call build_and_run,$(basename $(notdir $<))) \
		+RTS --machine-readable -t -RTS \
		>$@ \
		2>$@.stderr

output/quantum-examples/%.txt: quantum-examples/%.hs
	mkdir -p $(dir $@)
	$(call build_and_run,quant$(basename $(notdir $<))) \
		+RTS --machine-readable -t -RTS run \
		>$@ \
		2>$@.stderr

.PHONY: test
test:

README.pdf: README.md metadata.yaml
	pandoc --pdf-engine=lualatex --metadata-file metadata.yaml --output $@ $<
