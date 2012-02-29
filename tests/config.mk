DEBUG := false

ifeq ($(DEBUG), true)
  DEBUG_FLG := -debug
else
  DEBUG_FLG :=
endif

GHC_OPTS :=
GHC_OPTS += $(GHC_OPTS) --make

GHC    := $(HOME)/test-install/ghc-7.4.1-lwc/bin/ghc $(GHC_OPTS)

all: $(TARGETS)

%.bin:	%.hs
	$(GHC) $< -o $@

%.cmm:  %.hs
	$(GHC) -c $< -ddump-cmm >$@

clean:
	rm -f *.bin *.o *.hi *.cmm *~ *.stat
