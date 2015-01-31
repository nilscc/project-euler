.phony: clean

OUTPUTDIR = build

GHC ?= ghc

PROBLEMS = $(wildcard P*.hs)
OBJECTS  = $(PROBLEMS:%.hs=$(OUTPUTDIR)/%)

GHC_FLAGS = -O3
ifdef PROF
	GHC_FLAGS += -prof
endif
ifdef RTSOPTS
	GHC_FLAGS += -rtsopts
endif

all: $(OBJECTS)

$(OUTPUTDIR):
	mkdir $(OUTPUTDIR)

$(OUTPUTDIR)/P%: P%.hs | $(OUTPUTDIR)
	$(GHC) --make $(GHC_FLAGS) $< -outputdir $(OUTPUTDIR) -o $@
	rm $(OUTPUTDIR)/Main.o $(OUTPUTDIR)/Main.hi

clean:
	-rm -r $(OUTPUTDIR)
