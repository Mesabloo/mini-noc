SOURCES = Variables.hs Runtime/Internal.hs Runtime/Stack.hs Runtime/Value.hs Expr.hs Bytecode.hs Primitives.hs Compiler.hs VM.hs
EXECUTABLE := VM

GHC_FLAGS = -Weverything -Wno-missing-import-lists -Wno-unsafe -Wno-name-shadowing -Wno-missing-export-lists

GHC_FLAGS_RELEASE = -O2 -rtsopts -flate-dmd-anal -fspecialise-aggressively -flate-specialise -fstatic-argument-transformation -DDEBUG=0
GHC_FLAGS_DEBUG = -debug -g3 -O0 -rtsopts -DDEBUG=1 -lasan
            
GHC_EXTS := -XStrict -XNoStarIsType -XNoImplicitPrelude

GHC := ghc

RR := rr
RR_TRACE_DIR := rr_traces

all: VM

VM: release
	./$(EXECUTABLE)

debugging: debug
	-mkdir $(RR_TRACE_DIR)
	-env _RR_TRACE_DIR=$(RR_TRACE_DIR) $(RR) record $(EXECUTABLE)-debug # --args +RTS -V0 -DS -Dg -Dn
	$(RR) replay ./$(RR_TRACE_DIR)/latest-trace
	

debug: $(SOURCES)
	$(GHC) -o $(EXECUTABLE)-debug $^ $(GHC_FLAGS_DEBUG) $(GHC_FLAGS) $(GHC_EXTS)

release: $(SOURCES)
	$(GHC) -o $(EXECUTABLE) $^ $(GHC_FLAGS_RELEASE) $(GHC_FLAGS) $(GHC_EXTS)

.PHONY: clean
clean: 
	-rm -r $(SOURCES:.hs=.o) $(SOURCES:.hs=.hi) $(EXECUTABLE) $(EXECUTABLE)-debug
	-yes | rm -r $(RR_TRACE_DIR)
