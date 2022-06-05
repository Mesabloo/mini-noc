SOURCES = Variables.hs Runtime/Internal.hs Runtime/Stack.hs Runtime/Value.hs Expr.hs Examples.hs Bytecode.hs Primitives.hs Compiler.hs VM.hs
EXECUTABLE := VM

GHC_FLAGS = -Weverything -Wno-missing-import-lists -Wno-unsafe -Wno-name-shadowing -Wno-missing-export-lists

GHC_FLAGS_RELEASE = -O2 -rtsopts -DDEBUG=0 -flate-dmd-anal -fstatic-argument-transformation -flate-specialise -fspecialise-aggressively 
GHC_FLAGS_DEBUG = -debug -DDEBUG=1 -g3 -O0 -rtsopts
GHC_FLAGS_PROF = -rtsopts -DDEBUG=0 -prof -fprof-auto -DPROF=1 
            
GHC_EXTS := -XStrict -XNoStarIsType -XNoImplicitPrelude

GHC := ghc

RR := rr
RR_TRACE_DIR := rr_traces

all: VM

VM: release
	-./$(EXECUTABLE)

debugging: debug
	-mkdir $(RR_TRACE_DIR)
	-env _RR_TRACE_DIR=$(RR_TRACE_DIR) $(RR) record -n $(EXECUTABLE)-debug --args +RTS -V0 -DS -Dg -Dn -t
	$(RR) replay ./$(RR_TRACE_DIR)/latest-trace

profiling: prof
	-./$(EXECUTABLE)-prof +RTS -p -pa
	profiteur $(EXECUTABLE)-prof.prof
	

debug: $(SOURCES)
	$(GHC) -o $(EXECUTABLE)-debug $^ $(GHC_FLAGS_DEBUG) $(GHC_FLAGS) $(GHC_EXTS)

release: $(SOURCES)
	$(GHC) -o $(EXECUTABLE) $^ $(GHC_FLAGS_RELEASE) $(GHC_FLAGS) $(GHC_EXTS)

prof: $(SOURCES)
	$(GHC) -o $(EXECUTABLE)-prof $^ $(GHC_FLAGS_PROF) $(GHC_FLAGS) $(GHC_EXTS)

.PHONY: clean
clean: 
	-rm -r $(SOURCES:.hs=.o) $(SOURCES:.hs=.hi) $(EXECUTABLE) $(EXECUTABLE)-debug $(EXECUTABLE)-prof
	-yes | rm -r $(RR_TRACE_DIR)
