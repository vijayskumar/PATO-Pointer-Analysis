#######################
## The configuration ##
#######################
CXX = clang++-3.7
CXXFLAGS = -fno-rtti -O0 -g -std=c++11

LLVM_BIN_PATH = 
LLVM_CONFIG := llvm-config-3.7
LLVM_SRC_PATH := `$(LLVM_CONFIG) --src-root`
LLVM_BUILD_PATH := `$(LLVM_CONFIG) --obj-root`

LLVM_CXXFLAGS := `$(LLVM_CONFIG) --cxxflags`
LLVM_LDFLAGS := `$(LLVM_CONFIG) --ldflags --libs --system-libs`

# List of Clang libraries to link. The proper -L will be provided by the
# call to llvm-config
# Note that I'm using -Wl,--{start|end}-group around the Clang libs; this is
# because there are circular dependencies that make the correct order difficult
# to specify and maintain. The linker group options make the linking somewhat
# slower, but IMHO they're still perfectly fine for tools that link with Clang.
# CLANG_LIBS := \
# 	-Wl,--start-group \
# 	-lclangAST \
# 	-lclangAnalysis \
# 	-lclangBasic \
# 	-lclangDriver \
# 	-lclangFrontend \
# 	-lclangFrontendTool \
# 	-lclangLex \
# 	-lclangParse \
# 	-lclangSema \
# 	-lclangEdit \
# 	-lclangASTMatchers \
# 	-lclangRewrite \
# 	-lclangRewriteFrontend \
# 	-lclangStaticAnalyzerFrontend \
# 	-lclangStaticAnalyzerCheckers \
# 	-lclangStaticAnalyzerCore \
# 	-lclangSerialization \
# 	-lclangToolingCore \
# 	-lclangTooling \
# 	-Wl,--end-group

CLANG_LIBS := \
	-lclangFrontendTool \
    -lclangFrontend \
    -lclangDriver \
    -lclangSerialization \
    -lclangTooling \
    -lclangCodeGen \
    -lclangParse \
    -lclangSema \
    -lclangAnalysis \
    -lclangRewriteFrontend \
    -lclangRewrite \
    -lclangEdit \
    -lclangAST \
    -lclangLex \
    -lclangBasic


#############################	
## The compilation process ##
#############################

objs = sample pg

all: $(objs)

sample: sample.cpp
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $^ \
	 $(CLANG_LIBS) $(LLVM_LDFLAGS) -o $@
	 
## $< the first prerequisite
## $^ all prerequisite
pg: pg.cpp visitor.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< \
	 $(CLANG_LIBS) $(LLVM_LDFLAGS) -o $@


test: pg
	./pg test.c --
	

clean:
	rm -rf *.o *.ll $(objs)
