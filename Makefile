#######################
## The configuration ##
#######################
# CXX = clang++-3.7
CXX = g++
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
# CLANG_LIBS_X := \
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

BIN = ./bin/
SRC = ./src/
TEST = ./test/

objs = $(addprefix $(BIN), sample pg)

all: $(objs)

$(BIN)sample: $(SRC)sample.cpp
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $^ \
	 $(CLANG_LIBS) $(LLVM_LDFLAGS) -o $@
	 
## $< the first prerequisite
## $^ all prerequisite
$(BIN)pg: $(SRC)pg.cpp $(SRC)visitor.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< \
	 $(CLANG_LIBS) $(LLVM_LDFLAGS) -o $@

## For debugging
# LLVM_CXXFLAGS_X := -I/usr/lib/llvm-3.7/include \
# -DNDEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS \
# -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -fomit-frame-pointer \
# -std=c++11 -fvisibility-inlines-hidden -fno-exceptions -fPIC \
# -ffunction-sections -fdata-sections -Wcast-qual

## This is the save as the one step compilation
## The order matters, the object is before the libs, so the unresolved 
## symbols are known when scanning the libs
# $(BIN)pg: $(SRC)pg.o
# 	$(CXX) $< $(CLANG_LIBS) $(LLVM_LDFLAGS) -o $@

# $(SRC)pg.o: $(SRC)pg.cpp $(SRC)visitor.h
# 	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) -c $< -o $@

test: $(BIN)pg
	$(BIN)pg $(TEST)type.c -- -Wall -I/usr/lib/llvm-3.7/lib/clang/3.7.1/include
	

clean:
	rm -rf *.o *.ll $(objs) *.out
