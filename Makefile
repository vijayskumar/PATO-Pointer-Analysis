#######################
## The configuration ##
#######################
# CXX = clang++-3.7
CXX = g++
CXXFLAGS = -fno-rtti -O0 -g -std=c++11 #-DANDERSEN

LLVM_CONFIG ?= llvm-config-3.7
LLVM_SRC_PATH := `$(LLVM_CONFIG) --src-root`
LLVM_BUILD_PATH := `$(LLVM_CONFIG) --obj-root`

LLVM_CXXFLAGS := `$(LLVM_CONFIG) --cxxflags`
LLVM_LDFLAGS := `$(LLVM_CONFIG) --ldflags --libs --system-libs`

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

TRANSLATOR = ast2db
objs = $(addprefix $(BIN), sample $(TRANSLATOR))

all : env $(objs)

env: 
	@command -v $(LLVM_CONFIG) > /dev/null 2>&1 \
	 || { echo >&2 "Could not detect $(LLVM_CONFIG), \
	please set LLVM_CONFIG to correct llvm-config-3.7. \
	Aabort."; exit 1; }

$(BIN)sample : $(SRC)sample.cpp
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $^ \
	 $(CLANG_LIBS) $(LLVM_LDFLAGS) -o $@
	 
## $< the first prerequisite
## $^ all prerequisite
$(BIN)$(TRANSLATOR) : $(SRC)$(TRANSLATOR).cpp $(SRC)visitor.h
	$(CXX) $(CXXFLAGS) $(LLVM_CXXFLAGS) $< \
	 $(CLANG_LIBS) $(LLVM_LDFLAGS) -o $@

.PHONY : test
test : $(BIN)$(TRANSLATOR)
	@$(BIN)$(TRANSLATOR) $(TEST)test.c -- -Wall -I/usr/lib/llvm-3.7/lib/clang/3.7.1/include
	
.PHONY : clean
clean :
	rm -rf $(SRC)*.o $(SRC)*.ll $(objs) $(BIN)*.out
