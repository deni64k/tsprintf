CXX=g++-HEAD

all: run

build/main: main.cxx tsprintf.hxx
	$(CXX) $(CXXFLAGS) -fconcepts -std=c++17 $< -o build/main

run: build/main
	@./$^

clean:
	-@rm -f build/main

.PHONY: all run clean
