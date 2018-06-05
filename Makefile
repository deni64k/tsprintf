all: run

build/main: main.cxx
	g++-8 $(CXXFLAGS) -fconcepts -std=c++17 $< -o build/main

run: build/main
	@./$^

clean:
	-@rm -f build/main

.PHONY: all run clean
