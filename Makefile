.PHONY: all build clean test docs run

build: 
	@echo
	@echo "==================="
	@echo "Building project..."
	@echo "==================="
	gprbuild -p sorting.gpr


all: default.cgpr build test docs

default.cgpr:
	gprconfig --batch --config Ada --config C -o $@

clean:
	@echo 
	@echo "==================="
	@echo "Cleaning project..."
	@echo "==================="
	gprclean -r sorting.gpr
	rm -rf build/obj
	rm -f build/default.cgpr
	rm -rf docs/gnatdoc
	rm -rf test/harness

test\harness\test_runner:
	@echo
	@echo "==================="
	@echo "Building tests...  "
	@echo "==================="
	mkdir -p test/harness
	gnat test -dd -P sorting.gpr -v -r
	gprbuild -p -Ptest/harness/test_driver.gpr


test: test\harness\test_runner 
	@echo 
	@echo "==================="
	@echo "Running tests...   "
	@echo "==================="
	test\harness\test_runner --passed-tests=hide --skeleton-default=pass


docs: 
	@echo 
	@echo "==================="
	@echo "Generating docs..."
	@echo "==================="
	mkdir -p docs/gnatdoc
	gnatdoc -P sorting.gpr

run: all
	@echo
	@echo "==================="
	@echo "Running project... "
	@echo "==================="
	build/sorting.exe
