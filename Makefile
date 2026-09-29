.PHONY: format

all:

format:
	air format .
	clang-format -i src/*.h src/*.c
