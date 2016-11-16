build:
	stack build

run: build
	stack exec visa-bulletin

test: build
	stack test
