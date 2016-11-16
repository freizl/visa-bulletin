default: run

build:
	stack build

run: build
	stack exec visa-bulletin
