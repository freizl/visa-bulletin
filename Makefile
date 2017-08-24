build:
	stack build

run: build
	stack exec visa-bulletin

watch:
	stack test --file-watch

test: build
	stack test
