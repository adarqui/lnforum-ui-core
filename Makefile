build:
	stack build --fast

build-watch:
	stack build --fast --file-watch

clean:
	stack clean

until:
	until stack build; do echo eek; done

make tests:
	stack test --fast

ghci:
	stack ghci lnforum-ui-core
