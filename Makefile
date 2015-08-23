all: deploy

CMD=.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/teef/teef

cabal-build:
	cabal clean
	cabal build

clean:
	${CMD} clean

build:
	${CMD} build

deploy: clean build
	${CMD} deploy

watch: clean
	${CMD} watch
