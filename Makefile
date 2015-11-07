all: deploy

CMD=stack exec -- teef

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
