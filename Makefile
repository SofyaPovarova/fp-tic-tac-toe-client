APP_NAME=tic-tac-toe
SRC_DIR=src
BUILD_DIR=build


.PHONY: build dev clean run

build:
	ghc ${SRC_DIR}/* -outputdir ${BUILD_DIR} -o ${APP_NAME} -threaded -Wall -O2

run: build
	./${APP_NAME}

dev:
	ghcid ${SRC_DIR}/*

clean:
	rm -rf ${BUILD_DIR}
