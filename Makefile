APP_NAME=tic-tac-toe
SRC_DIR=src
BUILD_DIR=build


.PHONY: build dev clean run

build-boot:
	ghc -c ${SRC_DIR}/*.hs-boot -outputdir ${BUILD_DIR}

build build-boot:
	ghc ${SRC_DIR}/*.hs -outputdir ${BUILD_DIR} -o ${APP_NAME} -threaded -Wall -O2

run: build
	./${APP_NAME}

dev:
	ghcid ${SRC_DIR}/*

clean:
	rm -rf ${BUILD_DIR}
