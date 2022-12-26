# for Windows
UNAME=$(shell uname)
PREFIX=winpty
ifeq (${UNAME}, Darwin)
	PREFIX=
endif

up:
	docker compose up -d
build:
	docker compose build --no-cache --force-rm
down:
	docker compose down