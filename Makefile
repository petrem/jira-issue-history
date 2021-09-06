SOURCES := $(wildcard app/*.hs src/*.hs src/Jira/*.hs)
RESOLVER := $(strip $(shell sed -n '/^resolver:/s/resolver: // p' stack.docker.yaml))

build: $(SOURCES) Dockerfile
	RESOLVER=$(RESOLVER) DOCKER_BUILDKIT=1 docker-compose build

update-base:
	docker pull ubuntu:20.04

push:
	docker-compose push

debug:
	echo $(RESOLVER)

.PHONY: build update-base debug
