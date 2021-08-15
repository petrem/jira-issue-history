SOURCES := $(wildcard app/*.hs src/*.hs src/Jira/*.hs)
RESOLVER := $(strip $(shell sed -n '/^resolver:/s/resolver: // p' stack.docker.yaml))

build: $(SOURCES) Dockerfile
	RESOLVER=$(RESOLVER) docker-compose build
debug:
	echo $(RESOLVER)

