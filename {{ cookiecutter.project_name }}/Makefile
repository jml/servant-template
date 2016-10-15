.PHONY: check clean docs format image lint

IMAGE_NAME := {{ cookiecutter.project_name }}
IMAGE_TAG := $(shell ./scripts/image-tag)
EXE_NAME := {{ cookiecutter.project_name }}

LINUX_BINARY_PATH = $(shell stack --docker path --local-install-root)

check:
	stack test --fast

clean:
	stack clean
	stack --docker clean

docs:
	stack haddock

format:
	./scripts/hindent-everything

lint:
	hlint -q .

image:
	stack --docker build
	./scripts/build-image \
		$(LINUX_BINARY_PATH)/bin/$(EXE_NAME) \
		$(IMAGE_NAME) \
		$(IMAGE_TAG)
