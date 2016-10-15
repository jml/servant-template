.PHONY: check clean docs format image lint

IMAGE_NAME := {{ cookiecutter.project_name }}
IMAGE_TAG := $(shell ./scripts/image-tag)

check:
	stack test --fast

clean:
	stack clean

docs:
	stack haddock

format:
	./scripts/hindent-everything

lint:
	hlint -q .

image:
	./scripts/build-image "{{ cookiecutter.project_name }}" $(IMAGE_NAME):$(IMAGE_TAG)
