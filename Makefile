.PHONY: check

check:
	cookiecutter --no-input -o .test-project .
	cd .test-project/awesome-service; git init; git add .; git commit -m "Initial commit"
	cd .test-project/awesome-service; stack --no-terminal test --fast
	cd .test-project/awesome-service; hlint .
	cd .test-project/awesome-service; ./scripts/hindent-everything && git diff --exit-code
	rm -rf .test-project
