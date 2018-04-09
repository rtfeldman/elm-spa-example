
MAIN_FILE = src/Main.elm
OUTPUT_FILE = elm.js
RUN_UGLIFY = npx uglify-js $(OUTPUT_FILE) --output=$(OUTPUT_FILE)

.PHONY: production dev test

production:
	npx elm make --optimize $(MAIN_FILE) --output=$(OUTPUT_FILE) && echo "Uglifying output..." && $(RUN_UGLIFY) --compress --mangle

dev:
	npx elm-live $(MAIN_FILE) --output=$(OUTPUT_FILE) --pushstate --debug

test:
	npx elm-test
