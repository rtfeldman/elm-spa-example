
MAIN_FILE = src/Main.elm
OUTPUT_FILE = elm.js
RUN_UGLIFY = npx uglify-js $(OUTPUT_FILE) --output=$(OUTPUT_FILE)

.PHONY: production dev test

production:
	npx elm make --optimize $(MAIN_FILE) --output=$(OUTPUT_FILE) && echo "Uglifying output..." && $(RUN_UGLIFY) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=3' && $(RUN_UGLIFY) --mangle

dev:
	npx elm-live $(MAIN_FILE) --output=$(OUTPUT_FILE) --pushstate --debug

test:
	npx elm-test
