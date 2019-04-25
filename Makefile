.PHONY: all
all: .installed dist

# Run development server
.PHONY: run
run: .installed copy-assets
	@npx parcel src/index.html

# Build distribution files and place them where they are expected
.PHONY: dist
dist: .installed copy-assets
	@npx parcel build src/index.html

.PHONY: copy-assets
copy-assets:
	@mkdir -p dist/assets/images/
	@cp -r assets/images/* dist/assets/images/

.PHONY: install
install:
	@rm -rf .installed
	@make .installed

.installed: package.json package-lock.json elm.json
	@echo "Dependencies files are newer than .installed; (re)installing."
	@npm clean-install
	@echo "This file is used by 'make' for keeping track of last install time. If package.json, package-lock.json or elm.json are newer then this file (.installed) then all 'make *' commands that depend on '.installed' know they need to run npm install first." \
		> .installed



# Nuke from orbit
clean:
	@rm -rf dist/ elm-stuff/ node_modules/ .cache
	@rm -f .installed
