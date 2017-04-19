.PHONY: local ci build lint autoformat autoformat-check

local: autoformat       build lint
ci:    autoformat-check build lint

build:
	@nix-build
lint:
	@nix-shell --pure --run 'exec hlint .'
autoformat:
	@nix-shell --pure --run "exec $(MAKE) _autoformat"
autoformat-check: autoformat
	@nix-shell --pure --run 'status=$$(git status --porcelain | grep -v "^M ") ; [ -z "$$status" ] || { printf >&2 "%s\n%s\n" "fatal: some files are unformatted (or repo unclean):" "$$status" ; exit 1 ; }'


#———————————————————————————————————————————————————————————————————————————————


.PHONY: _autoformat

_autoformat: $(shell find . -name '*.hs' -a -not -path '*/.*' -a -not -path './dist/*' -printf 'dist/autoformat/%P_fmt\n')

dist/autoformat/%_fmt: %
	@echo "Formatting $<..."
	@hindent --line-length 80 "$<" \
		&& stylish-haskell --inplace "$<" \
		&& mkdir -p "$(dir $@)" && touch "$@" \
		|| true # we want to see real compilation errors
