# pi.el Makefile
#
# Targets:
#   make test        - Run unit tests (fast, batch mode)
#   make test-gui    - Run GUI tests (requires display)
#   make test-all    - Run all tests
#   make check       - Full check: compile, lint, unit tests
#   make compile     - Byte-compile
#   make lint        - Check docstrings
#   make clean       - Remove generated files

EMACS ?= emacs
BATCH = $(EMACS) --batch -L .

.PHONY: test test-gui test-integration test-all check compile lint clean help

# Default target
help:
	@echo "pi.el build targets:"
	@echo ""
	@echo "  make test          Run unit tests (fast, batch mode)"
	@echo "  make test-gui      Run GUI integration tests (requires display)"
	@echo "  make test-all      Run all tests"
	@echo "  make check         Full check: compile, lint, unit tests"
	@echo "  make compile       Byte-compile Elisp files"
	@echo "  make lint          Check docstrings with checkdoc"
	@echo "  make clean         Remove generated files"

# Unit tests (batch mode, fast)
# Clean first to avoid stale bytecode issues
test: clean
	@echo "=== Unit Tests ==="
	$(BATCH) -L test -l pi -l pi-core-test -l pi-test -f ert-run-tests-batch-and-exit

# GUI integration tests (requires display)
test-gui:
	@echo "=== GUI Tests ==="
	./test/integration/run-gui-tests.sh

# Integration tests with real pi process (batch mode)
test-integration: clean
	@echo "=== Integration Tests ==="
	@command -v pi >/dev/null 2>&1 || { echo "Error: pi not found in PATH"; exit 1; }
	PI_RUN_INTEGRATION=1 $(BATCH) -L test -l pi -l pi-integration-test -f ert-run-tests-batch-and-exit

# All tests
test-all: test test-gui

# Byte-compile (clean first to ensure fresh build)
compile: clean
	@echo "=== Byte-compile ==="
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile pi-core.el pi.el

# Lint with checkdoc
lint:
	@echo "=== Checkdoc ==="
	@$(BATCH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq sentence-end-double-space nil)" \
		--eval "(checkdoc-file \"pi-core.el\")" \
		--eval "(checkdoc-file \"pi.el\")" 2>&1 | \
		{ grep -q "^Warning" && { grep "^Warning"; exit 1; } || echo "OK"; }

# Full check (what CI would run)
check: compile lint test

# Clean generated files (also used as dependency to avoid stale bytecode)
clean:
	@rm -f *.elc test/*.elc
