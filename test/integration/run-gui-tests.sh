#!/bin/bash
# run-gui-tests.sh - Run ERT tests that require a GUI
#
# Usage:
#   ./test/integration/run-gui-tests.sh [options] [test-selector]
#
# Options:
#   --keep    Keep Emacs open after tests for inspection
#
# Examples:
#   ./test/integration/run-gui-tests.sh                        # Run all tests
#   ./test/integration/run-gui-tests.sh pi-gui-test-session    # Run specific test
#   ./test/integration/run-gui-tests.sh --keep                 # Run and keep open
#
# These tests require a display and will open an Emacs window.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

KEEP=false
SELECTOR="\"pi-gui-test-\""

# Parse args
while [[ $# -gt 0 ]]; do
    case "$1" in
        --keep) KEEP=true; shift ;;
        *) SELECTOR="\"$1\""; shift ;;
    esac
done

WIDTH=80
HEIGHT=22
RESULTS_FILE=$(mktemp /tmp/ert-results.XXXXXX)

echo "=== Pi.el GUI Tests ==="
echo "Project: $PROJECT_DIR"
echo "Selector: $SELECTOR"
echo ""

# Create elisp to run tests
cat > /tmp/run-gui-tests.el << 'ELISP_END'
;; Setup
(setq inhibit-startup-screen t)
ELISP_END

cat >> /tmp/run-gui-tests.el << EOF
(set-frame-size (selected-frame) $WIDTH $HEIGHT)
(add-to-list 'load-path "$PROJECT_DIR")
(add-to-list 'load-path "$PROJECT_DIR/test")
(add-to-list 'load-path "$PROJECT_DIR/test/integration")

;; Load test utilities and tests
(require 'pi-gui-test-utils)
(require 'pi-gui-tests)

;; Redirect messages to file for results
(defvar pi-gui-test--output-file "$RESULTS_FILE")

;; Run tests and collect results
(let ((selector $SELECTOR)
      (passed 0)
      (failed 0)
      (total 0))
  (with-temp-buffer
    (insert "=== GUI Test Results ===\n\n")
    (dolist (test (ert-select-tests selector t))
      (let* ((name (ert-test-name test))
             (result (ert-run-test test)))
        (setq total (1+ total))
        (cond
         ((ert-test-passed-p result)
          (setq passed (1+ passed))
          (insert (format "  PASS: %s\n" name)))
         (t
          (setq failed (1+ failed))
          (insert (format "  FAIL: %s\n" name))
          (when (ert-test-failed-p result)
            (insert (format "        %S\n" (ert-test-result-with-condition-condition result))))))))
    (insert (format "\n=== %d tests: %d passed, %d failed ===\n" total passed failed))
    (write-region (point-min) (point-max) pi-gui-test--output-file))
  (kill-emacs (if (> failed 0) 1 0)))
EOF

# Run Emacs (not in batch mode - we need GUI)
# Temporarily disable set -e since Emacs returns non-zero on test failure
set +e
emacs -Q -l /tmp/run-gui-tests.el 2>&1
EXIT_CODE=$?
set -e

# Show results
if [[ -f "$RESULTS_FILE" ]]; then
    cat "$RESULTS_FILE"
    rm -f "$RESULTS_FILE"
fi
rm -f /tmp/run-gui-tests.el

exit $EXIT_CODE
