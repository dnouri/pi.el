;;; pi-gui-tests.el --- GUI integration tests for pi.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests that require a real Emacs GUI (windows, frames, scrolling).
;; Run with: ./test/integration/run-gui-tests.sh
;;
;; Tests are organized into groups:
;; - pi-gui-test-session-*   : Session management
;; - pi-gui-test-scroll-*    : Scroll behavior
;; - pi-gui-test-window-*    : Window management
;; - pi-gui-test-input-*     : Input buffer behavior
;;
;; Tests in the same group share a session for speed.
;; Use `pi-gui-test-with-fresh-session` when isolation is needed.

;;; Code:

(require 'ert)
(require 'pi-gui-test-utils)

;;;; Session Tests

(ert-deftest pi-gui-test-session-starts ()
  "Test that pi session starts with proper layout."
  (pi-gui-test-with-session
    (should (pi-gui-test-session-active-p))
    (should (pi-gui-test-chat-window))
    (should (pi-gui-test-input-window))
    (should (pi-gui-test-verify-layout))))

(ert-deftest pi-gui-test-session-model-set ()
  "Test that model is set correctly."
  (pi-gui-test-with-session
    (let* ((chat-buf (plist-get pi-gui-test--session :chat-buffer))
           (model (with-current-buffer chat-buf
                    (plist-get pi--state :model))))
      ;; Model should be set (might be string or plist)
      (should model))))

;;;; Scroll Detection Tests

(ert-deftest pi-gui-test-scroll-detection-at-end ()
  "Test that window at end is detected correctly."
  (pi-gui-test-with-session
    (pi-gui-test-scroll-to-end)
    (should (pi-gui-test-at-end-p))))

(ert-deftest pi-gui-test-scroll-detection-not-at-end ()
  "Test that scrolled-up window is detected correctly."
  (pi-gui-test-with-session
    (pi-gui-test-ensure-scrollable)
    (pi-gui-test-scroll-up 15)
    (should-not (pi-gui-test-at-end-p))))

;;;; Scroll Preservation Tests

(ert-deftest pi-gui-test-scroll-preserved-streaming ()
  "Test scroll position preserved during streaming response.
Uses line number for stricter checking - not even a single line drift."
  (pi-gui-test-with-session
    (pi-gui-test-ensure-scrollable)
    (should (pi-gui-test-verify-layout))
    (pi-gui-test-scroll-up 20)
    (should-not (pi-gui-test-at-end-p))
    (let ((line-before (pi-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-gui-test-send "Write a haiku about mountains.")
      (should (pi-gui-test-verify-layout))
      (should (= line-before (pi-gui-test-top-line-number))))))

(ert-deftest pi-gui-test-scroll-preserved-tool-use ()
  "Test scroll position preserved when pi uses tools.
Uses line number for stricter checking - not even a single line drift."
  (pi-gui-test-with-session
    (pi-gui-test-ensure-scrollable)
    (should (pi-gui-test-verify-layout))
    (let ((test-file (pi-gui-test-create-temp-file "test.txt" "Hello World\n")))
      (unwind-protect
          (progn
            (pi-gui-test-scroll-up 20)
            (should-not (pi-gui-test-at-end-p))
            (let ((line-before (pi-gui-test-top-line-number)))
              (should (> line-before 1))
              (pi-gui-test-send (format "Read %s and tell me what it says." test-file))
              (should (pi-gui-test-verify-layout))
              (should (= line-before (pi-gui-test-top-line-number)))))
        (pi-gui-test-delete-temp-file test-file)))))

(ert-deftest pi-gui-test-scroll-preserved-multiple ()
  "Test scroll preserved across multiple messages.
Uses line number for stricter checking - not even a single line drift."
  (pi-gui-test-with-session
    (pi-gui-test-ensure-scrollable)
    (should (pi-gui-test-verify-layout))
    (pi-gui-test-scroll-up 25)
    (should-not (pi-gui-test-at-end-p))
    (let ((line-before (pi-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-gui-test-send "Say 'one'.")
      (should (= line-before (pi-gui-test-top-line-number)))
      (pi-gui-test-send "Say 'two'.")
      (should (= line-before (pi-gui-test-top-line-number)))
      (pi-gui-test-send "Say 'three'.")
      (should (= line-before (pi-gui-test-top-line-number))))))

(ert-deftest pi-gui-test-scroll-auto-when-at-end ()
  "Test auto-scroll when user is at end of buffer."
  (pi-gui-test-with-session
    (pi-gui-test-scroll-to-end)
    (should (pi-gui-test-at-end-p))
    (pi-gui-test-send "Write a very short haiku.")
    (should (pi-gui-test-at-end-p))))

;;;; Window Management Tests

(ert-deftest pi-gui-test-window-both-visible ()
  "Test both chat and input windows are visible."
  (pi-gui-test-with-session
    (should (pi-gui-test-chat-window))
    (should (pi-gui-test-input-window))
    (should (window-live-p (pi-gui-test-chat-window)))
    (should (window-live-p (pi-gui-test-input-window)))))

(ert-deftest pi-gui-test-window-kill-both ()
  "Test killing chat buffer also kills input buffer."
  (pi-gui-test-with-fresh-session
    (let ((chat-buf (plist-get pi-gui-test--session :chat-buffer))
          (input-buf (plist-get pi-gui-test--session :input-buffer)))
      (should (buffer-live-p chat-buf))
      (should (buffer-live-p input-buf))
      ;; Kill chat
      (kill-buffer chat-buf)
      ;; Both should be dead
      (should-not (buffer-live-p chat-buf))
      (should-not (buffer-live-p input-buf)))))

;;;; Input Buffer Tests

(ert-deftest pi-gui-test-input-history-navigation ()
  "Test M-p and M-n navigate input history."
  (pi-gui-test-with-session
    (let ((input-buf (plist-get pi-gui-test--session :input-buffer)))
      ;; Send a few messages to build history
      (pi-gui-test-send "First message")
      (pi-gui-test-send "Second message")
      (pi-gui-test-send "Third message")
      ;; Now test history navigation in input buffer
      (with-current-buffer input-buf
        (erase-buffer)
        ;; M-p should show previous (Third)
        (pi-previous-input)
        (should (string= (string-trim (buffer-string)) "Third message"))
        ;; M-p again should show Second
        (pi-previous-input)
        (should (string= (string-trim (buffer-string)) "Second message"))
        ;; M-n should go back to Third
        (pi-next-input)
        (should (string= (string-trim (buffer-string)) "Third message"))))))

(ert-deftest pi-gui-test-input-cleared-after-send ()
  "Test input buffer is cleared after sending."
  (pi-gui-test-with-session
    (let ((input-buf (plist-get pi-gui-test--session :input-buffer)))
      (with-current-buffer input-buf
        (erase-buffer)
        (insert "Test message"))
      (should (string= (pi-gui-test-input-content) "Test message"))
      (pi-gui-test-send "Test message")
      (should (string= (string-trim (pi-gui-test-input-content)) "")))))

;;;; Content Tests

(ert-deftest pi-gui-test-content-user-message-shown ()
  "Test that user messages appear in chat."
  (pi-gui-test-with-session
    (pi-gui-test-send "Hello from test")
    (should (pi-gui-test-chat-contains "Hello from test"))))

(ert-deftest pi-gui-test-content-tool-output-shown ()
  "Test that tool output appears in chat."
  (pi-gui-test-with-session
    (let ((test-file (pi-gui-test-create-temp-file "content-test.txt" "Test Content\n")))
      (unwind-protect
          (progn
            (pi-gui-test-send (format "Read %s" test-file))
            ;; Should show the file path or content
            (should (or (pi-gui-test-chat-contains "content-test.txt")
                        (pi-gui-test-chat-contains "Test Content"))))
        (pi-gui-test-delete-temp-file test-file)))))

;;;; Spacing Tests (run before window-kill-both to check accumulated output)

(defun pi-gui-test--extract-context (content pattern)
  "Find PATTERN in CONTENT and return surrounding context."
  (when (string-match pattern content)
    (let* ((start (max 0 (- (match-beginning 0) 20)))
           (end (min (length content) (+ (match-end 0) 30)))
           (excerpt (substring content start end)))
      (replace-regexp-in-string "\n" "↵" excerpt))))

(defun pi-gui-test--check-spacing (content)
  "Check spacing rules in CONTENT, return list of violations or nil.
Rules: blank line after headers, no triple+ blank lines."
  (let (violations)
    ;; Headers should have blank line after them
    (unless (string-match-p "You ─+\n\n" content)
      (push "Missing blank line after 'You' header" violations))
    (unless (string-match-p "Assistant ─+\n\n" content)
      (push "Missing blank line after 'Assistant' header" violations))
    ;; No triple blank lines (more than one blank line)
    (when (string-match-p "\n\n\n\n" content)
      (push (format "Too many consecutive blank lines: ...%s..."
                    (pi-gui-test--extract-context content "\n\n\n\n"))
            violations))
    (nreverse violations)))

(ert-deftest pi-gui-test-spacing-consistency ()
  "Check spacing consistency in accumulated chat output."
  (pi-gui-test-with-session
    ;; Generate some output if session is fresh
    (unless (pi-gui-test-chat-contains "Assistant")
      (pi-gui-test-send "Say: hello"))
    (let* ((buf (plist-get pi-gui-test--session :chat-buffer))
           (content (with-current-buffer buf (buffer-string)))
           (violations (pi-gui-test--check-spacing content)))
      (when violations
        (ert-fail (format "Spacing violations:\n  - %s"
                          (string-join violations "\n  - ")))))))

(provide 'pi-gui-tests)
;;; pi-gui-tests.el ends here
