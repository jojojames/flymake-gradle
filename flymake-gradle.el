;;; flymake-gradle.el --- Flymake extension for Gradle. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Authors: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/flymake-gradle
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages gradle

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Flymake extension for Gradle.
;;
;; (with-eval-after-load 'flymake
;;   (flymake-gradle-setup))

;;; Code:

(require 'flymake)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;;; Flymake

(defcustom flymake-gradle-executable "gradle"
  "Executable for gradle."
  :type 'string
  :group 'flymake-gradle)

(defcustom flymake-gradle-args nil
  "Args to pass to gradle."
  :type 'list
  :group 'flymake-gradle)

(defvar-local flymake-gradle--lint-process nil
  "Buffer-local process started for linting the buffer.")

(defcustom flymake-gradle-log-level "-quiet"
  "The log level gradle should use.

This log level should match an actual gradle log level.

e.g. warn, info, or a custom log level.

Warn should be used to check for warnings but isn't available in gradle
versions below 3 so it's safer choice to use error."
  :type 'string
  :group 'flymake-gradle)

(defcustom flymake-gradle-compile-function 'flymake-gradle-get-compile-commands
  "Function used to find build command for gradle.

ex. This function may return '\(\"clean\" \"build\"\)
which will then change the final command to be \"gradle clean build\"."
  :type 'function
  :group 'flymake-gradle)

(defcustom flymake-gradle-adjust-log-level-automatically nil
  "Whether or not to adjust gradle's log level automatically.

The log level variables are stored in `flymake-gradle-log-level'."
  :type 'boolean
  :group 'flymake-gradle)

;;;###autoload
(defun flymake-gradle-setup ()
  "Set up Flymake for Gradle."
  (interactive)
  (add-hook 'java-mode-hook #'flymake-gradle-add-hook)
  (add-hook 'kotlin-mode-hook #'flymake-gradle-add-hook))

;;;###autoload
(defun flymake-gradle-add-hook ()
  "Add `flymake-gradle-lint' to `flymake-diagnostic-functions'."
  (when flymake-gradle-adjust-log-level-automatically
    (flymake-gradle--set-log-level-auto))

  (add-hook 'flymake-diagnostic-functions
            'flymake-gradle-lint-if-possible nil t))

(defun flymake-gradle-lint-if-possible (report-fn &rest args)
  "Run `flymake-gradle-lint' if possible."
  (when (flymake-gradle--root-directory)
    (apply #'flymake-gradle-lint report-fn args)))

(defun flymake-gradle-lint (report-fn &rest _args)
  "A Flymake backend for gradle check.

REPORT-FN will be called when gradle process finishes."
  (when (and flymake-gradle--lint-process
             (process-live-p flymake-gradle--lint-process))
    (kill-process flymake-gradle--lint-process))
  (let ((default-directory (flymake-gradle--root-directory))
        (source-buffer (current-buffer))
        (output-buffer (generate-new-buffer " *flymake-gradle-lint*")))
    (setq flymake-gradle--lint-process
          (make-process
           :name "flymake-gradle-lint"
           :buffer output-buffer
           :command `(,(flymake-gradle--executable)
                      ,@(funcall flymake-gradle-compile-function)
                      ,flymake-gradle-log-level
                      "--console"
                      "plain"
                      ,@flymake-gradle-args)
           :connection-type 'pipe
           :sentinel
           (lambda (proc _event)
             (when (eq (process-status proc) 'exit)
               (unwind-protect
                   (cond
                    ((not (and (buffer-live-p source-buffer)
                               (eq proc (with-current-buffer source-buffer
                                          flymake-gradle--lint-process))))
                     (flymake-log :warning
                                  "byte-compile process %s obsolete" proc))
                    ((zerop (process-exit-status proc))
                     ;; No gradle errors/warnings..
                     (funcall report-fn nil))
                    ((= 1 (process-exit-status proc))
                     (flymake-gradle--lint-done report-fn
                                                source-buffer
                                                output-buffer))
                    (:error
                     (funcall report-fn
                              :panic
                              :explanation
                              (format "gradle process %s errored." proc))))
                 (kill-buffer output-buffer))))))))

;; Helpers
(defun flymake-gradle--lint-done (report-fn
                                  source-buffer
                                  output-buffer)
  "Process gradle result and call REPORT-FN.

SOURCE-BUFFER is the buffer to apply flymake to.
OUTPUT-BUFFER is the result of running gradle on SOURCE-BUFFER."
  (with-current-buffer
      source-buffer
    (save-excursion
      (save-restriction
        (widen)
        (funcall
         report-fn
         (cond
          ((eq major-mode 'kotlin-mode)
           (flymake-gradle--kotlin-parse-buffer source-buffer output-buffer))
          ((eq major-mode 'java-mode)
           (flymake-gradle--java-parse-buffer source-buffer output-buffer))
          (:default nil)))))))

(defun flymake-gradle--kotlin-parse-buffer (source-buffer output-buffer)
  "Parse OUTPUT-BUFFER for gradle diagnostics and return `flymake-diagnostics'.

Diagnostics will be provided for SOURCE-BUFFER."
  (with-current-buffer output-buffer
    (let* ((result '()) ;; Accumulate results here.
           (lines (split-string (buffer-string) "\n" t))
           (numLines (length lines))
           (i 0)
           (source-buffer-name
            (with-current-buffer source-buffer
              (file-name-nondirectory buffer-file-name))))
      ;; Example error patterns:
      ;; e: /kotlin/MainActivity.kt: (10, 46): Expecting ')'
      ;; w: /kotlin/MainActivity.kt: (12, 13): Variable 'a' is never used
      (while (< i numLines)
        ;; Filter out messages that don't match buffer name.
        (when (string-match-p source-buffer-name (nth i lines))
          (let* ((line (nth i lines))
                 (split (split-string line ":" t))
                 (type (nth 0 split))  ;; "e" or "w"
                 (_ (nth 1 split)) ;; filename
                 (row-column (nth 2 split)) ;; " (10, 46)"
                 (line (flymake-gradle--row-from-row-column row-column))
                 (column (flymake-gradle--col-from-row-column row-column))
                 (message (mapconcat (lambda (str) str)
                                     (cdddr split)
                                     ""))
                 (point (flymake-gradle--find-point
                         source-buffer
                         line
                         column)))
            ;; Accumulate the result.
            (push (flymake-make-diagnostic
                   source-buffer
                   (1- point)
                   point
                   (if (equal type "e") :error :warning)
                   message)
                  result)))
        (setq i (1+ i)))
      result)))

(defun flymake-gradle--java-parse-buffer (source-buffer output-buffer)
  "Parse OUTPUT-BUFFER for gradle diagnostics and return `flymake-diagnostics'.

Diagnostics will be provided for SOURCE-BUFFER."
  (with-current-buffer output-buffer
    (let* ((result '()) ;; Accumulate results here.
           (lines (split-string (buffer-string) "\n" t))
           (numLines (length lines))
           (i 0)
           (source-buffer-name
            (with-current-buffer source-buffer
              (file-name-nondirectory buffer-file-name))))
      ;; Example error patterns:
      ;; /java/MainActivity.java:11: error: ';' expected setContentView(R.layout.activity_main)
      (while (< i numLines)
        ;; Filter out messages that don't match buffer name.
        (when (string-match-p source-buffer-name (nth i lines))
          (let* ((line (nth i lines))
                 (split (split-string line ":" t))
                 (_ (nth 0 split)) ;; filename
                 (line (string-to-number (nth 1 split)))
                 (column 1) ;; Java error messages don't provide columns.
                 (message (nth 3 split))
                 (point (flymake-gradle--find-point
                         source-buffer
                         line
                         column)))
            ;; Accumulate the result.
            (push (flymake-make-diagnostic
                   source-buffer
                   (1- point)
                   point
                   :error
                   message)
                  result)))
        (setq i (1+ i)))
      result)))

(defun flymake-gradle--row-from-row-column (row-column)
  "Return row given ROW-COLUMN of the format \" (10, 46)\""
  (string-to-number
   (nth 0 (split-string (string-trim row-column " (" ")") ", "))))

(defun flymake-gradle--col-from-row-column (row-column)
  "Return column given ROW-COLUMN of the format \" (10, 46)\""
  (string-to-number
   (nth 1 (split-string (string-trim row-column " (" ")") ", "))))

(defun flymake-gradle--find-point (source-buffer line column)
  "Return point given LINE and COLUMN in SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (point))))

(defun flymake-gradle--set-log-level-auto ()
  "Automatically set the log level for gradle depending on gradle version."
  (let ((buffer (current-buffer)))
    (when-let* ((gradlew-path (flymake-gradle--find-gradlew-executable)))
      (flymake-gradle--async-shell-command-to-string
       (format "%s -v" gradlew-path)
       (lambda (result)
         (let ((major-version (string-to-number
                               (substring (caddr (split-string result)) 0 1))))
           (with-current-buffer buffer
             (if (>= major-version 3)
                 (setq-local flymake-gradle-log-level "-warn")
               (setq-local flymake-gradle-log-level "-quiet")))))))))

(defun flymake-gradle--async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.
Return the temporary output buffer which command is writing to
during execution.
When the command is finished, call CALLBACK with the resulting
output as a string."
  (let ((output-buffer (generate-new-buffer " *temp*")))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun flymake-gradle--has-error-p ()
  "Return whether or not `flymake' is currently reporting errors."
  (> (length (flymake-diagnostics)) 0))

(defun flymake-gradle--root-directory ()
  "Return root directory containing gradle related project files.

Return nil if project isn't a gradle project."
  (or
   (locate-dominating-file buffer-file-name "gradlew")
   (locate-dominating-file buffer-file-name "settings.gradle")
   (locate-dominating-file buffer-file-name "build.gradle")))

(defun flymake-gradle--find-gradlew-executable ()
  "Return path containing gradlew, if it exists."
  (when-let* ((path (locate-dominating-file buffer-file-name "gradlew")))
    (expand-file-name
     (concat path "gradlew"))))

(defun flymake-gradle--executable ()
  "Return which gradle executable to use."
  (if-let* ((gradlew-path (flymake-gradle--find-gradlew-executable)))
      gradlew-path
    flymake-gradle-executable))

;; Compile Target Functions

(defun flymake-gradle-get-compile-commands ()
  "Return compile command given active modes."
  (cond
   ((eq major-mode 'kotlin-mode)
    (flymake-gradle-kotlin-compile->compile))
   ((bound-and-true-p android-mode)
    (flymake-gradle-java-compile->android))
   (:default
    (flymake-gradle-compile->build))))

(defun flymake-gradle-compile->build ()
  "Target gradle build."
  (if (flymake-gradle--has-error-p)
      '("build")
    '("clean" "build")))

(defun flymake-gradle-kotlin-compile->compile ()
  "Target gradle compile for kotlin."
  (let ((cmd (if (and
                  buffer-file-name
                  (string-match-p "test" buffer-file-name))
                 "compileDebugUnitTestKotlin"
               "compileReleaseKotlin")))
    (if (flymake-gradle--has-error-p)
        `(,cmd)
      `("clean" ,cmd))))

(defun flymake-gradle-java-compile->android ()
  "Target gradle compile for android java."
  (let ((cmd
         (cond
          ((and buffer-file-name
                (string-match-p "androidTest" buffer-file-name))
           "compileDebugAndroidTestSources")
          ((and buffer-file-name
                (string-match-p "test" buffer-file-name))
           "compileDebugUnitTestSources")
          (:default
           "compileDebugSources"))))
    (if (flymake-gradle--has-error-p)
        `(,cmd)
      `("clean" ,cmd))))

(provide 'flymake-gradle)
;;; flymake-gradle.el ends here
