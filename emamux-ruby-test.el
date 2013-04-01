;;; emamux-ruby-test.el --- Ruby test with emamux

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emamux-ruby-test

;; This program is free software; you can redistribute it and/or modify
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

;; emamux-ruby-test makes you test ruby file with emamux.
;; This package is inspired by vimux-ruby-test.
;;
;; To use emamux-ruby-test, add the following code into your init.el or .emacs:
;;
;;    (require 'emamux-ruby-test)
;;
;; emamux-ruby-test provides following commands:
;;
;; Run all tests/specs in the current file
;;     M-x emamux-ruby-test:run-all
;;
;; Run focused test/spec
;;     M-x emamux-ruby-test:run-focused-test
;;
;; Run current context(rspec, shoulda)
;;     M-x emamux-ruby-test:run-focused-context
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'emamux)

(defun emamux-rt:spec-p (file)
  (string-match "\\(spec_\\|_spec\\)" file))

(defun emamux-rt:top-directory-p (dir)
  (let ((default-directory dir))
    (loop for file in '("Rakefile")
          when (file-exists-p file)
          return t)))

(defun emamux-rt:parent-dir (dir)
  (file-name-directory (replace-regexp-in-string "/$" "" dir)))

(defun emamux-rt:find-top-dir ()
  (let ((dir (file-name-directory (buffer-file-name))))
    (while (and (not (string= dir "/"))
                (not (emamux-rt:top-directory-p dir)))
      (setq dir (emamux-rt:parent-dir dir)))
    (if (string= dir "/")
        (error "Are you in Ruby module directory ?"))
    (file-name-as-directory dir)))

(defun emamux-rt:spec-command ()
  (let ((default-directory (emamux-rt:find-top-dir))
        (version (emamux-rt:bundle-rspec-version)))
    (cond ((file-exists-p "Gemfile")
           (if (and version (< version 2))
               "bundle exec spec"
             "bundle exec rspec"))
          ((executable-find "rspec") "rspec")
          ((executable-find "spec")  "spec")
          (t (error "Please install RSpec")))))

(defun emamux-rt:bundle-rspec-version ()
  (let ((version (shell-command-to-string "bundle show rspec")))
    (if (string-match "\\([0-9]+\.[0-9]+\\)\.[0-9]+$" version)
        (string-to-number (match-string 1 version)))))

(defun emamux-rt:run-spec (file &optional test-line)
  (let ((cmd (emamux-rt:spec-command))
        (line (or test-line (line-number-at-pos (point)))))
   (emamux:run-command (format "%s %s -l %d" cmd file line))))

(defun emamux-rt:test-in-def ()
  (save-excursion
    (if (re-search-backward "def \\(test_[a-zA-Z0-9]+\\)" nil t)
        (match-string-no-properties 1))))

(defun emamux-rt:test-in-test ()
  (save-excursion
    (if (re-search-backward "test +\"\\([^\"]+\\)\"" nil t)
        (match-string-no-properties 1))))

(defun emamux-rt:test-in-def ()
  (save-excursion
    (if (re-search-backward "should +\"\\([^\"]+\\)\"" nil t)
        (match-string-no-properties 1))))

(defun emamux-rt:search-current-method ()
  (or (emamux-rt:test-in-def)
      (emamux-rt:test-in-test)
      (emamux-rt:test-in-should)
      (error "Are you in method or test/should block")))

(defun emamux-rt:test-command (file)
  (if (emamux-rt:spec-p file)
      (emamux-rt:spec-command)
    "ruby"))

(defun emamux-rt:run-unit-test (file method)
  (emamux:run-command (format "ruby %s -n %s" file method)))

(defun emamux-rt:quoted-context ()
  (let ((re "\\(context\\|describe\\) +\\(\"\\|'\\)\\([^\\2]+?\\)\\2"))
    (save-excursion
      (if (re-search-backward re nil t)
          (cons (match-string-no-properties 3)
                (line-number-at-pos (match-beginning 1)))))))

(defun emamux-rt:noquoted-context ()
  (let ((re "\\(context\\|describe\\) +\\([^ ]+\\)"))
    (save-excursion
      (if (re-search-backward re nil t)
          (cons (match-string-no-properties 2)
                (line-number-at-pos (match-beginning 1)))))))

(defun emamux-rt:search-current-context ()
  (or (emamux-rt:quoted-context)
      (emamux-rt:noquoted-context)))

(defun emamux-rt:run-context (cmd file line)
  (emamux:run-command (format "%s %s -l %s" cmd file line)))

;;;###autoload
(defun emamux-ruby-test:run-all ()
  (interactive)
  (let* ((file (buffer-file-name))
         (cmd  (emamux-rt:test-command file)))
    (emamux:run-command (format "%s %s" cmd file))))

;;;###autoload
(defun emamux-ruby-test:run-focused-test ()
  (interactive)
  (let ((file (buffer-file-name)))
    (if (emamux-rt:spec-p file)
        (emamux-rt:run-spec file)
      (emamux-rt:run-unit-test file (emamux-rt:search-current-method)))))

;;;###autoload
(defun emamux-ruby-test:run-focused-context ()
  (interactive)
  (let ((ret (emamux-rt:search-current-context))
        (file (buffer-file-name)))
    (unless ret
      (error "Failed searching current context"))
    (let ((method (car ret))
          (line (cdr ret)))
      (if (emamux-rt:spec-p file)
          (emamux-rt:run-spec file line)
        (emamux-rt:run-unit-test file method)))))

(provide 'emamux-ruby-test)

;;; emamux-ruby-test.el ends here
