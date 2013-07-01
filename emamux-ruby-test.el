;;; emamux-ruby-test.el --- Ruby test with emamux

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emamux-ruby-test
;; Package-Requires: ((emamux "0.01") (projectile "0.9.1"))

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
;; Run all tests/specs in the current project
;;     M-x emamux-ruby-test:run-all
;;
;; Run all tests/specs in the current file
;;     M-x emamux-ruby-test:run-current-test
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'emamux)
(require 'projectile)

(defun emamux-rt:relative-file-name (file)
  "Return relative path name for FILE."
  (substring file (length (projectile-project-root))))

(defun emamux-rt:relative-test-name (file)
  "Return relative test name for FILE."
  (if (projectile-test-file-p file)
      (emamux-rt:relative-file-name file)
    (projectile-find-matching-test file)))

(defun emamux-rt:run-test-engine (test &optional definition)
  "Run TEST with current test engine."
  (let* ((project-type (projectile-project-type))
         (dir (projectile-project-root))
         (cmd (cond
               ((member project-type '(rails-rspec ruby-rspec))
                (format "%s %s" projectile-ruby-rspec-cmd test))
               ((member project-type '(rails-test ruby-test))
                (format "%s TEST=%s" projectile-ruby-test-cmd test))
               (t (error "No test engine found")))))
    (emamux:run-command cmd dir)))

;;;###autoload
(defun emamux-ruby-test:run-all ()
  "Run all tests/specs in the current project."
  (interactive)
  (let* ((dir (projectile-project-root))
         (cmd (projectile-test-command dir)))
    (emamux:run-command cmd dir)))

;;;###autoload
(defun emamux-ruby-test:run-current-test ()
  "Run all tests/specs in the current file."
  (interactive)
  (emamux-rt:run-test-engine
   (emamux-rt:relative-test-name
    (buffer-file-name))))

(provide 'emamux-ruby-test)

;;; emamux-ruby-test.el ends here
