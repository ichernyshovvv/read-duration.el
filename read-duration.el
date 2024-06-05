;;; read-duration.el --- Time duration reader -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1"))
;; Keywords: convenience, minibuffer
;; URL: https://github.com/ichernyshovvv/read-duration

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a function that reads a human-readable time duration
;; from a user and returns the value in seconds, minutes, hours or any other
;; time unit defined in the `read-duration-return-units` custom variable.  The
;; function shows available input characters (available time unit multipliers
;; (see `read-duration-multipliers`) and digits) in the prompt and checks the
;; input while the user is typing.  If the user mistypes, the function calls the
;; `ding' function.

;; USAGE:

;; (read-duration "Duration:")

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'map)

(defface read-duration-shadow '((t (:inherit shadow)))
  "Face used to mark unavailable input."
  :group 'read-duration)

(defcustom read-duration-multipliers
  '((?w . 604800)
    (?d . 86400)
    (?h . 3600)
    (?m . 60))
  "An alist of unit multipliers used in `read-duration'.

Each entry is a pair (UNIT . MULTIPLIER), where
UNIT is a time unit character
MULTIPLIER is a number

In a duration input, a number followed by UNIT is multiplied by
the associated MULTIPLIER to calculate a duration in seconds."
  :group 'read-duration
  :type '(alist :key-type (character :tag "Unit")
                :value-type (number :tag "Multiplier")))

(defvar read-duration--smallest
  (caar (seq-sort (lambda (x y) (< (cdr x) (cdr y)))
                  read-duration-multipliers))
  "The unit with the smallest multiplier.")

(defun read-duration-typed-prompt (acc)
  "Generate prompt with already typed values from from ACC."
  (mapconcat
   (lambda (x)
     (concat (cdr x)
             (if (eq 'current (car x))
                 ""
               (char-to-string (car x)))))
   (seq-sort (lambda (x y)
               ;; always make 'current the last
               (let* ((units (append (map-keys read-duration-multipliers)
                                     '(current)))
                      (x-pos (seq-position units (car x)))
                      (y-pos (seq-position units (car y))))
                 (< x-pos y-pos)))
             acc)
   ""))

(defun read-duration--rename-key (m old-key new-key)
  "Rename OLD-KEY to NEW-KEY in alist M."
  (map-apply (lambda (k v)
               (cons (if (eq old-key k)
                         new-key
                       k)
                     v))
             m))

(defun read-duration--read-internal (prompt acc active-units)
  "Read a time value in the minibuffer, prompting with PROMPT.
The result is collected in ACC, ACTIVE-UNITS holds currently active units."
  (if active-units
      (let* ((current (map-elt acc 'current))
             (unit-chars (map-keys active-units))
             (unit-prompt (propertize
                           (concat "[" unit-chars "]")
                           'face
                           (unless current 'read-duration-shadow)))
             (typed (read-duration-typed-prompt acc))
             (prompt* (concat (propertize prompt 'face 'minibuffer-prompt)
                              " ([0-9]+" unit-prompt "):"
                              typed))
             (choices (append (and current unit-chars)
                              (number-sequence ?0 ?9)
                              (list ?\C-m)))
             (ch (read-char-choice prompt* choices)))
        (cond ((= ?\C-m ch) acc)
              ((alist-get ch active-units)
               (let ((acc (read-duration--rename-key acc 'current ch))
                     (new-units (map-delete active-units ch)))
                 (if (= read-duration--smallest ch)
                     acc
                   (read-duration--read-internal prompt acc new-units))))
              (t (let ((acc (map-merge-with 'alist #'append
                                            acc `((current . (,ch))))))
                   (read-duration--read-internal prompt acc active-units)))))
    acc))

;;;###autoload
(defun read-duration (prompt &optional default)
  "Read time duration and return seconds as an integer.

PROMPT is a string to prompt with.  DEFAULT specifies a default value to return
if the user just types RET.  The prompt shows currently valid characters for
the next input character.

Valid duration formats:
2h
2h30m
2h30
45
1d
1w3h30m"
  (if-let* ((duration (read-duration--read-internal
                       prompt
                       '()
                       (copy-sequence read-duration-multipliers)))
            (normalized (map-apply
                         (lambda (k vs)
                           (let ((mult (map-elt read-duration-multipliers k))
                                 (value (string-to-number (concat vs))))
                             (* value mult)))
                         duration)))
      (seq-reduce #'+ normalized 0)
    default))

(provide 'read-duration)

;;; read-duration.el ends here
