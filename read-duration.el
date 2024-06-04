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
;; input while the user types.  If the user mistypes, the function calls the
;; `ding' function.

;; USAGE:

;; (read-duration "Duration:")

;;; Code:

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

(defcustom read-duration-return-units
  '((weeks . 604800)
    (days . 86400)
    (hours . 3600)
    (minutes . 60))
  "An alist of units used to divide the result value of `read-duration'.

Each entry is a pair (UNIT . DIVISOR), where
UNIT is a time unit symbol
DIVISOR is a number."
  :group 'read-duration
  :type '(alist :key-type (character :tag "Unit")
                :value-type (number :tag "Divisor")))

;;;###autoload
(defun read-duration (&optional prompt divisor)
  "Read time duration and return seconds as an integer.

PROMPT is a string to prompt with.  DIVISOR is one of the units
from `read-duration-return-units' that controls the return value.

Beep or flash the screen when an invalid character is typed.  The
prompt shows currently valid characters for the next input
character.

Valid duration formats:
2h
2h30m
2h30
45
1d
1w3h30m"
  (let* ((divisor
          (if divisor
              (or (alist-get divisor read-duration-return-units)
                  (user-error "Unknown time unit"))
            1))
         (input "")
         (all-multipliers (mapcar #'car read-duration-multipliers))
         (smallest
          (caar (seq-sort (lambda (x y) (< (cdr x) (cdr y)))
                          read-duration-multipliers)))
         (valid-multipliers all-multipliers)
         typed-multipliers
         (seconds
          (catch 'return-value
            (while-let ((multipliers
                         (propertize
                          (concat "[" valid-multipliers "]")
                          'face
                          (and (or (length= input 0)
                                   (member (string-to-char (substring input -1))
                                           all-multipliers))
                               'read-duration-shadow)))
                        (ch (read-char-exclusive
                             (concat (or prompt "DURATION")
                                     " ([0-9]+" multipliers "):" input))))
              (cond
               ((<= ?0 ch ?9)
                (setq input (format "%s%c" input ch)))
               ((or (and (eq ch ?\C-m) (length> input 0))
                    (and (member ch valid-multipliers)
                         (string-match-p "[0-9]+$" input)))
                (when-let (((member ch `(,smallest ?\C-m)))
                           (return-value 0.0)
                           (unit (char-to-string smallest))
                           (start 0))
                  (setq input (concat input unit))
                  (while (string-match
                          (rx-to-string
                           `(: (group (+ digit))
                               (group (in ,@(cons unit typed-multipliers)))))
                          input start)
                    (cl-incf return-value
                             (* (alist-get
                                 (string-to-char (match-string 2 input))
                                 read-duration-multipliers)
                                (string-to-number (match-string 1 input))))
                    (setq start (match-end 0)))
                  (throw 'return-value return-value))
                (setq input (format "%s%c" input ch)
                      valid-multipliers (cdr (member ch valid-multipliers)))
                (push ch typed-multipliers))
               ((and (eq ?\C-? ch) (not (length= input 0)))
                (when (eq (string-to-char (substring input -1))
                          (car typed-multipliers))
                  (pop typed-multipliers)
                  (setq valid-multipliers
                        (let ((ms all-multipliers))
                          (when typed-multipliers
                            (while (not (eq (pop ms) (car typed-multipliers)))))
                          ms)))
                (setq input (substring input 0 -1)))
               (t (ding)))))))
    (/ seconds divisor)))

(provide 'read-duration)

;;; read-duration.el ends here
