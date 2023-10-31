;;; fle-mode.el --- edit FLE amatuer radio logging files -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Stephen Fromm

;; Author: Stephen Fromm
;; URL: https://github.com/sfromm/fle-mode
;; Package-Requires: ((emacs "24.1"))
;; Keywords: fle
;; Version: 0.1

;; This program is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;;
;;; Commentary:
;; See https://df3cb.com/fle/documentation/ for FLE Format
;;
;; Emacs Lisp pointers:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Rx-Constructs.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html

;;; Code:

(require 'rx)

(defgroup fle-mode nil
  "FLE Mode.
Mode for editing FLE (fast-log-entry) amatuer radio logging files."
  :group 'editing)

(defcustom fle-mode-hook nil
  "Hook called by `fle-mode'."
  :group 'fle-mode
  :type 'hook)

(defcustom fle-mycall nil
  "My callsign for `fle-mode'."
  :group 'fle-mode
  :type 'string)


;; Constants

(defconst fle-mode-version "0.1" "Version of `fle-mode'.")

(defconst fle-date-regex
  (rx bol (0+ space) "date" space (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit))
  "Regular expression for FLE date format.")

(defconst fle-hour-min-regex
  (rx (or bol space) (or (repeat 4 digit) (repeat 2 digit)) space)
  "Regular expression for FLE hour and minute format.")

;; A collection of regexs to match callsigns
;;   https://regex101.com/library/6QhGuD
;;   https://regex101.com/library/uP6xD2
(defconst fle-callsign-regex
  (rx
   (0+ alnum "/")
   (repeat 1 3 alpha) (1+ digit) (repeat 0 3 alpha)
   (0+ "/" alnum))
  "Regular expression for a callsign.")

(defconst fle-operator-regex
  (rx "@" (1+ alnum))
  "Regular expression for operator name.")

(defconst fle-frequency-regex
  (rx (repeat 1 3 digit) "." (repeat 3 digit))
  "Regular expression for operating frequency.")

;; example #JN49
(defconst fle-gridlocator-regex
  (rx "#" (group (1+ alnum)))
  "Regular expression for grid locator.")

;; example w6/ct-226
(defconst fle-sota-regex
  (rx (repeat 1 3 alnum) "/" (repeat 2 alnum) "-" (repeat 3 digit))
  "Regular expression for SOTA summit ID.")

;; example DA-1234
(defconst fle-pota-regex
  (rx (repeat 1 3 alnum) "-" (repeat 4 digit))
  "Regular expression for POTA park ID.")

(defconst fle-remark-regex
  (rx "<" (0+ space) (1+ print) (0+ space) ">")
  "Regular expression for remark/comment syntax.")

(defconst fle-comment-regex
  (rx "{" (0+ space) (1+ print) (0+ space) "}")
  "Regular expression for alt comment syntax.")

(defconst fle-supported-bands
  (regexp-opt
   '("2190m" "630m" "560m" "160m" "80m" "60m" "40m" "30m" "20m" "17m" "15m"
     "12m" "10m" "6m" "4m" "2m" "1.25m" "70cm" "33cm" "23cm" "13cm" "9cm"
     "6cm" "3cm" "1.25cm" "6mm" "4mm" "25mm" "2mm" "1mm") 'words)
  "Regular expressions for supported bands in FLE.")

(defconst fle-supported-modes
  (regexp-opt
   '("cw" "ssb" "AM" "FM" "RTTY" "FT8" "PSK" "JT65" "JT9" "FT4" "JS8" "ARDOP"
     "ATV" "C4FM" "CHIP" "CLO" "CONTESTI" "DIGITALVOICE" "DOMINO" "DSTAR" "FAX"
     "FSK441" "HELL" "ISCAT" "JT4" "JT6M" "JT44" "MFSK" "MSK144" "MT63" "OLIVIA"
     "OPERA" "PAC" "PAX" "PKT" "PSK2K" "Q15" "QRA64" "ROS" "RTTYM" "SSTV" "T10"
     "THOR" "THRB" "TOR" "V4" "VOI" "WINMOR" "WSPR") 'words)
  "Regular expressions for supported modes in FLE.")

(defconst fle-supported-headers
  (regexp-opt
   '("mycall" "mygrid" "operator" "qslmsg" "syn" "keyword"
     "fle_header" "mywwff" "mysota" "mypota" "nickname") 'words)
  "Regular expressions for FLE headers.")


;; For font-lock faces, see
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
(defvar fle-font-lock-keywords
  (list
   (list fle-date-regex 0 font-lock-string-face)
   (list fle-hour-min-regex 0 font-lock-string-face)
   (list fle-frequency-regex 0 font-lock-builtin-face)
   (list fle-supported-bands 0 font-lock-type-face)
   (list fle-supported-modes 0 font-lock-type-face)
   (list fle-supported-headers 0 font-lock-constant-face)
   (list fle-callsign-regex 0 font-lock-keyword-face)
   (list fle-operator-regex 0 font-lock-string-face)
   (list fle-gridlocator-regex 0 font-lock-builtin-face)
   (list fle-remark-regex 0 font-lock-string-face)
   (list fle-comment-regex 0 font-lock-comment-face)
   (list fle-sota-regex 0 font-lock-builtin-face)
   (list fle-pota-regex 0 font-lock-builtin-face)
   )
  "Font locking definitions for FLE mode.")


;; Mode setup

(defvar fle-imenu-expression
  `(
    ("Date"    "^date *\\(.*\\)" 1)
    ("Band"    "^\\([0-9]+[cm]?m\\)" 1)
    )
  "Imenu configuration for `fle-mode'.")

(defvar fle-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)  ;; All !'s start comments.
    (modify-syntax-entry ?\n ">" st) ;; All newlines end comments.
    (modify-syntax-entry ?\r ">" st) ;; All linefeeds end comments.
    st)
  "Syntax table for Arista `fle-mode'.")

(define-derived-mode fle-mode text-mode "FLE"
  "Major mode for Fast-Log-Entry (FLE) configuration files."
  :syntax-table fle-mode-syntax-table
  :group 'fle-mode
  (set (make-local-variable 'font-lock-defaults) '(fle-font-lock-keywords))
  (set (make-local-variable 'comment-start) "#")
  ;;  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)!+ *")
  (set (make-local-variable 'indent-tabs-mode) nil)
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression) fle-imenu-expression)
  (imenu-add-to-menubar "Imenu"))

(defun fle-mode-version ()
  "Display version of `fle-mode'."
  (interactive)
  (message "fle-mode %s" fle-mode-version)
  fle-mode-version)

(provide 'fle-mode)

;;; fle-mode.el ends here
