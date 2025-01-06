;;; fle-mode.el --- edit FLE amatuer radio logging files -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024 Stephen Fromm

;; Author: Stephen Fromm
;; URL: https://github.com/sfromm/fle-mode
;; Package-Requires: ((emacs "24.1"))
;; Keywords: fle
;; Version: 0.5

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

;; How to install with package-vc-install
;; (package-vc-install "https://github.com/sfromm/fle-mode")
;; (progn (unload-feature 'fle-mode) (eval-buffer))

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

(defgroup fle-faces nil
  "Faces for highlighting text in FLE and related modes."
  :prefix "fle-"
  :group 'fle-mode)



;;; Faces
(defface fle-callsign-face '((t (:inherit font-lock-keyword-face)))
  "Face for callsigns in FLE and related modes."
  :group 'fle-faces)

(defface fle-op-frequency-face '((t (:inherit font-lock-builtin-face)))
  "Face for operating frequency in FLE and related modes."
  :group 'fle-faces)

(defface fle-op-band-face '((t (:inherit font-lock-type-face)))
  "Face for operating band in FLE and related modes."
  :group 'fle-faces)

(defface fle-op-mode-face '((t (:inherit font-lock-type-face)))
  "Face for operating mode in FLE and related modes."
  :group 'fle-faces)

(defface fle-gridlocator-face '((t (:inherit (bold font-lock-type-face))))
  "Face for grid location in FLE and related modes."
  :group 'fle-faces)

(defface fle-pota-reference-face '((t (:inherit font-lock-builtin-face)))
  "Face for POTA park reference in FLE and related modes."
  :group 'fle-faces)

(defface fle-sota-reference-face '((t (:inherit font-lock-builtin-face)))
  "Face for SOTA reference in FLE and related modes."
  :group 'fle-faces)

(defface fle-keyword-face '((t (:inherit font-lock-constant-face)))
  "Face for keywords FLE and related modes."
  :group 'fle-faces)

(defface fle-string-face '((t (:inherit font-lock-string-face)))
  "Face for strings in FLE and related modes."
  :group 'fle-faces)

(defface fle-comment-face '((t (:inherit font-lock-comment-face)))
  "Face for commented text in FLE and related modes."
  :group 'fle-faces)


;;; Constants

(defconst fle-mode-version "0.5" "Version of `fle-mode'.")

(defconst fle-supported-bands
  '("2190m" "630m" "560m" "160m" "80m" "60m" "40m" "30m" "20m" "17m" "15m"
    "12m" "10m" "6m" "4m" "2m" "1.25m" "70cm" "33cm" "23cm" "13cm" "9cm"
    "6cm" "3cm" "1.25cm" "6mm" "4mm" "2.5mm" "2mm" "1mm")
  "Supported bands in FLE.")

(defconst fle-supported-modes
  '("CW" "SSB" "AM" "FM" "RTTY" "FT8" "PSK" "JT65" "JT9" "FT4" "JS8" "ARDOP"
    "ATV" "C4FM" "CHIP" "CLO" "CONTESTI" "DIGITALVOICE" "DOMINO" "DSTAR" "FAX"
    "FSK441" "HELL" "ISCAT" "JT4" "JT6M" "JT44" "MFSK" "MSK144" "MT63" "OLIVIA"
    "OPERA" "PAC" "PAX" "PKT" "PSK2K" "Q15" "QRA64" "ROS" "RTTYM" "SSTV" "T10"
    "THOR" "THRB" "TOR" "V4" "VOI" "WINMOR" "WSPR")
  "Supported modes in FLE.")

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
  (rx (zero-or-one "#")  (repeat 2 alpha) (repeat 2 digit) (repeat 0 2 alnum))
  "Regular expression for grid locator.")

;; example w6/ct-226
(defconst fle-sota-regex
  (rx (repeat 1 3 alnum) "/" (repeat 2 alnum) "-" (repeat 3 digit))
  "Regular expression for SOTA summit ID.")

;; example DA-1234
(defconst fle-pota-regex
  (rx (repeat 1 3 alnum) "-" (repeat 4 5 digit))
  "Regular expression for POTA park ID.")

(defconst fle-qso-comment-regex
  (rx "<" (0+ space) (1+ print) (0+ space) ">")
  "Regular expression for QSO comment syntax.")

(defconst fle-qso-remark-regex
  (rx "{" (0+ space) (1+ print) (0+ space) "}")
  "Regular expression for QSO remark syntax.")

(defconst fle-comment-regex
  (rx bol "#" (0+ print))
  "Regular expression for syntax commenting a line.")

(defconst fle-supported-bands-regex
  (regexp-opt fle-supported-bands 'words)
  "Regular expressions for supported bands in FLE.")

(defconst fle-supported-modes-regex
  (regexp-opt fle-supported-modes 'words)
  "Regular expressions for supported modes in FLE.")

(defconst fle-supported-keywords-regex
  (regexp-opt
   '("mycall" "mygrid" "operator" "qslmsg" "nickname"
     "mywwff" "wwff" "mysota" "sota" "mypota" "pota" ) 'words)
  "Regular expressions for FLE headers and keywords.")

(defvar fle-callsign-face 'fle-callsign-face
  "Face for callsigns in FLE and related modes.")

(defvar fle-op-frequency-face 'fle-op-frequency-face
  "Face for operating frequency in FLE and related modes.")

(defvar fle-op-band-face 'fle-op-band-face
  "Face for operating band in FLE and related modes.")

(defvar fle-op-mode-face 'fle-op-mode-face
  "Face for operating mode in FLE and related modes.")

(defvar fle-gridlocator-face 'fle-gridlocator-face
  "Face for grid location in FLE and related modes.")

(defvar fle-pota-reference-face 'fle-pota-reference-face
  "Face for POTA park reference in FLE and related modes.")

(defvar fle-sota-reference-face 'fle-sota-reference-face
  "Face for SOTA reference in FLE and related modes.")

(defvar fle-keyword-face 'fle-keyword-face
  "Face for keywords FLE and related modes.")

(defvar fle-string-face 'fle-string-face
  "Face for strings in FLE and related modes.")

(defvar fle-comment-face 'fle-comment-face
  "Face for commented text in FLE and related modes.")

;; For font-lock faces, see
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
(defvar fle-font-lock-keywords
  (list
   (list fle-comment-regex 0 fle-comment-face)
   (list fle-date-regex 0 font-lock-string-face)
   (list fle-hour-min-regex 0 font-lock-string-face)
   (list fle-frequency-regex 0 fle-op-frequency-face)
   (list fle-supported-bands-regex 0 fle-op-band-face)
   (list fle-supported-modes-regex 0 fle-op-mode-face)
   (list fle-supported-keywords-regex 0 fle-keyword-face)
   (list fle-gridlocator-regex 0 fle-gridlocator-face)
   (list fle-callsign-regex 0 fle-callsign-face)
   (list fle-operator-regex 0 font-lock-string-face)
   (list fle-qso-comment-regex 0 fle-string-face)
   (list fle-qso-remark-regex 0 fle-comment-face)
   (list fle-sota-regex 0 fle-sota-reference-face)
   (list fle-pota-regex 0 fle-pota-reference-face)
   )
  "Font locking definitions for FLE mode.")

(defvar fle-messages
  "*FLE-Messages*"
  "Buffer for FLE and FLEcli messages.")

(defvar fle-qrz-query-url
  "https://www.qrz.com/db/"
  "URL to query QRZ for a call-sign.")

(defvar fle-pota-park-url
  "https://pota.app/#/park/"
  "URL to query QRZ for a call-sign.")


;;; Mode commands
(defun fle-find-mycall ()
  "Find MYCALL and save in buffer variable."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^mycall " nil t)
        (setq fle-mycall (thing-at-point 'symbol 'no-properties)))))

(defun fle-find-mygrid ()
  "Find Gridsquare identifier and save in buffer variable."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^mygrid " nil t)
        (setq fle-mygrid (thing-at-point 'symbol 'no-properties)))))

(defun fle-find-mypota ()
  "Find POTA park identifier and save in buffer variable."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^mypota " nil t)
        (setq fle-mypota (thing-at-point 'symbol 'no-properties)))))

(defun fle-init-log ()
  "Initialize log with standard boilerplate."
  (interactive)
  (insert "mycall " fle-mycall "\n")
  (insert "operator " fle-mycall "\n")
  (insert "# mygrid\n")
  (insert "# mypota\n")
  (fle-insert-date) (insert " ") (fle-insert-time)
  (insert "\n"))

(defun fle-insert-date ()
  "Insert today's date in expected format."
  (interactive)
  (insert "date " (format-time-string "%Y-%m-%d" (current-time) t)))

(defun fle-insert-time ()
  "Insert current time in expected format."
  (interactive)
  (insert (format-time-string "%H%M" (current-time) t)))

(defun fle-insert-mode ()
  "Insert operating mode."
  (interactive)
  (insert (completing-read "Mode: " fle-supported-modes)))

(defun fle-insert-band ()
  "Insert operating band."
  (interactive)
  (insert (completing-read "Band: " fle-supported-bands)))

(defun fle-insert-mygrid ()
  "Insert grid location identifier if not already set."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (rx bol (0+ "# ") "mygrid") nil t)
      (beginning-of-line)
      (kill-line))
    (setq fle-mygrid (upcase (read-string "Gridsquare: ")))
    (insert "mygrid " fle-mygrid "\n")))

(defun fle-insert-mypota ()
  "Insert POTA Park identifier if not already set."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^mypota " nil t)
      (beginning-of-line)
      (kill-line))
    (setq fle-mypota (read-string "POTA: "))
    (insert "mypota " fle-mypota "\n")))

(defun fle-qrz-query-call ()
  "Query QRZ for call-sign at point."
  (interactive)
  (browse-url (concat fle-qrz-query-url (thing-at-point 'word 'no-properties))))

(defun fle-pota-query-park ()
  "Query POTA for park information."
  (interactive)
  (browse-url (concat fle-pota-park-url (thing-at-point 'symbol 'no-properties))))

(defun fle-qso-prompt (call grid)
  "Prompt for QSO details."
  (interactive
   (list
    (read-string "Call: ")
    (read-string "Grid: ")))
  (unless (string= "" grid) (setq grid (concat " #" grid)))
  (beginning-of-line)
  (fle-insert-time)
  (insert
   (concat " " (upcase call) (upcase grid) "\n")))

(defun fle-comment-pota-logfile-name ()
  "Insert comment with POTA log file name."
  (interactive)
  (insert
   (concat "# "
           fle-mycall
           "@"
           fle-mypota
           "_"
           (format-time-string "%Y%m%d" (current-time) t)
           "\n")))

(defun fle-get-log-path ()
  "Return path of FLE log, whether an Org Src block or actual file."
  (expand-file-name
   (if (org-in-src-block-p)
       (cdr (assq :tangle (caddr (org-babel-get-src-block-info))))
     (buffer-file-name))))


;;; Integration with FLEcli

(defun fle-flecli-run-command (cmd)
  "Run FLEcli command CMD."
  (shell-command cmd fle-messages))

(defun fle-flecli-load ()
  "Load and validate a FLE log with FLEcli."
  (interactive)
  (unless (executable-find "FLEcli")
    (error "Missing FLEcli."))
  (let* ((path (fle-get-log-path))
         (default-directory (file-name-directory path))
         (flecli (executable-find "FLEcli")))
    (fle-flecli-run-command (concat flecli " load " path))))

(defun fle-flecli-gen-adif ()
  "Invoke flecli on FLE log to generate an ADIF log."
  (interactive)
  (unless (executable-find "FLEcli")
    (error "Missing FLEcli."))
  (let* ((path (fle-get-log-path))
         (default-directory (file-name-directory path))
         (flecli (executable-find "FLEcli")))
    (fle-flecli-run-command (concat flecli " adif -i --overwrite " path))))

(defun fle-flecli-gen-adif-pota ()
  "Invoke flecli on FLE log to generate an ADIF log for POTA."
  (interactive)
  (unless (executable-find "FLEcli")
    (error "Missing FLEcli."))
  (let* ((path (fle-get-log-path))
         (default-directory (file-name-directory path))
         (flecli (executable-find "FLEcli")))
    (fle-flecli-run-command (concat flecli " adif -i --overwrite --pota " path))))


;; Mode setup

(defvar fle-imenu-expression
  `(
    ("Date"    "^date *\\(.*\\)" 1)
    ("Band"    "^\\([0-9]+[cm]?m\\)" 1)
    )
  "Imenu configuration for `fle-mode'.")

(defvar fle-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; (modify-syntax-entry ?# "<" st)  ;; All #'s start comments.
    (modify-syntax-entry ?\n ">" st) ;; All newlines end comments.
    (modify-syntax-entry ?\r ">" st) ;; All linefeeds end comments.
    st)
  "Syntax table for Arista `fle-mode'.")

(defvar fle-mode-pota-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'fle-insert-mypota)
    (define-key map (kbd "n") 'fle-comment-pota-logfile-name)
    map)
  "Submap for POTA-related commands")
(fset 'fle-mode-pota-map fle-mode-pota-map)

(defvar fle-mode-qso-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'fle-qso-prompt)
    (define-key map (kbd "z") 'fle-qrz-query-call)
    map)
  "Submap for QSO-related commands")
(fset 'fle-mode-qso-map fle-mode-qso-map)

(defvar fle-mode-map
  (let ((map (make-sparse-keymap)))
    ;; these bindings are a work-in-progress to see what feels right.
    (define-key map (kbd "C-c C-f d") 'fle-insert-date)
    (define-key map (kbd "C-c C-f t") 'fle-insert-time)
    (define-key map (kbd "C-c C-f b") 'fle-insert-band)
    (define-key map (kbd "C-c C-f m") 'fle-insert-mode)
    (define-key map (kbd "C-c C-f q") 'fle-mode-qso-map)
    (define-key map (kbd "C-c C-f p") 'fle-mode-pota-map)
    map)
  "Keymap for fle-mode.")

(define-derived-mode fle-mode text-mode "FLE"
  "Major mode for Fast-Log-Entry (FLE) configuration files."
  :syntax-table fle-mode-syntax-table
  :group 'fle-mode
  (use-local-map fle-mode-map)
  (hl-line-mode)
  (display-line-numbers-mode)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'font-lock-defaults) '(fle-font-lock-keywords))
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'fle-mygrid) nil)
  (set (make-local-variable 'fle-mypota) nil)
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression) fle-imenu-expression)
  (fle-find-mycall)
  (fle-find-mygrid)
  (fle-find-mypota)
  (imenu-add-to-menubar "Imenu"))

(defun fle-mode-version ()
  "Display version of `fle-mode'."
  (interactive)
  (message "fle-mode %s" fle-mode-version)
  fle-mode-version)

(provide 'fle-mode)

;;; fle-mode.el ends here
