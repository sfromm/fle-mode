;;; pota.el --- Mode to hunt and spot for POTA activity -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stephen Fromm

;; Author: Stephen Fromm
;; URL: https://github.com/sfromm/fle-mode
;; Package-Requires: ((emacs "24.1"))
;; Keywords: fle pota
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
;;  See https://api.pota.app/spot/activator
;;      https://pota.app/#/
;;
;; Emacs Lisp pointers:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Rx-Constructs.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html

;; How to install with package-vc-install
;; (package-vc-install "https://github.com/sfromm/fle-mode")

;;; Code:

(require 'fle-mode)
(require 'url)
(require 'json)

(defcustom pota-spot-api-url
  "https://api.pota.app/spot"
  "URL to query POTA for current activations."
  :group 'fle-mode
  :type 'string)

(defcustom pota-activator-api-url
  "https://api.pota.app/spot/activator"
  "URL to query POTA for current activations."
  :group 'fle-mode
  :type 'string)

(defcustom pota-buffer-name "*POTA Active Spots*"
  "Name of POTA Spots buffer."
  :group 'fle-mode
  :type 'string)

(defcustom pota-filter-qrt t
  "Whether to filter QRT stations."
  :group 'fle-mode
  :type 'boolean)

(defvar pota-filter-mode ""
  "Variable for what mode to filter by.")

(defvar pota-filter-band ""
  "Variable for what band to filter by.")

(defvar pota-filter-region ""
  "Variable for what region to filter by.")


;;;
(defun pota-get-activations ()
  "Query POTA API for current activations.  Returns JSON object."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read-from-string
     (with-current-buffer (url-retrieve-synchronously pota-activator-api-url)
       (goto-char (point-min-marker))
       (re-search-forward "^$")
       (prog1
           (buffer-substring-no-properties (point) (point-max))
         (kill-buffer (current-buffer)))))))

(defun pota-spot-data (reference frequency mode &optional comments)
  "Return JSON object representing a spot."
  (json-encode
   `(
     :activator ,fle-mycall
     :spotter ,fle-mycall
     :reference ,reference
     :frequency ,frequency
     :mode ,mode
     :comments ,comments)))

(defun pota--buffer ()
"Return POTA Spot buffer."
(with-current-buffer (get-buffer-create pota-buffer-name)
  (pota-mode)
  (current-buffer)))

(defun pota--filter-qrt-p (spot)
  "Check a SPOT to see if activator is QRT."
  (if pota-filter-qrt
      (string-match-p (regexp-quote "qrt")
                      (downcase (plist-get spot :comments)))
    pota-filter-qrt))

(defun pota-frequency-to-band (f)
  "Convert frequency F to a corresponding band."
  (cond
   ((and (>= f 130) (<= f 140) "2190m"))
   ((and (>= f 450) (<= f 505) "630m"))
   ((and (>= f 500) (<= f 505) "560m"))
   ((and (>= f 1700) (<= f 2100) "160m"))
   ((and (>= f 3400) (<= f 4100) "80m"))
   ((and (>= f 5300) (<= f 5500) "60m"))
   ((and (>= f 6900) (<= f 7400) "40m"))
   ((and (>= f 10000) (<= f 10200) "30m"))
   ((and (>= f 13900) (<= f 14400) "20m"))
   ((and (>= f 18000) (<= f 18200) "17m"))
   ((and (>= f 20900) (<= f 21600) "15m"))
   ((and (>= f 24500) (<= f 25200) "12m"))
   ((and (>= f 27900) (<= f 30000) "10m"))
   ((and (>= f 50000) (<= f 54000) "6m"))
   ((and (>= f 70000) (<= f 71000) "4m"))
   ((and (>= f 140000) (<= f 150000) "2m"))
   ((and (>= f 219000) (<= f 230000) "1.25m"))
   ((and (>= f 400000) (<= f 460000) "70cm"))
   ((and (>= f 900000) (<= f 930000) "33cm"))
   ((and (>= f 1180000) (<= f 1420000) "23cm"))
   ((and (>= f 2300000) (<= f 2450000) "13cm"))
   ((and (>= f 3290000) (<= f 3510000) "9cm"))
   ((and (>= f 5640000) (<= f 5926000) "6cm"))
   ((and (>= f 10000000) (<= f 10500000) "3cm"))
   ((and (>= f 24000000) (<= f 24250000) "1.25cm"))
   ((and (>= f 47000000) (<= f 47200000) "6mm"))
   ((and (>= f 75000000) (<= f 81000000) "4mm"))
   ((and (>= f 122250000) (<= f 123000000) "2.5mm"))
   ((and (>= f 134000000) (<= f 141000000) "2mm"))
   ((and (>= f 241000000) (<= f 250000000) "1mm"))))

(defun pota--filter-match-band-p (spot)
  "Filter SPOT based on band."
  (if (string= "" pota-filter-band) t
    (string-match-p
     (regexp-quote pota-filter-band)
     (pota-frequency-to-band (string-to-number (plist-get spot :frequency))))))

(defun pota--filter-match-mode-p (spot)
  "Predicate to filter SPOT based on mode."
  (if (string= "" pota-filter-mode) t
    (string-match-p (regexp-quote pota-filter-mode) (upcase (plist-get spot :mode)))))

(defun pota--filter-match-region-p (spot)
  "Predicate to filter SPOT based on region."
  (if (string= "" pota-filter-region) t
    (string-match-p (regexp-quote pota-filter-region) (upcase (plist-get spot :locationDesc)))))

(defun pota--filter-match-spot-p (spot)
  "Check if all the predicates match this SPOT."
  (and
   (pota--filter-match-mode-p spot)
   (pota--filter-match-band-p spot)
   (pota--filter-match-region-p spot)))

(defun pota--build-current-regions (spots)
  "Build list of current regions."
  (dolist (spot spots)
    (let ((region (plist-get spot :locationDesc)))
      (unless (member region pota-current-regions)
        (push region pota-current-regions)))))


;;;
(defun pota-filter-by-band ()
  "Filter POTA spots by band."
  (interactive)
  (setq pota-filter-band (completing-read "Band: " fle-supported-bands))
  (pota-spots-refresh))

(defun pota-filter-by-mode ()
  "Filter POTA spots by mode."
  (interactive)
  (setq pota-filter-mode (completing-read "Mode: " fle-supported-modes))
  (pota-spots-refresh))

(defun pota-filter-by-region ()
  "Filter POTA spots by region."
  (interactive)
  (setq pota-filter-region (completing-read "Region: " pota-current-regions))
  (pota-spots-refresh))

(defun pota-filters-clear ()
  "Clear all POTA spot filters."
  (interactive)
  (setq pota-filter-band "")
  (setq pota-filter-mode "")
  (setq pota-filter-region "")
  (pota-spots-refresh))

(defun pota-spot-self (reference frequency mode)
  "Spot onself on POTA."
  (interactive
   (list
    (read-string "Park Reference: ")
    (read-string "Frequency: ")
    (read-string "Mode: ")))
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (pota-spot-data reference frequency mode)))
    (message url-request-data)))

(defun pota-spots-refresh ()
  "Erase and redraw POTA spots buffer."
  (interactive)
  (with-current-buffer (pota--buffer)
    (let* ((inhibit-read-only t)
           (pota-spots (pota-get-activations)))
      (pota--build-current-regions pota-spots)
      (erase-buffer)
      (dolist (spot pota-spots)
        (let ((time (plist-get spot :spotTime))
              (activator (plist-get spot :activator))
              (frequency (plist-get spot :frequency))
              (mode (plist-get spot :mode))
              (grid (plist-get spot :grid4))
              (loc (plist-get spot :locationDesc))
              (reference (plist-get spot :reference))
              (name (plist-get spot :name)))
          (catch 'continue
            (when (pota--filter-qrt-p spot) (throw 'continue t))
            (unless (pota--filter-match-spot-p spot) (throw 'continue t))
            (insert
             (format "%s  %-10s  %-10s  %-6s  %4s  %-8s  %-6s  %s\n"
                     time
                     (propertize activator 'face 'fle-callsign-face)
                     (propertize frequency 'face 'fle-op-frequency-face)
                     (propertize mode 'face 'fle-op-mode-face)
                     (propertize grid 'face 'fle-gridlocator-face)
                     (propertize reference 'face 'fle-pota-reference-face)
                     (propertize loc 'face 'fle-keyword-face)
                     name)))))
      (goto-char (point-min-marker)))))

(defun pota-mode-exit ()
  "Bury POTA buffer."
  (interactive)
  (quit-window (get-buffer (pota--buffer))))

(defvar pota-mode-map
  (let ((map (make-sparse-keymap)))
    ;; these bindings are a work-in-progress to see what feels right.
    (define-key map "b" 'pota-filter-by-band)
    (define-key map "m" 'pota-filter-by-mode)
    (define-key map "r" 'pota-filter-by-region)
    (define-key map "g" 'pota-spots-refresh)
    (define-key map "c" 'pota-filters-clear)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'pota-mode-exit)
    map)
  "Keymap for pota-mode.")

(define-derived-mode pota-mode special-mode "POTA"
  "Major mode for interacting with POTA."
  :group 'fle-mode
  (use-local-map pota-mode-map)
  (set (make-local-variable 'pota-current-regions) '())
  (hl-line-mode)
  (setf truncate-lines t
        header-line-format
        (format "%s%-10s  %-10s  %-6s  %4s  %4s  %-8s  %-6s  %s"
                (propertize " " 'display '((space :align-to 0)))
                "Time (UTC)" "Activator" "Frequency" "Mode" "Grid" "Reference" "Region" "Park")))

(defun pota ()
  "Display POTA spots."
  (interactive)
  (pota-spots-refresh)
  (pop-to-buffer (pota--buffer)))

(provide 'pota)

;;; pota.el ends here
