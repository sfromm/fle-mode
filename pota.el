;;; pota.el --- Mode to hunt and spot for POTA activity -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Stephen Fromm

;; Author: Stephen Fromm
;; URL: https://github.com/sfromm/ham-el
;; Version: 0.7
;; Package-Requires: ((emacs "24.1") (ham "0.6"))
;; Keywords: pota, ham, amateur, radio

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

(require 'ham)
(require 'fle-mode)
(require 'url)
(require 'json)

(defcustom pota-spot-api-url
  "https://api.pota.app/spot"
  "URL to query POTA for current activations."
  :group 'ham
  :type 'string)

(defcustom pota-activator-api-url
  "https://api.pota.app/spot/activator"
  "URL to query POTA for current activations."
  :group 'ham
  :type 'string)

(defcustom pota-buffer-name "*POTA Active Spots*"
  "Name of POTA Spots buffer."
  :group 'ham
  :type 'string)

(defcustom pota-filter-qrt t
  "Whether to filter QRT stations."
  :group 'ham
  :type 'boolean)

(defcustom pota-wait-for-refresh 60
  "Whether to filter QRT stations."
  :group 'ham
  :type 'integer)

(defconst pota-mode-version "0.2" "Version of `pota-mode'.")

(defvar pota-filter-mode ""
  "Variable for what mode to filter by.")

(defvar pota-filter-band ""
  "Variable for what band to filter by.")

(defvar pota-filter-region ""
  "Variable for what region to filter by.")

(defvar pota--spots-last-refresh nil
  "Time when POTA spots were last refreshed.")

(defvar pota--spots nil
  "Current POTA activations.")


;;;
(defun pota--json-query-api ()
  "Query POTA API for current activations.  Returns JSON object."
  (ham--rest-query pota-activator-api-url))

(defun pota-get-activations ()
  "Return current activations from cache or get fresh spot information."
  (unless pota--spots-last-refresh
    (setq pota--spots-last-refresh (time-subtract (current-time) pota-wait-for-refresh)))
  (when (<= pota-wait-for-refresh (float-time (time-subtract (current-time) pota--spots-last-refresh)))
    (message "refreshing pota cache...")
    (setq pota--spots (pota--json-query-api))
    (setq pota--spots-last-refresh (current-time)))
  pota--spots
  )

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

(defun pota--filter-match-band-p (spot)
  "Filter SPOT based on band."
  (if (string= "" pota-filter-band) t
    (string-match-p
     (regexp-quote pota-filter-band)
     (ham-frequency-to-band (string-to-number (plist-get spot :frequency))))))

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
  (setq pota-filter-band (completing-read "Band: " ham-supported-bands))
  (pota-spots-refresh))

(defun pota-filter-by-mode ()
  "Filter POTA spots by mode."
  (interactive)
  (setq pota-filter-mode (completing-read "Mode: " ham-supported-modes))
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
  :group 'ham
  (use-local-map pota-mode-map)
  (set (make-local-variable 'pota-current-regions) '())
  (hl-line-mode)
  (setf truncate-lines t
        header-line-format
        (format "%s%-10s  %-10s  %-6s  %4s  %4s  %-8s  %-6s  %s  %s"
                (propertize " " 'display '((space :align-to 0)))
                "Time (UTC)" "Activator"
                "Frequency" "Mode"
                "Grid" "Reference"
                "Region" "Park"
                (concat "Fetched "
                        (if pota--spots-last-refresh
                            (format-time-string "%a %H:%M" pota--spots-last-refresh)
                          "now."))
                )))

(defun pota-mode-version ()
  "Display version of `pota-mode'."
  (interactive)
  (message "pota %s; ham-el %s" pota-mode-version ham-el-version)
  pota-mode-version)

(defun pota ()
  "Display POTA spots."
  (interactive)
  (pota-spots-refresh)
  (pop-to-buffer (pota--buffer)))

(provide 'pota)

;;; pota.el ends here
