;;; ham.el --- Helpers for amatuer radio -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Stephen Fromm

;; Author: Stephen Fromm
;; URL: https://github.com/sfromm/ham-el
;; Version: 0.7
;; Package-Requires: ((emacs "24.1"))
;; Keywords: fle, ham, amateur, radio

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

;;; Code:

(require 'fle-mode)
(require 'pota)

(defgroup ham nil
  "HAM.
Group of modes and helpers for operating an amateur radio."
  :group 'comm)

(defgroup spacewx nil
  "Space Weather Mode.
Mode for collecting space weather data."
  :group 'ham)

(defcustom ham-spacewx-kp-hour-cutoff
  "Hour of the day to cutoff to determine what date to use for Kp lookup."
  6
  :type 'int)


;;; Constants

(defconst ham-el-version "0.6" "Version of `ham-el'.")

(defconst ham-supported-bands
  '("2190m" "630m" "560m" "160m" "80m" "60m" "40m" "30m" "20m" "17m" "15m"
    "12m" "10m" "6m" "4m" "2m" "1.25m" "70cm" "33cm" "23cm" "13cm" "9cm"
    "6cm" "3cm" "1.25cm" "6mm" "4mm" "2.5mm" "2mm" "1mm")
  "Supported amateur radio bands.")

(defconst ham-supported-modes
  '("CW" "SSB" "AM" "FM" "RTTY" "FT8" "PSK" "JT65" "JT9" "FT4" "JS8" "ARDOP"
    "ATV" "C4FM" "CHIP" "CLO" "CONTESTI" "DIGITALVOICE" "DOMINO" "DSTAR" "FAX"
    "FSK441" "HELL" "ISCAT" "JT4" "JT6M" "JT44" "MFSK" "MSK144" "MT63" "OLIVIA"
    "OPERA" "PAC" "PAX" "PKT" "PSK2K" "Q15" "QRA64" "ROS" "RTTYM" "SSTV" "T10"
    "THOR" "THRB" "TOR" "V4" "VOI" "WINMOR" "WSPR")
  "Supported amateur radio modes.")

(defconst ham-spacewx-potsdam-kp-api-url
  "https://kp.gfz-potsdam.de/app/json/?"
  "GFZ Potsdam URL to query space weather information.")

(defconst ham-spacewx-noaa-swpc-kp-url
  "https://services.swpc.noaa.gov/products/noaa-planetary-k-index.json"
  "NOAA URL to query space weather information.
For more information, see https://www.swpc.noaa.gov/products/planetary-k-index.")

(defconst ham-spacewx-noaa-swpc-wwv-url
  "https://services.swpc.noaa.gov/text/wwv.txt"
  "NOAA URL to query WWV space weather information.
For more information, see https://www.swpc.noaa.gov/products/geophysical-alert-wwv-text.")

(defconst ham-spacewx-silso-current-url
  "https://www.sidc.be/SILSO/DATA/EISN/EISN_current.txt"
  "URL for Royal Observatory of Belgium Sunspot Index and Long-term Solar Observations.
For more information, see https://www.sidc.be/SILSO/home.")



;;; Functions - frequency and band

(defun ham-frequency-to-band (f)
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

(defun ham-mode-to-frequency (mode band)
  "With provided MODE and BAND, return operating frequency in kHz.
Only useful for certain digital modes."
  (cond
   ((and (string= "160m" band) (string= "FT8" mode)) "1840")
   ((and (string= "160m" band) (string= "WSPR" mode)) "1836.6")
   ((and (string= "80m" band) (string= "FT8" mode)) "3573")
   ((and (string= "80m" band) (string= "FT4" mode)) "3575")
   ((and (string= "80m" band) (string= "WSPR" mode)) "3568.6")
   ((and (string= "60m" band) (string= "FT8" mode)) "5357")
   ((and (string= "40m" band) (string= "FT8" mode)) "7074")
   ((and (string= "40m" band) (string= "FT4" mode)) "7047")
   ((and (string= "40m" band) (string= "WSPR" mode)) "7038.6")
   ((and (string= "30m" band) (string= "FT8" mode)) "10136")
   ((and (string= "30m" band) (string= "FT4" mode)) "10140")
   ((and (string= "30m" band) (string= "WSPR" mode)) "10138.7")
   ((and (string= "20m" band) (string= "FT8" mode)) "14074")
   ((and (string= "20m" band) (string= "FT4" mode)) "14080")
   ((and (string= "20m" band) (string= "WSPR" mode)) "14095.6")
   ((and (string= "17m" band) (string= "FT8" mode)) "18100")
   ((and (string= "17m" band) (string= "FT4" mode)) "18104")
   ((and (string= "17m" band) (string= "WSPR" mode)) "18104.6")
   ((and (string= "15m" band) (string= "FT8" mode)) "21074")
   ((and (string= "15m" band) (string= "FT4" mode)) "21140")
   ((and (string= "15m" band) (string= "WSPR" mode)) "21094.6")
   ((and (string= "12m" band) (string= "FT8" mode)) "24915")
   ((and (string= "12m" band) (string= "FT4" mode)) "24919")
   ((and (string= "12m" band) (string= "WSPR" mode)) "24924.6")
   ((and (string= "10m" band) (string= "FT8" mode)) "28074")
   ((and (string= "10m" band) (string= "FT4" mode)) "28180")
   ((and (string= "10m" band) (string= "WSPR" mode)) "28124.6")
   ((and (string= "6m" band) (string= "FT8" mode)) "50313")
   ((and (string= "6m" band) (string= "FT4" mode)) "50318")
   ((and (string= "6m" band) (string= "WSPR" mode)) "50293")
   ((and (string= "2m" band) (string= "FT8" mode)) "144174")
   ((and (string= "2m" band) (string= "FT4" mode)) "144170")
   ((and (string= "2m" band) (string= "WSPR" mode)) "144489")
   ((and (string= "1.25cm" band) (string= "FT8" mode)) "222065")
   ((and (string= "70cm" band) (string= "FT8" mode)) "432065")
   ((and (string= "70cm" band) (string= "WSPR" mode)) "432300")
   (t nil)))

(defun ham-default-rst (mode rst)
  "Return RST report a default for the MODE if RST is nil."
  (cond (rst rst)
        ((member mode '("CW" "RTTY" "PSK")) "599")
        ((member mode '("SSB" "AM" "FM")) "59")
        ((member mode '("JT65" "JT9" "JT6M" "JT4" "JT44"
                        "FSK441" "FT8" "FT4" "ISCAT" "MSK144"
                        "QRA64" "T10" "WSPR")) "-10")
        (t "59")))


;;; utilities

(defun ham--rest-query (url)
  "Do REST query against url and return results."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read-from-string
     (with-current-buffer (url-retrieve-synchronously url)
       (goto-char (point-min-marker))
       (re-search-forward "^$")
       (prog1
           (buffer-substring-no-properties (point) (point-max))
         (kill-buffer (current-buffer)))))))


;;; spacewx

(defun ham--get-lookup-date ()
  "Return date string to use for lookup of index values."
  (let ((now-hour (format-time-string "%H" (current-time) t)))
    (if (>= ham-spacewx-kp-hour-cutoff (string-to-number now-hour))
        (format-time-string "%Y-%m-%d"
                            (time-subtract (current-time) (days-to-time 1)) t)
      (format-time-string "%Y-%m-%d" (current-time) t))))

(defun ham-spacewx-get-noaa-swpc-kp-values ()
  "Return Kp values from NOAA SWPC."
  (let ((kpvals nil)
        (observations (cdr (ham--rest-query ham-spacewx-noaa-swpc-kp-url))))
    (dolist (vals observations)
      (push (nth 1 vals) kpvals))
    (reverse kpvals)))

(defun ham-spacewx-get-noaa-swpc-ap-values ()
  "Return Ap values from NOAA SWPC."
  (let ((apvals nil)
        (observations (cdr (ham--rest-query ham-spacewx-noaa-swpc-kp-url))))
    (dolist (vals observations)
      (push (nth 2 vals) apvals))
    (reverse apvals)))

(defun ham-spacewx-noaa-swpc-solar-flux ()
  "Return WWV values from NOAA SWPC."
  (let ((solarflux nil))
    (with-current-buffer (url-retrieve-synchronously ham-spacewx-noaa-swpc-wwv-url)
      (goto-char (point-min-marker))
      (re-search-forward "^Solar flux")
      (setq solarflux (nth 0 (split-string (buffer-substring-no-properties (point) (line-end-position)))))
      solarflux)))

(defun ham--spacewx-sidc-silso-values ()
  "Return SILSO sunspot values from SIDC."
  (let ((sunspots nil)
        (measurement nil)
        (date nil)
        (sn nil))
    (with-current-buffer (url-retrieve-synchronously ham-spacewx-silso-current-url)
      (goto-char (point-min-marker))
      (re-search-forward "^$")
      (forward-line)
      (while (not (eobp))
        (let* ((line (substring (thing-at-point 'line t) 0 -1))
               (parts (split-string line)))
          (setq sn (nth 4 parts))
          (setq date (concat (nth 0 parts) "-" (nth 1 parts) "-" (nth 2 parts)))
          (setq measurement (list date sn))
          (push measurement sunspots))
        (forward-line))
      (reverse sunspots))))

(defun ham-spacewx-get-last-kp-value ()
  "Query for most current Kp index."
  (car (last (ham-spacewx-get-noaa-swpc-kp-values))))

(defun ham-spacewx-get-last-ap-value ()
  "Query for most recent Kp index."
  (interactive)
  (car (last (ham-spacewx-get-noaa-swpc-ap-values))))

(defun ham-spacewx-get-last-sunspot-value ()
  "Query for last sunspot measurement."
  (interactive)
  (cadr (car (last (ham--spacewx-sidc-silso-values)))))

(defun ham-spacewx ()
  "Return current spacewx as an alist."
  (interactive)
  (let ((kindex (ham-spacewx-get-last-kp-value))
        (aindex (ham-spacewx-get-last-ap-value))
        (sunspots (ham-spacewx-get-last-sunspot-value))
        (solarflux (ham-spacewx-noaa-swpc-solar-flux)))
    (list (list 'kindex kindex)
          (list 'aindex aindex)
          (list 'sunspots sunspots)
          (list 'solarflux solarflux))))

(provide 'ham)

;;; ham.el ends here
