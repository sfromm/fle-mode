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


(provide 'ham)

;;; ham.el ends here
