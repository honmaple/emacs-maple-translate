;;; maple-translate-google.el ---  translate with translate.googleapis.com.	-*- lexical-binding: t -*-

;; Copyright (C) 2023 lin.jiang

;; URL: https://github.com/honmaple/emacs-maple-translate

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; translate word with translate.googleapis.com
;;

;;; Code:
(require 'json)
(require 'maple-translate-core)

(defvar maple-translate-google-hl (if current-iso639-language (symbol-name current-iso639-language) "en"))
(defvar maple-translate-google-url "https://translate.googleapis.com/translate_a/single")
(defvar maple-translate-google-proxies nil)

(defun maple-translate-google-format(json)
  "Format seatch with google JSON."
  (concat
   (let ((result (aref (aref (aref json 0) 1) 2)))
     (unless (null result)
       (format "读音:\n%s\n\n" result)))

   (let ((result (aref (aref (aref json 0) 0) 0)))
     (unless (null result)
       (format "基本释义:\n%s\n\n" result)))

   (let ((result (cl-loop for child across-ref (aref (aref (aref json 5) 0) 2)
                          collect (aref child 0))))
     (unless (null result)
       (format "详尽释义:\n%s\n\n" (string-join result "; "))))

   (let ((result (cl-loop for child across-ref (aref json 1)
                          collect (format "- %s:\n%s"
                                          (aref child 0)
                                          (let ((res (cl-loop for index from 0
                                                              for item across-ref (aref child 2)
                                                              collect (format "  %d. %s: %s"
                                                                              (1+ index)
                                                                              (aref item 0)
                                                                              (string-join (aref item 1) "; ")))))
                                            (string-join res "\n"))))))
     (unless (null result)
       (format "%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child across-ref (if (> (length json) 12) (aref json 12) [])
                          collect (format "- %s:\n%s"
                                          (aref child 0)
                                          (let ((res (cl-loop for index from 0
                                                              for item across-ref (aref child 1)
                                                              collect (format "  %d. %s"
                                                                              (1+ index)
                                                                              (aref item 0)))))
                                            (string-join res "\n"))))))
     (unless (null result)
       (format "解释:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child across-ref (aref (or (if (> (length json) 13) (aref json 13) [[]]) [[]]) 0)
                          collect (format "- %s" (aref child 0)))))
     (unless (null result)
       (format "例句:\n%s" (string-join result "\n"))))))

;; https://stackoverflow.com/questions/26714426/what-is-the-meaning-of-google-translate-query-params
(defun maple-translate-google(text &optional callback)
  "Search TEXT with google, use async request if CALLBACK non-nil."
  (let ((params `(("client" . "gtx")
                  ("ie"     . "UTF-8")
                  ("oe"     . "UTF-8")
                  ("hl"     . ,maple-translate-google-hl)
                  ("sl"     . "auto")
                  ("tl"     . ,(if (string-match "\\cc" text) "en" "zh-CN"))
                  ("q"      . ,text)
                  ("dt"     . "bd")
                  ("dt"     . "ex")
                  ("dt"     . "ld")
                  ("dt"     . "md")
                  ("dt"     . "qc")
                  ("dt"     . "rw")
                  ("dt"     . "rm")
                  ("dt"     . "ss")
                  ("dt"     . "t")
                  ("dt"     . "at")
                  ("pc"     . "1")
                  ("otf"    . "1")
                  ("srcrom" . "1")
                  ("ssel"   . "0")
                  ("tsel"   . "0"))))
    (maple-translate-request (concat maple-translate-google-url "?" (maple-translate-query-string params))
      :format (maple-translate-google-format
               (json-read-from-string (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
      :headers t
      :proxies maple-translate-google-proxies
      :callback callback)))

(provide 'maple-translate-google)
;;; maple-translate-google.el ends here