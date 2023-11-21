;;; maple-translate-iciba.el ---  translate with www.iciba.com.	-*- lexical-binding: t -*-

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
;; translate word with www.iciba.com
;;

;;; Code:
(require 'maple-translate-core)

(defun maple-translate-iciba-format(dom)
  "Format seatch with iciba DOM."
  (when dom
    (concat
     (let ((result (cl-loop for child in (maple-translate-dom-find dom ".Mean_mean/.Mean_symbols/li")
                            when (consp child)
                            collect (dom-text child))))
       (unless (null result)
         (format "读音:\n%s\n\n" (string-join result "\n"))))

     (let ((result (cl-loop for child in (maple-translate-dom-find dom ".Mean_definition/.Mean_part/li")
                            when (consp child)
                            collect (dom-texts child ""))))

       (unless (null result)
         (format "基本释义:\n%s\n\n" (string-join result "\n"))))

     (let ((result (cl-loop for child in (maple-translate-dom-find dom ".Morphology_morphology/li")
                            when (consp child)
                            collect (dom-texts child ""))))

       (unless (null result)
         (format "词态变化:\n%s\n\n" (string-join result "\n"))))

     (let ((result (cl-loop for child in (maple-translate-dom-find dom ".SceneSentence_scene/.NormalSentence_sentence")
                            when (consp child)
                            collect (format "- %s\n  %s"
                                            (dom-texts (maple-translate-dom-find child ".NormalSentence_en") "")
                                            (dom-texts (maple-translate-dom-find child ".NormalSentence_cn") "")))))
       (unless (null result)
         (format "例句:\n%s" (string-join result "\n"))))

     (let ((result (maple-translate-dom-find dom ".Mean_definition/.Mean_trans/p[0]")))
       (unless (null result)
         (format "翻译:\n%s" (dom-texts result)))))))

(defun maple-translate-iciba(text &optional callback)
  "Translate TEXT with iciba, use async request if CALLBACK non-nil."
  (let ((url (format "https://www.iciba.com/word?w=%s" (string-replace "%20" "+" (url-hexify-string text)))))
    (maple-translate-request url
      :format (maple-translate-iciba-format (libxml-parse-html-region (point)))
      :headers t
      :callback callback)))

(provide 'maple-translate-iciba)
;;; maple-translate-iciba.el ends here