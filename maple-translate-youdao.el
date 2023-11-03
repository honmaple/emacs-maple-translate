;;; maple-translate-youdao.el ---  translate with youdao.	-*- lexical-binding: t -*-

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
;; translate word with youdao
;;

;;; Code:
(require 'maple-translate-core)

(defun maple-translate-youdao-format(dom)
  "Format seatch with bing DOM TEXT."
  (unless dom
    (throw 'not-found nil))

  (concat
   (let ((result (cl-loop for child in (maple-translate-dom-find dom ".fanyi dict-module/.trans-container/.trans-content")
                          when (consp child)
                          collect (format "- %s" (dom-text child)))))
     (unless (null result)
       (format "翻译:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child in (maple-translate-dom-find dom ".simple dict-module/.trans-container/.word-exp")
                          when (consp child)
                          collect (dom-texts child))))
     (unless (null result)
       (format "基本释义:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child in (maple-translate-dom-find dom ".web_trans dict-module/.trans-container/.webPhrase/.mcols-layout")
                          when (consp child)
                          collect (format "- %s\n  %s"
                                          (dom-text (maple-translate-dom-find child ".point"))
                                          (dom-text (maple-translate-dom-find child ".sen-phrase"))))))
     (unless (null result)
       (format "组词:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child in (maple-translate-dom-find dom ".blng_sents_part dict-module/.trans-container/.mcols-layout")
                          when (consp child)
                          collect (format "- %s\n  %s"
                                          (dom-texts (maple-translate-dom-find child ".col2/.word-exp[0]"))
                                          (dom-texts (maple-translate-dom-find child ".col2/.word-exp[1]"))))))
     (unless (null result)
       (format "例句:\n%s" (string-join result "\n"))))))

(defun maple-translate-youdao(text &optional callback)
  "Search TEXT with youdao, use async request if CALLBACK non-nil."
  (let ((url (format "https://dict.youdao.com/result?word=%s&lang=en" (url-hexify-string text))))
    (maple-translate-request url
      :format (maple-translate-youdao-format (libxml-parse-html-region))
      :headers t
      :callback callback)))

(provide 'maple-translate-youdao)
;;; maple-translate-youdao.el ends here
