;;; maple-translate-youdao.el ---  translate with youdao.	-*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 lin.jiang

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
  "Format seatch with youdao DOM."
  (when dom
    (let (results)
      (let ((result (cl-loop for child in (maple-translate-dom-find dom ".simple dict-module/.trans-container/.word-exp")
                             when (consp child)
                             collect (dom-texts child))))
        (unless (null result)
          (push (cons 'basic (string-join result "\n")) results)))

      (let ((result (cl-loop for child in (maple-translate-dom-find dom ".simple dict-module/.trans-container/.word-wfs-cell-less")
                             when (consp child)
                             collect (format "%s: %s"
                                             (dom-text (maple-translate-dom-find child ".wfs-name"))
                                             (dom-text (maple-translate-dom-find child ".transformation"))))))
        (unless (null result)
          (push (cons 'morphology (string-join result "\n")) results)))

      (let ((result (cl-loop for child in (maple-translate-dom-find dom ".ec dict-module/.trans-container/.phone_con/.per-phone")
                             when (consp child)
                             collect (dom-texts child))))
        (unless (null result)
          (push (cons 'phonetic (string-join result "\n")) results)))

      (let ((result (cl-loop for child in (maple-translate-dom-find dom ".web_trans dict-module/.trans-container/.webPhrase/.mcols-layout")
                             when (consp child)
                             collect (format "- %s\n  %s"
                                             (dom-text (maple-translate-dom-find child ".point"))
                                             (dom-text (maple-translate-dom-find child ".sen-phrase"))))))
        (unless (null result)
          (push (cons 'phrase (string-join result "\n")) results)))

      (let ((result (cl-loop for child in (maple-translate-dom-find dom ".blng_sents_part dict-module/.trans-container/.mcols-layout")
                             when (consp child)
                             collect (format "- %s\n  %s"
                                             (dom-texts (maple-translate-dom-find child ".col2/.word-exp[0]"))
                                             (dom-texts (maple-translate-dom-find child ".col2/.word-exp[1]"))))))
        (unless (null result)
          (push (cons 'sentence (string-join result "\n")) results)))
      results)))

(defun maple-translate-youdao(text &optional callback)
  "Search TEXT with youdao, use async request if CALLBACK non-nil."
  (let ((url (format "https://dict.youdao.com/result?word=%s&lang=en" (url-hexify-string text))))
    (maple-translate-request url
      :format (maple-translate-youdao-format (libxml-parse-html-region (point)))
      :headers t
      :callback callback)))

(provide 'maple-translate-youdao)
;;; maple-translate-youdao.el ends here
