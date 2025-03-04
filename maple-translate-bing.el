;;; maple-translate-bing.el ---  translate with bing.com/dict.	-*- lexical-binding: t -*-

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
;; translate word with bing.com/dict
;;

;;; Code:
(require 'maple-translate-core)

(defun maple-translate-bing-format(dom)
  "Format seatch with bing DOM."
  (when dom
    (let (results)
      (let ((result (cl-loop for child in (maple-translate-dom-find dom ".qdef/*[0]/.b_primtxt")
                             when (consp child)
                             collect (dom-text child))))
        (unless (null result)
          (push (cons 'phonetic (string-join result "\n")) results)))

      (let ((result (cl-loop for child in (maple-translate-dom-find dom ".qdef/*[1]/*")
                             when (consp child)
                             collect (format "%s %s"
                                             (dom-text (nth 2 child))
                                             (dom-texts (nth 3 child) "")))))

        (unless (null result)
          (push (cons 'basic (string-join result "\n")) results)))

      (let ((result (cl-loop for child in (maple-translate-dom-find dom "#sentenceSeg/.se_li")
                             when (consp child)
                             collect (format "- %s\n  %s"
                                             (dom-texts (maple-translate-dom-find child ".sen_en") "")
                                             (dom-texts (maple-translate-dom-find child ".sen_cn") "")))))
        (unless (null result)
          (push (cons 'sentence (string-join result "\n")) results)))
      results)))

(defun maple-translate-bing(word &optional callback)
  "Search WORD with bing, use async request if CALLBACK non-nil."
  (let ((url (format "https://www.bing.com/dict/search?mkt=zh-cn&q=%s" (url-hexify-string word))))
    (maple-translate-request url
      :format (maple-translate-bing-format (dom-by-class (libxml-parse-html-region (point)) "content"))
      :callback callback)))

(provide 'maple-translate-bing)
;;; maple-translate-bing.el ends here