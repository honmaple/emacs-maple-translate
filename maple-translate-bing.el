;;; maple-translate-bing.el ---  translate with bing.com/dict.	-*- lexical-binding: t -*-

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
;; translate word with bing.com/dict
;;

;;; Code:
(require 'maple-translate-core)

(defun maple-translate-bing-format(dom)
  "Format seatch with bing DOM."
  (unless dom
    (throw 'not-found nil))

  (concat
   (let ((result (cl-loop for child in (maple-translate-dom-find dom ".qdef/*[0]/.b_primtxt")
                          when (consp child)
                          collect (dom-text child))))
     (unless (null result)
       (format "读音:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child in (maple-translate-dom-find dom ".qdef/*[1]/*")
                          when (consp child)
                          collect (format "%s %s"
                                          (dom-text (nth 2 child))
                                          (dom-texts (nth 3 child) "")))))

     (unless (null result)
       (format "基本释义:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child in (maple-translate-dom-find dom "#sentenceSeg/.se_li")
                          when (consp child)
                          collect (format "- %s\n  %s"
                                          (dom-texts (maple-translate-dom-find child ".sen_en") "")
                                          (dom-texts (maple-translate-dom-find child ".sen_cn") "")))))
     (unless (null result)
       (format "例句:\n%s" (string-join result "\n"))))))

(defun maple-translate-bing(word)
  "Search WORD with bing."
  (let ((url (format "https://www.bing.com/dict/search?mkt=zh-cn&q=%s" (url-hexify-string word))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (re-search-forward "^$" nil t)
      (prog1 (maple-translate-bing-format (dom-by-class (libxml-parse-html-region) "content"))
        (kill-buffer (current-buffer))))))

(provide 'maple-translate-bing)
;;; maple-translate-bing.el ends here