;;; maple-translate-dictcn.el ---  translate with dict.cn.	-*- lexical-binding: t -*-

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
;; translate word with dict.cn
;;

;;; Code:
(require 'maple-translate-core)

(defun maple-translate-dictcn-format(dom)
  "Format seatch with dictcn DOM."
  (when dom
    (concat
     (let ((result (cl-loop for child in (butlast (maple-translate-dom-find dom ".word/.phonetic/*"))
                            when (consp child)
                            collect
                            (format "%s %s"
                                    (string-trim (dom-text child))
                                    (dom-text (maple-translate-dom-find child "bdo"))))))
       (unless (null result)
         (format "读音:\n%s\n\n" (string-join result "\n"))))

     (let ((result (cl-loop for child in (butlast (maple-translate-dom-find dom ".dict-basic-ul/li"))
                            when (consp child)
                            collect (format "%s %s"
                                            (dom-text (maple-translate-dom-find child "span"))
                                            (dom-text (maple-translate-dom-find child "strong"))))))
       (unless (null result)
         (format "基本释义:\n%s\n\n" (string-join result "\n"))))

     (let ((result (cl-loop for child in (maple-translate-dom-find dom ".layout detail/*")
                            when (consp child)
                            collect (pcase (dom-tag child)
                                      ('span
                                       (format "%s " (string-trim (dom-text child))))
                                      ('ol
                                       (format "%s\n" (string-join (cl-loop for li in (dom-children child)
                                                                            when (consp li)
                                                                            collect (dom-text li))
                                                                   "; ")))))))
       (unless (null result)
         (format "详尽释义:\n%s\n\n" (string-join result))))

     (let ((result (cl-loop for child in (maple-translate-dom-find dom ".layout sort/*")
                            when (consp child)
                            collect (pcase (dom-tag child)
                                      ('div
                                       (format "%s " (string-trim (dom-text (dom-by-tag child 'b)))))
                                      ('ol
                                       (format "%s\n" (string-join (cl-loop for li in (dom-children child)
                                                                            when (consp li)
                                                                            collect (format "- %s\n  %s" (nth 2 li) (nth 4 li)))
                                                                   "\n")))))))
       (unless (null result)
         (format "例句:\n%s" (string-join result "\n")))))))

(defun maple-translate-dictcn(word &optional callback)
  "Search WORD with dictcn, use async request if CALLBACK non-nil."
  (let ((url (format "https://dict.cn/search?q=%s" (url-hexify-string word))))
    (maple-translate-request url
      :format (maple-translate-dictcn-format (libxml-parse-html-region (point)))
      :headers t
      :callback callback)))

(provide 'maple-translate-dictcn)
;;; maple-translate-dictcn.el ends here