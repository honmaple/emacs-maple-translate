;;; maple-translate-dictcn.el ---  translate with dict.cn.	-*- lexical-binding: t -*-

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
;; translate word with dict.cn
;;

;;; Code:
(require 'dom)

(defun maple-translate-dictcn-format(dom)
  "Format seatch with dictcn DOM."
  (unless (dom-by-class dom "dict-basic-ul")
    (throw 'not-found nil))

  (concat
   (let ((result (cl-loop for li in (butlast (dom-by-tag (dom-by-class dom "dict-basic-ul") 'li))
                          when (consp li)
                          collect (format "%s %s"
                                          (dom-text (nth 3 li))
                                          (dom-text (nth 5 li))))))
     (unless (null result)
       (format "基本释义:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child in (dom-children (dom-by-class dom "layout detail"))
                          when (consp child)
                          collect (pcase (dom-tag child)
                                    ('span
                                     (format "%s " (string-trim (dom-text child))))
                                    ('ol
                                     (format "%s\n" (string-join (cl-loop for li in (dom-children child)
                                                                          when (consp li)
                                                                          collect (format "%s" (nth 2 li)))
                                                                 "; ")))))))
     (unless (null result)
       (format "详尽释义:\n%s\n\n" (string-join result))))

   (let ((result (cl-loop for child in (dom-children (dom-by-class dom "layout sort"))
                          when (consp child)
                          collect (pcase (dom-tag child)
                                    ('div
                                     (format "%s " (string-trim (dom-text (dom-by-tag child 'b)))))
                                    ('ol
                                     (format "%s\n" (string-join (cl-loop for li in (dom-children child)
                                                                          when (consp li)
                                                                          collect (format "- %s\n  %s" (nth 2 li) (nth 4 li)))
                                                                 "\n"))
                                     )))))
     (unless (null result)
       (format "例句:\n%s" (string-join result "\n"))))))

(defun maple-translate-dictcn-search(word)
  "Search WORD with dictcn."
  (let* ((url (format "https://dict.cn/search?q=%s" (url-hexify-string word)))
         (url-request-extra-headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (re-search-forward "^$" nil t)
      (prog1 (maple-translate-dictcn-format (libxml-parse-html-region))
        (kill-buffer (current-buffer))))))

(provide 'maple-translate-dictcn)
;;; maple-translate-dictcn.el ends here