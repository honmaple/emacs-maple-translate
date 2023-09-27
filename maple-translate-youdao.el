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
(require 'xml)

(defun maple-translate-youdao-result(node child-name)
  "Get result with NODE CHILD-NAME."
  (decode-coding-string (car (xml-node-children (car (xml-get-children node child-name)))) 'utf-8))

(defun maple-translate-youdao-format(dom)
  "Format search result from youdao DOM."
  (unless dom
    (throw 'not-found nil))

  (concat
   (let ((result (cl-loop for child in (xml-get-children (car (xml-get-children (car dom) 'custom-translation)) 'translation)
                          collect (maple-translate-youdao-result child 'content))))
     (unless (null result)
       (format "基本释义:\n%s\n\n" (string-join result "\n"))))

   (let ((result (cl-loop for child in (xml-get-children (car (xml-get-children (car dom) 'yodao-web-dict)) 'web-translation)
                          collect (format "- %s:\n  %s" (maple-translate-youdao-result child 'key)
                                          (mapconcat
                                           (lambda(tran) (string-trim (maple-translate-youdao-result tran 'value)))
                                           (xml-get-children child 'trans) "; ")))))
     (unless (null result)
       (format "组词:\n%s" (string-join result "\n"))))))

(defun maple-translate-youdao-search(word)
  "Search WORD with youdao."
  (let* ((url (format "https://dict.youdao.com/fsearch?client=deskdict&keyfrom=chrome.extension&q=%s&pos=-1
&doctype=xml&xmlVersion=3.2&dogVersion=1.0&vendor=unknown&appVer=3.1.17.4208&le=eng" (url-hexify-string word)))
         (url-request-extra-headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (re-search-forward "^$" nil t)
      (prog1 (maple-translate-youdao-format (xml-parse-region))
        (kill-buffer (current-buffer))))))

(provide 'maple-translate-youdao)
;;; maple-translate-youdao.el ends here
