;;; maple-translate-youdao.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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

(defun maple-translate-youdao-format(content)
  "Format from search CONTENT."
  (when content
    (let* ((root (with-temp-buffer (insert content)
                                   (xml-parse-region (point-min) (point-max))))
           (xml-data (car root))
           (yodao-web-dict (car (xml-get-children xml-data 'yodao-web-dict)))
           (custom-translation (car (xml-get-children xml-data 'custom-translation))))
      (format "%s\n\n%s"
              (mapconcat (lambda(translation) (maple-translate-youdao-result translation 'content))
                         (xml-get-children custom-translation 'translation) "\n")
              (mapconcat (lambda(translation)
                           (format "- %s:\n  %s" (maple-translate-youdao-result translation 'key)
                                   (mapconcat
                                    (lambda(tran) (string-trim (maple-translate-youdao-result tran 'value)))
                                    (xml-get-children translation 'trans) "; ")))
                         (xml-get-children yodao-web-dict 'web-translation) "\n")))))

(defun maple-translate-youdao-search(word)
  "Search WORD."
  (let* ((url (format "https://dict.youdao.com/fsearch?client=deskdict&keyfrom=chrome.extension&q=%s&pos=-1
&doctype=xml&xmlVersion=3.2&dogVersion=1.0&vendor=unknown&appVer=3.1.17.4208&le=eng" word))
         (url-request-extra-headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (re-search-forward "^$" nil t)
      (prog1 (maple-translate-youdao-format (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer (current-buffer))))))

(provide 'maple-translate-youdao)
;;; maple-translate-youdao.el ends here
