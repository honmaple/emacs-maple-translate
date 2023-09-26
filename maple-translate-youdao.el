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

(defun maple-translate-youdao-format(content)
  "Format from search CONTENT."
  (when content
    (let* ((root (with-temp-buffer (insert content)
                                   (xml-parse-region (point-min) (point-max))))
           (xml-data (car root))
           (custom-translation (car (xml-get-children xml-data 'custom-translation)))
           (translation (car (xml-get-children custom-translation 'translation)))
           (content (car (xml-get-children translation 'content)))
           (result (car (xml-node-children content))))
      (if (null result) (format "no result")
        (decode-coding-string result 'utf-8)))))

(defun maple-translate-youdao-search(word)
  "Search WORD."
  (let ((url (format "https://dict.youdao.com/fsearch?client=deskdict&keyfrom=chrome.extension&q=%s&pos=-1
&doctype=xml&xmlVersion=3.2&dogVersion=1.0&vendor=unknown&appVer=3.1.17.4208&le=eng" word)))
    (with-current-buffer (url-retrieve-synchronously url)
      (prog1 (maple-translate-youdao-format (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer (current-buffer))))))

(provide 'maple-translate-youdao)
;;; maple-translate-youdao.el ends here