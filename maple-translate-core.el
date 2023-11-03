;;; maple-translate-core.el ---  translate from chinese to english.	-*- lexical-binding: t -*-

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
;; translate from chinese to english
;;

;;; Code:
(require 'dom)

(defvar maple-translate-request-headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36")))

(defun maple-translate-dom-by-key(dom key)
  "Find DOM by KEY."
  (let (func num)
    (when (string-match "\\[\\([0-9]+\\)\\]" key)
      (setq num (match-string 1 key))
      (setq key (substring key 0 (- (length key) (length num) 2))))
    (cond ((string-prefix-p "." key)
           (setq key (substring key 1) func 'dom-by-class))
          ((string-prefix-p "#" key)
           (setq key (substring key 1) func 'dom-by-id))
          ((string-prefix-p "*" key)
           (setq func (lambda(p _) (dom-children p))))
          (t (setq key (intern key) func 'dom-by-tag)))
    (if (null num)
        (funcall func dom key)
      (nth (string-to-number num) (funcall func dom key)))))

(defun maple-translate-dom-find(dom xpath)
  "Find DOM by XPATH.
example: a-tag/#b-id/.c-class[2]/*[1]."
  (cl-loop for key in (string-split (string-trim xpath) "/")
           if (consp dom)
           do (setq dom (maple-translate-dom-by-key dom key))
           else return dom)
  dom)

(defmacro maple-translate-request(url &rest body)
  "Request translate engine and call BODY by URL."
  (declare (indent defun))
  `(let ((url-request-extra-headers maple-translate-request-headers))
     (with-current-buffer (url-retrieve-synchronously ,url t)
       (re-search-forward "^$" nil t)
       (prog1 ,@body
         (kill-buffer (current-buffer))))))

(provide 'maple-translate-core)
;;; maple-translate-core.el ends here