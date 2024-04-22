;;; maple-translate-core.el ---  translate from chinese to english.	-*- lexical-binding: t -*-

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

(defun maple-translate-query-string(params)
  "Convert PARAMS to query string."
  (mapconcat #'(lambda (p)
                 (format "%s=%s"
                         (url-hexify-string (car p))
                         (url-hexify-string (cdr p))))
             params "&"))

(defmacro maple-translate-request(url &rest body)
  "Request translate engine and call BODY by URL."
  (declare (indent defun))
  (cl-destructuring-bind (&key format callback headers proxies) body
    `(let ((url-proxy-services (or ,proxies url-proxy-services))
           (url-request-extra-headers (when ,headers maple-translate-request-headers)))
       (if ,callback
           (url-retrieve ,url
                         (lambda(_)
                           (goto-char url-http-end-of-headers)
                           (prog1 (funcall ,callback ,format)
                             (kill-buffer (current-buffer)))))
         (with-current-buffer (url-retrieve-synchronously ,url t)
           (goto-char url-http-end-of-headers)
           (prog1 ,format
             (kill-buffer (current-buffer))))))))

(defmacro maple-translate-execute(program &rest body)
  "Execute translate command by PROGRAM with some BODY."
  (declare (indent defun))
  (cl-destructuring-bind (&key args format callback) body
    `(if ,callback
         (let ((name (format "maple-translate-process %s" ,program)))
           (set-process-sentinel
            (apply 'start-process name (format "*%s*" name) ,program ,args)
            (lambda(process _)
              (unless (process-live-p process)
                (with-current-buffer (process-buffer process)
                  (prog1 (funcall ,callback ,format)
                    (kill-buffer (current-buffer))))))))
       (with-temp-buffer
         (apply 'call-process ,program nil t nil ,args)
         ,format))))

(provide 'maple-translate-core)
;;; maple-translate-core.el ends here