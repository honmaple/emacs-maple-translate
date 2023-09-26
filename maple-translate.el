;;; maple-translate.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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
;; translate word between chinese and english.
;;

;;; Code:
(require 'maple-translate-youdao)

(declare-function evil-make-overriding-map 'evil)

(defgroup maple-translate nil
  "Translate word between chinese and english."
  :group 'maple)

(defcustom maple-translate-buffer "*maple-translate*"
  "Show translate result in buffer."
  :group 'maple-translate
  :type 'string)

(defcustom maple-translate-alist '((youdao . maple-translate-youdao-search))
  "Translate function with different engine."
  :group 'maple-translate
  :type '(alist :key-type symbol :value-type function))

(defcustom maple-translate-engine 'youdao
  "Translate engine."
  :group 'maple-translate
  :type 'symbol)

(defun maple-translate-word()
  "Get translate word."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun maple-translate-show(result)
  "Show RESULT."
  (with-current-buffer (get-buffer-create maple-translate-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "基本释义：\n%s\n" result))
      (maple-translate-mode)
      (goto-char (point-min)))
    (unless (get-buffer-window (current-buffer))
      (switch-to-buffer-other-window maple-translate-buffer))))

(defun maple-translate-result()
  "Show RESULT."
  (let ((engine (cdr (or (assq maple-translate-engine maple-translate-alist) (assq t maple-translate-alist)))))
    (if engine (funcall engine (maple-translate-word))
      (error "No translate engine found"))))

(defvar maple-translate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    map)
  "Maple-imenu-mode keymap.")

(define-derived-mode maple-translate-mode special-mode "maple-translate"
  "Major mode for maple translate.
\\{maple-translate-mode-map}"
  (read-only-mode 1)
  (with-eval-after-load 'evil
    (evil-make-overriding-map maple-translate-mode-map 'normal)))

;;;###autoload
(defun maple-translate+()
  "Translate word at point and display result with buffer."
  (interactive)
  (let ((result (maple-translate-result)))
    (maple-translate-show result)))

;;;###autoload
(defun maple-translate()
  "Translate word at point and display result in echoarea."
  (interactive)
  (let ((result (maple-translate-result)))
    (princ (format "基本释义：\n%s\n" result))))

(provide 'maple-translate)
;;; maple-translate.el ends here