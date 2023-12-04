;;; maple-translate.el ---  maple translate configuration.	-*- lexical-binding: t -*-

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
(require 'maple-translate-sdcv)
(require 'maple-translate-bing)
(require 'maple-translate-iciba)
(require 'maple-translate-youdao)
(require 'maple-translate-dictcn)
(require 'maple-translate-google)

(declare-function evil-make-overriding-map 'evil)

(defgroup maple-translate nil
  "Translate word between chinese and english."
  :group 'maple)

(defcustom maple-translate-buffer "*maple-translate*"
  "Show translate result in buffer."
  :group 'maple-translate
  :type 'string)

(defcustom maple-translate-alist '((sdcv   . maple-translate-sdcv)
                                   (bing   . maple-translate-bing)
                                   (iciba  . maple-translate-iciba)
                                   (youdao . maple-translate-youdao)
                                   (dictcn . maple-translate-dictcn)
                                   (google . maple-translate-google))
  "Translate function with different engine."
  :group 'maple-translate
  :type '(alist :key-type symbol :value-type function))

(defcustom maple-translate-engine 'youdao
  "Translate engine."
  :group 'maple-translate
  :type '(or symbol list))

(defun maple-translate-word()
  "Get translate word."
  (let ((word (string-trim (or (if (use-region-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (thing-at-point 'word t))
                               ""))))
    (if (string= word "") (read-from-minibuffer "Translate word: ") word)))

(defun maple-translate-show(word fn)
  "Show translate result of WORD with FN."
  (let ((result (maple-translate-result word)))
    (if (string= result "") (princ "No result found")
      (funcall fn result))))

(defun maple-translate-result(word &optional engine)
  "Get translate result of WORD with ENGINE."
  (unless engine (setq engine maple-translate-engine))

  (if (listp engine)
      (let ((results (cl-loop for e in engine
                              collect (let ((result (maple-translate-result word e)))
                                        (unless (string= result "")
                                          (format "%s\n%s" (propertize (upcase (symbol-name e)) 'face 'font-lock-constant-face) result))))))
        (string-join (cl-remove nil results) "\n\n"))
    (let ((fn (or (assq engine maple-translate-alist) (assq t maple-translate-alist))))
      (if fn (string-trim (or (funcall (cdr fn) word) ""))
        (error "No translate engine found")))))

(defun maple-translate-show-in-buffer(result)
  "Show RESULT in buffer."
  (with-current-buffer (get-buffer-create maple-translate-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert result)
      (maple-translate-mode)
      (goto-char (point-min)))
    (unless (get-buffer-window (current-buffer))
      (switch-to-buffer-other-window maple-translate-buffer))))

(defvar maple-translate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    map)
  "Maple-translate-mode keymap.")

(define-derived-mode maple-translate-mode special-mode "maple-translate"
  "Major mode for maple translate.
\\{maple-translate-mode-map}"
  (read-only-mode 1)
  (with-eval-after-load 'evil
    (evil-make-overriding-map maple-translate-mode-map 'normal)))

;;;###autoload
(defun maple-translate+(word)
  "Translate WORD and display result in buffer."
  (interactive (list (maple-translate-word)))
  (maple-translate-show word 'maple-translate-show-in-buffer))

;;;###autoload
(defun maple-translate(word)
  "Translate WORD and display result in echoarea."
  (interactive (list (maple-translate-word)))
  (maple-translate-show word 'princ))

(provide 'maple-translate)
;;; maple-translate.el ends here