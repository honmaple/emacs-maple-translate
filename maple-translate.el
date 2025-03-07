;;; maple-translate.el ---  maple translate configuration.	-*- lexical-binding: t -*-

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
;; translate word between chinese and english.
;;

;;; Code:
(require 'maple-translate-sdcv)
(require 'maple-translate-bing)
(require 'maple-translate-iciba)
(require 'maple-translate-youdao)
(require 'maple-translate-dictcn)
(require 'maple-translate-google)

(declare-function evil-make-overriding-map "evil")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-workable-p "posframe")

(defgroup maple-translate nil
  "Translate word between chinese and english."
  :group 'maple)

(defcustom maple-translate-buffer "*maple-translate*"
  "Show translate result in buffer."
  :group 'maple-translate
  :type 'string)

(defcustom maple-translate-alist
  '((sdcv   . maple-translate-sdcv)
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

(defcustom maple-translate-engine-alist nil
  "Translate engine with different command."
  :group 'maple-translate
  :type '(alist :key-type symbol :value-type function))

(defcustom maple-translate-section
  '(phonetic basic detail morphology phrase sentence)
  "Translate result with different section."
  :group 'maple-translate
  :type '(list symbol))

(defcustom maple-translate-section-alist
  '((maple-translate . (phonetic basic detail)))
  "The section of translate result with different command."
  :group 'maple-translate
  :type '(alist :key-type symbol :value-type function))

(defcustom maple-translate-display-alist nil
  "The display method of translate result with different command."
  :group 'maple-translate
  :type '(alist :key-type symbol :value-type function))

(defmacro maple-translate-with(&rest body)
  "Wrap translate command with BODY."
  (declare (indent 0) (debug t))
  `(let ((engine (maple-translate--get this-command maple-translate-engine-alist))
         (section (maple-translate--get this-command maple-translate-section-alist))
         (display (maple-translate--get this-command maple-translate-display-alist)))
     ,@body))

(defun maple-translate--get(key alist)
  "Get ALIST value by KEY."
  (or (alist-get key alist) (alist-get t alist)))

(defun maple-translate-word()
  "Get translate word."
  (let ((word (string-trim (or (if (use-region-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (thing-at-point 'word t))
                               ""))))
    (if (string= word "") (read-from-minibuffer "Translate word: ") word)))

(defun maple-translate-show(word fn &optional engine sections)
  "Show translate result with FN ENGINE SECTIONS of WORD."
  (let ((result (maple-translate-result word engine sections)))
    (if (string= result "") (princ "No result found")
      (funcall fn result))))

(defun maple-translate-result(word &optional engine sections)
  "Get translate result with ENGINE SECTIONS of WORD."
  (unless engine (setq engine maple-translate-engine))

  (if (listp engine)
      (string-join (cl-loop for e in engine
                            as result = (maple-translate-result word e sections)
                            when (not (string= result ""))
                            collect (format "%s\n%s" (propertize (upcase (symbol-name e)) 'face 'font-lock-constant-face) result))
                   "\n\n")
    (let* ((fn (or (assq engine maple-translate-alist) (assq t maple-translate-alist)))
           (results (if fn (funcall (cdr fn) word))))
      (unless fn
        (error "No translate engine found"))
      (string-join (cl-loop for section in (or sections maple-translate-section)
                            as result = (alist-get section results)
                            when result
                            collect (maple-translate-section-result section (string-trim (or result ""))))
                   "\n\n"))))

(defun maple-translate-section-result(section result)
  "Get translate SECTION RESULT."
  (pcase section
    ('basic (format "基本释义:\n%s" result))
    ('detail (format "详尽释义:\n%s" result))
    ('phrase (format "组词:\n%s" result))
    ('sentence (format "例句:\n%s" result))
    ('phonetic (format "读音:\n%s" result))
    ('morphology (format "词态:\n%s" result))))

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

(defun maple-translate-posframe--hide()
  "Hide posframe."
  (remove-hook 'pre-command-hook #'maple-translate-posframe--hide t)
  (posframe-hide maple-translate-buffer))

(defun maple-translate-show-in-posframe(result)
  "Show RESULT in posframe."
  (maple-translate-posframe--hide)
  (when (posframe-workable-p)
    (add-hook 'pre-command-hook #'maple-translate-posframe--hide nil t)
    (posframe-show (with-current-buffer (get-buffer-create maple-translate-buffer)
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert result))
                     (current-buffer))
                   :position (point)
                   :internal-border-width 1)))

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
(defun maple-translate-offline(word)
  "Translate WORD and display result when offline."
  (interactive (list (maple-translate-word)))
  (maple-translate-with
    (maple-translate-show word (or display 'princ) (or engine 'sdcv) section)))

;;;###autoload
(defun maple-translate+(word)
  "Translate WORD and display result in buffer."
  (interactive (list (maple-translate-word)))
  (maple-translate-with
    (maple-translate-show word (or display 'maple-translate-show-in-buffer) engine section)))

;;;###autoload
(defun maple-translate(word)
  "Translate WORD and display result in echoarea."
  (interactive (list (maple-translate-word)))
  (maple-translate-with
    (maple-translate-show word (or display 'princ) engine section)))

(provide 'maple-translate)
;;; maple-translate.el ends here