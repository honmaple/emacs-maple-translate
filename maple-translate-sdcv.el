;;; maple-translate-sdcv.el ---  translate with sdcv.	-*- lexical-binding: t -*-

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
;; translate word with sdcv
;; stardict-* is fork from https://www.emacswiki.org/emacs/stardict.el
;;

;;; Code:
(defvar maple-translate-sdcv-dir user-emacs-directory)
(defvar maple-translate-sdcv-dicts '())
(defvar maple-translate-sdcv--init nil)
(defvar maple-translate-sdcv--cache nil)

(defun stardict-str2int (str)
  "Convert string `STR' to integer.
\x21\x22 => 0x2122"
  (let ((sum 0))
    (mapc (lambda (c)
            (setq sum (+ (* sum #x100)
                         (mod c #x100))))
          str)
    sum))

(defun stardict-open (dir name)
  "Internal function used by `stardict-open'.
`DIR' is dictionary location, `NAME' is dictionary name."
  (let ((ifo  (expand-file-name (concat name ".ifo") dir))
        (idx  (expand-file-name (concat name ".idx") dir))
        (dict (expand-file-name (concat name ".dict") dir))
        (idx-offset-bytes 4)
        (word-count 0)
        ifo-ht idx-ht)
    (unless (file-exists-p idx)
      (setq idx (concat idx ".gz")))
    (unless (file-exists-p dict)
      (setq dict (concat dict ".dz")))
    ;;(message "List %S" (list idx dict ifo))
    (unless (and (file-exists-p idx)
                 (file-exists-p dict)
                 (file-exists-p ifo))
      (error "File not found"))
    (setq ifo-ht (make-hash-table :test 'equal))
    (setq idx-ht (make-hash-table :test 'equal))
    ;; get info
    (with-temp-buffer
      (insert-file-contents ifo)
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z]+\\)=\\(.*\\)$" nil t)
        (puthash (match-string 1) (match-string 2) ifo-ht)))
    (when (gethash "idxoffsetbits" ifo-ht)
      (setq idx-offset-bytes
            (/ (string-to-number (gethash "idxoffsetbits" ifo-ht)) 8)))
    (setq word-count
          (string-to-number (gethash "wordcount" ifo-ht)))
    ;; get index
    (with-temp-buffer
      (insert-file-contents idx)
      (goto-char (point-min))
      (let ((rpt (make-progress-reporter (format "read %s index: " name) 0 (1- word-count))))
        (dotimes (i word-count)
          (progress-reporter-update rpt i)
          (let (p word offset size)
            (re-search-forward "\\([^\x00]+?\\)\x00" nil t)
            (setq p (point))

            (setq word (decode-coding-string (encode-coding-string (match-string 1) 'no-conversion) 'utf-8))
            (setq offset
                  (stardict-str2int
                   (buffer-substring-no-properties p
                                                   (+ p idx-offset-bytes))))
            (setq size
                  (stardict-str2int
                   (buffer-substring-no-properties (+ p idx-offset-bytes)
                                                   (+ p idx-offset-bytes 4))))
            (forward-char (+ idx-offset-bytes 4))
            (puthash word (cons offset size) idx-ht)
            )))
      (list ifo-ht idx-ht dict))))

(defun stardict-lookup (dict word)
  "Lookup `WORD' in `DICT', return nil when not found."
  (let ((info (gethash word (nth 1 dict)))
        (file (nth 2 dict))
        buffer
        offset size)
    (when info
      (setq offset (car info))
      (setq size (cdr info))
      ;; find any opened dict file
      (dolist (buf (buffer-list))
        (when (equal file (buffer-file-name buf))
          (setq buffer buf)))
      (if buffer
          (with-current-buffer buffer
            (buffer-substring-no-properties (byte-to-position (1+ offset))
                                            (byte-to-position (+ 1 offset size))))
        (with-temp-buffer
          (insert-file-contents (nth 2 dict) nil offset (+ offset size))
          (buffer-string))))))

(defun maple-translate-init()
  "Init sdcv dicts."
  (setq maple-translate-sdcv--cache
        (cl-loop for dicts in maple-translate-sdcv-dicts
                 collect (stardict-open (expand-file-name (cdr dicts) maple-translate-sdcv-dir) (car dicts))))
  (message "maple-translate-sdcv词典初始化成功"))

(defun maple-translate-sdcv(word)
  "Search WORD with sdcv."
  (unless (or maple-translate-sdcv--cache maple-translate-sdcv--init)
    (setq maple-translate-sdcv--init t)
    (run-with-idle-timer 0.1 nil 'maple-translate-init))
  (if (and maple-translate-sdcv--init (not maple-translate-sdcv--cache))
      (format "%s" "词典正在初始化，请稍后再试")
    (let (results)
      (dolist (dict maple-translate-sdcv--cache)
        (let ((result (stardict-lookup dict word)))
          (when result (push result results))))
      (unless (null results)
        (string-join results "\n")))))

(provide 'maple-translate-sdcv)
;;; maple-translate-sdcv.el ends here