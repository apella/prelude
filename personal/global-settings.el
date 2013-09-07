;;; global-settings --- settings that I prefer

;; Copyright (c) 2013 Bart Spiers

;; Author: Bart Spiers
;; Version: 0.2
;; Created: 08-06-2013

;; This file is NOT part of GNU Emacs

;;; Commentary:

;; This file contains a number of general settings that I use for my
;; emacs setup. They have evolved out of my daily usage of Emacs. Some
;; you might agree with, others not. 

;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(prelude-ensure-module-deps '(google-c-style
                              window-number
                              paredit
                              iy-go-to-char
                              ;; command-frequency
                              ))
;; show the linenumbers
;; (global-linum-mode 1)

;; quick key for compilation
(global-set-key [(f5)] 'recompile)

;; open a shell at the current path
;;(global-set-key "\C-cs" 'shell)

;; always use unix line endings
;; (setq default-buffer-file-coding-system 'utf-8-unix)

;; replace string
(global-set-key "\C-c!" 'replace-string)

;; map goto-line to M-g
(global-set-key "\M-g" 'goto-line)

;; eval last sexp and print the result
(global-set-key (kbd "C-c C-j") 'eval-print-last-sexp)

;; move point to next occurance of char.
;; Can start typing immediately.
(global-set-key "\M-m" 'iy-go-to-char)
(global-set-key "\M-M" 'iy-go-to-char-backward)

;; auto-indent new lines
(define-key global-map (kbd "RET") 'newline-and-indent)

;; display the time down below
(setq display-time-24hr-format t)
(display-time-mode)

;; Change the search functions to ones that use regular expressions
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-past-before-kill t
      )

;; source: emacs redux
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t))
         )))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)




;; default with window-numbers on
;; ----------------------------------------------------------------------------

(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
  t)

(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)

(window-number-mode 1)
(window-number-meta-mode 1)

;; Start the server so we can just connect to this
(server-start)

(provide 'global-settings)
