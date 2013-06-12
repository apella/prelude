(prelude-ensure-module-deps '(google-c-style
                              window-number-mode
                              command-frequency
                              ))
;; show the linenumbers
(global-linum-mode 1)

;; quick key for compilation
(global-set-key [(f5)] 'recompile)

;; open a shell at the current path
;;(global-set-key "\C-cs" 'shell)

;; always use unix line endings
(setq default-buffer-file-coding-system 'utf-8-unix)

;; replace string
(global-set-key "\C-c!" 'replace-string)

;; map goto-line to M-g
(global-set-key "\M-g" 'goto-line)

;; eval last sexp and print the result
(global-set-key (kbd "C-c C-j") 'eval-print-last-sexp)

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
)


;; google c-style
(eval-after-load 'google-c-style
  (add-hook 'c-mode-common-hook 'google-set-c-style))


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

(provide 'global-settings)
