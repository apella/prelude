;;; loads the emms module


(add-to-list 'load-path "~/emms/lisp")
(require 'emms-setup)
(emms-all)
(emms-default-players)
(global-set-key (kbd "<f2>") 'emms-smart-browse)

;; when you have compiled emms-print-metadata we can use that to gather the metadata for in the library
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))
