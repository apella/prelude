;;; loads the emms module


(add-to-list 'load-path "~/emms/lisp")
(require 'emms-setup)
(emms-all)
(emms-default-players)
(global-set-key (kbd "<f2>") 'emms-smart-browse)
(setq emms-source-file-default-directory "~/Dropbox/Music/")

;; when you have compiled emms-print-metadata we can use that to gather the
;; metadata for in the library
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

;; use the volume controls of ems
(require 'emms-volume)
