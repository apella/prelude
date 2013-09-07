;;; Setup of 'keyfreq to track command frequency.
;;; use keyfreq-show to see how many times you used a command.

(prelude-require-package 'keyfreq)

(eval-after-load 'keyfreq
  '(progn
     (keyfreq-mode 1)
     (keyfreq-autosave-mode 1)))
