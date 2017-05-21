;;
;; Hydra configuration
;;

(use-package hydra
  :config
  (defhydra hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset")
    ("q" nil "quit")))

(provide 'hydra-config-pvj)
