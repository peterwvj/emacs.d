;;
;; Hydra configuration
;;

(use-package hydra
  :bind
  (("C-c j" . hydra-dumb-jump/body))
  :config
  (defhydra hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset")
    ("q" nil "quit"))

  (defhydra hydra-window-resize ()
    "resize windows"
    ("<down>" shrink-window "shrink vertically")
    ("<up>" enlarge-window "enlarge vertically")
    ("<left>" shrink-window-horizontally "shrink horizontally")
    ("<right>" enlarge-window-horizontally "enlarge vertically")
    ("j" ace-window "jump to window")
    ("b" balance-windows "balance windows")
    ("q" nil "quit"))

  (defhydra hydra-dumb-jump ()
    "dumb jump"
    ("g" dumb-jump-go "go")
    ("b" dumb-jump-back "back")
    ("l" dumb-jump-quick-look "look")
    ("e" dumb-jump-go-prefer-external "external")
    ("w" dumb-jump-go-other-window "window")
    ("q" nil "Quit"))

  (defhydra hydra-expand (global-map "C-c" :columns 1)
    "expand-region"
    ("c" er/contract-region "Contract" :bind nil)
    ("e" er/expand-region "Expand")))

(provide 'hydra-config-pvj)
