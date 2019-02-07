;;
;; Hydra configuration
;;

(use-package hydra
  :bind
  (("C-c j" . hydra-dumb-jump/body)
   ("C-c f" . hydra-flycheck/body))
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

  (defhydra hydra-kill-whole-line (global-map "C-c" :columns 1)
    "kill-line"
    ("d" pvj/smart-kill-whole-line))
  
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("g" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("q"  nil)))

(provide 'hydra-config-pvj)
