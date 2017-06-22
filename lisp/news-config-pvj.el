
;;
;; News feeds
;;

(use-package elfeed
  :bind
  (:map elfeed-search-mode-map
        ("U" . elfeed-search-fetch))
  :config
  (setq elfeed-feeds
        '(("https://newz.dk/rss" it)
          ("http://planet.emacsen.org/atom.xml" emacs))))

(provide 'news-config-pvj)
