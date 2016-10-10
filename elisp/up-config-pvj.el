
;;
;; Using "/" is Linux/MacOSX specific
;;

(defun str-rev (str)
  "Reverse string"
  (apply #'string 
         (reverse 
          (string-to-list str))))

(defun up-find-parent-dir (match)
  (if (or (not (stringp match)) (string= "" match))
      default-directory
    (let* ((path-rev (str-rev default-directory))
           (match-rev (str-rev match))
           (idx-rev (string-match (regexp-quote match-rev) path-rev)))
      (if (not (null idx-rev))
          (let* ((path-length (length default-directory))
                (idx (- path-length idx-rev)))
            (find-sub-str idx))
        default-directory))))
    
(defun find-sub-str (i)
  (if (= i (length default-directory))
      default-directory
    (let 
        ((c (string (aref default-directory i)) ))
      (if (string= c "/")
          (substring default-directory 0 (+ i 1))
        (find-sub-str (+ i 1))))))


(defun up-goto-dir (match)
  (let ((parent-dir (up-find-parent-dir match)))
    (eshell/cd parent-dir)))

(provide 'up-config-pvj)
