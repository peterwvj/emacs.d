;;; go-up.el --- Summary here

;;
;; TODO: Add silent flag (global variable)
;; TODO: Change 'ignore-case' to global variable
;;


;;; Commentary:
;; Package for quickly navigating to a parent folder in eshell.

;;; Code:

(defun get-sep (os)
  "Utility function to get file separator, e.g. '/', for an operating system, OS.
Argument OS a string representation of an operating
system. e.g. the value of system-type."
  (if (or (string= os "windows-nt") (string= os "ms-dos"))
      "\\"
    "/"))

(defun str-rev (str)
  "Utility function to reverse a string.
Argument STR the string to reverse."
  (apply #'string
         (reverse
          (string-to-list str))))

(defun find-parent-dir (match path sep ignore-case)
  "Find parent directory of PATH identified by MATCH.
Argument MATCH a string that identifies the parent directory to search for.
Argument PATH the source directory to search from.
Argument SEP the file separator, e.g. '/'.
Argument IGNORE-CASE non-nil if searches must ignore case."
  (if (or (not (stringp match)) (string= "" match))
      path
    (let* ((path-rev (str-rev path))
           (match-rev (str-rev match))
           (case-fold-search ignore-case)
           (idx-rev (string-match (regexp-quote match-rev) path-rev)))
      (if (not (null idx-rev))
          (let* ((path-length (length path))
                 (idx (- path-length idx-rev)))
            (find-sub-str idx path sep))
        path))))

(defun find-sub-str (i path sep)
  "Find the parent directory of PATH pointed to by I.
Argument I the PATH index to search from.
Argument SEP the file separator."
  (if (= i (length path))
      path
    (let
        ((c (string (aref path i)) ))
      (if (string= c sep)
          (substring path 0 (+ i 1))
        (find-sub-str (+ i 1) path sep)))))


(defun go-up (match &optional ignore-case)
  "Go to parent directory in eshell. This is the main function of the package.
Argument MATCH a string that identifies the parent directory to go to.
Argument IGNORE-CASE non-nil if searches must ignore case"
  (let* ((path default-directory)
         (parent-dir (find-parent-dir match path (get-sep system-type) ignore-case)))
    (eshell/cd parent-dir)))

(provide 'go-up)

;;; go-up.el ends here
