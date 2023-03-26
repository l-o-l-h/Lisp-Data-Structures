;;; lolh-filesz.lisp --- Filesz Data Structure for files and sizes
;;; Time-stamp: <2023-03-25 18:58:28 minilolh>

;;; Author: LOLH-LINC
;;; Created: 2023-03-25

;;; Commentary:
;;; This program is designed to remove duplicates and sort a list of files
;;; obtained from a directory, such as JPGs or PDFs or the like.  It removes
;;; duplicates by comparing, first, the file size (obtained from using
;;; `trivial-file-size', then, if the files are the same size, by content
;;; using the Unix procedures `cmp'.  It sorts the resulting list after
;;; duplicates have been removed and then returns just the pathnames.

;;; The procedure `filesz-load' takes a list of absolute pathnames and
;;; creates a list of `filesz' objects.
;;; The procedure `filesz-dedup' removes duplicate files from this list.
;;; The procedure `filesz-sort' sorts the remaining list of pathnames.
;;; The procedure `filesz-dedup-and-sort' calls the three fore-going
;;; procedures and returns the pathnames at the end.
;;; At this time, only the final procedures is exported.

;;; Code:

(in-package :lolh.utils)

;; FILESZ: Data structure to hold an absolute pathname and its size in bytes.
;; make-filesz
;; filesz-p FILESZ
;; filesz-path FILESZ
;; filesz-size FILESZ
;; copy-filesz FILESZ
(defstruct filesz
  (path "No Name")
  (size 0))

(defun filesz-new (path)
  "Constructor for the FILESZ data type.
Requires a PATH argument."
  (make-filesz :path path
                  :size (trivial-file-size:file-size-in-octets path)))

(defun filesz-load (path-ls)
  "Given a list of pathnames, return a list of filesz objects."
  (mapcar #'filesz-new path-ls))

(defun filesz-dedup (path-ls)
  "This procedure destructively removes duplicates from the path list
using the built-in procedure `delete-duplicates'."
  (delete-duplicates path-ls :from-end t :test #'filesz-equal))

(defun filesz-sort (path-ls)
  "The built-in (destructive) `sort' function is used to the paths list,
but the list is first de-duped using the built-in procedure
`delete-duplicates' (since `sort' is destructive).
It will be faster to implement a quicksort procedure that 
eliminates duplicates during the sort.  But maybe this is fast enough."
  (sort (filesz-dedup path-ls) #'filesz-less-than))

(defun filesz-cmp (fsz1 fsz2)
  "This procedure compares two FILESZ objects and returns
-1 if the first is less than the second in size;
 0 if the two are equal in size and content;
+1 if the first is greater than or equal in size (but if equal in size,
   it will NOT be equal in content because of the prior test)."
  (cond
    ((< (filesz-size fsz1)
        (filesz-size fsz2))
     -1)
    ((= (filesz-size fsz1)
        (filesz-size fsz2))
     (filesz-content-eq fsz1 fsz2))
    (t 1)))

(defun filesz-equal (fsz1 fsz2)
  "This procedure will return true if the two files are identical
in both size and content (`filesz-cmp' returns 0)."
  (zerop (filesz-cmp fsz1 fsz2)))

(defun filesz-less-than (fsz1 fsz2)
  "This procedure will return true only if the first file is smaller 
in size than the second (`filesz-cmp' returns -1)."
  (minusp (filesz-cmp fsz1 fsz2)))

(defun filesz-content-eq (filesz1 filesz2)
  "Compare two FILESZ objects that are equal in size by content.  Return
0 if they contain the same content (`cmp' returns 0 for a match);
1 if they contain different content (`cmp' returns 1 for no match).
These numbers work well with the `filesz-cmp' procedure.
`run-program' returns 3 values, but only the third has any meaning here."
  (third (multiple-value-list
          (eval `(uiop:run-program '("cmp" "-s"
                                     ,(uiop:unix-namestring (filesz-path filesz1))
                                     ,(uiop:unix-namestring (filesz-path filesz2)))
                                   :ignore-error-status t)))))

(defun filesz-dedup-and-sort (path-ls)
  "The main procedure to dedup and sort a list of pathnames."
  (mapcar #'filesz-path (filesz-sort (filesz-load path-ls))))

;;; lolh-filesz.lisp ends here
