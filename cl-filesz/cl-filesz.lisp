;;; cl-filesz.lisp --- Filesz Data Structure for files and sizes
;;; Time-stamp: <2023-03-25 14:25:33 minilolh>

;;; Author: LOLH-LINC
;;; Created: 2023-03-25

;;; Commentary:

;;; Code:

(in-package :lolh.utils)

;; CL-FILESZ: Data structure to hold an absolute pathname and its size in bytes.
;; make-cl-filesz
;; cl-filesz-p FILESZ
;; cl-filesz-path FILESZ
;; cl-filesz-size FILESZ
;; copy-cl-filesz FILESZ
(defstruct cl-filesz
  (path "No Name")
  (size 0))

(defun cl-filesz-new (path)
  "Constructor for the CL-FILESZ data type.
Requires a PATH argument."
  (let ((new-file (make-cl-filesz :path path
                                  :size (trivial-file-size:file-size-in-octets path))))
    new-file))

(defun cl-filesz-load (path-ls)
  "Given a list of pathnames, return a list of cl-filesz objects."
  (mapcar #'new-cl-filesz path-ls))

(defun cl-filesz-cmp (filesz1 filesz2)
  "Compare two CL-FILESZ objects and return
-1 if the first is less than the second in size;
 0 if the two are equal in size and content;
+1 if the first is greater than or equal in size."
  (cond
    ((< (cl-filesz-size filesz1)
        (cl-filesz-size filesz2))
     -1)
    ((= (cl-filesz-size filesz1)
        (cl-filesz-size filesz2))
     (cl-filesz-eq filesz1 filesz2))
    (t 1)))

(defun cl-filesz-eq (filesz1 filesz2)
  "Compare two CL-FILESZ objects for equality in both size and content.  Return
0 if they are the same size and contain the same content;
1 if they are the same size but contain differnt content."
  (third (multiple-value-list
          (eval `(uiop:run-program '("cmp" "-s"
                                     ,(uiop:unix-namestring (cl-filesz-path filesz1))
                                     ,(uiop:unix-namestring (cl-filesz-path filesz2)))
                                   :ignore-error-status t)))))
;;; cl-filesz.lisp ends here
