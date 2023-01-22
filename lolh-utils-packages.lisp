;;; lolh-utils-packages.lisp - Packages used by lolh-utils
;;; Time-stamp: <2023-01-21 17:41:07 wlh>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Created: 2023-01-16
;;; Version 0.1.3

;;; Commentary:

;;; Code:

(defpackage :lolh.utils
  (:use :cl)
  (:export :*cl-bst*
	   :*cl-bst-eqs*
	   :*cl-bst-lt*
	   :*cl-bst-gt*
	   :*cl-bst-eq*
	   :make-bst-node
	   :empty-bst-node
           :bst-node-left
	   :bst-node-data
	   :bst-node-right
	   :cl-bst-set-cmp-funcs
	   :bst-insert!-node
	   :bst-delete!node
	   :bst-delete-node
	   :bst-inorder-traversal
	   :bst-preorder-traversal
	   :bst-postorder-traversal
	   :bst-find-node
	   :bst-min
	   :bst-max
	   :bst-size
	   :bst-height))


;;; End lolh-utils-packages.lisp
