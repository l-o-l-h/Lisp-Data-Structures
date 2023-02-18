;;; lolh-utils-packages.lisp - Packages used by lolh-utils
;;; Time-stamp: <2023-02-18 12:47:58 lolh-mbp-16>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Created: 2023-01-16
;;; Version 0.1.5

;;; Commentary:

;;; Code:

(defpackage :lolh.utils
  (:use :cl)
  (:export
;; BS-TREE
	   :*cl-bst*
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
	   :bst-height
;; RB-TREE
	   :rb-elem
	   :value
	   :make-rb-elem
	   :make-rb-color
	   :make-rb-tree
	   :rb-color-value
	   :color
	   :tree
	   :make-rb
	   :+rb-empty-tree+))


;;; End lolh-utils-packages.lisp
