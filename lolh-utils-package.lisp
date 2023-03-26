;;; lolh-utils-packages.lisp - Packages used by lolh-utils
;;; Time-stamp: <2023-03-26 03:17:39 wlh>

;;; Author: LOLH <lincolnlaw@mac.com>
;;; Created: 2023-01-16
;;; Version 0.1.6

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
	   :color
	   :tree
	   :make-rb-elem
	   :make-rb-color
	   :make-rb-tree
	   :make-rb
	   :rb-elem-value
	   :rb-color-value
	   :rb-tree-elem-value
	   :rb-tree-elem
	   :rb-tree-left
	   :rb-tree-right
	   :rb-tree-color
	   :rb-tree-values
	   :rb-insert
	   :+rb-red+
	   :+rb-black+
	   :+rb-empty-tree+
           :rb-black-p
	   :rb-red-p
;; LOLH-FILESZ           
           :filesz-load-and-dedup
           ))


;;; End lolh-utils-packages.lisp
