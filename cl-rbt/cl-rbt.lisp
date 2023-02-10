;;; cl-rbt.lisp
;;; Time-stamp: <2023-02-09 17:37:00 minilolh3>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-09
;;; Version 0.0.1

;;; Commentary:
;; My version of Red-Black Trees based upon Chris Okasaki's "Purely Functional Data Structures" 1998

;;; Code:
(in-package :lolh.utils)

;;; Invariants
;;  Invariant 1: No red node has a red child
;;  Invariant 2: Every path from the root to an empty node contains the same number of black nodes
;;  Taken together, these two invariants guarantee that the longest possible path in a red-black tree,
;;  one with alternating black and red nodes, is no longer than twice as long as the shortest possible
;;  path, one with black nodes only.
;;  Exercise: Prove that the maximinum depth of a node in a red-black tree of size 'n' is at most
;;  2 [ log ( n + 1 ) ]

(defstruct color
  (r-b :black)) ; :red | :black

(defstruct rb-tree
  (color (make-color))
  left
  elem
  right)

(defun rb-lt (tree-a tree-b)
  "RBT less-than procedure."
  (< (rb-tree-elem tree-a)
     (rb-tree-elem tree-b)))

(defun rb-gt (tree-a tree-b)
  "RBT greater-than procedure."
  (> (rb-tree-elem tree-a)
     (rb-tree-elem tree-b)))

(defun tree-empty-p (tree)
  "Return T if TREE is empty.  Ignore the COLOR field."
  (not (or
	(rb-tree-left tree)
	(rb-tree-right tree)
	(rb-tree-elem tree))))

;;; Member
;;  The 'member' procedure on a red-black tree ignores the color field.  Except for a wildcard in the
;;  T case, it is identical to the 'member' procedure on unbalanced search trees.

(defun rbt-member (x tree)
  "The RBT-MEMBER procedure returns T if X (an ELEM) is found in TREE."
  (cond
    ((tree-empty-p tree) nil)
    ((when (rb-lt x (rb-tree-elem tree)) (rbt-member x (rb-tree-left tree))))
    ((when (rb-gt x (rb-tree-elem tree)) (rbt-member x (rb-tree-right tree))))
    (t)))

;;; Insert
;;  The 'insert' function must maintain the two balance invariants.

(defun rbt-insert (x s)
  (let ((y (rb-tree-elem s)))
    ()))

;;; End cl-rbt.lisp
