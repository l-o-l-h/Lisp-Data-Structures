;;; cl-rbt.lisp -- RED-BLACK TREES
;;; Time-stamp: <2023-02-10 09:02:58 minilolh3>

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

(defclass rb-elem ()
  ((value :accessor value :initarg :value :initform :empty))
  (:documentation "Base class for an RB-TREE ELEM value.
This should be subclassed by the package that uses this RBT package."))

(defstruct rb-color
  "A structure containing one slot, R-B, which should be either
:red or :black.  Use the RB-MAKE constructor to construct an instance
to enforce this constraint.
"
  (r-b :black)) ; :red | :black

(defstruct rb-tree
  "A structure containing four slots,
:color -- holds an instance of the RB-COLOR object
:left  -- holds an instance of this RB-TREE object
:elem  -- holds an instance of the RB-ELEM object
:right -- holds an instance of this RB-TREE object.
Use the RB-MAKE function to create an instance to enforce these constraints.
"
  (color (make-rb-color))
  left
  (elem (make-instance 'rb-elem))
  right)

(defparameter +empty-rb-tree+
  (make-rb-tree :left (make-rb-tree :elem :empty)
		:elem :empty
		:right (make-rb-tree :elem :empty)))

(defun rb-make (rb-type value &optional (color (make-rb-color)))
  "Constructor for the RB-COLOR and RB-TREE data types.
RB-TYPE := 'rb-color | 'rb-tree
VALUEs of rb-color := :red | :black
VALUEs of rb-tree := rb-elem &optional color

Examples:
(rb-make 'rb-color :red)
(rb-make 'rb-tree (make-rb-elem :value [elem-value]))

An ERROR will be raised if something other than an RB-COLOR or RB-TREE
is attempted to be constructed.
An ERROR will be raised if something other than an RB-ELEM is used as
a value.
"
  (ecase rb-type
    (rb-color (ecase value
		(:red (make-rb-color :r-b :red))
		(:black (make-rb-color :r-b :black))))
    (rb-tree (etypecase value
	       (rb-elem (make-rb-tree :left +empty-rb-tree+
				      :elem value
				      :right +empty-rb-tree+
				      :color color))))))

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
