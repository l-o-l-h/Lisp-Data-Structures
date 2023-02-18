;;; cl-rbt.lisp -- RED-BLACK TREES
;;; Time-stamp: <2023-02-18 12:44:07 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-09
;;; Version 0.0.2

;;; Commentary:
;; My version of Red-Black Trees based upon Chris Okasaki's
;; "Purely Functional Data Structures" 1998

;;; Code:
(in-package :lolh.utils)

;;; Invariants
;;  Invariant 1: No  red node has a red child

;;  Invariant 2:  Every path from the  root to an empty  node contains
;;  the  same  number  of  black   nodes.

;;  Taken together,  these two  invariants guarantee that  the longest
;;  possible path in a red-black  tree, one with alternating black and
;;  red  nodes, is  no  longer  than twice  as  long  as the  shortest
;;  possible path,  one with black  nodes only.

;;  Exercise: Prove that the maximinum depth  of a node in a red-black
;;  tree of size 'n' is at most 2 [ log ( n + 1 ) ]

(defclass rb-elem ()
  ((value :accessor value :initarg :value :initform :empty))
  (:documentation "Base class for an RB-TREE ELEM value.
This should be subclassed by the package that uses this RBT package."))

(defun make-rb-elem (&key (class 'rb-elem) (value 1))
  "Constructor for the RB-ELEM class instance or subclass."
  (make-instance class :value value))

(defstruct rb-color
  "A structure containing one slot, R-B, which should be either
:red or :black.  Use the MAKE-RB constructor to construct an instance
to enforce this constraint."
  
  (value :black)) ; :red | :black

(defstruct rb-tree
  "A structure containing four slots,
:color -- holds an instance of the RB-COLOR object
:left  -- holds an instance of this RB-TREE object
:elem  -- holds an instance of the RB-ELEM object
:right -- holds an instance of this RB-TREE object.
Use the MAKE-RB function to create an instance to enforce these
constraints."
  
  (color (make-rb-color))
  left
  (elem (make-instance 'rb-elem))
  right)

(defparameter +rb-empty-tree+
  (make-rb-tree :left (make-rb-tree :elem :empty)
		:elem :empty
		:right (make-rb-tree :elem :empty)))

(defun rb-empty-tree-p (rbt)
  "Returns T if RBT is empty."
  (eq +rb-empty-tree+ rbt))

(defun make-rb (rb-type value &optional (color (make-rb-color)))
  "Constructor for the RB-COLOR and RB-TREE data types.
RB-TYPE := 'color | 'tree
VALUEs of rb-color := :red | :black
VALUEs of rb-tree := rb-elem &optional color

Examples:
(make-rb 'color :red)
(make-rb 'tree (make-rb-elem :value [elem-value]))

An ERROR will be raised if something other than an RB-COLOR or RB-TREE
is attempted to be constructed.
An ERROR will be raised if something other than an RB-ELEM is used as
a value."
  
  (ecase rb-type
    (color (ecase value
		(:red (make-rb-color :value :red))
		(:black (make-rb-color :value :black))))
    (tree (etypecase value
	       (rb-elem (make-rb-tree :left +rb-empty-tree+
				      :elem value
				      :right +rb-empty-tree+
				      :color color))))))

(defmethod rb-lt ((elem rb-elem) (tree rb-tree))
  "RBT less-than procedure."
  (< (value elem) (value (rb-tree-elem tree))))

(defmethod rb-gt ((elem rb-elem) (tree rb-tree))
  "RBT greater-than procedure."
  (> (value elem) (value (rb-tree-elem tree))))

;;; Member
;;  The  'member' procedure  on  a red-black  tree  ignores the  color
;;  field.  Except  for a wildcard in  the T case, it  is identical to
;;  the 'member' procedure on unbalanced search trees.

(defun rb-member (x tree)
  "RB-MEMBER returns T if the element X is found in the tree TREE.
Otherwise it returns NIL."
  (labels ((mem-aux (x tree)
	     (cond
	       ((when (rb-empty-tree-p tree)
		  (return-from rb-member nil)))
	       ((when (rb-lt x tree)
		  (mem-aux x (rb-tree-left tree))))
	       ((when (rb-gt x tree)
		  (mem-aux x (rb-tree-right tree))))
	       (t (return-from rb-member t)))))
    (mem-aux x tree)))

;;; Insert
;;  The 'insert' function must maintain the two balance invariants.

(defun rb-insert (x s)
  (let ((y (rb-tree-elem s)))
    ()))

;;; End cl-rbt.lisp
