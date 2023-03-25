;;; cl-rbt.lisp -- RED-BLACK TREES
;;; Time-stamp: <2023-03-25 12:16:50 minilolh>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-09
;;; Version 0.0.3

;;; Commentary:
;; My version of Red-Black Trees based upon Chris Okasaki's
;; "Purely Functional Data Structures" 1998

;;; Code:
(in-package :lolh.utils)

;;; Invariants
;;  Invariant 1: No red node has a red child

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

(defun rb-elem-value (rb-elem)
  (value rb-elem))

(defun make-rb-elem (&key (class 'rb-elem) (value 1))
  "Constructor for the RB-ELEM class instance or subclass."
  (make-instance class :value value))

(defstruct rb-color
  "A structure containing one slot, R-B, which should be either
:red or :black.  Use the MAKE-RB constructor to construct an instance
to enforce this constraint."
  
  (value :black)) ; :red | :black

(defparameter +rb-red+ (make-rb-color :value :red))
(defparameter +rb-black+ (make-rb-color :value :black))

(defun rb-black-p (rb-col)
  (eq +rb-black+ rb-col))
(defun rb-red-p (rb-col)
  (eq +rb-red+ rb-col))

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

;; (defconstant +rb-empty-tree+
;;   (make-rb-tree :left (make-rb-tree :elem :empty)
;; 		:elem :empty
;; 		:right (make-rb-tree :elem :empty)))

(defparameter +rb-empty-tree+
  (make-rb-tree :elem :empty))

(defun rb-empty-tree-p (rbt)
  "Returns T if RBT is empty."
  (eq +rb-empty-tree+ rbt))

(defun make-rb (rb-type value &optional (color :black))
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
		(:red +rb-red+)
		(:black +rb-black+)))
    (tree (etypecase value
	       (rb-elem (make-rb-tree :left +rb-empty-tree+
				      :elem value
				      :right +rb-empty-tree+
				      :color (make-rb 'color color)))))))

(defun rb-tree-elem-value (rb-tree)
  (value (rb-tree-elem rb-tree)))

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

(defun rb-member (x rb-tree)
  "RB-MEMBER returns T if the element X is found in the tree RB-TREE.
Otherwise it returns NIL."
  (labels ((mem-aux (x rb-tree)
	     (cond
	       ((when (rb-empty-tree-p rb-tree)
		  (return-from rb-member nil)))
	       ((when (rb-lt x rb-tree)
		  (mem-aux x (rb-tree-left rb-tree))))
	       ((when (rb-gt x rb-tree)
		  (mem-aux x (rb-tree-right rb-tree))))
	       (t (return-from rb-member t)))))
    (mem-aux x rb-tree)))

(defun rb-tree-values (rb-tree)
  "Given an RB-TREE, return the values of the four slots as multiple values."
  (let ((c (rb-tree-color rb-tree))
	(l (rb-tree-left rb-tree))
	(r (rb-tree-right rb-tree))
	(e (rb-tree-elem rb-tree)))
    (values c l e r)))

(defun rb-balance-default-tree (rb-c rb-l rb-e rb-r)
  "Auxiliary function for rb-balance to make a default tree when
the current pattern maintains the invariants."
  (make-rb-tree :color rb-c
		:left rb-l
		:elem rb-e
		:right rb-r))

(defun rb-balance-new-tree (a x b y c z d)
  "An auxiliary procedure for rb-balance to create a new tree
structure with the correct RED-BLACK pattern of RED ancestor with
two BLACK sibling descendants."
  (make-rb-tree :color +rb-red+
		:left (make-rb-tree :color +rb-black+
				    :left a
				    :elem x
				    :right b)
		:elem y
		:right (make-rb-tree :color +rb-black+
				     :left c
				     :elem z
				     :right d)))

(defun rb-balance (rb-c rb-l rb-e rb-r)
  "The Red-Black Balance procedure.

This procedure checks for a pattern containing a BLACK node containing
two RED descendant nodes.

It uses the auxiliary procedure RB-BALANCE-NEW-TREE when such a pattern
is found.  It uses the auxiliary procedure RB-BALANCE-DEFAULT-TREE when
the pattern maintains the invariants."
  (if (rb-black-p rb-c)
      (cond
	((and (rb-red-p rb-l)
	      (rb-red-p (rb-tree-left rb-l)))
	 (multiple-value-bind (lc lt y c) (rb-tree-values rb-l)
	   (declare (ignore lc))
	   (multiple-value-bind (llc a x b) (rb-tree-values lt)
	     (declare (ignore llc))
	     (rb-balance-new-tree a x b y c rb-e rb-r))))
	((and (rb-red-p rb-l)
	      (rb-red-p (rb-tree-right rb-l)))
	 (multiple-value-bind (lc a x rt) (rb-tree-values rb-l)
	   (declare (ignore lc))
	   (multiple-value-bind (lrc b y c) (rb-tree-values rt)
	     (declare (ignore lrc))
	     (rb-balance-new-tree a x b y c rb-e rb-r))))
	((and (rb-red-p rb-r)
	      (rb-red-p (rb-tree-left rb-r)))
	 (multiple-value-bind (lc lt z d) (rb-tree-values rb-r)
	   (declare (ignore lc))
	   (multiple-value-bind (rlc b y c) (rb-tree-values lt)
	     (declare (ignore rlc))
	     (rb-balance-new-tree rb-l rb-e b y c z d))))
	((and (rb-red-p rb-r)
	      (rb-red-p (rb-tree-right rb-r)))
	 (multiple-value-bind (lc b y rt) (rb-tree-values rb-r)
	   (declare (ignore lc))
	   (multiple-value-bind (rrc c z d) (rb-tree-values rt)
	     (declare (ignore rrc))
	     (rb-balance-new-tree rb-l rb-e b y c z d))))
	(t (rb-balance-default-tree rb-c rb-l rb-e rb-r)))
      (rb-balance-default-tree rb-c rb-l rb-e rb-r)))

;;; Insert
;;  The 'insert' function must maintain the two balance invariants.

(defun rb-insert (x s)
  "A prcoedure to insert X, an ELEM, into S, an RB-TREE, while maintaining
the two invariants:

Invariant 1: No red node has a red child

Invariant 2:  Every path from the  root to an empty  node contains
the  same  number  of  black   nodes.

It uses the auxiloary procedure RB-BALANCE."
  (labels ((ins (rb-tree)
	     (if (rb-empty-tree-p rb-tree)
		 (make-rb-tree :color +rb-red+
			       :left +rb-empty-tree+
			       :elem x
			       :right +rb-empty-tree+)
		 (multiple-value-bind (color a y b)
		     (rb-tree-values rb-tree)
		   (if (rb-lt x rb-tree)
		       (rb-balance color (ins a) y b)
		       (if (rb-gt x rb-tree)
			   (rb-balance color a y (ins b))
			   rb-tree))))))
    (let ((rb-new-tree (ins s)))
      (multiple-value-bind (cc a y b) (rb-tree-values rb-new-tree)
	(declare (ignore cc))
	(make-rb-tree :color +rb-black+
		      :left a
		      :elem y
		      :right b)))))

;;; End cl-rbt.lisp
