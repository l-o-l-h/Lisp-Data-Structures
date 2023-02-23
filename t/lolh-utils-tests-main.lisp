;;; lolh-utils-tests-main.lisp --- Main Lolh.Utils Test File
;;; Time-stamp: <2023-02-23 02:16:51 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-12
;;; Version: 0.0.2
;;; License

;;; Commentary:

;;; Code:

(in-package :lolh.utils.tests)

(def-suite all-tests
  :description "Suite of all tests.")

(in-suite all-tests)

(defun test-lolh-utils ()
  "Main procedure to run all tests for the Lolh-Utils system."
  (run! 'all-tests))

(test dummy-test
  "Placeholder to check that the test system is operational."
  (is (eq t t))
  (is (not (eq nil t))))

(test rb-elem
  (is-true (make-instance 'rb-elem :value 1))
  (is-true (make-rb-elem :class 'rb-elem :value 1))
  (is-true (make-rb 'color :red))
  (is-true (make-rb 'tree (make-rb-elem :value 1) :red)))

(def-suite test-rb-elem
  :description "Tests for the class 'rb-elem"
  :in all-tests)

(in-suite test-rb-elem)

(test make-rb-elem
  (finishes (defparameter rb-elem-empty (make-instance 'rb-elem)))
  (finishes (describe rb-elem-empty))
  (finishes (defparameter rb-elem-1 (make-rb-elem :value 1)))
  (finishes (describe rb-elem-1))
  (is (eql 1 (value rb-elem-1))))

(test rb-color
  (finishes (defparameter rb-color-default (make-rb-color)))
  (finishes (describe rb-color-default))
  (finishes (defparameter rb-color-red (make-rb-color :value :red)))
  (finishes (describe rb-color-red))
  (is (eql :red (rb-color-value rb-color-red))))

(test rb-tree
  (finishes (defparameter rb-tree-default (make-rb-tree)))
  (finishes (describe rb-tree-default))
  (finishes (defparameter rb-tree-1 (make-rb-tree
				     :color :red
				     :left (make-rb-tree :color :black)
				     :elem (make-rb-elem :value 2)
				     :right (make-rb-tree :color :red))))
  (finishes (describe rb-tree-1)))

(test make-rb
  (finishes (defparameter make-rb-color-def (make-rb 'color :black)))
  (finishes (describe make-rb-color-def))
  (signals type-error (defparameter make-rb-color-def2 (make-rb 'color :purple)))
  (finishes (defparameter make-rb-tree-def (make-rb 'tree (make-rb-elem))))
  (finishes (describe make-rb-tree-def))
  (finishes (defparameter make-rb-tree-1 (make-rb 'tree (make-rb-elem :value 1) :red)))
  (signals type-error (defparameter wrong-tree (make-rb 'shrub 1)))
  (is (eql 1 (value (rb-tree-elem make-rb-tree-1))))
  (is (eql 1 (rb-tree-elem-value make-rb-tree-1)))
  (finishes (defparameter rb-tree-values (multiple-value-list (rb-tree-values make-rb-tree-1))))
  (finishes (describe rb-tree-values)))

(def-suite test-rb-balance
  :description "Tests for the balance procedure."
  :in all-tests)

(in-suite test-rb-balance)

(test basic-rb-balance
  (finishes (defparameter e1 (make-rb-elem :value 50)))
  (finishes (defparameter t1 (make-rb 'tree e1)))
  (is (eql 50 (rb-tree-elem-value t1)))
  (finishes (describe t1))
  (finishes (defparameter e2 (make-rb-elem :value 10)))
  (finishes (defparameter e3 (make-rb-elem :value 80)))
  (finishes (defparameter t2 (rb-insert e3 (rb-insert e2 t1))))
  (is (eql 50 (rb-tree-elem-value t2)))
  (finishes (describe (rb-tree-elem-value t2))))

;;; lolh-utils-tests-main.lisp end of file
;;; __________________________________________________________________
