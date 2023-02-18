;;; lolh-utils-tests-main.lisp --- Main Lolh.Utils Test File
;;; Time-stamp: <2023-02-18 12:47:06 lolh-mbp-16>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-02-12
;;; Version: 0.0.1
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
  (finishes (defparameter rb-tree-default (make-rb-tree))))

;;; lolh-utils-tests-main.lisp end of file
;;; __________________________________________________________________
