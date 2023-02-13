;;; lolh-utils-tests-main.lisp --- Main Lolh.Utils Test File
;;; Time-stamp: <2023-02-12 22:23:26 wlh>

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
  (is-true (rb-make 'rb-color :red))
  (is-true (rb-make 'rb-tree (make-rb-elem :value 1) :red)))

;;; lolh-utils-tests-main.lisp end of file
;;; __________________________________________________________________
