;;; lolh-utils.asd
;;; Time-stamp: <2023-03-25 14:38:52 minilolh>

;;; Author: LOLH-LINC <lincolnlaw@mac.com>
;;; Created: 2023-01-12
;;; Version: 0.1.10

;;; Commentary
;;  The system 'lolh-utils' handles binary search trees (cl-bst)
;;  and red-black trees (cl-rbt).
;;  It also handles the cl-filesz data structure, which holds pathnames
;;  and compares them for size and equality.

;;  To test the system, run the ASDF command
;;  (asdf:test-sytem "lolh-utils")

;;; Code

(defsystem "lolh-utils"
  :description "My version of a binary search tree."
  :version "0.1.9"
  :author "LOLH <lincolnlaw@mac.com"
  :license "CCO 1.0 Universal"
  :depends-on ("trivial-file-size")
  :components ((:file "lolh-utils-package")
	       (:module "cl-bst" :depends-on ("lolh-utils-package")
		:components ((:file "cl-bst")))
	       (:module "cl-rbt" :depends-on ("lolh-utils-package")
		:components ((:file "cl-rbt")
			     (:file "cl-rbt-definitions"
			      :depends-on ("cl-rbt"))))
               (:module "cl-filesz" :depends-on ("lolh-utils-package")
                :components ((:file "cl-filesz"))))
  :in-order-to ((test-op (test-op "lolh-utils/tests"))))

(defsystem "lolh-utils/tests"
  :description "Tests for the Lolh-Utils system."
  :version "0.0.1"
  :author "LOLH-LINC <lincolnlaw@mac.com>"
  :license "CCO 1.0 Universal"
  :depends-on ("fiveam" "lolh-utils")
  :components ((:module "t"
		:components ((:file "lolh-utils-tests-package")
			     (:file "lolh-utils-tests-main"
			      :depends-on ("lolh-utils-tests-package")))))
  :perform (test-op (o c) (symbol-call :lolh.utils.tests :test-lolh-utils)))

;;; lolh-utils.asd ends here
;;; ------------------------------------------------------------------
