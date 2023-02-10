(defsystem "lolh-utils"
  :description "My version of a binary search tree."
  :version "0.1.4"
  :author "LOLH <lincolnlaw@mac.com"
  :license "mine"
  :depends-on ()
  :components ((:file "lolh-utils-package")
	       (:module "cl-bst" :depends-on ("lolh-utils-package") :components ((:file "cl-bst")))
	       (:module "cl-rbt" :depends-on ("lolh-utils-package") :components ((:file "cl-rbt")
										 (:file "cl-rbt-definitions"
										  :depends-on ("cl-rbt"))))))
