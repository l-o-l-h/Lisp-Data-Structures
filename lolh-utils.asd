(defsystem "lolh-utils"
  :description "My version of a binary search tree."
  :version "0.1.3"
  :author "LOLH <lincolnlaw@mac.com"
  :license "mine"
  :depends-on ()
  :components ((:file "lolh-utils-packages")
	       (:file "./cl-bst/cl-bst" :depends-on ("lolh-utils-packages"))))
