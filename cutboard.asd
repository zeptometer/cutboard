(asdf:defsystem :cutboard
    :description "tools for development fofr retrokitchen architecture"
    :version "20150509"
    :author "Yuito Murase"
    :license "MIT"
    :depends-on (:iterate)
    :components ((:file "compiler")
                 (:file "simulator")))
