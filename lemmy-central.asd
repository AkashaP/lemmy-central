(defsystem #:lemmy-central
  :author "Akasha Peppermint"
  :depends-on (#:cl-html-parse #:hunchentoot #:com.inuoe.jzon #:drakma #:cl-naive-dom #:cl-naive-dom.abt #:str #:local-time)
  :serial t
  :components ((:file "package")
               (:file "lemmy-central")))
