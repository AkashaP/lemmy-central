(defsystem #:lemmy-central
  :author "Akasha Peppermint"
  :depends-on (#:cl-html-parse #:hunchentoot #:hunchensocket #:com.inuoe.jzon #:drakma #:cl-naive-dom #:cl-naive-dom.abt #:str #:local-time
                               #:parenscript #:sse-demo #:cl-math #:opticl #:trivial-download)
  :serial t
  :components ((:file "package")
               ;; (:file "lemmy-central")
               ))
