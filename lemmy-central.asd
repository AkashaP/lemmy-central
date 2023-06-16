(defsystem #:lemmy-central
  :author "Akasha Peppermint"
  :depends-on (#:cl-html-parse #:hunchentoot #:com.inuoe.jzon #:drakma #:cl-naive-dom #:cl-naive-dom.abt #:str)
  :local-nicknames ((:nd #:cl-naive-dom)
                    (:json #:com.inuoe.jzon)
                    (:time #:local-time)
                    (:nd.abt #:cl-naive-dom.abt))
  :serial t
  :components ((:file "lemmy-central")))
