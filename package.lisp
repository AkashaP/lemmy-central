(defpackage :lemmy-central
  (:use :cl)
  (:export
   :load-library
   :unload-library
   :unlink-queue
   :open-queue
   :close-queue 
   :with-open-queue
   :send-message-raw
   :receive-message-raw
   :send-message
   :receive-message
   :f-message
   :send-message-timed
   :receive-message-timed
   :flush
   
   :send-message-raw-timed 
   :receive-message-raw-timed
   :get-attributes 
   :set-attributes
   :non-blocking-p
   :set-non-blocking
   :with-unblocked))

(in-package :lemmy-central)
#+sbcl (import 'sb-ext:add-package-local-nickname)
(add-package-local-nickname :nd :cl-naive-dom :lemmy-central)
(add-package-local-nickname :json :com.inuoe.jzon :lemmy-central)
(add-package-local-nickname :time :local-time :lemmy-central)
(add-package-local-nickname :nd.abt :cl-naive-dom.abt :lemmy-central)
