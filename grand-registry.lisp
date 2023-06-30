(in-package :lemmy-central)
;; (defvar *errors* (make-array 100 :adjustable t :initial-element nil))

;; (defmacro dispatch-issue (issue env)
;;   `(progn
;;      (cl-destruction:nrotate-right *errors* 1)
;;      (setf (aref *errors* 0) (lambda () (declare (optimize (debug 3))) (error "Error")))))

;; (defmethod handle-issue :before (issue env)
;;   (cl-destruction:nrotate-right *errors* 1)
;;   (setf (aref *errors* 0) (lambda () (error "Error"))))

(defclass lemmy-instance ()
  ((anchor :initarg :anchor :accessor anchor)
   (image :initarg :image :accessor image)
   (header :initarg :header :accessor header)
   ;; (users-per-month :initarg :users-per-month :accessor users-per-month)
   (nsfw-graded :initarg :jnsfw-graded :accessor nsfw-graded :initform nil)
   (join-desc :initarg :join-desc :accessor join-desc)
   (posts :initarg :posts :accessor posts :initform (make-array 100 :adjustable t :initial-element nil))
   (poll-condition :accessor poll-condition :initform (bt2:make-condition-variable :name "poll condition"))
   (consequtive-fails :accessor consequtive-fails :initform (bt2:make-atomic-integer))))

(defmethod handle-issue (issue env)
  (print (format nil "ISSUE RAISED ~a ~a" issue env)))

(defmethod handle-issue ((issue (eql 'inconsistent-instance-scraping)) env)
  (print (list issue :issue issue :env env)))

(defmethod handle-issue ((issue (eql 'bad-anchor)) env)
  (print (list issue :issue issue :env env)))

(defmethod handle-issue ((issue (eql 'bad-heading)) env)
  (print (list issue :issue issue :env env)))


(defmethod raise-issue (issue env)
  (handle-issue issue env))

;; (defun run-through-errors ()
;;   (restart-case (loop for error across *errors*
;;                       for i from 0
;;                       do (funcall error) (setf (aref *errors* i) nil))
;;     (skip () "Got bored, handle it later")))

(defvar *long-instance-register-keep-running-p* t)

(defvar *active-instances* (make-hash-table :synchronized t :test #'equalp))
#++(defvar *active-instances-test* (alexandria:copy-hash-table *active-instances* ))
(defvar *ignored-instances* (make-hash-table :synchronized t :test #'equalp))
(defvar *all-instances* (make-hash-table :synchronized t :test #'equalp))
(defvar *anon-instances* (make-hash-table :synchronized t :test #'equalp))
(defvar *default-instances* (make-hash-table :synchronized t :test #'equalp))

;; not working
(defvar *potentially-expired-instances* (make-hash-table :synchronized t :test #'equalp))
(defvar *server* nil)

(defclass image ()
  ((anchor :initarg :anchor :accessor anchor)
   (thumbnail-b64 :initarg :thumbnail-b64 :accessor thumbnail-b64)
   (thumbnail :initarg :thumbnail :accessor thumbnail)))

;; URL to image instance
(defvar *image-cache* (make-hash-table :synchronized t :test #'equalp))
(defvar *image-ct* (bt2:make-atomic-integer))
(defvar *image-indices* (make-hash-table :synchronized t))

;; (defun ensure-cached-image (image)
;;   (if (gethash image *image-cache*)
;;       (gethash image *image-cache*)
;;       (let* ((b64 (handler-case (cl-base64:string-to-base64-string (flex:octets-to-string (drakma:http-request image)))
;;                     (error () (return-from ensure-cached-image nil))))
;;              (id (bt2:atomic-integer-incf *image-ct*))
;;              (instance (make-instance 'image :anchor image
;;                                              :thumbnail-b64 b64
;;                                              :thumbnail nil)))
;;         (setf (gethash image *image-cache*) instance)
;;         (setf (gethash id *image-indices*) image)

;;         (alexandria:when-let ((old-image (gethash (- id 5000) *image-indices*)))
;;           (alexandria:when-let ((old (remhash old-image *image-indices*)))
;;             (remhash old *image-cache*)))

;;         image)))

(defun read-file (path)
  (declare (type (or pathname string) path))
  (with-open-file (file path :element-type 'flex:octet)
    (let ((data (make-array (file-length file) :element-type 'flex:octet)))
      (read-sequence data file)
      data)))

(defun thumbnailify (url output-dir)
  (let* ((filepath (concatenate 'string output-dir (string (gensym))))
         (filepath-output (concatenate 'string output-dir (string (gensym)) ".webp")))
    (trivial-download:download url filepath)
    (unwind-protect
         (progn
           (with-standard-io-syntax
             (print (format nil "converting ~a" url)))
           (uiop:run-program (concatenate 'string "convert " filepath " -resize 200x200^ -gravity center -extent 200x200 " filepath-output))
           (base64:string-to-base64-string (flex:octets-to-string (read-file filepath-output))))
      (delete-file filepath)
      (delete-file filepath-output))))

(defun ensure-cached-image (post)  
  (alexandria:when-let ((image (gethash "url" post)))
    (if (or (equalp image "")
            (equalp image 'null)
            (equalp image #'null)
            (not (stringp image)))
        (return-from ensure-cached-image nil))
    
    (if (gethash image *image-cache*)
        (gethash image *image-cache*)
        
        (let ((img image))

          (if (stringp img)
              (if (and (not (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp).*" img))
                       (stringp (gethash "thumbnail_url" post)))
                  (if (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp).*" (gethash "thumbnail_url" post))
                      (setf img (gethash "thumbnail_url" post)))))
          
          (ignore-errors
           (when (and (stringp img)
                      (not (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp|mp4|webm|ogv).*" img)))
             (let* ((link (drakma:http-request img))
                    (html (html-parse:parse-html link)))
               (labels ((recurse (list)
                          (loop for elements in list
                                if (listp elements)
                                  do (if (subsetp '(:META :PROPERTY "og:image" :CONTENT) elements :test #'equalp)
                                         (setq img (getf (cdr elements) :CONTENT))
                                         (recurse elements)))))
                 (recurse html)))))
          
          (let (b64)
            (if (ignore-errors (setq b64 (thumbnailify img "home/server1/tmpfs/")))
                (let ((id (bt2:atomic-integer-incf *image-ct*))
                      (instance (make-instance 'image :anchor image
                                                      :thumbnail-b64 b64
                                                      :thumbnail nil)))
                  (setf (gethash image *image-cache*) instance)
                  (setf (gethash id *image-indices*) image)
                  
                  (alexandria:when-let ((old-image (gethash (- id 5000) *image-indices*)))
                    (alexandria:when-let ((old (remhash old-image *image-indices*)))
                      (remhash old *image-cache*)))

                  instance)))))))

(defun uncache-image-by-url (url)
  (remhash url *image-cache*)
  (loop for k being the hash-keys of *image-indices*
          using (hash-value v)
        if (equalp url v)
          do (remhash k *image-cache*)))

(defun verify-instance-scrape-validity (anchor image header join-desc)
  (block nil
    (when (or (null anchor)
              (equalp "" anchor)
              ;; URLp. etc.
              )
      (raise-issue 'bad-anchor (list :anchor anchor :image image :header header :join-desc join-desc))
      (return))
    (when (or (null header)
              (equalp "" header)
              ;; safe instance name, etc.
              )
      (raise-issue 'inconsistent-instance-scraping (list :anchor anchor :image image :header header :join-desc join-desc))
      (return))
    t))

(defun instance-equalp (a b)
  (and (equalp (anchor a) (anchor b))
       (equalp (header a) (header b))))

(defvar *instance-ct* (bt2:make-atomic-integer))

(defun %poll-scrape-all-instances-2 ()
  "Scrape the join-lemmy.org website for lemmy instances"
  (let* ((html (drakma:http-request "https://join-lemmy.org/instances"))
         (sexp (cl-html-parse:parse-html html))
         (container 
           ;; First find the container for the instances
           (block block
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (equalp '(:DIV :CLASS "container") elements)
                                       (return-from block list)
                                       (recurse elements)))))
               (recurse sexp))))
         ;; Then scrape for anchor tags
         (anchors
           (let ((anchors (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '(:A :CLASS "button primary" :HREF) elements :test #'equalp)
                                       (push (getf (cdr elements) :HREF) anchors)
                                       (recurse elements)))))
               (recurse container))
             anchors))
         ;; ... and other stuff
         (images
           (let ((images (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '(:IMG :CLASS "join-banner" :SRC) elements :test #'equalp)
                                       (push (getf (cdr elements) :SRC) images)
                                       (recurse elements)))))
               (recurse container))
             images))
         (headers
           (let ((headers (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '((:H4 :CLASS "col")) elements :test #'equalp)
                                       (push (second elements) headers)
                                       (recurse elements)))))
               (recurse container))
             headers))
         (join-desc
           (let ((join-desc (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '((:P :CLASS "join-desc")) elements :test #'equalp)
                                       (push (second elements) join-desc)
                                       (recurse elements)))))
               (recurse container))
             join-desc)))
    (values anchors images headers join-desc)))

(defun register-instances ()
  (multiple-value-bind (anchors images headers join-desc)
      (%poll-scrape-all-instances-2)
    (unless (= (length anchors) (length images) (length headers) (length join-desc))
      (handle-issue 'inconsistent-instance-scraping (list :anchor anchors :image images :header headers :join-desc join-desc))
      (return-from register-instances))

    ;; (clrhash *active-instances*)
    ;; (clrhash *ignored-instances*)
    ;; (clrhash *all-instances*)

    ;; Register all instances
    ;; (loop for (anchor . rest1) on anchors
    ;;       for (image . rest2) on images
    ;;       for (header . rest3) on headers
    ;;       for (join-desc . rest4) on join-desc
    ;;       for i from 0
    ;;       do (setf (gethash (format nil "~a-~a" i header) *all-instances*)
    ;;                (make-instance 'lemmy-instance :anchor (or anchor "") :image (or image "") :header (or header "") :join-desc (or join-desc ""))))
    ;; (loop for (anchor . rest1) on anchors
    ;;       for (image . rest2) on images
    ;;       for (header . rest3) on headers
    ;;       for (join-desc . rest4) on join-desc
    ;;       for i from 0
    ;;       do (setf (gethash (format nil "~a-~a" i header) *all-instances*)
    ;;                (make-instance 'lemmy-instance :anchor (or anchor "") :image (or image "") :header (or header "") :join-desc (or join-desc ""))))

    ;; ;; Register active and ignored instances
    ;; (loop for id being the hash-key of *all-instances*
    ;;         using (hash-value instance)
    ;;       if (verify-instance-scrape-validity (anchor instance) (image instance) (header instance) (join-desc instance))
    ;;         do (setf (gethash id *active-instances*) instance)
    ;;       else
    ;;         do (setf (gethash id *ignored-instances*) instance))

    (loop with untouched = (alexandria:hash-table-keys *all-instances*)
          for (anchor . rest1) on anchors
          for (image . rest2) on images
          for (header . rest3) on headers
          for (join-desc . rest4) on join-desc
          do (if (verify-instance-scrape-validity anchor image header join-desc)
                 ;; Search for existing
                 (loop for h being the hash-keys of *all-instances*
                         using (hash-value existing)
                       if (and (equalp anchor (anchor existing))
                               (equalp header (header existing)))
                         do (setf (image existing) image
                                  (join-desc existing) join-desc
                                  untouched (delete h untouched :test #'equalp))
                            (return)
                       finally (setf (gethash (format nil "~a-~a" (bt2:atomic-integer-incf *instance-ct*) header) *all-instances*)
                                     (make-instance 'lemmy-instance :anchor (or anchor "") :image (or image "") :header (or header "") :join-desc (or join-desc ""))))
                 ())
          finally
             ;; Set potentially expired instances
             ;; (when untouched
             ;;   (loop for untouch in untouched
             ;;         do (setf (gethash untouch *potentially-expired-instances*)
             ;;                  (gethash untouch *all-instances*))))
             ;; Set active instances
             (loop for h being the hash-keys of *all-instances*
                     using (hash-value instance)
                   do (unless (gethash h *potentially-expired-instances*)
                        (setf (gethash h *active-instances*) instance)))

             ;; Set default instances
             (loop for h being the hash-keys of *default-instances*
                   if (not (gethash h *active-instances*))
                     do (remhash h *default-instances*))
             ;; Grade instances ourselves for NSFW
             (loop for h being the hash-keys of *active-instances*
                     using (hash-value instance)
                   do (with-slots (anchor image header join-desc nsfw-graded) instance
                        (let* ((string-soup (string-downcase (concatenate 'string anchor " " image " " header; " " join-desc
                                                                          )))
                               (nsfw-graded-p (search "nsfw" string-soup :test #'equalp)))

                          (setq nsfw-graded nsfw-graded-p)
                          
                          (if nsfw-graded
                              (remhash h *default-instances*)
                              (setf (gethash h *default-instances*) instance)))))

             ;; (register-anon-instances)
          )))

;; Embedded into per-instance-poll
;; (defun register-anon-instances ()
;;   (loop with stagger = (loop repeat (hash-table-count *active-instances*)
;;                              collect (math:rand-range 0 10))
;;         for h being the hash-keys of *active-instances*
;;           using (hash-value instance)
;;         do (sb-thread:make-thread
;;             (lambda () (when (block nil
;;                           (labels ((reject ()
;;                                      (return)))
;;                             (prog1 t 
;;                               (with-slots (anchor) instance
;;                                         ;hunchentoot::with-debugger 
;;                                 (restart-case
;;                                     (let* ((request (handler-case (drakma:http-request (format nil "~a/api/v3/site" anchor))
;;                                                       ;; If this fails, site is probably down or blocking it
;;                                                       (error () (reject))))
;;                                            (string (handler-case (flex:octets-to-string request)
;;                                                      ;; If this fails, could be e.g 502 Bad Gateway HTML page
;;                                                      (error () (reject))))
;;                                            (continuep (if (equalp "" string)
;;                                                           (reject)))
;;                                            (json (json:parse string))
;;                                            (nsfwp (gethash "enable_nsfw" (gethash "local_site" (gethash "site_view" json)))))
;;                                       (declare (ignore continuep))

;;                                       (when nsfwp
;;                                         (reject)))
;;                                   (skip ()))))))
;;                     (setf (gethash h *anon-instances*) instance)))
;;             :name (format nil "are you nsfw? - ~a" h))))

(defvar *long-instance-register-condition* (bt2:make-condition-variable :name "per-instance-condition"))
(defvar *long-instance-total-pulls* (bt2:make-atomic-integer))

(defun long-instance-register ()
  (sb-thread:make-thread
   (lambda ()
     (loop while *long-instance-register-keep-running-p*
           ;; dont forget to check *long-instance-register-keep-running-p* again
           do (register-instances)
              (bt2:atomic-integer-incf *long-instance-total-pulls*)
              (sb-thread:condition-broadcast ;*long-instance-register-condition*
               condi)
              (sleep 200)))
   :name "long-instance-register"))

(defun terminate-long-instance-register ()
  (loop for thread in (bt2:all-threads)
        if (equalp "long-instance-register" (bt2:thread-name thread))
          do (bt2:destroy-thread thread)))

;; End all instance scrape


(defvar *per-instance-register-keep-running-p* t)
(defvar *per-instance-page-posts* (make-hash-table :synchronized t))
(defvar *per-instance-total-pulls* (bt2:make-atomic-integer))
(defvar *per-instance-pulls* 0)
(defvar *per-instance-condition* (sb-thread:make-waitqueue :name "per instance condition"))

(defun poll-posts (instance page)
  "NOTE: page is 1-indexed"
  (let ((ps (format nil "~A/api/v3/post/list?page=~A" (anchor instance) page)))
    (json:parse (drakma:http-request ps))))

;; (defvar *lock* (sb-thread:make-mutex :name "per-instance-mutex"))
;; (defvar *condition* (sb-thread:make-waitqueue :name "per-instance-condition"))
(defvar *long-instance-register-condition* *condition*)
(defvar lock (sb-thread:make-mutex :name "o"))
(defvar condi (sb-thread:make-waitqueue :name "w"))
;; (bt2:release-lock lock)

;; (fmakunbound 'cache-post-image)
;; (defun cache-post-image (entry)
;;   (alexandria:when-let*
;;       ((post (gethash "post" entry))
;;        ;; (link (gethash "ap_id" post))
;;        ;; (img (gethash "url" post))
;;        ;; (raw-img (gethash "url" post))
;;        )

;;     ;; (if (or (equalp img "")
;;     ;;         (equalp img 'null)
;;     ;;         (equalp img #'null))
;;     ;;     (setq img nil))

;;     ;; (ignore-errors
;;     ;;  (when (and (stringp img)
;;     ;;             (not (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp|mp4|webm|ogv)$" img)))
;;     ;;    (let* ((link (drakma:http-request img))
;;     ;;           (html (html-parse:parse-html link)))
;;     ;;      (labels ((recurse (list)
;;     ;;                 (loop for elements in list
;;     ;;                       if (listp elements)
;;     ;;                         do (if (subsetp '(:META :PROPERTY "og:image" :CONTENT) elements :test #'equalp)
;;     ;;                                (setq img (getf (cdr elements) :CONTENT))
;;     ;;                                (recurse elements)))))
;;     ;;        (recurse html)))))

;;     ;; try infer img
;;     ;; (if (stringp img)
;;     ;;     (if (and (not (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp)$" img))
;;     ;;              (stringp (gethash "thumbnail_url" (gethash "post" post))))
;;     ;;         (if (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp)$" (gethash "thumbnail_url" (gethash "post" post)))
;;     ;;             (setf img (gethash "thumbnail_url" (gethash "post" post))))))
;;     (ensure-cached-image post)
;;     ))

(defun cache-lemmy-instance-images (instance)
  (loop for posts across (aref (posts instance) 0)
        for post = (gethash "post" posts) 
        do (let ((post post))
             (bt2:make-thread
              (lambda ()
                (ensure-cached-image post))))))

(defun per-instance-register ()
  (bt2:make-thread
   (lambda ()
     (with-standard-io-syntax
       (loop with previous-threads = (make-hash-table :test #'equalp :synchronized t)
             for mutex = (sb-thread:make-mutex :name "o") 
             while *long-instance-register-keep-running-p*
             do  (print "lock wait")
                 ;; (bt2:condition-wait *long-instance-register-condition* *what*)*long-instance-register-condition*
                 (sb-thread:with-mutex (mutex)
                   (print "cond wait")
                   (sb-thread:condition-wait condi mutex)
                   (print "cond resume")
                   (loop for id being the hash-key of *active-instances*
                           using (hash-value instance)
                         if (not (and (gethash id previous-threads)
                                      (bt2:threadp (gethash id previous-threads))
                                      (bt2:thread-alive-p (gethash id previous-threads))))
                           do 
                              
                              (let ((id id)
                                    (instance instance))
                                (setf (gethash id previous-threads)
                                      (bt2:make-thread
                                       (lambda ()
                                         (with-standard-io-syntax
                                           (symbol-macrolet ((anchor (anchor instance))
                                                             (image (image instance))
                                                             (header (header instance))
                                                             (join-desc (join-desc instance))
                                                             (posts (posts instance))
                                                             (consequtive-fails (consequtive-fails instance)))
                                             (print "thread start")
                                             (loop while (and *per-instance-register-keep-running-p*
                                                              (gethash id *active-instances*))
                                                   do (handler-case
                                                          ;; Get the posts
                                                          (loop while (and *per-instance-register-keep-running-p*
                                                                           (gethash id *active-instances*))
                                                                do (alexandria:when-let ((ps (poll-posts instance 1)))
                                                                     ;; (cl-destruction:nrotate-right posts 1)
                                                                     (print (format nil "got posts for ~a" id))
                                                                     (print anchor)
                                                                     ;; (print ps)
                                                                     (setf (aref posts 0) (gethash "posts" ps))                                                          
                                                                     (bt2:atomic-integer-incf *per-instance-total-pulls*)
                                                                     (setf (bt2:atomic-integer-value consequtive-fails) 0)
                                                                     (bt2:thread-yield)

                                                                     ;; Cache images
                                                                     (cache-lemmy-instance-images instance)

                                                                     ;; Check if it is NSFW. This is kind of useless in the end...
                                                                     (let ((anonp
                                                                             (block nil ;hunchentoot::with-debugger 
                                                                               (labels ((reject ()
                                                                                          (return)))
                                                                                 (prog1 t                                                                                  
                                                                                   (restart-case
                                                                                       (let* ((request (handler-case (drakma:http-request (format nil "~a/api/v3/site" anchor))
                                                                                                         ;; If this fails, site is probably down or blocking it
                                                                                                         ;; (error () (reject))
                                                                                                         ))
                                                                                              (string (handler-case (flex:octets-to-string request)
                                                                                                        ;; If this fails, could be e.g 502 Bad Gateway HTML page
                                                                                                        ;; (error () (reject))
                                                                                                        ))
                                                                                              (continuep (if (equalp "" string)
                                                                                                             (reject)))
                                                                                              (json (json:parse string))
                                                                                              (nsfwp (gethash "enable_nsfw" (gethash "local_site" (gethash "site_view" json)))))
                                                                                         (declare (ignore continuep))
                                                                                         
                                                                                         (when nsfwp
                                                                                           (reject)))
                                                                                     (skip () (reject))))))))
                                                                       (print (format nil "NSFW flag ~a for ~a (~a)" anonp id (format nil "~a/api/v3/site" anchor)))
                                                                       (when anonp
                                                                         (setf (gethash id *anon-instances*) instance)))
                                                                     

                                                                     (bt2:condition-broadcast *per-instance-condition*)
                                                                     (bt2:thread-yield)                                                          
                                                                     (sleep (cl-math:rand-range 30 180))))
                                                        (error (c)
                                                          ;; If this happens too often should deregister
                                                          (bt2:atomic-integer-incf consequtive-fails)
                                                          ;; (cerror "poll failed" "failed")
                                                          (handle-issue 'post-poll-failed (list c id anchor image header join-desc instance))))
                                                      (bt2:thread-yield)                                                          
                                                      (sleep (cl-math:rand-range 60 400))))))
                                       :name (format nil "per instance 0-page poll for ~a" id))))
                              ;; NOTE should be 10-20 but it goes for HOURS
                              (sleep (cl-math:rand-range 60 400)))))))
   :name "register per instances"))

;; NOTE to run above:
#|
(per-instance-register)
(sb-thread:condition-broadcast condi)
|#

(defun force-terminate-per-instance-polling ()
  (loop for thread in (bt2:all-threads)
        if (or (search "per instance 0-page poll for " (bt2:thread-name thread))
               (search "register per instances" (bt2:thread-name thread)))
          do (bt2:destroy-thread thread)))







(defparameter *main-css* "
        body {
            font-family: sans-serif;
        }

.post {
display: table;
}


.post > * {
display: table-row;
}


.list-view.post-container {
    display: flex;
}

.grid-view.post-container {
    display: inline-flex;
    flex-direction: column;
    min-height: 400px;
    margin-top: 1rem;
    margin-bottom: 1rem;
}

.post-details.grid-view {
    display: inline-flex;
    flex-direction: column;
}

.grid-view * {
    max-width: 200px;
}

.grid-view.post-content {
    display: flex;
    flex-direction: column;
}

                                          .post-container {
                                          cursor: pointer;
                                          padding: 0.75rem;
                                          padding-top: 0.2rem;
                                          padding-bottom: 0.2rem;
                                          background-color: #f6f8fa;
                                          border: 1px solid #e1e4e8;
                                          border-radius: 0.5rem;
                                          transition: background-color 0.3s ease;
                                          }

                                          .post-container:hover {
                                          background-color: #e9f0f5;
                                          }

                                          .post-container a {
                                          text-decoration: none;
                                          color: #0366d6;
                                          }

  .post-container .post-body {
    font-size: 0.65rem;
  }

                                          .post-container h3 {
                                          font-size: 0.75rem;
                                          margin: 0.25rem 0;
                                          font-weight: 600;
                                          line-height: 1;
                                          }

                                          .post-container span {
                                          margin-right: 0.5rem;
                                          color: #586069;
                                          font-size: 0.6rem;
                                          }

                                          .post-container .syncing-time {
                                          color: #0366d6;
                                          font-weight: 600;
                                          }

                                          .post-container .post-body {
                                          color: #24292e;
                                          line-height: 1;
                                          }

    .thumbnail {
      position: relative;
      display: inline-block;
      overflow: hidden;
      cursor: pointer;
      transition: all 0.3s ease;
      width: 200px; /* Adjust the desired width here */
      height: 200px; /* Adjust the desired height here */
      border-radius: 0.5rem;
      box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
    }
    
  .thumbnail img {
    width: 100%;
    height: 100%;
    object-fit: cover;
  }
    
    .thumbnail:hover {
      transform: scale(1.05);
      box-shadow: 0 0 20px rgba(0, 0, 0, 0.2);
    }")

(setq hunchentoot::*catch-errors-p* nil)

(defun hunchentoot::start-session ()
  "Returns the current SESSION object. If there is no current session,
creates one and updates the corresponding data structures. In this
case the function will also send a session cookie to the browser."
  (let ((session (hunchentoot:session hunchentoot:*request*)))
    (when session
      (return-from hunchentoot::start-session session))
    (hunchentoot::with-session-lock-held ((hunchentoot:session-db-lock hunchentoot:*acceptor*))
      ;; Must be under a lock because creating a new session increments a global counter
      (setf session (make-instance 'hunchentoot:session))
      (setf (hunchentoot:session hunchentoot:*request*) session
            (hunchentoot:session-db hunchentoot:*acceptor*)
            (acons (hunchentoot:session-id session) session (hunchentoot:session-db hunchentoot:*acceptor*))))
    (hunchentoot:set-cookie (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
                            :value (hunchentoot:session-cookie-value session)
                            :path "/"
                            :http-only t
                            :secure t
                            :same-site "Strict")
    (hunchentoot:session-created hunchentoot:*acceptor* session)
    (setq hunchentoot:*session* session)))


(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (with-standard-io-syntax
    (hunchentoot:start-session)
    ;; (hunchentoot:session-string hunchentoot::*request*)
    ;; (setf (hunchentoot:session-value :sv) "testvalue")
    ;; (print (list "hello" (hunchentoot:session-value :sv)))
    "<!DOCTYPE html>
<html>
<head>
    <meta charset=\"utf-8\" />
    <meta name=\"viewport\" content=\"width=device-width, height=device-height\" />
    <title>Server-Sent Events Demo</title>
</head>
<body>

    <ul></ul>

    <script>
(async function() {
  \"use strict\";

  try {
    const response = await fetch('/setup').then(r => r.text()).then(text => eval?.(text));

    const socket = new WebSocket(\"wss://\" + location.host + \"/ws\");
    socket.onmessage = function(e) {
      try {
        eval?.(e.data);
      } catch (error) {
        console.error(e.data, e, error);
      }
    };
    
  } catch (error) {
    console.error(\"Error fetching setup:\", error);
  }
})();

                                          </script>
                                          </body>
                                          </html>
                                          "))

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(hunchentoot:define-easy-handler (events :uri "/events") ()
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
  (setf (hunchentoot:header-out "Connection") "keep-alive")
  (setf (hunchentoot:reply-external-format*) *utf-8*)
  (hunchentoot:no-cache)

  (print (hunchentoot:raw-post-data :force-text t))
  (let ((output-stream (flex:make-flexi-stream (hunchentoot:send-headers)
                                               :external-format *utf-8*)))
    (labels ((send-js (js)
               (sse-server:send-event! output-stream "event" js)
               (force-output output-stream)))
      
      (loop repeat 2
            do (sleep 2)
               ;; Without sse-server module
               ;;(format output-stream "event:my-custom-event~%data:Hello World! ~d~%~%" (incf counter))
               ;; With sse-server module
               (send-js
                ;; "let ul = document.querySelector('ul');
                ;; let li = document.createElement('li');
                ;; li.innerText = \"Hello World! ~d\";
                ;; ul.appendChild(li);"
                (ps:ps
                  (defparameter ul (document.query-selector 'ul))
                  (defparameter li (document.create-element 'li))
                  (setf (ps:@ li inner-text) "Hello World! ~d")
                  (ul.append-child li))))
      ;; (sleep 20)
      ;; (finish-output output-stream)
      )))

(hunchentoot:define-easy-handler (setup :uri "/setup") ()
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
  (setf (hunchentoot:header-out "Connection") "keep-alive")
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (setf (hunchentoot:reply-external-format*) *utf-8*)
  ;; (hunchentoot:no-cache)
  (let ((output-stream (flex:make-flexi-stream (hunchentoot:send-headers)
                                               :external-format *utf-8*)))
    (named-readtables:in-readtable trivial-escapes:readtable)
    ;; (write-string *sse-js* output-stream)
    (write-string (eval `(ps:ps
                           (defparameter style (new -c-s-s-style-sheet))
                           (style.replace-sync ,*main-css*)
                           (setf (ps:@ document adopted-style-sheets) (array style))))
                  output-stream)
    (terpri output-stream)
    (force-output output-stream))) 

(defun default-instances ()
  *default-instances*)



(defun generate-feed (instances &key (max-posts 128))
  (let* ((posts-vectors (loop for instance in instances
                              for posts = (if (typep instance 'lemmy-instance)
                                              (posts instance))
                              if posts collect (aref posts 0)))
         (posts-count (loop for vectors in posts-vectors
                            if (and vectors
                                    (every 'hash-table-p vectors)
                                    (typep vectors 'sequence))
                              sum (length vectors)))
         (posts-vector (make-array posts-count :adjustable t :fill-pointer t))
         (unique-posts (make-hash-table :test 'equalp)))
    
    (loop with i = 0
          for posts in posts-vectors
          if (arrayp posts)
            do (loop for post across posts
                     if (hash-table-p post) 
                       do (if (not (gethash (gethash "ap_id" (gethash "post" post)) unique-posts))
                              (progn
                                (setf (gethash (gethash "ap_id" (gethash "post" post)) unique-posts) t)
                                (setf (aref posts-vector i) post)
                                (incf i))))
          finally
             (setq posts-vector (subseq posts-vector 0 i)))
    (setq posts-vector
          (sort posts-vector #'time:timestamp> :key (lambda (post) (time:parse-timestring (gethash "published" (gethash "post" post))))))
    posts-vector))

(defun find-post-with-name (name)
  (loop for instance in (alexandria:hash-table-values (default-instances))
        for posts = (if (typep instance 'lemmy-instance)
                        (aref (posts instance) 0))
        if (arrayp posts)
          do (loop for post across posts
                   if (hash-table-p post)
                     do (if (string= (gethash "name" (gethash "post" post)) name)
                            (return-from find-post-with-name post)))))

(defun generate-feed-dom (feed &key (max-posts 128))
  (loop for post across feed
        for post-body = (str:shorten 1200 (let ((body (gethash "body" (gethash "post" post))))
                                            (if (eq 'null body)
                                                ""
                                                body)))

        for post-type = (cond ((<= 10 (length post-body))
                               :list)
                              (t :grid))
        repeat max-posts 
        for el = (when (gethash "post" post) 
                   (let ((link (gethash "ap_id" (gethash "post" post)))
                         (img (gethash "url" (gethash "post" post)))
                         ;; (img-url (gethash "url" (gethash "post" post)))
                         )
                     ;; (if (or (equalp img "")
                     ;;         (equalp img 'null)
                     ;;         (equalp img #'null))
                     ;;     (setq img nil))
                     ;; ;; try infer img
                     ;; (if (stringp img)
                     ;;     (if (and (not (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp)$" img))
                     ;;              (stringp (gethash "thumbnail_url" (gethash "post" post))))
                     ;;         (if (ppcre:scan ".*\\.(png|jpg|jpeg|gif|webp)$" (gethash "thumbnail_url" (gethash "post" post)))
                     ;;             (setf img (gethash "thumbnail_url" (gethash "post" post))))))
                     

                     ;; Convert image to base64 if possible
                     (when (stringp img)
                       (handler-case (let ((img-instance (ensure-cached-image (gethash "post" post))))
                                       (when (typep img-instance 'image)
                                         (setq img (concatenate 'string "data:image/webp;base64, " (thumbnail-b64 img-instance)))))
                         (error () (setq img nil))))
                     
                     (if (stringp img)
                         (setq img (concatenate 'string img)))
                     ;; (when img (break "~a" img))
                     
                     (case post-type 
                       (:grid
                        (cl-naive-dom:with-dom
                          (:div :class "post-container grid-view" 
                                :onclick (concatenate 'string "location.href='" link "'")
                                :style "cursor: pointer;"

                                ;; (break "~a" img)
                                ;; (print post)
                                
                                (:div :class "post-content grid-view"
                                      (:a :href (concatenate 'string link)
                                          (:h3 (str:shorten 80 (gethash "name" (gethash "post" post)))))
                                      (if (stringp img)
                                          (:span :class "thumbnail" 
                                                 (:img :src (concatenate 'string img)
                                                       :onerror "this.onerror=null;this.parentElement.style.display = 'none';"))) 
                                      (:div :class "post-body" post-body)
                                      (:div :class "post-details grid-view" "in" (:a :href (concatenate 'string (gethash "actor_id" (gethash "community" post)))
                                                                                     (concatenate 'string  (gethash "name" (gethash "community" post))))
                                            (:div
                                             (:span "upvotes:" (gethash "upvotes" (gethash "counts" post)))
                                             (:span "downvotes:" (gethash "downvotes" (gethash "counts" post)))) 
                                            (:span "&nbsp;")
                                            (:span "published: " (time:format-timestring nil (time:parse-timestring (gethash "published" (gethash "post" post)))))
                                            (:span "&nbsp;")
                                            (:div
                                             (:span "comments: " (gethash "comments" (gethash "counts" post)))
                                             (if (< 0 (length (gethash "newest_comment_time" (gethash "counts" post))))
                                                 (:span "(Last comment "
                                                        (:span :class "syncing-time" :data-time (time:format-timestring nil (time:parse-timestring (gethash "newest_comment_time" (gethash "counts" post)))) "&nbsp;")
                                                        " ago)"))))))))
                       (:list (nd:with-dom
                                (:div :class "post-container list-view"
                                      :onclick (concatenate 'string "location.href='" link "'")
                                      :style "cursor: pointer;"
                                      (if (stringp img)
                                          (:span :class "thumbnail" 
                                                 (:img :src (concatenate 'string img)
                                                       :onerror "this.onerror=null;this.parentElement.style.display = 'none';"))) 
                                      (:div :class (concatenate 'string "post-content list-view")
                                            (:a :href (concatenate 'string (concatenate 'string link))
                                                (:h3 (gethash "name" (gethash "post" post)))) 
                                            (:div :class "post-body" post-body)
                                            (:div :class "post-details list-view" "in" (:a :href (concatenate 'string (gethash "actor_id" (gethash "community" post)))
                                                                                           (concatenate 'string (gethash "name" (gethash "community" post))))
                                                  (:span "&nbsp;")
                                                  (:span "published: " (time:format-timestring nil (time:parse-timestring (gethash "published" (gethash "post" post)))))
                                                  (:span "&nbsp;")
                                                  (:span "comments: " (gethash "comments" (gethash "counts" post)))
                                                  (if (< 0 (length (gethash "newest_comment_time" (gethash "counts" post))))
                                                      (:span "(Last comment "
                                                             (:span :class "syncing-time" :data-time (time:format-timestring nil (time:parse-timestring (gethash "newest_comment_time" (gethash "counts" post)))) "&nbsp;")
                                                             " ago)"))
                                                  (:span "upvotes:" (gethash "upvotes" (gethash "counts" post)))
                                                  (:span "downvotes:" (gethash "downvotes" (gethash "counts" post)))))))))))
        if el collect el))



















(defclass preferences ()
  ((instances :initarg :instances :initform (default-instances) :reader preferences-instances)
   ;; topics
   ))

(defclass websocket-resource (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'client))

(defclass client (hunchensocket:websocket-client)
  ((user-agent :initarg :user-agent :reader client-user-agent)
   (account :initarg :account :reader client-account :initform nil)

   ;; need to start activating some feed algo in here, without an account
   ;;

   (preferences :initarg :preferences :reader client-preferences :initform (make-instance 'preferences))
   ))

(defvar *sessions-to-users* (make-hash-table :weakness :KEY-OR-VALUE :synchronized t))

(defmethod hunchentoot:session-created ((acceptor t) (session t))
  ;; (with-standard-io-syntax)
  ;; (print (list "session " session))
  ;; (cerror "hi" "hi")
  )

;; TODO
;; (defclass account ()
;; remember to merge preferences when user logs in or something.
;; NOTE on make account need to choose the instances, don't use them all!
;;   ((preferences :initarg :user-agent :reader client-preferences :initform (make-instance 'preferences :instances nil))))

;; pre-renderings. TODO this is going to be ram heavy

(defvar *websocket-resources* (list (make-instance 'websocket-resource :name "/ws")
                                    ;; (make-instance 'chat-room :name "/fury")
                                    ))

(defun find-room (request)
  ;; (break "~a" (find (hunchentoot:script-name request) *websocket-resources* :test #'string= :key #'name))
  (find (hunchentoot:script-name request) *websocket-resources* :test #'string= :key #'name))

;; (pushnew 'find-room hunchensocket:*websocket-dispatch-table*)
(setq hunchensocket:*websocket-dispatch-table* (list 'find-room))

;; (setq hunchensocket:*websocket-dispatch-table*
;;       (append hunchensocket:*websocket-dispatch-table*
;;               hunchentoot::*dispatch-table*))
(setq hunchentoot::*dispatch-table*
      (append hunchensocket:*websocket-dispatch-table*
              (list 'hunchentoot:dispatch-easy-handlers)))
;; (setq hunchentoot::*dispatch-table*
;;       hunchensocket:*websocket-dispatch-table*)
;; (defvar *working-table* (copy-seq hunchentoot::*dispatch-table*))

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room websocket-resource) incoming-user)
  (setf (hunchentoot:reply-external-format*) *utf-8*) 
  (with-standard-io-syntax

    (let* ((session (hunchentoot:session-verify (hunchensocket:client-request incoming-user)))
           (user (alexandria:ensure-gethash session *sessions-to-users* incoming-user)))

      ;; generate-feed-dom
      ;; poll-feed
      
      
      ;; (handler-case)
      (with-slots (preferences) user
        (with-slots (instances) preferences
          ;; (let ((instances instances)
          ;;       ;; (b (generate-feed-dom (generate-feed (alexandria:hash-table-values instances))))
          ;;       )
          ;; (break "~a" instances)
          ;;   (print instances)
          ;;   )
          
          (hunchensocket:send-text-message incoming-user
                                           (concatenate 'string
                                                        "document.body.innerHTML = `"
                                                        (str:replace-all "`" ""
                                                                         (nd.abt:with-abt
                                                                           (nd:with-dom
                                                                             (:div
                                                                              (generate-feed-dom (generate-feed (alexandria:hash-table-values instances)))))))
                                                        "`;"
                                                        "
let container = document.querySelector('.post-container').parentElement;
let boxes = container.querySelectorAll('.post-container.grid-view');
let lines = container.querySelectorAll('.post-container.list-view');

async function processLines() {
  let j = 0;
  let i = 0;

  do {
    let line = lines[i];
    let prevElement = line.previousElementSibling;
    let nextElement = line.nextElementSibling;

    while (prevElement && prevElement.classList.contains('post-container') && prevElement.classList.contains('list-view')) {
      prevElement = prevElement.previousElementSibling;
    }

    while (nextElement && nextElement.classList.contains('post-container') && nextElement.classList.contains('list-view')) {
      nextElement = nextElement.nextElementSibling;
    }

    if (prevElement && nextElement && prevElement.classList.contains('post-container') && prevElement.classList.contains('grid-view')) {
      if (prevElement.getBoundingClientRect().right + nextElement.getBoundingClientRect().width < line.offsetWidth) {
        nextElement.remove();
        container.insertBefore(nextElement, line);
        j++;
      }
    }

    i++;
  } while (i < lines.length);

  return j;
}

async function processAllLines() {
  let j = 0;

  do {
    j = await processLines();
  } while (j > 0);
}

processAllLines();

let resizeTimeout;
let previousWidth = window.innerWidth;

function debounce(func, delay) {
  clearTimeout(resizeTimeout);
  resizeTimeout = setTimeout(func, delay);
}

function handleResize() {
  const currentWidth = window.innerWidth;
  if (currentWidth > previousWidth) {
    debounce(processAllLines, 200);
  }
  previousWidth = currentWidth;
}

function handleResize() {
  debounce(processAllLines, 200);
}

window.addEventListener('resize', handleResize);
"

                                                        ))))
      
      ;; if (document.querySelector('.post-container') !)
      ;; (hunchensocket:send-text-message incoming-user )
      ;; (error (e)
      ;;   (print e)
      ;;   nil)
      )))

(defmethod hunchensocket:client-disconnected ((room websocket-resource) user)
  (print user)
  (broadcast room "~a has left ~a" (client-user-agent user) (name room)))

(defmethod hunchensocket:text-message-received ((room websocket-resource) user message)
  (broadcast room "~a says ~a" (client-user-agent user) message))

(defclass super-acceptor (hunchensocket:websocket-acceptor
                          hunchentoot:easy-acceptor)
  ())

(defun start ()

  (per-instance-register)
  (long-instance-register)

  (setq *server* (make-instance 'super-acceptor :port 6789))
  (hunchentoot:start *server*)

  ;; long and per poll
  t)

