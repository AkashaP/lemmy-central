(in-package :lemmy-central)

(defun %poll-scrape-all-instances ()
  "Scrape the join-lemmy.org website for lemmy instances"
  (let* ((html (drakma:http-request "https://join-lemmy.org/instances"))
         (sexp (cl-html-parse:parse-html html))
         (container 
           ;; First find the container for the instances
           (block block
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (equalp '(:DIV :CLASS "row") elements)
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
               (recurse sexp))
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
               (recurse sexp))
             images))
         (headers
           (let ((headers (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '((:H4 :CLASS "col")) elements :test #'equalp)
                                       (push (second elements) headers)
                                       (recurse elements)))))
               (recurse sexp))
             headers))
         (users-per-month
           (let ((users-per-month (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '((:H4 :CLASS "col text-right")) elements :test #'equalp)
                                       (push (second (second elements)) users-per-month)
                                       (recurse elements)))))
               (recurse sexp))
             users-per-month))
         (join-desc
           (let ((join-desc (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '((:P :CLASS "join-desc")) elements :test #'equalp)
                                       (push (second elements) join-desc)
                                       (recurse elements)))))
               (recurse sexp))
             join-desc)))
    (values anchors images headers users-per-month join-desc)))

(defclass lemmy-instance ()
  ((anchor :initarg :anchor :accessor anchor)
   (image :initarg :image :accessor image)
   (header :initarg :header :accessor header)
   (users-per-month :initarg :users-per-month :accessor users-per-month)
   (join-desc :initarg :join-desc :accessor join-desc)))

(defun make-all-instances ()
  (multiple-value-bind (anchors images headers users-per-month join-desc)
      (%poll-scrape-all-instances)
    (loop for anchor in anchors
          for image in images
          for header in headers
          for user-per-month in users-per-month
          for join-desc in join-desc
          collect (make-instance 'lemmy-instance
                                 :anchor anchor
                                 :image image
                                 :header header
                                 :users-per-month user-per-month
                                 :join-desc join-desc))))

(defvar *all-instances* nil)
(defvar *server* nil)

(nd:deftag (:instance-setting)
  ())

(defun start ()
  (setq *server* (make-instance 'hunchentoot:easy-acceptor :port 6789))
  (hunchentoot:start *server*)
  (setq *all-instances* (make-all-instances))
  t)

(defvar *current-user*)

(defun poll-posts (instance page)
  "NOTE: page is 1-indexed"
  (let ((ps (format nil "~A/api/v3/post/list?page=~A" (anchor instance) page)))
    (json:parse (drakma:http-request ps))))

(defun poll-feed (instances &key (max-posts 128))
  (restart-case
      (let* ((posts-hash-tables (loop for instance in instances
                                      collect (poll-posts instance 1)))
             (posts-count (loop for post-collection in posts-hash-tables
                                sum (length (gethash "posts" post-collection))))
             (posts-vector (make-array posts-count :adjustable t :fill-pointer t))
             (unique-posts (make-hash-table :test 'equalp)))
        (loop with i = 0
              for hash-table in posts-hash-tables
              do (if (arrayp (gethash "posts" hash-table))
                     (loop for post across (gethash "posts" hash-table)
                           do (if (not (gethash (gethash "ap_id" (gethash "post" post)) unique-posts))
                                  (progn
                                    (setf (gethash (gethash "ap_id" (gethash "post" post)) unique-posts) t)
                                    (setf (aref posts-vector i) post)
                                    (incf i)))))
              finally
                 (setq posts-vector (subseq posts-vector 0 i)))
        (setq posts-vector
              (sort posts-vector #'time:timestamp> :key (lambda (post) (time:parse-timestring (gethash "published" (gethash "post" post))))))
        posts-vector)
    (try-again ()
      :report "Try again"
      (poll-feed instances :max-posts max-posts))))

(defun generate-feed-dom (feed)
  (nd:with-dom
    (:div
     (loop for post across feed
           collect (:div :class "post-container"
                         :onclick (concatenate 'string "location.href='" (gethash "ap_id" (gethash "post" post)) "'")
                         :style "cursor: pointer;"
                         (:a :href (gethash "ap_id" (gethash "post" post))
                             (:h3 (gethash "name" (gethash "post" post))))
                         (:p (str:shorten 200 (let ((body (gethash "body" (gethash "post" post))))
                                                (if (eq 'null body)
                                                    ""
                                                    body))))
                         (:span "in" (:a :href (gethash "actor_id" (gethash "community" post))
                                         (gethash "name" (gethash "community" post)))
                                (:span "&nbsp;")
                                (:span "published: " (time:format-timestring nil (time:parse-timestring (gethash "published" (gethash "post" post)))))
                                (:span "&nbsp;")
                                (:span "comments: " (gethash "comments" (gethash "counts" post)))
                                (if (< 0 (length (gethash "newest_comment_time" (gethash "counts" post))))
                                    (:span "(Last comment "
                                           (:span :class "syncing-time" :data-time (time:format-timestring nil (time:parse-timestring (gethash "newest_comment_time" (gethash "counts" post)))) "&nbsp;")
                                           " ago)"))
                                (:span "upvotes:" (gethash "upvotes" (gethash "counts" post)))
                                (:span "downvotes:" (gethash "downvotes" (gethash "counts" post)))))))))

(defun inject-feed-js (dom)
  (make-instance 'cl-naive-dom:element
                 :tag (cl-naive-dom:tag dom)
                 :attributes (cl-naive-dom:attributes dom)
                 :children (append
                            (list
                             (nd:with-dom
                               (:style
                                "
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

  .post-container h3 {
    font-size: 1.5rem;
    margin: 0.25rem 0;
    font-weight: 600;
  }

  .post-container span {
    margin-right: 0.5rem;
    color: #586069;
    font-size: 0.875rem;
  }

  .post-container .syncing-time {
    color: #0366d6;
    font-weight: 600;
  }

  .post-container p {
    color: #24292e;
    line-height: 1.5;
  }"))
                             (nd:with-dom (:script :src "https://cdn.tailwindcss.com")))
                            (cl-naive-dom:children dom)
                            (cons
                             (nd:with-dom
                               (:script "
function updateSyncingTimes () {
var syncingTimes = document.getElementsByClassName('syncing-time');
for (var i = 0; i < syncingTimes.length; i++) {
        var time = syncingTimes[i].getAttribute('data-time');
        var timeElement = syncingTimes[i];
        var now = new Date();
        var then = new Date(time);
        var diff = now - then;
        var seconds = Math.floor(diff / 1000);
        var minutes = Math.floor(seconds / 60);
        var hours = Math.floor(minutes / 60);
        var days = Math.floor(hours / 24);
        var weeks = Math.floor(days / 7);
        var months = Math.floor(days / 30);
        var years = Math.floor(days / 365);
        var timeString = '';
        if (years > 0) {
        timeString = years + ' years';
        } if (months > 0) {
        timeString = months + ' months';
        } if (weeks > 0) {
        timeString = weeks + ' weeks';
        } if (days > 0) {
        timeString = days + ' days';
        } if (hours > 0) {
        timeString = hours + ' hours';
        } if (minutes > 0) {
        timeString = minutes + ' minutes';
        } if (timeString == '') {
        timeString = 'not long ago';
        }
        timeElement.innerHTML = timeString;
        }
}
updateSyncingTimes()
setInterval(updateSyncingTimes, 1800000);"))
                             nil))))

(hunchentoot:define-easy-handler (root-route :uri "/") (name)
  (format nil "Hey~@[ ~A~]!" name))

(start)
