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
         ;; (users-per-month
         ;;   (let ((users-per-month (list)))
         ;;     (labels ((recurse (list)
         ;;                (loop for elements in list
         ;;                      if (listp elements)
         ;;                        do (if (subsetp '((:H4 :CLASS "col text-right")) elements :test #'equalp)
         ;;                               (push (second (second elements)) users-per-month)
         ;;                               (recurse elements)))))
         ;;       (recurse sexp))
         ;;     users-per-month))
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
    
    (values anchors images headers join-desc)))

(defclass lemmy-instance ()
  ((anchor :initarg :anchor :accessor anchor)
   (image :initarg :image :accessor image)
   (header :initarg :header :accessor header)
   ;; (users-per-month :initarg :users-per-month :accessor users-per-month)
   (join-desc :initarg :join-desc :accessor join-desc)))

(defun make-all-instances ()
  (multiple-value-bind (anchors images headers join-desc)
      (%poll-scrape-all-instances)
    (loop for anchor in anchors
          for image in images
          for header in headers
          ;; for user-per-month in users-per-month
          for join-desc in join-desc
          collect (make-instance 'lemmy-instance
                                 :anchor anchor
                                 :image image
                                 :header header
                                 ;; :users-per-month user-per-month
                                 :join-desc join-desc))))

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
                                       (progn 
                                         (push (getf (cdr elements) :HREF) anchors)
                                         (return-from recurse))
                                       (recurse elements))
                              finally (push nil anchors))))
               (recurse sexp))
             (break "~a " anchors)
             anchors))
         ;; ... and other stuff
         (images
           (let ((images (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '(:IMG :CLASS "join-banner" :SRC) elements :test #'equalp)
                                       (progn
                                         (push (getf (cdr elements) :SRC) images)
                                         (return-from recurse))
                                       (recurse elements))
                              finally (push nil anchors))))
               (recurse sexp))
             images))
         (headers
           (let ((headers (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '((:H4 :CLASS "col")) elements :test #'equalp)
                                       (progn
                                         (push (second elements) headers)
                                         (return-from recurse))
                                       (recurse elements))
                              finally (push nil anchors))))
               (recurse sexp))
             headers))
         ;; (users-per-month
         ;;   (let ((users-per-month (list)))
         ;;     (labels ((recurse (list)
         ;;                (loop for elements in list
         ;;                      if (listp elements)
         ;;                        do (if (subsetp '((:H4 :CLASS "col text-right")) elements :test #'equalp)
         ;;                               (push (second (second elements)) users-per-month)
         ;;                               (recurse elements)))))
         ;;       (recurse sexp))
         ;;     users-per-month))
         (join-desc
           (let ((join-desc (list)))
             (labels ((recurse (list)
                        (loop for elements in list
                              if (listp elements)
                                do (if (subsetp '((:P :CLASS "join-desc")) elements :test #'equalp)
                                       (progn
                                         (push (second elements) join-desc)
                                         (return-from recurse))
                                       (recurse elements))
                              finally (push nil anchors))))
               (recurse sexp))
             join-desc)))
    
    (values anchors images headers join-desc)))


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
    
    (values anchors images headers join-desc)))


(defvar *all-instances* nil)
(defvar *server* nil)

(nd:deftag (:instance-setting)
  ())

(defclass super-acceptor (hunchensocket:websocket-acceptor
                          hunchentoot:easy-acceptor)
  ())

(defun start ()
  (setq *server* (make-instance ;'hunchentoot:easy-acceptor
                  'super-acceptor :port 6789))
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
                                      for posts = (labels ((logic () (poll-posts instance 1)))
                                                    (restart-case (logic)
                                                      (try-again ()
                                                        :report (lambda ()(format nil "Try again ~a" (anchor instance))) 
                                                        (logic))
                                                      (skip ()
                                                        :report (lambda ()(format nil "Skip instance ~a" (anchor instance))))))
                                      if posts collect posts))
             (posts-count (loop for post-collection in posts-hash-tables
                                if (and post-collection
                                        (gethash "posts" post-collection)
                                        (typep (gethash "posts" post-collection) 'sequence))
                                  sum (length (gethash "posts" post-collection))))
             (posts-vector (make-array posts-count :adjustable t :fill-pointer t))
             (unique-posts (make-hash-table :test 'equalp)))
        (loop with i = 0
              for hash-table in posts-hash-tables
              do (if (and
                      (hash-table-p hash-table)
                      (arrayp (gethash "posts" hash-table)))
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

;; just realised this is FOO code...
;; you could just.. make the user do it?
;; SERVER SENT EVENTS of course ahhh
;; wait why? use websockets instead
;; as in i can feed info to the user on the fly through ajax hookups

;; (defun poll-feed2 (instances &key (max-posts 128))
;;   (let* ((res (make-array (length instances))))
;;     (handler-case
;;         (sb-ext:with-timeout 15
;;           (loop for instance in instances
;;                 for i from 0
;;                 do (sb-thread:make-thread
;;                     (lambda () (handler-case
;;                               (sb-ext:with-timeout 15
;;                                 (setf (aref res i) (poll-posts instance 1)))
;;                             (sb-ext:timeout () (format t "timeout on instance ~A~%" instance))))
;;                     :name (format nil "poll-feed thread ~A" i))))
;;       (sb-ext:timeout () (format t "timeout all instances ~A~%" instance))))
;;   (restart-case
;;       (let* ((posts-hash-tables (loop for instance in instances
;;                                       collect (poll-posts instance 1)))
;;              (posts-count (loop for post-collection in posts-hash-tables
;;                                 sum (length (gethash "posts" post-collection))))
;;              (posts-vector (make-array posts-count :adjustable t :fill-pointer t))
;;              (unique-posts (make-hash-table :test 'equalp)))
;;         (loop with i = 0
;;               for hash-table in posts-hash-tables
;;               do (if (arrayp (gethash "posts" hash-table))
;;                      (loop for post across (gethash "posts" hash-table)
;;                            do (if (not (gethash (gethash "ap_id" (gethash "post" post)) unique-posts))
;;                                   (progn
;;                                     (setf (gethash (gethash "ap_id" (gethash "post" post)) unique-posts) t)
;;                                     (setf (aref posts-vector i) post)
;;                                     (incf i)))))
;;               finally
;;                  (setq posts-vector (subseq posts-vector 0 i)))
;;         (setq posts-vector
;;               (sort posts-vector #'time:timestamp> :key (lambda (post) (time:parse-timestring (gethash "published" (gethash "post" post))))))
;;         posts-vector)
;;     (try-again ()
;;       :report "Try again"
;;       (poll-feed instances :max-posts max-posts))))

;; (nd:with-dom (:div (generate-feed-dom (ensure-cache (default-instances)))))

;; (defun generate-feed-dom (feed &key (max-posts 128))
;;   (loop for post across feed
;;         repeat max-posts 
;;         collect (nd:with-dom
;;                   (:div :class "post-container"
;;                         :onclick (concatenate 'string "location.href='" (gethash "ap_id" (gethash "post" post)) "'")
;;                         :style "cursor: pointer;"
;;                         (uiop:if-let ((img (gethash "url" (gethash "post" post)) ))
;;                           (if (stringp img)
;;                               (:span :class "thumbnail" 
;;                                      (:img :src (concatenate 'string img "?format=webp&amp;thumbnail=256"))))) 
;;                         (let ((post-body (str:shorten 200 (let ((body (gethash "body" (gethash "post" post))))
;;                                                             (if (eq 'null body)
;;                                                                 ""
;;                                                                 body)))))
;;                           (:div :class (concatenate 'string "post-content" (if (<= 10 (length post-body))
;;                                                                                " list-view"
;;                                                                                ""))
;;                                 (:a :href (gethash "ap_id" (gethash "post" post))
;;                                     (:h3 (gethash "name" (gethash "post" post)))) 
;;                                 (:div :class "post-body" post-body)
;;                                 (:div :class "post-details" "in" (:a :href (gethash "actor_id" (gethash "community" post))
;;                                                                      (gethash "name" (gethash "community" post)))
;;                                       (:span "&nbsp;")
;;                                       (:span "published: " (time:format-timestring nil (time:parse-timestring (gethash "published" (gethash "post" post)))))
;;                                       (:span "&nbsp;")
;;                                       (:span "comments: " (gethash "comments" (gethash "counts" post)))
;;                                       (if (< 0 (length (gethash "newest_comment_time" (gethash "counts" post))))
;;                                           (:span "(Last comment "
;;                                                  (:span :class "syncing-time" :data-time (time:format-timestring nil (time:parse-timestring (gethash "newest_comment_time" (gethash "counts" post)))) "&nbsp;")
;;                                                  " ago)"))
;;                                       (:span "upvotes:" (gethash "upvotes" (gethash "counts" post)))
;;                                       (:span "downvotes:" (gethash "downvotes" (gethash "counts" post))))))))))

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
        for el = (case post-type 
                   (:grid (nd:with-dom
                            (:div :class "post-container grid-view"
                                  :onclick (concatenate 'string "location.href='" (gethash "ap_id" (gethash "post" post)) "'")
                                  :style "cursor: pointer;"
                                  
                                  
                                  (:div :class "post-content grid-view"
                                        (:a :href (gethash "ap_id" (gethash "post" post))
                                            (:h3 (str:shorten 80 (gethash "name" (gethash "post" post)))))
                                        (uiop:if-let ((img (gethash "url" (gethash "post" post)) ))
                                          (if (stringp img)
                                              (:span :class "thumbnail" 
                                                     (:img :src (concatenate 'string img "?format=webp&amp;thumbnail=32"))))) 
                                        (:div :class "post-body" post-body)
                                        (:div :class "post-details grid-view" "in" (:a :href (gethash "actor_id" (gethash "community" post))
                                                                                       (gethash "name" (gethash "community" post)))
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
                                  :onclick (concatenate 'string "location.href='" (gethash "ap_id" (gethash "post" post)) "'")
                                  :style "cursor: pointer;"
                                  (uiop:if-let ((img (gethash "url" (gethash "post" post)) ))
                                    (if (stringp img)
                                        (:span :class "thumbnail" 
                                               (:img :src (concatenate 'string img "?format=webp&amp;thumbnail=32"))))) 
                                  (:div :class (concatenate 'string "post-content list-view")
                                        (:a :href (gethash "ap_id" (gethash "post" post))
                                            (:h3 (gethash "name" (gethash "post" post)))) 
                                        (:div :class "post-body" post-body)
                                        (:div :class "post-details list-view" "in" (:a :href (gethash "actor_id" (gethash "community" post))
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
        if el collect el))

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
    display: flex;
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

  .post-container p {
    font-size: 0.65rem;
  }

  .post-container h3 {
    font-size: 1.5rem;
    margin: 0.25rem 0;
    font-weight: 600;
  }

  .post-container .post-details {
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
                               (:script
                                "
var source = new EventSource('exec');
source.onmessage = function(event) {
  eval(event.data);
};")
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

(hunchentoot:define-easy-handler (root-route :uri "/") ()
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
    const response = await fetch('/setup').then(r => r.text()).then(text => eval(text));

    const socket = new WebSocket(\"ws://\" + location.host + \"/events\");
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
                                          ")

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

(defparameter *main-css* "
  .post-container {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    grid-auto-rows: minmax(200px, auto);
    grid-gap: 1rem;
  }
  
  .post-container:nth-child(odd) .thumbnail {
    grid-row: span 2;
  }
  
  .thumbnail {
    border-radius: 0.5rem;
    box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
    overflow: hidden;
  }
  
  .thumbnail img {
    width: 100%;
    height: 100%;
    object-fit: cover;
  }
  
  .post-content {
    margin-top: 0.5rem;
  }
  
  .list-view .thumbnail {
    display: none;
  }
  
  .list-view .post-content {
    margin-top: 1rem;
    background-color: #f9f9f9;
    padding: 1rem;
    border-radius: 0.5rem;
    box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
  }")

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(hunchentoot:define-easy-handler (events :uri "/events") ()
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
  (setf (hunchentoot:header-out "Connection") "keep-alive")
  (setf (hunchentoot:reply-external-format*) *utf-8*)
  (hunchentoot:no-cache)

  (print (hunchentoot:raw-post-data :force-text t))
  (let ((counter 0)
        (output-stream (flex:make-flexi-stream (hunchentoot:send-headers)
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

;; TODO this is dangerous
(defun default-instances ()
  *all-instances*)

(defclass preferences ()
  ((instances :initarg :instances :initform (default-instances) :reader preferences-instances)
   ;; topics
   ))

;; TODO
;; (defclass account ()
;; remember to merge preferences when user logs in or something.
;; NOTE on make account need to choose the instances, don't use them all!
;;   ((preferences :initarg :user-agent :reader client-preferences :initform (make-instance 'preferences :instances nil))))

;; pre-renderings. TODO this is going to be ram heavy
(defvar *preference-cache* (make-hash-table :test #'equalp :synchronized t))

(defvar *websocket-resources* (list (make-instance 'websocket-resource :name "/events")
                                    ;; (make-instance 'chat-room :name "/fury")
                                    ))

(defun find-room (request)
  (find (hunchentoot:script-name request) *websocket-resources* :test #'string= :key #'name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(setq hunchensocket:*websocket-dispatch-table*
      (append hunchensocket:*websocket-dispatch-table*
              hunchentoot::*dispatch-table*))

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

;; (defvar *users* (make-hash-table))

(defvar *global-feed* nil)

(defun ensure-global-feed ()
  (or *global-feed*
      (setq *global-feed* (poll-feed preferences))))

(defun ensure-cache (preferences)
  (or (gethash preferences *preference-cache*)
      (setf (gethash preferences *preference-cache*)
            (poll-feed preferences)
            ;; (ensure-global-feed *global-feed*)
            )))

(defmethod hunchensocket:client-connected ((room websocket-resource) user)
  ;; (broadcast room "~a has joined ~a" (name user) (name room))
  (handler-case
      (progn
        (hunchensocket:send-text-message user "console.log(\"hello world\")")
        (let* ((account (client-account user))
               (preferences (if (not account)
                                (client-preferences user)
                                ;; TODO.
                                ())))
          ;; (loop for instance in (preferences-instances preferences)
          ;;       do (hunchensocket:send-text-message user (format nil "console.log(\"~a\")" instance)))

          (hunchensocket:send-text-message user (concatenate 'string
                                                             "document.body.innerHTML = `"
                                                             (str:replace-all "`" "'"
                                                                              (nd.abt:with-abt (nd:with-dom (:div (generate-feed-dom (ensure-cache (preferences-instances preferences)))
                                                                                                                  ;;                                                                                                                   (:script "
                                                                                                                  ;; const container = document.querySelector('.post-container');
                                                                                                                  ;; const boxes = container.querySelectorAll('.post-details,.grid-view');
                                                                                                                  ;; const lines = container.querySelectorAll('.post-details,.list-view');

                                                                                                                  ;; for (let i = 0; i < lines.length; i++) {
                                                                                                                  ;;   const line = lines[i];
                                                                                                                  ;;   const prevElement = line.previousElementSibling;

                                                                                                                  ;;   if (prevElement && prevElement.classList.contains('post-details') && prevElement.classList.contains('grid-view')) {
                                                                                                                  ;;     const remainingSpace = container.clientWidth - line.getBoundingClientRect().left;
                                                                                                                  
                                                                                                                  ;;     if (remainingSpace >= prevElement.offsetWidth) {
                                                                                                                  ;;       container.removeChild(prevElement);
                                                                                                                  ;;       container.insertBefore(prevElement, line);
                                                                                                                  ;;     }
                                                                                                                  ;;   }
                                                                                                                  ;; }
                                                                                                                  ;; ")
                                                                                                                  ))))
                                                             "`;"))
          (hunchensocket:send-text-message user "
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
"))
        (sleep 5)
        ;; (setf (gethash user *users*) )
        (hunchensocket:send-text-message user "console.log(\"hello world\")"))
    (error (e)
      (break "~a" e)
      e)))

(defmethod hunchensocket:client-disconnected ((room websocket-resource) user)
  (print user)
  (broadcast room "~a has left ~a" (client-user-agent user) (name room)))

(defmethod hunchensocket:text-message-received ((room websocket-resource) user message)
  (broadcast room "~a says ~a" (client-user-agent user) message))

;; (defvar *ws-server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
;; (hunchentoot:start *ws-server*)

;; (start)
