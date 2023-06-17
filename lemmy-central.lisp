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

;; Deprecated (blocking)
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

;; just realised this is FOO code...
;; you could just.. make the user do it?
;; SERVER SENT EVENTS of course ahhh
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



;; (hunchentoot:define-easy-handler (root-route :uri "/") ()
;;   "<!DOCTYPE html>
;; <html>
;; <head>
;;     <meta charset=\"utf-8\" />
;;     <meta name=\"viewport\" content=\"width=device-width, height=device-height\" />
;;     <title>Server-Sent Events Demo</title>
;;     <style type=\"text/css\">
;;         body {
;;             font-family: 'Open Sans', sans-serif;
;;         }
;;     </style>
;; </head>
;; <body>

;;     <ul></ul>

;;     <script>
;;         (function() { \"use strict\";

;;             var esOnce = new EventSource('/setup');
;;             function fn(text, event) {
;;                 try {
;; console.log('ping');
;;                     eval(text);
;;                 } catch (e) {
;;                     console.error(e, text, event);
;;                 }
;;             }
;;             function fnOnce(text, event) {
;; console.log('ping once');
;;                 fn(text, event);
;;                 esOnce.removeEventListener('event', fnOnce);
;;             }
;;             esOnce.addEventListener('event', fnOnce);

;;             var es = new EventSource('/events');
;;  //           es.addEventListener('open', function() {
;;  //               console.log('Server connected :)');
;;  //            });
;;             es.addEventListener('event', fn);
;;  //            es.addEventListener('error', function() {
;;  //                console.error('Server unavailable :(');
;;   //           });
;;         })();
;;     </script>
;; </body>
;; </html>
;; ")

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  "<!DOCTYPE html>
<html>
<head>
    <meta charset=\"utf-8\" />
    <meta name=\"viewport\" content=\"width=device-width, height=device-height\" />
    <title>Server-Sent Events Demo</title>
    <style type=\"text/css\">
        body {
            font-family: 'Open Sans', sans-serif;
        }
    </style>
</head>
<body>

    <ul></ul>

    <script>
(async function() {
  \"use strict\";

var poll = async function () {
 const response = await fetch('/events', {
    method: 'POST',
    headers: {
      'Content-Type': 'text/event-stream'
    },
    body: `{
      'user_id': 123
    }`
  })
const reader = response.body.pipeThrough(new TextDecoderStream()).getReader()
while (true) {
  const {value, done} = await reader.read();
  if (done) await poll();
  eval?.(value);
}}
poll();
})();

                                          </script>
                                          </body>
                                          </html>
                                          ")

(defvar *main-css* "
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
                                          }")

(defvar *sse-js* "
var SSE = function (url, options) {
  if (!(this instanceof SSE)) {
    return new SSE(url, options);
  }

  this.INITIALIZING = -1;
  this.CONNECTING = 0;
  this.OPEN = 1;
  this.CLOSED = 2;

  this.url = url;

  options = options || {};
  this.headers = options.headers || {};
  this.payload = options.payload !== undefined ? options.payload : '';
  this.method = options.method || (this.payload && 'POST' || 'GET');
  this.withCredentials = !!options.withCredentials;

  this.FIELD_SEPARATOR = ':';
  this.listeners = {};

  this.xhr = null;
  this.readyState = this.INITIALIZING;
  this.progress = 0;
  this.chunk = '';

  this.addEventListener = function(type, listener) {
    if (this.listeners[type] === undefined) {
      this.listeners[type] = [];
    }

    if (this.listeners[type].indexOf(listener) === -1) {
      this.listeners[type].push(listener);
    }
  };

  this.removeEventListener = function(type, listener) {
    if (this.listeners[type] === undefined) {
      return;
    }

    var filtered = [];
    this.listeners[type].forEach(function(element) {
      if (element !== listener) {
        filtered.push(element);
      }
    });
    if (filtered.length === 0) {
      delete this.listeners[type];
    } else {
      this.listeners[type] = filtered;
    }
  };

  this.dispatchEvent = function(e) {
    if (!e) {
      return true;
    }

    e.source = this;

    var onHandler = 'on' + e.type;
    if (this.hasOwnProperty(onHandler)) {
      this[onHandler].call(this, e);
      if (e.defaultPrevented) {
        return false;
      }
    }

    if (this.listeners[e.type]) {
      return this.listeners[e.type].every(function(callback) {
        callback(e);
        return !e.defaultPrevented;
      });
    }

    return true;
  };

  this._setReadyState = function(state) {
    var event = new CustomEvent('readystatechange');
    event.readyState = state;
    this.readyState = state;
    this.dispatchEvent(event);
  };

  this._onStreamFailure = function(e) {
    var event = new CustomEvent('error');
    event.data = e.currentTarget.response;
    this.dispatchEvent(event);
    this.close();
  }

  this._onStreamAbort = function(e) {
    this.dispatchEvent(new CustomEvent('abort'));
    this.close();
  }

  this._onStreamProgress = function(e) {
    if (!this.xhr) {
      return;
    }

    if (this.xhr.status !== 200) {
      this._onStreamFailure(e);
      return;
    }

    if (this.readyState == this.CONNECTING) {
      this.dispatchEvent(new CustomEvent('open'));
      this._setReadyState(this.OPEN);
    }

    var data = this.xhr.responseText.substring(this.progress);
    this.progress += data.length;
    data.split(/(\r\n|\r|\n){2}/g).forEach(function(part) {
      if (part.trim().length === 0) {
        this.dispatchEvent(this._parseEventChunk(this.chunk.trim()));
        this.chunk = '';
      } else {
        this.chunk += part;
      }
    }.bind(this));
  };

  this._onStreamLoaded = function(e) {
    this._onStreamProgress(e);

    // Parse the last chunk.
    this.dispatchEvent(this._parseEventChunk(this.chunk));
    this.chunk = '';
  };

  /**
   * Parse a received SSE event chunk into a constructed event object.
   */
  this._parseEventChunk = function(chunk) {
    if (!chunk || chunk.length === 0) {
      return null;
    }

    var e = {'id': null, 'retry': null, 'data': '', 'event': 'message'};
    var hasNewLine = false;
    chunk.split(/\n|\r\n|\r/).forEach(function(line) {
      line = line.trimRight();
      var index = line.indexOf(this.FIELD_SEPARATOR);
      if (index === 0) {
        // Line started with a separator and is a comment, ignore.
        return;
      }

      if (index < 0) {
        // Line was empty, use whole line as the field name and the empty string as value.
        index = line.length;
      }

      var field = line.substring(0, index);
      if (!(field in e)) {
        return;
      }

      var value = line.substring(index + 1);
      if (value.charAt(0) === ' ') {
        value = value.substring(1);
      }
      
      if (field === 'data') {
        if (hasNewLine) {
          e.data += '\n';
        }
        e.data += value;
        hasNewLine = true;
      } else {
        e[field] = value;
      }
    }.bind(this));

    var event = new CustomEvent(e.event);
    event.data = e.data;
    event.id = e.id;
    return event;
  };

  this._checkStreamClosed = function() {
    if (!this.xhr) {
      return;
    }

    if (this.xhr.readyState === XMLHttpRequest.DONE) {
      this._setReadyState(this.CLOSED);
    }
  };

  this.stream = function() {
    this._setReadyState(this.CONNECTING);

    this.xhr = new XMLHttpRequest();
    this.xhr.addEventListener('progress', this._onStreamProgress.bind(this));
    this.xhr.addEventListener('load', this._onStreamLoaded.bind(this));
    this.xhr.addEventListener('readystatechange', this._checkStreamClosed.bind(this));
    this.xhr.addEventListener('error', this._onStreamFailure.bind(this));
    this.xhr.addEventListener('abort', this._onStreamAbort.bind(this));
    this.xhr.open(this.method, this.url);
    for (var header in this.headers) {
      this.xhr.setRequestHeader(header, this.headers[header]);
    }
    this.xhr.withCredentials = this.withCredentials;
    this.xhr.send(this.payload);
  };

  this.close = function() {
    if (this.readyState === this.CLOSED) {
      return;
    }

    this.xhr.abort();
    this.xhr = null;
    this._setReadyState(this.CLOSED);
  };
};
")

(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))

(declaim (ignore (sb-ext:muffle-conditions 'parenscript::simple-style-warning)))

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

      
      ;; (send-js
      ;;  (drakma:http-request "https://cdn.tailwindcss.com"))

      ;; send stylesheet
      ;; (send-js
      ;;  (eval `(ps:ps
      ;;           ;; (defparameter body (document.query-selector 'body))
      ;;           ;; (defparameter style (document.create-element 'style))
      ;;           ;; (defparameter inner-text ,*main-css*)
      ;;           ;; (setf (ps:@ style inner-text) inner-text)
      ;;           ;; (document.head.insert-adjacent-h-t-m-l "beforeEnd" ,*main-css*)

      ;;           (defparameter style (new -c-s-s-style-sheet))
      ;;           (style.replace-sync ,*main-css*)

      ;;           (setf (ps:@ document adopted-style-sheets) (array style))
      ;;           ;; (setf (ps:@ style css-text) ,*main-css*)
      ;;           )))

      ;; add tailwind
      ;; (send-js (drakma:http-request "https://cdn.tailwindcss.com"))
      ;; (print "hello world")
      
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
  (let ((counter 0)
        (output-stream (flex:make-flexi-stream (hunchentoot:send-headers)
                                               :external-format *utf-8*)))
    ;; (labels ((send-js (js)
    ;;            (sse-server:send-event! output-stream "event" js)
    ;;            (force-output output-stream)))
    ;;   (send-js
    ;;    (drakma:http-request "https://cdn.tailwindcss.com"))
    ;;   (send-js *sse-js*)

    ;;   ;; send stylesheet
    ;;   (send-js
    ;;    (eval `(ps:ps
    ;;             ;; (defparameter body (document.query-selector 'body))
    ;;             ;; (defparameter style (document.create-element 'style))
    ;;             ;; (defparameter inner-text ,*main-css*)
    ;;             ;; (setf (ps:@ style inner-text) inner-text)
    ;;             ;; (document.head.insert-adjacent-h-t-m-l "beforeEnd" ,*main-css*)

    ;;             (defparameter style (new -c-s-s-style-sheet))
    ;;             (style.replace-sync ,*main-css*)

    ;;             (setf (ps:@ document adopted-style-sheets) (array style))
    ;;             ;; (setf (ps:@ style css-text) ,*main-css*)
    ;;             )))
    ;;   )
    (named-readtables:in-readtable trivial-escapes:readtable)
    (write-string *sse-js* output-stream)
    (write-string (eval `(ps:ps
                           ;; (defparameter body (document.query-selector 'body))
                           ;; (defparameter style (document.create-element 'style))
                           ;; (defparameter inner-text ,*main-css*)
                           ;; (setf (ps:@ style inner-text) inner-text)
                           ;; (document.head.insert-adjacent-h-t-m-l "beforeEnd" ,*main-css*)

                           (defparameter style (new -c-s-s-style-sheet))
                           (style.replace-sync ,*main-css*)

                           (setf (ps:@ document adopted-style-sheets) (array style))
                           ;; (setf (ps:@ style css-text) ,*main-css*)
                           ))
                  output-stream)
    (terpri output-stream)
    (force-output output-stream)))

;; (nd.abt:with-abt (inject-feed-js (generate-feed-dom (poll-feed (list (first *all-instances*))))))

(start)
