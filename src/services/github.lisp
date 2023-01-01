(in-package #:brandi)

(defgeneric db-insert (obj)
  (:documentation "Insert OBJ into database."))

(defclass github-event ()
  ((id :initarg :id :accessor event-id)
   (type :initarg :type :accessor event-type)
   (payload :initarg :payload :accessor event-payload)))

(defmethod db-insert ((ev github-event))
  (with-accessors ((id event-id)
                   (type event-type)
                   (payload event-payload)) ev
    (multiple-value-bind (stmt vals)
        (sxql:yield (sxql:insert-into :github_events
                      (sxql:set= :id id
                                 :type type
                                 :payload payload)))
      (log:d "Insert Query: ~a" stmt)
      (dbi:execute (dbi:prepare (make-connection) stmt) vals))))

(defun mk-gh-event (id type payload)
  (make-instance
   'github-event
   :id id
   :type type
   :payload payload))

(defun refresh-github-activity (username)
  (let ((events (yason:parse (with-output-to-string (str)
                               (uiop:run-program
                                (format nil "gh api /users/~a/events" username)
                                :output str))))
        (received-events (yason:parse
                          (with-output-to-string (str)
                            (uiop:run-program
                             (format nil
                                     "gh api /users/~a/received_events"
                                     username)
                             :output str)))))
    (loop :for event :in (concatenate 'list events received-events)
          :do (db-insert (mk-gh-event
                          (gethash "id" event)
                          (gethash "type" event)
                          (yason:with-output-to-string* ()
                            (yason:encode event)))))))
