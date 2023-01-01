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
                      (sxql:set=
                       :id id
                       :type type
                       :payload (yason:with-output-to-string* ()
                                  (yason:encode payload)))))
      (log:d "Inserting github-event: ~a [~a]" stmt vals)
      (dbi:execute (dbi:prepare (make-connection) stmt) vals))))

(defmacro query-gh-events (&rest q-frags)
  (labels ((to-gh-event (row)
             (mk-gh-event (getf row :|id|)
                          (getf row :|type|)
                          (yason:parse (getf row :|payload|)))))
    `(multiple-value-bind (stmt vals)
         (sxql:yield
          (sxql:select (:id :type :payload)
            (sxql:from :github_events) ,@q-frags))
       (log:d "Executing SQL: ~a ~%[With vals: ~a]" stmt vals)
       (let* ((conn (make-connection))
              (query (dbi:execute (dbi:prepare conn stmt) vals)))
         (mapcar ,(function to-gh-event) (dbi:fetch-all query))))))

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
                          (gethash "payload" event))))))
