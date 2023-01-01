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
      (log:d "Inserting github-event: ~a" stmt)
      (dbi:execute (dbi:prepare (db-connect) stmt) vals))))

(defun db-clear-gh-events ()
  "Delete all github-events from database."
  (dbi:execute
   (dbi:prepare
    (db-connect)
    (sxql:yield (sxql:delete-from :github_events)))))

(defmethod yason:encode ((event github-event) &optional stream)
  (with-accessors ((id event-id)
                   (type event-type)
                   (payload event-payload)) event
    (let ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase))
      (yason:encode-alist
       `((id . ,id)
         (type . ,type)
         (payload . ,payload))
       stream))))

(defun db-to-gh-event (row)
  (mk-gh-event (getf row :|id|)
               (getf row :|type|)
               (yason:parse (getf row :|payload|))))

(defmacro query-gh-events (&rest q-frags)
  `(multiple-value-bind (stmt vals)
       (sxql:yield
        (sxql:select (:id :type :payload)
          (sxql:from :github_events) ,@q-frags))
     (log:d "Executing SQL: ~a ~%[With vals: ~a]" stmt vals)
     (let* ((conn (db-connect))
            (query (dbi:execute (dbi:prepare conn stmt) vals)))
       (mapcar #'db-to-gh-event (dbi:fetch-all query)))))

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
    ;; FIXME This is a hack. Ideally we should overwrite existing events on
    ;; event-id unique constraint conflict
    (db-clear-gh-events)
    (loop :for event :in (concatenate 'list events received-events)
          :do (db-insert (mk-gh-event
                          (gethash "id" event)
                          (gethash "type" event)
                          (gethash "payload" event))))))
