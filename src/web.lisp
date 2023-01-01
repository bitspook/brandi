(in-package #:brandi)

(defun handler (env)
  (declare (ignore env))
  (let* ((gh-events (query-gh-events (sxql:limit 1)))
         (res-body (with-output-to-string (str)
                     (yason:encode gh-events str))))
    `(200 (:content-type "application/json") (,res-body))))

(defparameter *app* (lambda (env) (funcall #'handler env)))

;; to wrap with multiple middlewares at once, use lack's builder macro
(setf *app*
      (lack:builder
       *app*))

(defvar *web-server* nil)
(defun start-web ()
  (run-pending-migrations)
  (setf *web-server*
        (clack:clackup
         *app*
         :server :woo
         :port 8000)))

(defun stop-web ()
  (clack:stop *web-server*)
  (db-disconnect))
