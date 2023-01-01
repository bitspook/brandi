(in-package #:brandi)

(defun handler (env)
  '(200 (:content-type "application/json") ("[\"lol\"]")))

(defparameter *app* (lambda (env) (funcall #'handler env)))

;; to wrap with multiple middlewares at once, use lack's builder macro
(setf *app*
      (lack:builder
       *app*))

(defvar *server* nil)
(defun start ()
  (setf *server*
        (clack:clackup
         *app*
         :server :woo
         :port 8000)))

(defun stop ()
  (clack:stop *server*))
