(in-package #:cl-user)
(defpackage #:brandi
  (:use :cl :serapeum/bundle)
  (:export conf))
(in-package #:brandi)

(log:config :debug)

(defun conf (key)
  (case :db-name
    ((:db-name) "brandi.db")))
