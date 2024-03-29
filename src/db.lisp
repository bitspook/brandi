(in-package :brandi)

(defparameter *conn* nil
  "The database connection. Should not be used directly, use `db-connect'
instead.")

(defparameter *migration-driver* nil
  "`migratum.driver' for running migrations")

(defparameter *migratum-provider* nil)

;; `prep-migrations' should be called to prepare migrations before any code
;; related to migrations can be executed.
(defun prep-migrations ()
  (when *migration-driver*
    (return-from prep-migrations *migration-driver*))

  (let* ((migrations-dirs (list (asdf:system-relative-pathname :brandi "sql/migrations/"))))
    (setq *migratum-provider* (migratum.provider.local-path:make-provider migrations-dirs))
    (migratum:provider-init *migratum-provider*)

    (setq *migration-driver* (migratum.driver.dbi:make-driver *migratum-provider* (db-connect)))
    (migratum:driver-init *migration-driver*)
    *migration-driver*))

(defun db-connect ()
  (when *conn* (return-from db-connect *conn*))

  (setq *conn* (dbi:connect :sqlite3 :database-name (conf :db-name)))
  ;; WAL PRAGMA cannot be set using migrations, because migrations are ran
  ;; inside transactions, and sqlite won't set this pragma from inside a
  ;; transaction
  (dbi:execute (dbi:prepare *conn* "PRAGMA journal_mode=WAL;"))
  *conn*)

(defun db-disconnect ()
  (dbi:disconnect *conn*)
  (setf *conn* nil))

(defun run-pending-migrations ()
  (migratum:apply-pending (prep-migrations)))

(defun create-new-migration (desc &optional up-str down-str)
  "Create new up+down migrations with DESC as description.
This function is supposed to be used interactively, in the spirit of maximum
hackability."
  (when (not *migratum-provider*)
    (prep-migrations))
  (let ((mid (migratum:make-migration-id)))
    (migratum:provider-create-migration
     :up :sql *migratum-provider* mid desc
     (or up-str ""))
    (migratum:provider-create-migration
     :down :sql *migratum-provider* mid desc
     (or down-str ""))))
