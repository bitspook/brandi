(in-package :cl-user)

(ql:quickload "brandi")
(in-package :brandi)

(run-pending-migrations)

(refresh-github-activity "bitspook")

(query-gh-events (sxql:limit 10))
