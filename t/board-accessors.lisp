(in-package :michi-tests)

(def-suite board-accessors-suite
    :description "Test that the setf are properly defined")
(in-suite board-accessors-suite)

;; (def-fixture)
;; (with-fixture)

(test top-left-accessor ()
      (let ((board (list :1 :2 :3 :4 :5 :6 :7 :8 :9))
            (expected-board (list :empty :2 :3 :4 :5 :6 :7 :8 :9)))
        (setf (michi:top-left board) :empty)
        (is (equal board expected-board))))

(defun run-all-tests ()
  (run! 'board-accessors-suite))
