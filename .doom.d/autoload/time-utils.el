;;; ~/.config/doom/autoload/time-utils.el -*- lexical-binding: t; -*-

;;;###autoload
(defun gagbo-compare-time-during-day (lhs-time rhs-time)
  "Compare LHS-TIME and RHS-TIME as time list formats (see help on `decode-time').

This function totally ignores the day, the month, the year, the dow... It only cares
about the \"watch\" time. So you can pass lists with only 3 elements.

Return 'ts-before if LHS-TIME corresponds to an hour before RHS-TIME during the day.
Return 'ts-after  if LHS-TIME corresponds to an hour after RHS-TIME during the day.
Return 'ts-equal  if LHS-TIME is the same timestamp as RHS-TIME
"
  (let ((lhs-hour (caddr lhs-time))
        (lhs-minutes (cadr lhs-time))
        (lhs-seconds (car lhs-time))
        (rhs-hour (caddr rhs-time))
        (rhs-minutes (cadr rhs-time))
        (rhs-seconds (car rhs-time)))
    (cond ((< lhs-hour rhs-hour) 'ts-before)
          ((> lhs-hour rhs-hour) 'ts-after)
          ((< lhs-minutes rhs-minutes) 'ts-before)
          ((> lhs-minutes rhs-minutes) 'ts-after)
          ((< lhs-seconds rhs-seconds) 'ts-before)
          ((> lhs-seconds rhs-seconds) 'ts-after)
          (t 'ts-equal))))


;;;###autoload
(defun gagbo-timestamp-between-p (timestamp lower-bound upper-bound)
  "Return t if the timestamp TIMESTAMP is between LOWER-BOUND and UPPER-BOUND during a day.

Use the same date format as `decode-time'."

  (and (cl-equalp (gagbo-compare-time-during-day timestamp lower-bound) 'ts-after)
       (cl-equalp (gagbo-compare-time-during-day timestamp upper-bound) 'ts-before)))
