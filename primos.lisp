;; 2019-02-28: updated, massive speed improvement.

;; Benchmarking fun
;; https://www.linkedin.com/pulse/benchmarking-fun-dan-bellander/

;; To execute:
;; 1. Install SBCL 64-bit or 32-bit edition
;; 2. load this script
;;    (load "primos")
;;
;; 3. execute (timed-bench 1000000) or the amount you want to try.
;;    it will show time needed for the calculations.

;; example:
;; CL-USER> (timed-bench 10000000)
;; Evaluation took:
;; 0.406 seconds of real time
;; 0.406250 seconds of total run time (0.406250 user, 0.000000 system)
;; 100.00% CPU
;; 1,090,997,832 processor cycles
;; 0 bytes consed
;; 2285716
;; (vector 4 2 4 2 4 6 2 6)

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) ))

(deftype dtype () '(unsigned-byte 64)) ;type for n*n
(deftype stype () '(unsigned-byte 64)) ;type for n          
(deftype indextype () '(unsigned-byte 8)) ;type for small numbers

;(declaim (ftype (function (stype) boolean) prime))
;(declaim (inline prime))

;(sb-ext:defglobal *n* (the stype 7))

;; (make-array '(8)
;;             :element-type 'stype
;;             :adjustable nil
;;             :fill-pointer nil
;;             :displaced-to nil
;;             :initial-contents '(4 2 4 2 4 6 2 6)
;;             )

;(declaim (type stype *n*))

(defmacro first-test (xvar yvar blockname)
  `(progn
     (cond
       ((eq ,xvar ,yvar) (return-from ,blockname T))
       ((eq 0 (mod ,xvar ,yvar)) (return-from ,blockname nil)))))

(defmacro loop-unroll-1 (list xvar blockname)
  "Unroll the loop with incvar = number on the list"
  (let ((forms (loop for number in list collecting
                     `(first-test ,xvar (the indextype ,number) ,blockname))))
    `(progn ,@forms
            )))

(defmacro inc-comp (nvar incvar xvar blockname)
  `(progn (incf ,nvar ,incvar)
          (if (eq 0 (mod ,xvar ,nvar))
              (return-from ,blockname
                nil))))

(defmacro loop-unroll-2 (list nvar xvar blockname)
  "Unroll the loop with incvar = number on the list"
  (let ((forms (loop for number in list collecting
                     `(inc-comp ,nvar (the indextype ,number) ,xvar ,blockname))))
    `(progn ,@forms
            )))

(defmacro prime (xvar nvar)
  "T if number is prime, NIL if it doesn't"
  `(block main
    (setf ,nvar (the stype 7))
    (loop-unroll-1 (2 3 5 7) ,xvar main)
    (loop while (<= (the dtype (* ,nvar ,nvar)) ,xvar)
          do
          (loop-unroll-2 (4 2 4 2 4 6 2 6) ,nvar ,xvar main))
    T))


;; benchmark
(defun bench (limit)
  "Execute the prime function until reaching limit, 
accumulate the amount of primes found"
  (declare (type stype limit)
           (optimize (speed 3) (safety 0) (debug 0) (space 0) ))
  (let ((amount 0)
        (n 0))
    (declare (type stype amount n))
    (dotimes (x limit)
      (declare (type stype limit))
      (when (the boolean (prime x n))
        (incf amount)))
    (the stype (1- amount))))


(defun timed-bench (limit)
  "Timed benchmark!"
  (time (bench limit)))
