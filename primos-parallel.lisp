;; uses SBCL

;; Parallel version, set for 4 cores (creates 4 threads)

;; PRIMOS> (timed-bench 10000000)
;; Evaluation took:
;; 1.333 seconds of real time
;; 4.140625 seconds of total run time (4.140625 user, 0.000000 system)
;; 310.65% CPU
;; 3,621,557,694 processor cycles
;; 64 bytes consed

;; on my machine: Node.js does it in 2.403 s

;; PRIMOS> (timed-bench 100000000)
;; Evaluation took:
;; 37.718 seconds of real time
;; 109.921875 seconds of total run time (109.671875 user, 0.250000 system)
;; 291.43% CPU
;; 102,309,676,463 processor cycles
;; 522,016 bytes consed

;; on my machine: Node.js does it in 54.338 s

(eval-when (:compile-toplevel :load-toplevel)
  (unless (find :sbcl *features*)
    (error "This code is intended for SBCL.")))

(defpackage :primos
  (:use :lparallel :cl)
  (:export
   :timed-bench
   :bench
   :parallel))
(in-package :primos)

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) ))

(defparameter *debug* nil)

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
       ((zerop (mod ,xvar ,yvar)) (return-from ,blockname nil)))))

(defmacro loop-unroll-1 (list xvar blockname)
  "Unroll the loop with incvar = number on the list"
  (let ((forms (loop for number in list collecting
                     `(first-test ,xvar (the indextype ,number) ,blockname))))
    `(progn ,@forms
            )))

(defmacro inc-comp (nvar incvar xvar blockname)
  `(progn (incf ,nvar ,incvar)
          (if (zerop (mod ,xvar ,nvar))
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
(defun bench (limit &optional (start 0))
  "Execute the prime function until reaching limit, 
accumulate the amount of primes found"
  (declare (type stype limit start)
           (optimize (speed 3) (safety 0) (debug 0) (space 0) ))
  (let ((amount 0)
        (n 0))
    (declare (type stype amount n))
    (do ((x                             ;var
          start                         ;initial value
          (1+ x)))                      ;increment (next value)
        ((eq x limit)                   ;stop condition 
         (the stype (1- amount)))       ; return value
      (declare (type stype limit x))    ;block 
      (when (the boolean (prime x n))
        (incf amount)))))


(defmacro debugf (fmt &rest args)
  "Print message only if debug enabled"
  (if *debug*
      `(format nil ,fmt ,@args)))


(defun parallel (limit &optional (cores 4))
  (let* ((index 0)
         (sum 0)
         (index-step (truncate limit cores))
         (threads
           (loop for i from 0 below cores
                 collecting
                 (sb-thread:make-thread (let ((start index)
                                              (end (+ index index-step)))
                                          (declare (stype start end))
                                          (lambda ()
                                            (the stype (bench end start)))))
                 do (the stype (incf index index-step))
                 (debugf "SPAWNED THREAD"))))
    (declare (type stype cores index index index-step)
             (type stype sum limit))
    (dolist (thread threads)
      (incf sum (the stype (sb-thread:join-thread thread)))
      (debugf "Thread says: ~D~%" sum))
    sum))


(defun timed-bench (limit)
  "Timed benchmark!"
  (time (parallel limit)))
