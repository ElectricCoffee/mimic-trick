(defpackage :util
  (:use :cl)
  (:export :shuffle :prompt :random-element :rotate-to :push-back))

(in-package :util)

(defun shuffle (deck)
  "Fisher-Yates style shuffle function"
  (let ((vec (coerce deck 'vector)))                            ;; coerce deck to vector to allow for efficient shuffling
    (loop for i from (1- (length vec)) downto 1
          do (rotatef (aref vec i) (aref vec (random (1+ i)))))
    (coerce vec 'list)))

(defun prompt (message)
  "Prompts the user and returns the input"
  (format t message)
  (force-output)
  (read-line))

(defun random-element (list)
  "Returns a random element from a list."
  (nth (random (length list)) list))

(defun rotate-to (item list)
    (let ((pos (position item list)))
      (append (nthcdr pos list)
              (subseq list 0 pos))))

(defmacro push-back (item place)
  "Like push, but appends to the end."
  `(setf ,place (append ,place (list ,item))))
