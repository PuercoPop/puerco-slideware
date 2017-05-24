(defpackage "PUERCOPOP-SLIDEWARE"
  (:use "CL")
  (:documentation "
Slides are markdown
they have 2 axies, x-axis are numbers y-axis letters.
"))
(in-package "PUERCOPOP-SLIDEWARE")


(defparameter +x-axis+ 
  (apply #'vector
         (loop :for index :from 1 :to 99
               :collect (format nil "~2,'0D" index))))

(defparameter +y-axis+
  (apply #'vector
         (loop :for index :from 65 :upto 90
               :collect (code-char index))))

(defvar *slides-folder*)
#+(or)(setf *slides-folder* #P"/home/puercopop/quicklisp/local-projects/puerco-slideware/data/")

(defclass slide ()
  ((name :initarg :name
         :reader slide-name
         :documentation "For REPL purposes.")
   (content :initform ""
              :initarg :content
              :reader slide-content
              :documentation "")
   (up :initform nil
       :initarg :up
       :accessor slide-up
       :documentation "")
   (down :initform nil
         :initarg :down
         :accessor slide-down
         :documentation "")
   (prev :initform nil
         :initarg :prev
         :accessor slide-prev
         :documentation "")
   (next :initform nil
         :initarg :next
         :accessor slide-next
         :documentation "")))

(defun make-slide (pathname)
  (make-instance 'slide
                 :content (markdown.cl:parse (alexandria:read-file-into-string pathname))))

(defmethod print-object ((slide slide) stream)
  (print-unreadable-object (slide stream :type t)
    (format stream "~A" (slide-content slide))))

(defun assert-coord (coord)
  (let ((x (car coord))
        (y (cadr coord)))
    (assert (<= 0 x 99) nil "The X coordinate, ~A, has to be a number between 0 and 99." x)
    (assert (<= 0 y (- 90 65)) nil "The Y coordinate, ~A, has to be a number between 65 and 90." y)))

(defun slide-file (coord)
  (merge-pathnames (format nil "~A~A.md"
                           (aref +x-axis+ (car coord))
                           (string (aref +y-axis+ (cadr coord))))
                   *slides-folder*))

(defun get-coord-from-file (pathname)
  (let* ((filename (pathname-name pathname))
         (x (subseq filename 0 2))
         (y (subseq filename 2)))
    (list (position x +x-axis+ :test #'string-equal)
          (position y +y-axis+ :test #'string-equal))))

(defun down (coord)
  (list (car coord)
        (1+ (cadr coord))))

(defun right (coord)
  (list (1+ (car coord))
        0))

(defun next-slide (pathname)
  ;; If look down, if not go right.
  (let* ((coord (get-coord-from-file pathname))
         (down-file (slide-file (down coord)))
         (right-file (slide-file (right coord))))
    (cond
      ((uiop/filesystem:file-exists-p down-file)
       (values down-file nil))
      ((uiop/filesystem:file-exists-p right-file)
       (values right-file t))
      (t (values nil nil)))))

(defvar *start*)
(setf (documentation '*start 'variable)
      "The start of the slide graph.")

(defun load-slides ()
  "Load the slides from SLIDES-FOLDER."
  ;; Traverse the graph down then left. All the left links should point to the
  ;; top most element of the next column if one exists.
  (let ((current-coord (list 0 0)))
    (setf *start* (make-slide (slide-file current-coord)))
    (loop
      :for (next-slide leftp) := (multiple-value-list (next-slide (slide-file current-coord)))
      :while (or next-slide leftp)
      :do
         (if (not leftp)
             'foo
             'bar))))
