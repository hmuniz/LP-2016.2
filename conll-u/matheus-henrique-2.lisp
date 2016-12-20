
(defparameter *disney*
  '(("1" "Disney"  "nnp" "2"  "nsubj") 
    ("2" "acquired" "verb" "0" "root") 
    ("3" "the" "art" "4" "det") 
    ("4" "Pixar" "nnp" "2" "dobj")))


(defun build-second-format (list)
       (build-2 (list-arrow list
		      (node-root list))
	  (list-root list)
	  list))

(defun build-2 (arrows token list)
  (let ((brick (list
		(delrep token)
		(list (pos token) (word-a token)))))
  (if arrows
      (append
      brick
      (mapcar
       #'(lambda (x)
	   (build-2 (list-arrow list (node x))
		    x
		    list))
       arrows))
      brick)))

(defun pos (arrow)
  (nth 2 arrow))

(defun delrep (arrow)
  (car (last arrow)))

(defun word-a (arrow)
  (nth 1 arrow))

(defun node (arrow)
  (nth 0 arrow))
	    
(defun list-arrow (list node)
  (remove-if-not (lambda (x)
	   (equal node (nth 3 x)))
			     list))

(defun node-root (list)
  (nth 0 (list-root list)))

(defun word-root (list)
  (nth 1 (list-root list)))

(defun list-root (phrase)
  (car (remove-if-not (lambda (x)
			(equal "0" (nth 3 x)))
		      phrase)))
