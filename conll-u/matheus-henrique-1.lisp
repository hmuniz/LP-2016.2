
(defparameter *sentence*
  '(("1" "É" "cc" "9") ("2" "por" "case" "3") ("3" "isso" "nmod" "9")
    ("4" "que" "cc" "9") ("5" "," "punct" "6") ("6" "explica" "root" "0")
    ("7" "," "punct" "6") ("8" "não" "neg" "9") ("9" "tem" "ccomp" "6")
    ("10" "pena" "dobj" "9") ("11" "de" "case" "12") ("12" "Hillary" "nmod" "10")
    ("13" "Clinton" "name" "12") ("14" "." "punct" "6")))

(defparameter *disney*
  '(("1" "Disney"  "nnp" "2"  "nsubj") 
    ("2" "acquired" "verb" "0" "root") 
    ("3" "the" "art" "4" "det") 
    ("4" "Pixar" "nnp" "2" "dobj")))


(defun build-first-format (list)
  (build (list-arrow list
		     (node-root list))
	 (word-root list)
	 list))

(defun build (arrows word list)
  (if arrows
      (list (misc-arrow (car arrows))
	    (build  (cdr arrows) word list)
	    (build 
	     (list-arrow list (node-arrow (car arrows)))
	     (word-arrow (car arrows))
	     list))
      (list  (upostag word list) word)))

(defun upostag (word list)
  (nth 2
   (car
    (remove-if-not (lambda (x)
		   (equal (nth 1 x)
			  word))
		 list))))

(defun misc-arrow (arrow)
  (car (last arrow)))

(defun word-arrow (arrow)
  (nth 1 arrow))

(defun node-arrow (arrow)
  (nth 0 arrow))
	    
(defun list-arrow (list n-root)
  (remove-if-not (lambda (x)
		   (equal n-root (nth 3 x)))
		 list))

(defun node-root (list)
  (nth 0 (list-root list)))

(defun word-root (list)
  (nth 1 (list-root list)))

(defun list-root (phrase)
  (car (remove-if-not (lambda (x)
			(equal "0" (nth 3 x)))
		      phrase)))
