(defvar testTree '(7 (3 8)))
(defvar file "./data.txt")

;;; insert tree val


;;; insert tree filename
(defun insert (tree filename)
  (with-open-file (stream filename
                    :direction :input
                    :if-does-not-exist nil)
    (loop for line = (read-line stream nil)
          while line
          collect line
    )
  )
)


(print (insert testTree file))




;;; contains tree val


;;; size tree


;;; height tree


;;; getMax tree


;;; getMin tree


;;; remove tree val


;;; isEmpty tree


;;; addAll tree otherTree


;;; printLevelorder tree
