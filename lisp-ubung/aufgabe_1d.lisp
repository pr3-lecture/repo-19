;;; Aufgabe 1d
(defun my-lengthr(liste)
    (cond ((null liste) 0)
        ((atom (car liste)) (+ 1 (my-lengthr (cdr liste))))
        ((not (atom (car liste))) (+ (my-lengthr (car liste)) (my-lengthr (cdr liste))))
    )
)
