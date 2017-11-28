;;; Aufgabe 1f
(defun my-reverser (liste)
    (cond ((null liste) liste)
        ((atom (car liste)) (append  (my-reverser(cdr liste)) (list (car liste))))
        ((not (atom (car liste))) (append  (my-reverser(cdr liste)) (list (my-reverser (car liste)))))
    )
)
