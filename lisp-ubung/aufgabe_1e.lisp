;;; Aufgabe 1e
(defun my-reverse(liste)
    (if (null liste) liste
        (append (my-reverse(cdr liste)) (list (car liste))))
)
