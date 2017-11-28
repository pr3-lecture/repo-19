;;; Aufgabe 1c
(defun my-length(liste)
    (if (null liste)
        0
    (+ 1 (my-length (cdr liste))))
)
