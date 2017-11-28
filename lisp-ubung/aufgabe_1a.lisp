;;; Aufgabe 1a
(defun rotiere (liste)
    (if (or (null liste) (null (cdr liste)))
        (liste)
    (append (cdr liste)(list (car liste))))
)

;;; Alternativ: first/rest statt car/cdr
(defun rotiere_alternative (liste)
    (append (rest liste) (list (first liste)))
)
