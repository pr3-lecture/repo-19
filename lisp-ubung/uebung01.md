1514966 Fernando Azevedo
1330738 Florian Hrycaj

#Übungsblatt 1 - PR3 (Prof. Schramm)

##Aufgabe 1
**(a) Elemente tauschen**
'''lisp
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
'''

**(b) Element einfügen**
'''lisp
(defun neues-vorletztes (ele liste)
    (if (or (null liste) (null (cdr liste)))
        (liste)
    (reverse (cons (car (reverse liste)) (cons ele (cdr (reverse liste))))))
)
'''

**(c) Länge einer Liste berechnen**
'''lisp

'''

**(d) Länge einer geschachtelten Liste berechnen**
'''lisp

'''

**(e) Listen umkehren**
'''lisp

'''

**(f) Geschachtelte Listen umkehren**
'''lisp

'''

##Aufgabe 2
**(a) Darstellung eines Binärbaums**


**(b) Baumtraversierung**
'''lisp

'''

##Tests
'''
(defvar list1 '(eins zwei drei vier))
(defvar list2 '(eins zwei (zwei (zwei drei) eins) drei vier))
(defvar item1 'dreieinhalb)

(print "Aufgabe 1a")
(defvar ret1a1 (rotiere list1))
(print ret1a1)
(defvar ret1a2 (rotiere_alternative list1))
(print ret1a2)

(print "Aufgabe 1b")
(defvar ret1b (neues-vorletztes item1 list1))
(print ret1b)

(print "Aufgabe 1c")
(defvar ret1c (my-length list1))
(print ret1c)

(print "Aufgabe 1d")
(defvar ret1d (my-lengthr list2))
(print ret1d)

(print "Aufgabe 1e")
(defvar ret1e (my-reverse list2))
(print ret1e)

(print "Aufgabe 1f")
(defvar ret1f (my-reverser list2))
(print ret1f)
'''
