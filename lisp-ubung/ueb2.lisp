; knoten in baum einfügen
(defun insert (tree node)
  (cond ((or (null node)(contains tree node)) nil)
        ((isEmpty tree) (list node)) ; falls baum leer
        (T (insertHelp tree node))))

; hilfsfunktion insert
(defun insertHelp(tree node)
        ; Fall dass position zurück kommt
  (cond ((isEmpty tree) node)
        ; Fall das eltern knoten zurück kommt
        ((and (atom tree)(> tree node)) (list tree node '()))
        ((and (atom tree)(< tree node)) (list tree '() node))
        ; läuft durch den baum
        ((> (car tree) node) (list (car tree) (insertHelp (cadr tree) node)(caddr tree)))
        ((< (car tree) node) (list (car tree)(cadr tree)(insertHelp (caddr tree) node)))))


; Hilfsfunktion für insertTreeFromFile
(defun getFile(filename)
  (with-open-file (stream filename
                    :direction :input
                    :if-does-not-exist nil)
    (loop for line = (read stream nil 'eof)
        until(eql line 'eof)
        collect line
    )
  )
)

; insert tree filename
(defun insertTreeFromFile(tree filename)
    (addAll tree (getFile filename))
)

; Läuft durch baum
(defun bTreeWalkThrough(fn tree node)
  (cond ((= (car tree) node) (funcall fn (car tree) node))
        ((> (car tree) node) (funcall fn (cadr tree) node))
        ((< (car tree) node) (funcall fn (caddr tree) node))))

; Prüft auf gleichheit; nil heißt nein; T heißt ja
(defun contains(tree node)
  (cond ((or (isEmpty tree)(null node)) nil)
        ((and (atom tree)(= tree node)) T)
        ((and (atom tree)(not(= tree node))) NIL)
        ( T (bTreeWalkThrough #'contains tree node))))


; gibt anzahl der knoten zurück
(defun size(tree)
  (cond ((null tree) 0)
        ((atom (car tree)) (+ 1 (my-lengthr (cdr tree))))
        ((not (atom (car tree))) (+ (my-lengthr (car tree)) (my-lengthr (cdr tree))))))

(defun my-lengthr(l)
  (cond ((null l) 0)
        ((atom (car l)) (+ 1 (my-lengthr (cdr l))))
        ((not (atom (car l))) (+ (my-lengthr (car l)) (my-lengthr (cdr l))))))


; get max
(defun getMax(tree)
  (cond ((isEmpty tree) nil)
        ((atom tree) tree)
        ((null (caddr tree)) (car tree))
        ((not(null (caddr tree)))(getMax (caddr tree)))))


; get min
(defun getMin(tree)
  (cond ((isEmpty tree) nil)
        ((atom tree) tree)
        ((null (cadr tree)) (car tree))
        ((not(null (cadr tree)))(getMin (cadr tree)))))


; removes val from tree
(defun myremove(tree node)
  (cond ((not(contains tree node)) nil)
        ; der zustand vor dem finden
        ((and (atom tree)(= tree node)) nil)
        ; Fall das eltern knoten zurück kommt
        ; liste -> kleinstes element, ganzer linker teilbaum,
        ((= (car tree) node) (if (null (caddr tree))(cadr tree)(list (getMin (caddr tree))(cadr tree)(hilfsRemove (caddr tree) (getMin (caddr tree))))))
        ((> (car tree) node) (list (car tree)(myremove (cadr tree) node)(caddr tree)))
        ((< (car tree) node) (list (car tree)(cadr tree)(myremove (caddr tree) node)))))

;hilfsfunktion löscht element eines baums
(defun hilfsRemove(tree node)
  (cond ((atom tree) nil)
        ((= (car tree) node) (list (caddr tree)))
        ((> (car tree) node) (list (car tree)(hilfsRemove(cadr tree) node)(caddr tree)))
        ((< (car tree) node) (list (car tree)(cadr tree)(hilfsRemove(caddr tree) node)))))


; Schaut ob Baum leer ist
(defun isEmpty(tree) (null tree))


; Fügt Elemente des übergebenen Baums in den Vorhandenen ein
(defun addAll(tree otherTree)
  (if (car otherTree)
    (addAll (insert tree (car otherTree)) (cdr otherTree)) tree
  )
)


; Gibt die Höhe des Baums zurück
(defun height(tree)
  (check-type tree list)
  (if (null tree)
    0 ; empty tree
    (max (height-helper (car tree))
      (height (cdr tree))
    )
  )
)

; Hilfsfunktion für height
(defun height-helper(tree)
  (if (atom tree)
    1 ; tree without leafs has height of 1
    (1+ (height tree))
  )
)


; Gibt den Baum in Levelorder aus
(defun printLevelOrder(tree)
    (loop while (not (null tree))
      do
        (setq node (car tree) tree (cdr tree))
        (if (not (null (car node)))
          (print (car node))
        )
        (setq tree (append tree (cdr node)))
    )
)


; TESTS
(defvar file "./data.txt")
(defvar tree1 '(42 (30 (12 () (17)) (36)) (75 (56) ())))
(defvar tree2 '(7 3 (8 (2) (40))))
(defvar tree3 '(60 2 88))
(defvar tree4 '(42 (30 (12 () (17)) (36)) (75 (56) ())))
(defvar emptytree '())
(defvar val1 52)
(defvar val2 59)
(defvar val3 77)
(defvar val4 81)
(defvar val5 119)
(defvar val6 4)
(defvar val7 39)
(defvar val8 35)
(defvar existingVal 56)
(defvar existingVal2 30)
(defvar nonExistingVal 120)

(print "-----------------------------------------")
(print "Uebungsblatt 2, Aufgabe 1 - BinaryTree" )
(print "-----------------------------------------")
(print "INSERT TREE")
(print "Ausgangsbaum:")
(print tree1)
(print "Insert value:")
(print val5)
(defvar ret1 (insert tree1 val5))
(print "Baum danach:")
(print ret1)
(print "Insert another value:")
(print val6)
(defvar ret2 (insert ret1 val6))
(print ret2)
(print "-----------------------------------------")
(print "INSERT TREE FROM FILE")
(print "Ausgangsbaum:")
(print tree1)
(print "Daten in Datei data.txt: 2 14 9 111 99 4001 999")
(print "Baum danach:")
(print (insertTreeFromFile tree1 file))
(print "-----------------------------------------")
(print "CONTAINS")
(print "Ausgangsbaum:")
(print tree1)
(print "Pruefe existierenden Wert:")
(print existingVal)
(print "Wert vorhanden?")
(print (contains tree1 existingVal))
(print "Pruefe nicht existierenden Wert:")
(print nonExistingVal)
(print "Wert vorhanden?")
(print (contains tree1 nonExistingVal))
(print "-----------------------------------------")
(print "SIZE")
(print "Ausgangsbaum:")
(print tree2)
(print "Baumgroesse")
(print (size tree2))
(print "-----------------------------------------")
(print "HEIGHT")
(print "Ausgangsbaum:")
(print tree1)
(print "Baumhoehe")
(print (height tree1))
(print "-----------------------------------------")
(print "GET MAX")
(print "Ausgangsbaum:")
(print tree1)
(print "Maximum:")
(print (getMax tree1))
(print "-----------------------------------------")
(print "GET MIN")
(print "Ausgangsbaum:")
(print tree1)
(print "Minimum:")
(print (getMin tree1))
(print "-----------------------------------------")
(print "REMOVE")
(print "Ausgangsbaum:")
(print tree1)
(print "Remove value:")
(print existingVal)
(defvar ret3 (myremove tree1 existingVal))
(print "Baum danach:")
(print ret3)
;(print "Remove another value:")
;(print existingVal2)
;(defvar ret4 (insert ret3 existingVal2))
;(print "Baum danach:")
;(print ret4)
(print "-----------------------------------------")
(print "IS EMPTY")
(print "Gefuellter Ausgangsbaum:")
(print tree1)
(print "Baum leer?:")
(print (isEmpty tree1))
(print "Leerer Ausgangsbaum:")
(print emptytree)
(print "Baum leer?:")
(print (isEmpty emptytree))
(print "-----------------------------------------")
(print "ADD ALL")
(print "Ausgangsbaeume:")
(print tree1)
(print tree3)
(print "Baum danach:")
(print (addAll tree1 tree3))
(print "-----------------------------------------")
(print "PRINT LEVEL ORDER")
(print "Ausgangsbaum:")
(print tree1)
(print "Baum in Level-Order:")
(printLevelOrder (list tree1))
(print "-----------------------------------------")
