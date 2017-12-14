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


; Gibt Höhe des Baums aus
(defun height (tree)

)


; Gibt Baum in Levelorder aus
;1 Get the height of the tree.
;2 Put a for loop for each level in tree.
;3 for each level in step 2, do pre order tra­ver­sal and print only when height matches to the level.
(defun printLevelorder(tree)

)


(defvar file "./data.txt")
(defvar tree1 '(42 (30 (12 () 17) 36) (75 56 ())) )
(defvar tree2 '(7 3 8))
(defvar tree3 '(60 2 88))
(defvar tree4 '(42 (30 (12 () 17) 36) (75 56 ())) )
(defvar val1 52)
(defvar val2 59)
(defvar val3 77)
(defvar val4 81)
(defvar val5 19)
(defvar val6 4)
(defvar val7 39)
(defvar val8 35)
(defvar existingVal 30)
(defvar nonExistingVal 999)

(print "Aufgabe 1 - BinaryTree")
(print "Initialer Baum: ")
(print tree1)

(print "Teste Funktion: insert tree val  mit val:")
(print val5)
(defvar ret1 (insert tree1 val5))
(print ret1)

(print "Teste Funktion: insertTreeFromFile tree filename  mit val:")
(print (insertTreeFromFile tree2 file))

(print "Teste Funktion: contains tree val  mit existingVal:")
(print existingVal)
(print (contains tree1 existingVal))
(print "Teste Funktion: contains tree val  mit nonExistingVal:")
(print nonExistingVal)
(print (contains tree1 nonExistingVal))

(print "Teste Funktion: size tree:")
;(print (size tree4))

(print "Teste Funktion: height tree")
;(print (height tree1))

(print "Teste Funktion: getMax tree")
(print (getMax tree1))

(print "Teste Funktion: getMin tree")
(print (getMin tree1))

(print "Teste Funktion: remove tree val  mit val:")
(print existingVal)
(print (myremove tree1 existingVal))

(print "Teste Funktion: isEmpty mit gefülltem Baum")
(print (isEmpty tree1))
(print "Teste Funktion: isEmpty mit leerem Baum")
(print (isEmpty ()))

(print "Teste Funktion: addAll tree otherTree  mit tree:")
(print (addAll tree2 tree3))
;

(print "Teste Funktion: printLevelorder tree")
(printLevelorder tree4)
