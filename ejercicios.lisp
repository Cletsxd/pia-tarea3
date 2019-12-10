;; 1. Permutaciones.
(defun perms (liss)
    (if liss ;; Si la lista no está vacía.
        (mapcan #'(lambda (x) ;; Crea una lista de permutaciones.
            (mapcar #'(lambda (y) (cons x y))
                (perms (remove x liss))))
            liss)
        '(()))) ;; Devuelve la lista que se creó con anterioridad (lista de listas).

;;;;; 2. Programa que elimine todas las ocurrencias de un elemento en una lista.
(defun eliminar (num li)
    (if (null li) ;; Si la lista es nula, devuelve nil.
        nil
        (if (eql (car li) num) ;; Si no y el valor <<num>> se encuentra en <<li>>.
            (eliminar num (cdr li)) ;; Elimina
            (cons (car li) (eliminar num (cdr li)))))) ;; Si no es igual, sigue evaluando los siguientes elementos.

;;;;; 3. Operaciones sobre conjuntos representados como listas.
;; Subset.
(defun subset (l1 l2)
    (if (null l1)
        t ;; La lista <<l1>> debe llegar a NIL, lo que quiere decir que <<l1>> sí es subconjunto de <<l2>>.
        (if (member (car l1) l2) ;; Si el valor no se encuentra en <<l2>>, quiere decir que <<l1>> no es subconjunto de <<l2>>.
            (subset (cdr l1) l2)
            nil)))

;; Interception.
(defun inter (l1 l2)
    (if (null l1)
        nil
        (if (member (car l1) l2)
            (cons (car l1) (inter (cdr l1) l2)) ;; Se crea una lista con los valores que están en <<l1>> y <<l2>>.
            (inter (cdr l1) l2))))

;; Union.
(defun unio_n (l1 l2)
    (if (not (null l1))
        (if (member (car l1) l2) ;; Si el dato de <<l1>> está en <<l2>>.
            (cons (car l1) (unio_n (cdr l1) (eliminar (car l1) l2))) ;; Se elimina dicho dato de <<l2>>, pero se mantiene en la nueva lista.
            (cons (car l1) (unio_n (cdr l1) l2))) ;; Si no está el dato en <<l2>> se pega en la nueva lista sin ser eliminado de <<l2>>.
        l2))

;; Difference.
(defun dif (l1 l2)
    (if (not (null l1))
        (if (not (member (car l1) l2))
            (cons (car l1) (dif (cdr l1) l2)) ;; Se crea una lista con los valores que están solamente en <<l1>> que no están en <<l2>>.
            (dif (cdr l1) l2))
        nil))

;;;;; 5. Representación de un árbol basada en listas.
;; Raíz del árbol
(defun root (arbol)
    (if (null arbol)
        nil ;; Devuelve NIL si el árbol no tiene nada.
        (car arbol))) ;; Devuelve el valor raíz del árbol.

;; Rama izquierda del árbol.
(defun lbranch (arbol)
    (if (null arbol)
        nil ;; Devuelve NIL si la rama izquierda del árbol no tiene nada.
        (cadr arbol))) ;; Devuelve la lista de la rama izquierda.

;; Rama derecha del árbol.
(defun rbranch (arbol)
    (if (null arbol)
        nil ;; Devuelve NIL si la rama derecha del árbol no tiene nada.
        (caddr arbol))) ;; Devuelve la lista de la rama derecha.

;; Si el árbol es una hoja.
(defun isleaf (arbol)
    (if (and (null (lbranch arbol)) (null (rbranch arbol))) ;; Cuando sus dos ramas son NIL.
        t
        nil))

;; El valor insertado se convierte en hoja
(defun atom2leaf (at)
    (list at nil nil)) ;; Para crear nodos de la forma: (valor NIL NIL).

;; Insertar izquierda (obsoleta)
(defun insl (l1 l2)
    (list (car l1) l2 (caddr l1)))

;; Insertar derecha (obsoleta)
(defun insr (l1 l2)
    (list (car l1) (cadr l1) l2))

(defun atom2leafliss (ll)
    (mapcar #'atom2leaf ll)) ;; Crea tantas hojas (nodos) como los que contiene la lista <<ll>>

(defun listtreew (liss)
    (if (not (null liss))
        (progn
            (print (car liss))
            (print (cadr liss))
            (listtreew (cdr liss)))
        nil))

;; (defun treeins (rot liss)
;;    (if (null rot)
;;        (treeins (car liss) (cdr liss))
;;        (if (not (null liss))
;;            (if (not (null (car liss)))
;;                (if (> (root rot) (car (root liss)))
;;                    (treeins (insl rot (car liss)) (cdr liss))
;;                    (treeins (insr rot (car liss)) (cdr liss))))
;;            (print rot))))

;; (defun list2tree (liss)
;;    (treeins nil (atom2leafliss liss)))

;; Inserta dentro de árbol
(defun add-elt-tree (n tri)
    (if (null tri) ;; Si el árbol es NIL.
        (atom2leaf n) ;; Crea un átomo.
        (if (< n (root tri)) ;; Si el árbol no es NIL y <<n>> es mayor que la raíz de <<tri>> (el árbol)
            (if (null (lbranch tri)) ;; Inserta en la rama izquierda
                (list (root tri) (atom2leaf n) (rbranch tri))
                (list (root tri) (add-elt-tree n (lbranch tri)) (rbranch tri)))
            (if (null (rbranch tri)) ;; Si no: inserta en la rama derecha
                (list (root tri) (lbranch tri) (atom2leaf n))
                (list (root tri) (lbranch tri) (add-elt-tree n (rbranch tri)))))))

(defun treeins (lst tri)
    (if (not (null lst))
        (treeins (cdr lst) (add-elt-tree (car lst) tri)) ;; Inserta cada dato de la lista <<lst>> en el árbol.
        tri))
    
(defun list2tree (lst) ;; Simple interfaz entre el exterior y la inserción de datos al árbol.
    (treeins lst nil)) ;; El árbol inicia en NIL.

;; Escritura en In-Order
(defun in-order (rot)
    (progn
        (if (not (null (lbranch rot))) ;; Hasta que sea NIL la rama izquierda.
            (in-order (lbranch rot)))
        (print (root rot)) ;; Escribe raíz
        (if (not (null (rbranch rot))) ;; Hasta que sea NIL la rama derecha.
            (in-order (rbranch rot)))))

;; Escritura en Pre-Order
(defun pre-order (rot)
    (progn
        (print (root rot)) ;; Escribe la raíz.
        (if (not (null (lbranch rot))) ;; Hasta que la rama izquierda sea NIL.
            (pre-order (lbranch rot)))
        (if (not (null (rbranch rot))) ;; Hasta que la rama derecha sea NIL.
            (pre-order (rbranch rot)))))

;; Escritura en Post-Order
(defun post-order (rot)
    (progn
        (if (not (null (lbranch rot))) ;; Hasta que la rama izquierda sea NIL.
            (post-order (lbranch rot)))
        (if (not (null (rbranch rot))) ;; Y hasta que la rama derecha sea NIL.
            (post-order (rbranch rot)))
        (print (root rot)))) ;; Escribe la raíz.

;; 5. Macro
(defmacro nreps (val funoi)
    (list 'loop 'for 'a 'from 1 'to val ;; Realiza la función <<funoi>> tantas veces como la cantidad representada en <<val>>.
        'collect funoi))