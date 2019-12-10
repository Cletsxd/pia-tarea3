(in-package :cl-id3)

(defvar *cv-tra* '("train1.csv"
                    "train2.csv"
                    "train3.csv"
                    "train4.csv"
                    "train5.csv"
                    "train6.csv"
                    "train7.csv"
                    "train8.csv"
                    "train9.csv"
                    "train10.csv"))

(defvar *cv-tes* '("test1.csv"
                    "test2.csv"
                    "test3.csv"
                    "test4.csv"
                    "test5.csv"
                    "test6.csv"
                    "test7.csv"
                    "test8.csv"
                    "test9.csv"
                    "test10.csv"))

(defvar *trii* nil)

(defun sets (&optional (data *data*))
    (car *data*))

(defun wcvs (names)
    (dolist (n names)
        (print n)))

;;(defun load-dir-file (&optional (names *cv-tra*))
    ;;(dolist (n names)
    ;;    (progn
    ;;        (load-file n)
    ;;        (print-tree (induce)))))

;;(defun load-dir-file (&optional (ntrain *cv-tra*) (ntest *cv-tes*))
;;    (if (not (null ntrain))
;;        (progn
;;            (load-file (car ntrain))
;;            (print-tree (induce))
;;            (load-file (car ntest))
;;            (dolist (e *examples*)
;;                (classify e (induce)))
;;            (load-dir-file (cdr ntrain) (cdr ntest)))))

;;(defun accur (name)
;;    (progn (load-file name)
;;        (setf *trii* (induce))
;;        (load-file "test_probe.csv")
;;        (setf i 0)
;;        (loop for x in *data* do
;;            (if (eql (classify-new-instance (butlast x) *trii*) (car (last x)))
;;                (progn
;;                    (+ i 1)
;;                    (print (butlast x))
;;                    (print (classify-new-instance (butlast x) *trii*))
;;                    (print (last x)))
;;                (progn
;;                    (print "wrong")
;;                    (print (butlast x))
;;                    (print (classify-new-instance (butlast x) *trii*))
;;                    (print (last x)))))
;;            (setf acc (/ i (length *data*)))))

;;(defun accur2 (file-name)
;;    (progn
;;        (load-file file-name)
;;        (setf *trii* (induce))
;;        (load-file "test1.csv")
;;        (setf cont 0)
;;        (loop for x in *data* do
;;            (if (eql (classify-new-instance (butlast x) *trii*) (car (last x)))
;;                (setf cont (+ cont 1))))
;;        (/ cont (length *data*))))

(defun accuracy (file-train file-test)
    (progn
        (load-file file-train) ;; Carga archivo de entrenamiento.
        (setf *trii* (induce)) ;; Induce a un nuevo árbol.
        (load-file file-test) ;; Carga un archivo de prueba.
        (setf cont 0) ;; Inicializa contador en cero.
        (dolist (x *data*) ;; Para cada <<x>> en los datos de prueba.
            (if (eql (classify-new-instance (butlast x) *trii*) (car (last x))) ;; Clasifica la instancia nueva y chequea si la clase real corresponde con la mostrada por el árbol.
                (setf cont (+ cont 1)))) ;; Aumenta contador.
        (float (/ cont (length *data*))))) ;; Normaliza.

(defun cross-validation ()
    (loop for tr in *cv-tra* collect ;; Para cada base de datos de entrenamiento.
        (loop for ts in *cv-tes* collect ;; Para cada base de datos de prueba.
            (accuracy tr ts)))) ;; Calcula la presición.