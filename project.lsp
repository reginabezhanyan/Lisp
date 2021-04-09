(defconstant INT_MAX 2147483647)

;из графа вытащить название всех городов в список
;Acc - ()
(defun Town (GR Acc) (cond
    ((null GR) Acc)
    ((member (caar GR) Acc) (cond 
        ((member (cadar GR) Acc) (Town (cdr GR) Acc))
        (T (Town (cdr GR) (cons (cadar GR) Acc)))
    ))
    ((member (cadar GR) Acc) (Town (cdr GR) (cons (caar GR) Acc)))
    (T (Town (cdr GR) (cons (cadar GR) (cons (caar GR) Acc))))
))

;(print (Town '((A B A 5) (A D A 10) (A C A 4) (B C A 7) (F D A 5)) '()))

; GR - ((A B A 5) (A D A 10) (A C A 4) (B C A 7))


;Составление матрицы NxN 
;Acc
(defun Str (M) (cond
    ((= M 0) nil)
    (T (cons 0 (Str (- M 1))))
))

(defun Matrix (N M) (cond
    ((= N 0) nil)
    (T (cons (Str M) (Matrix (- N 1) M)))
))

;Добавление значений из графа GR в матрицу
; TL - список городов

;номер города в таблице
(defun TownN (T1 TL) (cond
    ((eq T1 (car TL)) 1)
    (T (+ 1 (TownN T1 (cdr TL))))
))

(defun AddStr3 (M K Matr) (cond
    ((= M 1) (cond
        ((eq (car Matr) 0) (cons K (cdr Matr)))
        (T (cons (list K (car Matr)) (cdr Matr)))
    ))
    (T (cons (car Matr) (AddStr3 (- M 1) K (cdr Matr)) ))
))

(defun AddStr2 (N M K Matr) (cond
    ((= N 1) (cons (AddStr3 M K (car Matr)) (cdr Matr)) )
    (T (cons (car Matr) (AddStr2 (- N 1) M K (cdr Matr))))
))

;N - номер первого города, M  - номер второго города, K - длина дороги между ними
(defun AddStr1 (N M K Matr)
    (AddStr2 M N K (AddStr2 N M K Matr))
)

;(print (AddStr1 1 2 5 '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))

;Добавление значений из графа GR в матрицу - матрица длины ребер
(defun AddMatrixK (TL GR Matr) (cond
    ((null GR) Matr)
    (T (AddMatrixK TL (cdr GR) (AddStr1 (TownN (caar GR) TL) (TownN (cadar GR) TL) (cadr (cddar GR)) Matr) ))
))

;Добавление значений из графа GR в матрицу - матрица дорог
(defun AddMatrixR (TL GR Matr) (cond
    ((null GR) Matr)
    (T (AddMatrixR TL (cdr GR) (AddStr1 (TownN (caar GR) TL) (TownN (cadar GR) TL) (car (cddar GR)) Matr) ))
))


(print (AddMatrixK '(A B C D) '((A B A 5) (A D A 10) (A C A 4) (B A R 7) (B C A 7)) '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))
;(print (AddMatrixR '(A B C D) '((A B A 5) (A D A 10) (A C A 4) (B A R 7) (B C A 7)) '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))
;(print (TownN 'A '(A B C D)))

;алгоритм дейкстры
;GR - матрица NxN, N - количество городов, TN - номер города, по которому мы строим минимальные пути, Dis - вектор-ответ с минимальными путями 
;(заполненный INT_MAX и 0 в нужном месте), Vis - вектор посещенных вершин, изначально с nil, Trans - вектор-ответ с количеством пересадок (с 0)


(defun elem (N L) (cond
    ((= N 1) (car L))
    (T (elem (- N 1) (cdr L)))
)) 

(defun index2 (S N) (cond
    ((null N) S)
    (T (index2 (elem (car N) S) (cdr N)))
))


(defun index (S &rest N) (cond
    ((null N) S)
    (T (index2 (elem (car N) S) (cdr N)))
))

(defun visit (M L) (cond
    ((= M 1) (cons T (cdr L)))
    (T (cons (car L) (visit (- M 1) (cdr L)) ))
)) 

(defun ind_min (Dis Vis N min i_min) (cond
    ((null Dis) i_min)
    (T (cond 
        ((and (eq (car Vis) nil) (< (car Dis) min)) (ind_min (cdr Dis) (cdr Vis) (+ N 1) (car Dis) N))
        (T (ind_min (cdr Dis) (cdr Vis) (+ N 1) min i_min))
    ))
))

;(print (ind_min '(2147483647 0 3 2147483647) '(0 0 0 0) 1 INT_MAX 0))

(defun SumDis (K i Dis) (cond 
    ((= i 1) (cons K (cdr Dis)))
    (T (cons (car Dis) (SumDis K (- i 1) (cdr Dis))))
))

;(print (SumDis 15 3 '(1 1 1 1)))

(defun Dijkstra1 (GR TN i j Dis Vis) (cond
    ((= TN 0) Dis)
    (T (cond
        ((and (not(elem i Vis)) (not (= (index GR j i) 0)) (not (= (elem j Dis) INT_MAX)) (< (+ (elem j Dis) (index GR j i)) (elem i Dis)))
                (Dijkstra1 GR (- TN 1) (+ i 1) j (SumDis (+ (elem j Dis) (index GR j i)) i Dis) Vis))
        (T (Dijkstra1 GR (- TN 1) (+ i 1) j Dis Vis))
    ))
)) 

;(print (Dijkstra1 '((0 7 4 10) (7 0 7 0) (4 7 0 0) (10 0 0 0)) 4 1 1 '(0 2147483647 2147483647 2147483647) '(T nil nil nil)))

(defun Dijkstra (GR TN N Dis Vis) (cond
    ((= TN 1) Dis)
    (T (Dijkstra GR (- TN 1) N (Dijkstra1 GR N 1 (ind_min Dis Vis 1 INT_MAX 0) Dis (visit (ind_min Dis Vis 1 INT_MAX 0) Vis)) 
                (visit (ind_min Dis Vis 1 INT_MAX 0) Vis)))
))

;(print (Dijkstra '((0 5 4 10) (5 0 7 2) (4 7 0 0) (10 2 0 0)) 4 4 '(0 2147483647 2147483647 2147483647) '(nil nil nil nil)))
;(print (Dijkstra '((0 5 4 10) (5 0 7 2) (4 7 0 0) (10 2 0 0)) 4 4 '(2147483647 0 2147483647 2147483647) '(nil nil nil nil)))

 
 
 
 
 
 
 
 
 
 
ВСе по-новому


;из графа вытащить название всех городов в список
;Acc - ()
(defun Town (GR Acc) (cond
    ((null GR) Acc)
    ((member (caar GR) Acc) (cond 
        ((member (cadar GR) Acc) (Town (cdr GR) Acc))
        (T (Town (cdr GR) (cons (cadar GR) Acc)))
    ))
    ((member (cadar GR) Acc) (Town (cdr GR) (cons (caar GR) Acc)))
    (T (Town (cdr GR) (cons (cadar GR) (cons (caar GR) Acc))))
))

;(print (Town '((A B A 5) (A D A 10) (A C A 4) (B C A 7) (F D A 5)) '()))

; GR - ((A B A 5) (A D A 10) (A C A 4) (B C A 7))


;Составление матрицы NxN 

(defun Matrix (N M) (cond
    ((= N 0) nil)
    (T (cons nil (Matrix (- N 1) M)))
))

;(print (Matrix 4 4))

;TL - список городов

;номер города в таблице
(defun TownN (T1 TL) (cond
    ((eq T1 (car TL)) 1)
    (T (+ 1 (TownN T1 (cdr TL))))
))

;Добавление значений из графа GR в матрицу

(defun AddStr2 (N M TV Matr) (cond
    ((= N 1) (cons (cons (cons M TV) (car Matr)) (cdr Matr)) )
    (T (cons (car Matr) (AddStr2 (- N 1) M TV (cdr Matr))))
))

;N - номер первого города, M  - номер второго города, K - длина дороги между ними
(defun AddStr1 (N M TV Matr)
    (AddStr2 M N TV (AddStr2 N M TV Matr))
)

;(print (AddStr1 1 2 5 '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))

;Добавление значений из графа GR в матрицу - матрица длины ребер
(defun AddMatrix (TL GR Matr) (cond
    ((null GR) Matr)
    (T (AddMatrix TL (cdr GR) (AddStr1 (TownN (caar GR) TL) (TownN (cadar GR) TL) (cddar GR) Matr) ))
)) 


;(print (AddMatrix '(A B C D) '((A B 1 5) (A D 1 10) (A C 1 4) (B A 2 7) (B C 1 7)) '(() () () ()) )) 
;=> (((2 2 7) (3 1 4) (4 1 10) (2 1 5)) ((3 1 7) (1 2 7) (1 1 5)) ((2 1 7) (1 1 4)) ((1 1 10))) 
;(print (TownN 'A '(A B C D)))

;поиск элемента вектора
(defun elem (N L) (cond
    ((= N 1) (car L))
    (T (elem (- N 1) (cdr L)))
)) 
 
;алгоритм поиска в глубину
;route - текущий маршрут (начальное значение - nil), routes - вектор с векторами маршрутами (начальное значение - nil)
;tr_id -  вершина, до которой необходимо дойти, id - текущая вершина, GRRoad - доступные вершины для текущей, GR - граф всех вершин

;функция, которой можно проверить дороги
(defun dfs_с (tr_id id route routes GRRoad GR) (cond
    ((= tr_id id) (cons (cons id route) routes))
    ((null GRRoad) routes)
    ((member (caar GRRoad) route) (dfs_с tr_id id route routes (cdr GRRoad) GR))
    (T (dfs_с tr_id id route (dfs_с tr_id (caar GRRoad) (cons id route) routes (elem (caar GRRoad) GR) GR) (cdr GRRoad) GR))
))

;(print (dfs_с 2 1 '() '() '((2 2 7) (3 1 4) (4 1 10) (2 1 5)) '(((2 2 7) (3 1 4) (4 1 10) (2 1 5)) ((3 1 7) (1 2 7) (1 1 5)) ((2 1 7) (1 1 4)) ((1 1 10))) ))