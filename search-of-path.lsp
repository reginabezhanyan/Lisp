(defconstant INT_MAX 2147483647)

;Построение списка всех городов графа
;Acc - начальное значение ()
(defun Town (GR Acc) (cond
    ((null GR) Acc)
    ((member (caar GR) Acc) (cond 
        ((member (cadar GR) Acc) (Town (cdr GR) Acc))
        (T (Town (cdr GR) (cons (cadar GR) Acc)))
    ))
    ((member (cadar GR) Acc) (Town (cdr GR) (cons (caar GR) Acc)))
    (T (Town (cdr GR) (cons (cadar GR) (cons (caar GR) Acc))))
)) 

;Составление матрицы NxN - список из N nil
(defun Matrix (N) (cond
    ((= N 0) nil)
    (T (cons nil (Matrix (- N 1))))
))

;(print (Matrix 4))

;поиск элемента вектора
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

;алгоритм поиска в глубину
 
;подсчет пересадок
(defun Trans (TrH TrT K) (cond
    ((= TrH 0) K)
    ((= TrH TrT) K)
    (T (+ K 1))
))

;номер города в списке городов
;TL - список городов
(defun TownN (T1 TL) (cond
    ((eq T1 (car TL)) 1)
    (T (+ 1 (TownN T1 (cdr TL))))
))

;Добавление значений из графа GR в матрицу
(defun AddStr2 (N M TV Matr) (cond
    ((= N 1) (cons (cons (cons M TV) (car Matr)) (cdr Matr)) )
    (T (cons (car Matr) (AddStr2 (- N 1) M TV (cdr Matr))))
))

;N - номер первого города, M  - номер второго города, TV - вектор из Типа дороги и длины дороги
(defun AddStr1 (N M TV Matr)
    (AddStr2 M N TV (AddStr2 N M TV Matr))
) 

;Добавление значений из графа GR в матрицу  
(defun AddMatrix (TL GR Matr) (cond
    ((null GR) Matr)
    (T (AddMatrix TL (cdr GR) (AddStr1 (TownN (caar GR) TL) (TownN (cadar GR) TL) (cddar GR) Matr) ))
)) 
 
;перевод из номера города в название города по списку городов
(defun TownT (T1 TL) (cond
    ((= T1 1) (car TL))
    (T (TownT (- T1 1) (cdr TL)))
)) 

;преобразует путь в красивый вид: переводит номера городов в их названия и переворачивает список
(defun TransformR (route Acc TL) (cond
    ((null route) Acc)
    (T (TransformR (cdr route) (cons (TownT (car route) TL) Acc) TL))
))

;переворот списка
(defun rev (tran Acc) (cond
    ((null tran) Acc)
    (T (rev (cdr tran) (cons (car tran) Acc)))
))

;основной алгоритм (поиск в глубину), в котором сразу ведется подсчет количества пересадок и длина маршрута 
;tr_id -  вершина, до которой необходимо дойти, id - текущая вершина, TrH - тип дороги, по которому мы ехали раньше (начальное - 0, 1 - автомобильная, 2 - ж/д дорога) 
;Tr - количество пересадок (начальное значение - 0), K - длина пути (начальное значение - 0)
;tran - вектор пересадок (начальное значение - nil), route - текущий маршрут (начальное значение - nil)
;routes - вектор с векторами маршрутами (начальное значение - nil), GRRoad - доступные вершины для текущей, GR - граф всех вершин
(defun dfs (tr_id id TrH Tr K tran route routes GRRoad GR TL) (cond
    ((= tr_id id) (cons (list K Tr (TransformR (cons id route) nil TL) (rev tran nil)) routes))
    ((null GRRoad) routes)
    ((member (index GRRoad 1 1) route) (dfs tr_id id TrH Tr K tran route routes (cdr GRRoad) GR TL))
    (T (dfs tr_id id TrH Tr K tran route (dfs tr_id (caar GRRoad) (index GRRoad 1 2) (Trans TrH (index GRRoad 1 2) Tr) (+ K (index GRRoad 1 3)) 
            (cons (index GRRoad 1 2) tran) (cons id route) routes (elem (caar GRRoad) GR) GR TL) (cdr GRRoad) GR TL))
))

;L - вектор путей, minR - текущий минимальный путь (начальное значение INT_MAX == 2147483647), i - номер элемента, по которому идет сравнение
(defun minRoute (L i minR) (cond
    ((null L) minR)
    ((< (index L 1 i) minR) (minRoute (cdr L) i (index L 1 i)))
    (T (minRoute (cdr L) i minR))
))

(defun minTrans (L minTr) (cond
    ((null L) L)
    ((= (index L 1 2) minTr) (print (car L)) (minTrans (cdr L) minTr))
    (T (minTrans (cdr L) minTr))
))

;Главная функция
;L - список всех доступных путей T1 -> T2, minR - минимальный путь, minT - 1.5*minR, Acc - накапливается вектор с длинной < 1.5*min
(defun search4 (L minR minT Acc) (cond
    ((null L) (print "Пути с минимальным числом пересадок, длина которых < 1.5 * minRoute:") (minTrans (sort Acc #'<= :key #'first) (minRoute Acc 2 2147483647)))
    ((= (index L 1 1) minR) (search4 (cdr L) minR minT (cons (print (car L)) Acc)) )  
    ((< (index L 1 1) minT) (search4 (cdr L) minR minT (cons (car L) Acc)))
    (T (search4 (cdr L) minR minT Acc))
)) 

;L - список всех путей T1 -> T2 с длиной пути и количестовом пересадок
(defun search3 (L TL GR) (print "Ответ:") (cond
    ((null L) (print "Искомого пути не существует, вот другой путь:") (print (cons (list (TownT 1 TL) (TownT (caaar GR) TL)) (cdaar GR))))
    (T (print "Минимальные по длине пути:") (search4 L (minRoute L 1 INT_MAX) (* 1.5 (minRoute L 1 INT_MAX)) nil))
))

;T1 - номер 1й вершины, T2 - номер последней вершины, GR - измененный граф
(defun search2 (T1 T2 TL GR)
    (print "Найденные всевозможные пути T1 -> T2:")
    (search3 (print (dfs T2 T1 0 0 0 nil nil nil (elem T1 GR) GR TL)) TL GR)
)

;TL - вектор городов
(defun search1 (TL T1 T2 GR)
    (print "Заполненная матрица:") 
    (search2 (TownN T1 TL) (TownN T2 TL) TL (print (AddMatrix TL GR (Matrix (length TL)))))
)

;GR - входной граф, T1 -> T2
(defun searchR (T1 T2 GR)
    (print "Список городов TL:")
    (search1 (print (Town GR nil)) T1 T2 GR)
)

;(searchR 'A 'B '((A B 1 5) (A D 1 10) (A C 1 4) (B C 1 7)) )
;(searchR 'A 'B '((A B 1 5) (A D 1 3) (A C 1 4) (B C 1 7) (B D 2 2)) )
;(searchR 'A 'B '((A B 1 10) (A D 1 3) (A C 1 4) (B C 1 7) (B D 2 8)) )
;(searchR 'A 'F '((A B 1 5) (A C 1 4) (B C 1 7) (F D 1 5)))