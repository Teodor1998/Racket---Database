;#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()
    )
)

;Tabela: ("nume" (("col1" "col2" "col3") ("camp1" "camp2" "camp3")))
(define create-table
  (λ (table columns-name)
    (list table columns-name)
    )
  )

(define get-name
  (λ (table)
    (car table)
    )
  )

(define get-columns
  (λ (table)
    (cadr table)
    )
  )

;db: (tabela1 tabela2 ..)
(define get-tables
  (λ (db)
    db
    )
  )

(define get-table
  (λ (db table-name)
    ;Functie anonima pentru filter: pt fiecare tabela, daca numele ei = numele dat, o intorc
    (car (filter (λ (current-table) (equal? (get-name current-table) table-name)) (get-tables db)))
    )
  )

(define add-table
  (λ (db table)
    (reverse (apply list table (reverse (get-tables db))))
    )
  )

(define remove-table
  (λ (db table-name)
    (remove (get-table db table-name) db)
    )
  )

            ;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

;Functie care genereaza o lista care are pe pozitiile corecte elementele intrarii (sau null)
;ex: ("Student" (("nume" "prenume" "grupa")("Apostol" "Teodor" "322CC"))
;functia genereaza lista ("Apostol" "Teodor" "322CC")
;Se vor parcurge prin recursivitate coloanele tabelei, extragandu-se pt fiecare din record-info datele
(define generate-record
  (λ (table-columns record-info)
    (if (null? table-columns)
        '()
        ;current-info din let va fi valoarea corespunzatoarea coloanei curente extrasa din record-info
        (let ((current-info (filter (λ (current-info) (equal? (car table-columns) (car current-info))) record-info)))
          (if (null? current-info)
              (reverse (apply list '() (reverse (generate-record (cdr table-columns) record-info))))
              (reverse (apply list (cdr (car current-info)) (reverse (generate-record (cdr table-columns) record-info))))
              )
          )
        )
    )
  )

;Functie pentru inlocuirea elementelor '() cu null.
(define replace-null
  (λ (list1)
    (map (λ (element) (if (null? element) NULL element)) list1)
    )
  )

;Adaug la tabela record-ul generat
(define add-record
  (λ (table record)
    (reverse (apply list (reverse (replace-null (generate-record (get-columns table) record))) (reverse table)))
    )
  )

;Aplic modificarile in baza de date
(define insert
  (λ (db table-name record)
    (map (λ (current-table) (if (equal? (get-name current-table) table-name) (add-record current-table record) current-table)) db)
    )
  )

(define db (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (add-table (add-table (init-database) (create-table "Studenți" '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie"))) (create-table "Cursuri" '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))) "Studenți"
                      (list '("Număr matricol" . 123)
                            '("Nume" . "Ionescu")
                            '("Prenume" . "Gigel")
                            '("Grupă" . "321CA")
                            '("Medie" . 9.82)))
        "Studenți" (list '("Număr matricol" . 124)
                            '("Nume" . "Popescu")
                            '("Prenume" . "Maria")
                            '("Grupă" . "321CB")
                            '("Medie" . 9.91)))
        "Studenți" (list '("Număr matricol" . 125)
                            '("Nume" . "Popa")
                            '("Prenume" . "Ionel")
                            '("Grupă" . "321CC")
                            '("Medie" . 9.99)))
        "Studenți" (list '("Număr matricol" . 126)
                            '("Nume" . "Georgescu")
                            '("Prenume" . "Ioana")
                            '("Grupă" . "321CD")
                            '("Medie" . 9.87)))
        
        "Cursuri" (list '("Anul" . "I")
                            '("Semestru" . "I")
                            '("Disciplină" . "Programarea calculatoarelor")
                            '("Număr credite" . 5)
                            '("Număr teme" . 2)))
        "Cursuri" (list '("Anul" . "II")
                            '("Semestru" . "II")
                            '("Disciplină" . "Paradigme de programare")
                            '("Număr credite" . 6)
                            '("Număr teme" . 3)))
        "Cursuri" (list '("Anul" . "III")
                            '("Semestru" . "I")
                            '("Disciplină" . "Algoritmi paraleli și distribuiți")
                            '("Număr credite" . 5)
                            '("Număr teme" . 3)))
        "Cursuri" (list '("Anul" . "IV")
                            '("Semestru" . "I")
                            '("Disciplină" . "Inteligență artificială")
                            '("Număr credite" . 6)
                            '("Număr teme" . 3)))
        "Cursuri" (list '("Anul" . "I")
                            '("Semestru" . "II")
                            '("Disciplină" . "Structuri de date")
                            '("Număr credite" . 5)
                            '("Număr teme" . 3)))
        "Cursuri" (list '("Anul" . "III")
                            '("Semestru" . "II")
                            '("Disciplină" . "Baze de date")
                            '("Număr credite" . 5)
                            '("Număr teme" . 0))))

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

;Functie ce ia un element de la un anumit index
(define get-pos
  (λ (list1 index)
    (if (= index 0)
        (car list1)
        (get-pos (cdr list1) (- index 1))
        )
    )
  )

;Functie care ia elementele de pe acelasi index din liste diferite
(define multi-get-pos
  (λ (lists index)
    (if (null? lists)
        '()
        (reverse (apply list (get-pos (car lists) index) (reverse (multi-get-pos (cdr lists) index))))
        )
   )
 )

;Functie ce gaseste index-ul unul element intr-o lista
(define get-index
  (λ (list search cont)
    (if (equal? (car list) search)
        cont
        (get-index (cdr list) search (+ cont 1))
     )
    )
  )

(define simple-select
  (λ (db table-name columns)
    (if (null? columns)
        '()
        (remove-nulls (apply list (reverse (multi-get-pos (cdr (cdr (get-table db table-name))) (get-index (get-columns (get-table db table-name)) (car columns) 0))) (simple-select db table-name (cdr columns))))
     )
    )
  )


;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

;Functii pentru "despachetarea conditiei"

;Extrage comparatorul
(define cond-op
  (λ (condition)
    (car condition)
    )
  )

;Extrage coloana pe care se aplica conditia
(define cond-column
  (λ (condition)
    (cadr condition)
    )
  )

;Extrage elementul cu care se compara coloana
(define cond-with
  (λ (condition)
    (cadr (cdr condition))
    )
  )

;Inainte de toate, reconstruiesc tabela doar cu acele elemente ce indeplinesc conditia
(define conditioned-table
  (λ (table entry-list conditions)
    (if (null? conditions)
        entry-list
        (conditioned-table table (filter (λ (entry)
                                           (if (equal? (get-pos entry (get-index (get-columns table) (cond-column (car conditions)) 0)) 'null)
                                               #f
                                               ((cond-op (car conditions)) (get-pos entry (get-index (get-columns table) (cond-column (car conditions)) 0)) (cond-with (car conditions)))
                                               )
                                           )
                                         entry-list) (cdr conditions))
        )
    )
  )

;Din tabela extrag minimul de pe o anumita coloana (parcurgand direct intrarile tabelei)
(define min
  (λ (list1 index)
    (foldr (λ (elem acc)
             (if (null? (get-pos elem index))
                 acc
                 (if (> (get-pos elem index) acc)
                     acc
                     (get-pos elem index)
                     )
                 )
             )
           (get-pos (car list1) index)
           list1
           )
    )
  )

;Extrag maximul, parcurgand direct intrarile tabelei
(define max
  (λ (list1 index)
    (foldr (λ (elem acc)
             (if (null? (get-pos elem index))
                 acc
                 (if (< (get-pos elem index) acc) acc (get-pos elem index))
                 )
             )
           (get-pos (car list1) index)
           list1
           )
    )
  )

;Contorizez toate elementele (inclusiv cele care se repeta)
(define count_all
  (λ (list1 index)
    (foldr (λ (elem acc)
             (if (null? (get-pos elem index))
                 acc
                 (+ acc 1)
                 )
             )
           0
           list1
           )
    )
  )

;Contorizez elementele (le iau doar o data pe cele care se repeta)
(define count
  (λ (list1 acc)
    (if (null? list1)
        acc
        (if (member (car list1) (cdr list1))
            (count (cdr list1) acc)
            (count (cdr list1) (+ acc 1))
            )
        )
     )
  )

;Calculez suma de pe o coloana (parcurgand direct lista de intrari)
(define sum
  (λ (list1 index)
    (foldr (λ (elem acc)
             (if (null? (get-pos elem index))
                 acc
                 (+ acc (get-pos elem index))
                 )
             )
           0
           list1
           )
    )
  )

;Calculez media pe baza functiilor sum si count_all
(define avg
  (λ (list1 index)
    (/ (sum list1 index) (count_all list1 index))
    )
  )

;Urmatoarele functii le voi folosi pentru sortare:

;Elimin prima aparitie a unui element dintr-o lista
(define remove-simple
  (λ (elem list1)
    (if (null? list1)
        '()
        (if (equal? (car list1) elem)
            (cdr list1)
            (cons (car list1) (remove-simple elem (cdr list1)))
            )
        )
    )
  )

;Extrag cel mai mic element dintr-o lista de numere
(define get-min-simple
  (λ (list1)
    (foldr (λ (elem acc)
             (if (> elem acc)
                 acc
                 elem
                 )
             )
           (car list1)
           list1
           )
   )
  )

;Extrag cel mai mare element dintr-o lista de numere
(define get-max-simple
  (λ (list1)
    (foldr (λ (elem acc)
             (if (< elem acc)
                 acc
                 elem
                 )
             )
           (car list1)
           list1
           )
   )
  )

;Sortez crescator:
(define sort-asc
  (λ (list1)
    (if (null? list1)
        '()
        (cons (get-min-simple list1) (sort-asc (remove-simple (get-min-simple list1) list1)))
        )
    )
  )

;Sortez descrescator:
(define sort-desc
  (λ (list1)
    (if (null? list1)
        '()
        (cons (get-max-simple list1) (sort-desc (remove-simple (get-max-simple list1) list1)))
        )
    )
  )

(define manage-columns
  (λ (table entry-list columns)
    (if (null? columns)
        '()
        (if (pair? (car columns))
            (if (equal? (car (car columns)) 'min)
                (apply list (min entry-list (get-index (get-columns table) (cdr (car columns)) 0)) (manage-columns table entry-list (cdr columns)))
                (if (equal? (car (car columns)) 'max)
                    (apply list (max entry-list (get-index (get-columns table) (cdr (car columns)) 0)) (manage-columns table entry-list (cdr columns)))
                    (if (equal? (car (car columns)) 'count)
                        (apply list (count (multi-get-pos entry-list (get-index (get-columns table) (cdr (car columns)) 0)) 0) (manage-columns table entry-list (cdr columns)))
                        (if (equal? (car (car columns)) 'sum)
                            (apply list (sum entry-list (get-index (get-columns table) (cdr (car columns)) 0)) (manage-columns table entry-list (cdr columns)))
                            (if (equal? (car (car columns)) 'avg)
                                (apply list (avg entry-list (get-index (get-columns table) (cdr (car columns)) 0)) (manage-columns table entry-list (cdr columns)))
                                (if (equal? (car (car columns)) 'sort-asc)
                                    ;Functiilor de sortare le dau lista prelucrata, adica lista cu elementele caracteristice coloanelor extrase, nu lista de intrari
                                    (apply list (sort-asc (multi-get-pos entry-list (get-index (get-columns table) (cdr (car columns)) 0))) (manage-columns table entry-list (cdr columns)))
                                    (if (equal? (car (car columns)) 'sort-desc)
                                        (apply list (sort-desc (multi-get-pos entry-list (get-index (get-columns table) (cdr (car columns)) 0))) (manage-columns table entry-list (cdr columns)))
                                        '()
                                        )
                                    )
                                )
                            )
                        )
                 )
                )
            (apply list (reverse (multi-get-pos entry-list (get-index (get-columns table) (car columns) 0))) (manage-columns table entry-list (cdr columns)))
            )
        )
    )
  )
  
(define select
  (λ (db table-name columns conditions)
    (manage-columns (get-table db table-name) (conditioned-table (get-table db table-name) (cdr (cdr (get-table db table-name))) conditions) columns)
    )
  )

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

;Voi reconstrui pas cu pas intrarea, fie cu vechiul element fie cu cel nou (daca exista)
(define update-record
  (λ (table-columns record-info entry)
    (if (null? table-columns)
        '()
        ;current-info din let va fi valoarea corespunzatoarea coloanei curente extrasa din record-info
        (let ((current-info (filter (λ (current-info) (equal? (car table-columns) (car current-info))) record-info)))
          (if (null? current-info)
              (cons (car entry) (update-record (cdr table-columns) record-info (cdr entry)))
              (cons (cdr (car current-info)) (update-record (cdr table-columns) record-info (cdr entry)))
              )
          )
        )
    )
  )

;Aleg doar intrarile care satisfac conditiile pentru a fi updatate, celelalte vor ramane neschimbate
(define choose-entry
  (λ (table values conditions)
    (map (λ (entry)
           (if (member entry (conditioned-table table (cdr (cdr table)) conditions))
               (update-record (get-columns table) values entry)
               entry
               )
           )
         (cdr (cdr table))
         )
    )
  )

;Construiesc un tabel avand nume, coloane si intrarile
(define build-table
  (λ (name columns entry-list)
    (if (null? entry-list)
        (create-table name columns)
        (reverse (apply list (car entry-list) (reverse (build-table name columns (cdr entry-list)))))
     )
    )
  )

(define update
  (λ (db table-name values conditions)
    (map (λ (table)
           (if (equal? (get-name table) table-name)
               (build-table table-name (get-columns table) (reverse (choose-entry table values conditions)))
               table
               )
           )
         db
         )
    )
  )

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

;Functie ce elimina toate intrarile ce se afla in conditiile date (folosing functie filtru pentru select)
(define remove-entry
  (λ (table conditions)
    (filter (λ (entry)
              (if (member entry (conditioned-table table (cdr (cdr table)) conditions))
                  #f
                  #t
                  )
              )
            (cdr (cdr table)))
    )
  )

;Functie pentru a converti o lista de forma '(() () ()) -> '()
(define remove-nulls
  (λ (list1)
    (filter (λ (entry)
              (if (null? entry)
                  #f
                  #t
                  )
              )
            list1)
    )
  )

;Modific in place baza de date cu map, inlocuind tabela cautata cu o tabela noua ce contine aceleasi data dar cu intrari filtrate
(define delete
  (λ (db table-name conditions)
    (map (λ (table)
           (if (equal? (get-name table) table-name)
               (build-table table-name (get-columns table) (reverse (remove-entry table conditions)))
               table
               )
           )
         db
         )
    )
  )


;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================

(define column-of
  (λ (table column)
    (member column (get-columns table))
    )
  )

(define natural-join
  (λ (db tables columns conditions)
     ;'your-code-here
    (if (null? columns)
        '()
        (if (column-of (get-table db (car tables)) (car columns))
            (cons (car (manage-columns (get-table db (car tables)) (conditioned-table (get-table db (car tables)) (cdr (cdr (get-table db (car tables)))) conditions) (list (car columns)))) (reverse (natural-join db tables (cdr columns) conditions)))
            (cons (car (manage-columns (get-table db (cadr tables)) (conditioned-table (get-table db (cadr tables)) (cdr (cdr (get-table db (cadr tables)))) conditions) (list (car columns)))) (reverse (natural-join db tables (cdr columns) conditions)))
            )
        )
    )
  )
(natural-join (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (insert (add-table (add-table (init-database) (create-table "Category" '("ID" "Category_Name"))) (create-table "Product" '("ID" "Product_Name"))) "Category" (list '("ID" . 1) '("Category_Name" . "Mobiles"))) "Category" (list '("ID" . 2) '("Category_Name" . "Laptops"))) "Category" (list '("ID" . 3) '("Category_Name" . "Tablet"))) "Category" (list '("ID" . 4) '("Category_Name" . "Cameras"))) "Category" (list '("ID" . 5) '("Category_Name" . "Gaming"))) "Product" (list '("ID" . 1) '("Product_Name" . "Nokia"))) "Product" (list '("ID" . 1) '("Product_Name" . "Samsung"))) "Product" (list '("ID" . 2) '("Product_Name" . "HP"))) "Product" (list '("ID" . 2) '("Product_Name" . "Dell"))) "Product" (list '("ID" . 3) '("Product_Name" . "Apple"))) "Product" (list '("ID" . 4) '("Product_Name" . "Nikon"))) "Product" (list '("Product_Name" . "Playstation"))) '("Product" "Category") '("ID" "Product_Name" "Category_Name") '())
 '((1 1 2 2 3 4) ("Nokia" "Samsung" "HP" "Dell" "Apple" "Nikon") ("Mobiles" "Mobiles" "Laptops" "Laptops" "Tablet" "Cameras"))