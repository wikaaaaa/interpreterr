# interpreter

Iterpreter języka imperatywnego, wzorowanego na języku Latte.
* Gramatyka języka znajduję się w pliku gramar.cf
* Deklaracja języka znajduję się w pliku deklaracja.pdf
* Gramatyka Latte, na której wzorowany jest język: https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2022/Latte/Latte.cf

***

Uruchomienie interpretera na students:
1. make
2. ./interpreter program

 gdzie program oznacza plik z programem do interpretacji.

Można też uruchomić interperter bez podawania nazwy pliku, wtedy można podać treść programu na wejście i zaakceptować Ctrl + D.


***

Są pewne różnice względem wstępnej deklaracji, np:
* tablice i krotki zostaną zaimplementowane dopiero w drugim terminie
* doprecyzowanie struktury funkcji, m.in.:
    * każda funkcja musi zawierać return
    * return musi znajdować się na końcu bloku
    * jeden return na jedną funkcję
    * return nie może znajdować się w zagnieżdzonych blokach while i if
* Plik deklaracja.pdf zawiera aktualną deklarację języka

***

AKTUALNA TABELA CECH:

    Na 15 punktów
    +  01 (trzy typy)
    +  02 (literały, arytmetyka, porównania)
    +  03 (zmienne, przypisanie)
    +  04 (print)
    +  05 (while, if)
    +  06 (funkcje lub procedury, rekurencja)
    +  07 (przez zmienną / przez wartość / in/out)
    -  08 (zmienne read-only i pętla for)
    Na 20 punktów
    +  09 (przesłanianie i statyczne wiązanie)
    +  10 (obsługa błędów wykonania)
    +  11 (funkcje zwracające wartość)
    Na 30 punktów
    +  12 (4) (statyczne typowanie)
    +  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
    -  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
    -  15 (2) (krotki z przypisaniem)
    +  16 (1) (break, continue)
    -  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
    -  18 (3) (generatory)

Razem: 27