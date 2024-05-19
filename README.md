# Zmodyfikowany język Latte

Interpreter interpretuje język będący zmodyfikowaną wersją języka Latte. Aby bo skompilować należy wykonać polecenie
```
make
```
Aby wykonać program zapisany w pliku ```prog.txt``` należy wykonać
```
./interpreter prog.txt
```
Możliwe jest także wykonanie programu wczytanego z wejścia standardowego przy użyciu
```
./intepreter < prog.txt
```

Główne nieoczywiste zasady poprawności programów:
- Program jest listą definicji funkcji
- Program musi zawierać funkcję main, która nie ma żadnych argumentów i zwraca int
- Wykonanie programu zaczyna się od wykonania funkcji main
- Każda funkcja musi posiadać return oraz każde wykonanie funkcji musi kończyć się returnem
- Aby przekazać argument do funkcji poprzez referencję należy poprzedzić zmienną słowem kluczowym ```var```
- print() wypisuje na wyjście standardowe bez znaku nowej linii
- println() wypisuje na wyjście standardowe ze znakiem nowej linii
- Zmienne nie mogą być niezainicjowane, tzn. ```int a;``` nie przechodzi, ale ```int a = 5;``` już tak

Przykłady użycia znajdują się w folderze ```good```.

## Tabelka cech
Na 15 punktów
- **1.** trzy typy - **TAK**
- **2.** literały, arytmetyka, porównania - **TAK**
- **3.** zmienne, przypisanie - **TAK**
- **4.** print - **TAK**
- **5.** while, if - **TAK**
- **6.** funkcje lub procedury, rekurencja - **TAK**
- **7.** przez zmienną / przez wartość / in/out - **TAK**
- **8.** zmienne read-only i pętla for - **NIE**

Na 20 punktów
- **9.** przesłanianie i statyczne wiązanie - **TAK**
- **10.** obsługa błędów wykonania - **TAK**
- **11.** funkcje zwracające wartość - **TAK**

Na 30 punktów
- **12.** statyczne typowanie **(4)** - **TAK**
- **13.** funkcje zagnieżdżone ze statycznym wiązaniem **(2)** - **TAK**
- **14.** rekordy/listy/tablice/tablice wielowymiarowe **(1 lub 2)** - **NIE**
- **15.** krotki z przypisaniem **(2)** - **NIE**
- **16.** break, continue **(1)** - **NIE**
- **17.** funkcje wyższego rzędu, anonimowe, domknięcia **(4)** - **NIE**
- **18.** generatory **(3)** - **NIE**

**Razem 26/30**
