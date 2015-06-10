# Dokumentacja techniczna

## Język

Jedną z kluczowych części projektu jest język programowania o nazwie kodowej Radom. Język w zamierzeniu
ma umożliwiać łatwe i wydajne operacje na wektorach i macierzach oraz dobrze integrować się z drugą częścią
projektu, czyli koprocesorem FPGA.

### Główne koncepcje języka

1. Statyczne typowanie
    Pozwala uniknąć dużej ilości błędów czasu wykonia programu oraz ogranicza potrzebę pisania testów
    do testowania logiki programu, a nie jego semantyki. Doświadczenie uczy, że przy projektach pisanych
    w językach dynamicznych, ilość testów niejednokrotnie przekracza ilość kodu obsługującego logikę biznesową,
    a większość z nich poświęcona jest zagadnieniom takim, jak np. zwracanie odpowiedniego typu przez metodę.
    Przeniesienie takiego sprawdzenia poprawności na czas kompilacji pozwala zaoszczędzić czas i uniknąć
    krępujących błędów w produkcji.

2. Prostota
    Język posiada maksymalnie odchudzoną składnię i nie zawiera skomplikowanych konstrukcji, które
    mogłyby utrudniać zrozumienie czy optymalizację programu.

3. Wektorowość
    Język przystosowany jest do danych wektorowych co znajduje swoje odzwierciedlenie w składni. Dzięki
    temu operacje na wektorach są proste i wydajne.


### Składnia

Składnia języka przypomina nieco język Ruby, rozszerzony o statyczne typowanie.

1. Zmienne

        var x : Int = 5
        var v : IntVector[5] = [1, 2, 3, 4, 5]

    Deklaracje zmiennych są zawsze połączone z ich inicjalizacją, co eliminuje błędy polegające na
    odwoływaniu się do niezainicjalizowanych zmiennych. Takie ograniczenie nie odbiera siły ekspresji
    językowi, a utrudnia popełnienie błędu.

2. Typy danych
    Główne typy danych to: liczby całkowite (Int), wektory liczb całkowitych (IntVector), zmienne logiczne
    (Bool) oraz atomy. Liczby całkowite są 32-bitowe, natomiast wartości logiczne przechowywane są na jednym
    bicie (w przeciwieństwie do większości obecnych kompilatorów, w których taka zmienna zajmuje dokładnie
    tyle samo miejsca, co int). Atomy są koncepcyjnie identyczne jak atomy w języku Erlang -- nie niosą ze
    sobą żadnej wartości, są tylko identyfikatorami. Dzieki temu są niezwykle lekkie i wydajne w obsłudze.

    Dostępne są również macierze, jednak nie są one, ściśle mówiąc, osobnym typem, lecz nakładką na wektory.
    Podejście polegające na linearyzowaniu wszystkich macierzy jest popularnym i w zasadzie jedynym sposobem
    przechowywania macierzy w bibliotekach naukowych i językach takich, jak Matlab.

3. Instrukcja warunkowa

    var y: Int = if x > 10 then
                 11
             else
                 x + 1
             end

    Mówiąc dokładniej jest to wyrażenie warunkowe, gdyż zawsze zwraca wartość. Dzięki temu możemy podstawić
    powyższe wyrażenie pod zmienną. Nie ma znaczenia rozłożenie białych znaków, a więc powyższy fragment kodu
    moglibyśmy zapisać również w bardziej zwartej formie:

    var y: Int = if x > 10 then 11 else x + 1 end

4. Pętle

    for (x: Int) in [1, 2, 3, 4, 5] do
        a = x + 5
        x * 2
    end

    For jest jedyną pętlą dostępną w języku Radom. Jest to specyficzna odmiana pętli, gdyż polega na iteracji
    po wektorze. Można ją również traktować jako konstruktor nowego wektora: powyższa pętla zwróci wynik
    [12, 14, 16, 18, 20]. Tak więc za pomocą jednej instrukcji obsługiwane są dwie koncepcje: pętle oraz
    tzw. for-comprehensions.

5. Funkcje
    Definicja funkcji odbywa się za pomocą słowa kluczowego "def":
    
    def square(x: Int): Int do
        x * x
    end

    def axpy(a: Int, x: IntVector[s], y: IntVector[s]): IntVector do
        var ax = a * x  # you can declare variables inside functions
        y + ax
    end

    Funkcje nie używają słowa kluczowego "return". Zamiast tego zawsze zwracana jest wartość ostatniej
    instrukcji w funkcji (jako, że w języku Radom wszystko jest wyrażeniem, a więc ma jakąś wartość,
    mamy pewność, że nie dostaniemy nieokreślonej wartości).


