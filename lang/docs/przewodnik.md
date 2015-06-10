# Części projektu

1. fpga
    Znajduje się tutaj część sprzętowa projektu, tj. implementacja sprzętowego koprocesora dla układu FPGA
    w języku opisu sprzętu VHDL. Na tą część składa się głównie projekt programu Quartus, który używany jest
    do implementacji układu. Kluczowe są pliki .vhdl, które zawierają opis faktycznej logiki programu, choć
    nie bez znaczenia są również pliki .qsf (przypisania pinów) oraz .sdc (design constraints).

2. lang
    Tutaj znajduje się implementacja kompilatora języka programowania Radom. Kompilator pisany jest w języku
    Haskell, a pliki źródłowe znajdują się w folderze src. Plik .cabal określa zależności projektu i sposób jego
    budowania.


# Uruchamianie

1. fpga
    Potrzebny jest program Quartus w wersji co najmniej 13.0, który możliwy jest do pobrania ze strony Altery (https://dl.altera.com/?edition=web). Instalacja Quartusa potrzebuje, zależnie od platformy, kilkanaście gigabajtów wolnego miejsca na dysku.
    Dodatkowo potrzebny jest sterownik USB-Blaster (zainstalowanie go na systemie Windows jest bezproblemowe,
    natomiast na Linuksie wymaga modyfikacji reguł udev). Następnie wystarczy zaimportować projekt do Quartusa
    i uruchomić przyciskiem. Po pomyślnym ukończeniu kompilacji można załadować projekt na płytkę (testowane
    na płytce Altera De0-Nano z układem Cyclone IV).

    Do komunikacji z komputerem wykorzystywany jest UART, czyli potrzebny jest kabel z wtyczką RS-232 lub,
    w przypadku Raspberry Pi, bezpośrednie połączenie pinów GPIO.

2. lang
    Budowanie przebiega w sposób standardowy dla wszystkich projektów Haskella. Potrzebny jest kompilator Haskella
    (ghc, w wersji 7.8.4, dostępny ze strony ghc.haskell.org) oraz program cabal (również ze strony haskell.org)
    w wersji co najmniej 1.20. Uaktualnić program cabal można za pomocą poleceń:

        cabal update
        cabal install cabal cabal-install   # powtorzenie slowa 'cabal' jest zamierzone

    Następnie przechodzimy do katalogu 'lang' w repozytorium i wykonujemy:

        cabal sandbox init  # spowoduje, że pakiety będą w odizolowanym środowisku
        cabal install

    Poszczególne moduły można przetestować interaktywnie, wykonując:

        cabal exec ghci  # uruchomi interpreter haskella, który będzie "widział" wszystkie moduły

    Lub odszukać plik wykonywalny w katalogu dist.

# Podłączenie układu

Do komunikacji między układem fpga i komputerem potrzebne są pewne komponenty. Można użyć dedykowanego kabla RS-232, jednakże w przypadku większości współczesnych komputerów to rozwiązanie wymaga użycia dodatkowej przejściówki
USB na RS-232 oraz emulatora terminala. Rozwiązaniem dostarczającym dużo więcej radości z użytkowania jest
wykorzystanie do uruchomienia całego ekosystemu języka komputera pokroju Rasberry Pi, który m.in. udostępnia
złącza GPIO. Dzięki temu możemy podłączyć bezpośrednio piny GPIO na FPGA i Raspberry Pi. Najprościej zrealizować to
przy użyciu płytki stykowej (breadboard). Podłączamy pin RX na RPi do pinu TX na FPGA i vice-versa oraz łączymy ze sobą masy. Dla bezpieczeństwa polecane jest wpiąć optoizolatory lub chociaż oporniki 1kOhm.

