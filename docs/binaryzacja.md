# Protokół binaryzacji danych

## Konwencja

Każdy z symboli posiada swoje unikalne ID o długości 8 bitów. Następnie niektórych symboli (JumpIP, JumpIPZ, JumpZ, Jump, Label) kodowane jest kolejne 8 bitów przechowywanej informacji. Dla symboli Load i Store kodowane jest 8 bitów adresu i 8 bitów rozmiaru. Dla Push kodowane jest 8 bitów rozmiaru i po 8 bitów na każdą z ośmiu wartości.

* **ADD** - 0 :: Word8
* **SUB** - 1 :: Word8
* **Mul** - 2 :: Word8
* **Div** - 3 :: Word8
* **Dup** - 4 :: Word8
* **AddS** - 5 :: Word8
* **SubS** - 6 :: Word8
* **MulS** - 7 :: Word8
* **DivS** - 8 :: Word8
* **MovS1** - 9 :: Word8
* **MovS2** - 10 :: Word8
* **JumpIP** - 11 :: Word8 <> i :: Word8
* **JumpIPZ** - 12 :: Word8 <> i :: Word8
* **JumpZ** - 13 :: Word8 <> i :: Word8
* **Jump** - 14 :: Word8 <> i :: Word8
* **Label** - 15 :: Word8 <> i :: Word8
* **Load** - 16 :: Word8 <> adres :: Word8 <> rozmiar :: Word8
* **Store** - 17 :: Word8 <> adres :: Word8 <> rozmiar :: Word8
* **Push** - 18 :: Word8 <> size :: Word8 <> dane :: 8 * Word8

## Zastosowanie
W tej chwili main.hs zapisuje program do pliku "binarka". Jego zawartość można podejrzeć poleceniem **xxd -b binarka**. Uzywanie polecenia hexdump może wprowadzać w błąd :) Rozmiary pola ID mogą być za duże (wystarczyłoby po 5 bitów), jednak okrągłe ósemki o wiele łatwiej debugować.