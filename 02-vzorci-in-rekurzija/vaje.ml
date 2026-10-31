(*----------------------------------------------------------------------------*
 # Vzorci in rekurzija
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite spodaj opisane funkcije, ne da bi uporabljali funkcije iz standardne
 knjižnice. Kjer to kažejo primeri, napišite tudi repno rekurzivne različice z
 imenom `ime_funkcije_tlrec`.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Funkcija `reverse`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte pomožno funkcijo za obračanje seznamov. Funkcija naj bo repno
 rekurzivna.
[*----------------------------------------------------------------------------*)
(* Definiramo seznam:
type 'a list = [] | 'a :: 'a list --> ali je prazen ali pa sestavljen iz glave in repa
type intlist = Prazen | Sestavljen (int * intlist) 
--> v sestavljen moramo podati število in seznam
Prazen in Sestavljen sta konstruktorja
*)
let rec reverse l = 
  match l with
  | [] -> []
  | x::xs -> (reverse xs) @ [x] (*prvega damo na konec in preostanek obrnemo*)

  (*variantni tip: ločimo možnosti*)
  (* z @ staknemo sezname*)

let reverse_tlrec l =
  let rec reverse_aux l acc = match l with
    | [] -> acc (*če smo konec, vrnemo, kar smo nabrali*)
    | x::xs -> reverse_aux xs (x::acc) (*x dodamo na začetek akumulatorja*)
  in
  reverse_aux l []

(*----------------------------------------------------------------------------*
 ## Funkcija `repeat`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `repeat x n` vrne seznam `n` ponovitev vrednosti `x`. Za neprimerne
  vrednosti `n` funkcija vrne prazen seznam.
[*----------------------------------------------------------------------------*)

let rec repeat x n = 
  if n <= 0 then []
  else x :: repeat x (n-1)

let rec repeat2 x n =
  match n with
  | n when n <= 0 -> [] (*varovani vzorec*)
  | _ -> x :: repeat2 x (n-1)

let rec repeat3 x  =
  function
  | n when n <= 0 -> []
  | n -> x :: repeat3 x (n - 1)

(*zadnja stvar v funkciji je konstrukcija seznama - v teoriji to ni repno rekurzivno,
v praksi pa je to poseben primer repne rekurzije - če je zadnja stvar konstruktor*)

let primer_repeat_1 = repeat "A" 5
(* val primer_repeat_1 : string list = ["A"; "A"; "A"; "A"; "A"] *)

let primer_repeat_2 = repeat "A" (-2)
(* val primer_repeat_2 : string list = [] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `range`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `range` naj sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente naj funkcija vrne prazen
 seznam. Funkcija naj bo repno rekurzivna. Pri tem ne smete uporabiti vgrajene
 funkcije `List.init`.
[*----------------------------------------------------------------------------*)
let rec range n =
  if n < 0 then []
  else (range (n - 1)) @ [n]

let rec range2 n =
  match n with
  | n when n < 0 -> []
  | _ -> (range2 (n - 1)) @ [n]

let rec range3 =
  function
  | n when n < 0 -> []
  | n -> (range3 (n - 1)) @ [n]

let range_tlrec n =
  let rec aux n acc = (*acc je seznam, v katerega shranjujemo rezultate*)
    match n with
    | n when n < 0 -> acc (*ko n postane premajhen, vrnemo akulumator*)
    | _ -> aux (n - 1) (n :: acc) (*dodamo trenutno število na začetek akumulatorja*)
  in
  aux n [] (*pomožno funkcijo začnemo izvajati z začetnim številom n in praznim akumulatorjem*)

let primer_range = range 10
(* val primer_range : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `map`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `map f list` naj sprejme seznam `list` oblike `x0; x1; x2; ...` in
 funkcijo `f` ter vrne seznam preslikanih vrednosti, torej `f x0; f x1; f x2;
 ...`. Pri tem ne smete uporabiti vgrajene funkcije `List.map`.
[*----------------------------------------------------------------------------*)
let rec map f l = 
  match l with
  | [] -> []
  | x::xs -> f x :: map f xs
  (*je repno rekurzivno - zadnja stvar je konstruktor*)

let primer_map_1 =
  let plus_two = (+) 2 in
  map plus_two [0; 1; 2; 3; 4]
(* val primer_map_1 : int list = [2; 3; 4; 5; 6] *)

(*----------------------------------------------------------------------------*
 Napišite še funkcijo `map_tlrec`, ki naj bo repno rekurzivna različica funkcije
 `map`.
[*----------------------------------------------------------------------------*)

let map_tlrec f l = 
  let rec aux l acc =
    match l with
    | [] -> reverse acc 
    (*ko obdelamo vse elemente, obrnemo akumulator, da dobimo pravilen vrstni red*)
    | x::xs -> aux xs (f x::acc) 
    (*izračunamo f x, rezultat dodamo na začetek akumulatorja in nadaljujemo z ostankom seznama*)
  in
  aux l [] (*začnemo z začetnim seznamom l in praznim akumulatorjem*)

let primer_map_2 =
  let plus_two = (+) 2 in
  map_tlrec plus_two [0; 1; 2; 3; 4]
(* val primer_map_2 : int list = [2; 3; 4; 5; 6] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `mapi`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `mapi` naj bo ekvivalentna Python kodi:

 ```python
 def mapi(f, lst):
     mapi_lst = []
     index = 0
     for x in lst:
         mapi_lst.append(f(index, x))
         index += 1
     return mapi_lst
 ```

 Pri tem ne smete uporabiti vgrajene funkcije `List.mapi`.
[*----------------------------------------------------------------------------*)

let mapi f = 
  let rec aux acc i = function
  | [] -> reverse acc
  | x::xs -> aux (f i x::acc) (i + 1) xs
in
aux [] 0

let primer_mapi = mapi (+) [0; 0; 0; 2; 2; 2]
(* val primer_mapi : int list = [0; 1; 2; 5; 6; 7] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `zip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `zip` naj sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine, naj vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije `List.combine`.
[*----------------------------------------------------------------------------*)

let rec zip l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x :: xs, y :: ys -> (x,y) :: zip xs ys
  | _ -> failwith "seznama razlicnih dolzin"

let zip2 s1 s2 = 
  let rec aux acc = function 
    | ([], []) -> reverse acc
    | ([], _) | (_, []) -> failwith "seznama razlicnih dolzin"
    | (x :: xs, y :: ys) -> aux ((x,y) :: acc) (xs, ys)
  in
  aux [] (s1, s2)

let primer_zip_1 = zip [1; 1; 1; 1] [0; 1; 2; 3]
(* val primer_zip_1 : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)] *)

(* let primer_zip_2 = zip [1; 1; 1; 1] [1; 2; 3; 4; 5] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `unzip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `unzip` je inverz funkcije `zip`, torej sprejme seznam parov
  `(x0, y0); (x1, y1); ...` in vrne par seznamov (`x0; x1; ...`, `y0; y1; ...`).
  Pri tem ne smete uporabiti vgrajene funkcije `List.split`.
[*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> [], []
  | (x, y) :: xs ->
    let left, right = unzip xs in
    x :: left, y :: right

let rec unzip2 l =
  match l with
  | [] -> [], []
  | (x, y) :: xs ->
    (match unzip xs with
      | (left, right) -> x :: left, y :: right)

let primer_unzip_1 = unzip [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_1 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)

(*----------------------------------------------------------------------------*
 Funkcija `unzip_tlrec` je repno rekurzivna različica funkcije `unzip`.
[*----------------------------------------------------------------------------*)

let unzip_tlrec l =
  let rec aux l acc1 acc2 =
    match l with
    | [] -> reverse acc1, reverse acc2
    | (x, y) :: xs -> aux xs (x :: acc1) (y :: acc2)
  in
  aux l [] []

let primer_unzip_2 = unzip_tlrec [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_2 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)

(*----------------------------------------------------------------------------*
 ## Zanke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `loop condition f x` naj se izvede kot python koda:

 ```python
 def loop(condition, f, x):
     while condition(x):
         x = f(x)
     return x
 ```
[*----------------------------------------------------------------------------*)

let rec loop condition f x = 
  (*najprej izračunamo vrednost condition x
  če je true, z novo vrednostjo pokličemo loop
  condition je isti, f je isti, x je f x*)
  if condition x then loop condition f (f x)
  else x

let rec loop2 condition f x =
  match condition x with
  | true -> loop2 condition f (f x)
  | false -> x

let primer_loop = loop (fun x -> x < 10) ((+) 4) 4
(* val primer_loop : int = 12 *)

(*----------------------------------------------------------------------------*
 ## Funkcija `fold_left`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `fold_left_no_acc f list` naj sprejme seznam `x0; x1; ...; xn` in
 funkcijo dveh argumentov `f` in vrne vrednost izračuna `f(... (f (f x0 x1) x2)
 ... xn)`. V primeru seznama z manj kot dvema elementoma naj vrne napako.
[*----------------------------------------------------------------------------*)

let fold_left_no_acc f list  =
  match list with
  | [] | [_] -> failwith "seznam je prekratek" (*seznam z 0 ali 1 elementom*)
  | x0 :: x1 :: xs -> (*vsaj dva elementa - ostanek je lahko prazen*)
    let rec aux acc l = (*acc je do zdaj izračunan rezultat, l so elementi, ki jih je treba še obdelati*)
      match l with
      | [] -> acc (*obdelali smo vse elemente, vrnemo acc, kjer je vse obdelano*)
      | y :: ys -> aux (f acc y) ys (*nov akumulator postane f acc y, nadaljujemo z ys*)
    in
    aux (f x0 x1) xs


let rec fold_left_no_acc2 f = function
    | [] | [_] -> failwith "prekratek seznam"
    | x :: [y] -> f x y (*seznam z natanko dvema elementoma*)
    | x :: y :: ostalo -> fold_left_no_acc2 f (f x y :: ostalo)
    (*seznam z vsak tremi elementi; prva dva elementa zamenjamo z enim, f x y,
      in ga damo nazaj kot glavo seznama*)

let primer_fold_left_no_acc =
  fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"]
(* val primer_fold_left_no_acc : string = "FICUS" *)

(*----------------------------------------------------------------------------*
 ## Funkcija `apply_sequence`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `apply_sequence f x n` naj vrne seznam zaporednih uporab funkcije `f`
 na vrednosti `x` do vključno `n`-te uporabe, torej `[x; f x; f (f x); ...; fⁿ
 x]`. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec apply_sequence f x = function
  | n when n < 0 -> [] 
  (*ni več korakov, vrnemo prazen seznam; to pokrije tudi negativen n na začetku*)
  | n -> x :: (apply_sequence f (f x) (n - 1))
  (*trenutni x postane glava, rep pa je zaporedje, ki se začne pri f x in ima še n - 1 korakov*)

let apply_sequence_tlrec f x = 
  let rec aux acc c = function
  (*acc so doslej zbrane vrednosti (v obratnem vrstnem redu);
  c je trenutna vrednost v zaporedju;
  n (implicitno) pomeni, koliko vrednosti moramo še dodati, minus ena*)
    | n when n < 0 -> reverse acc
    | n -> aux (c :: acc) (f c) (n - 1) 
    (*c dodamo na začetek acc, naslednja vrednost je f c, n zmanjšamo za 1*)
  in
  aux [] x 

let apply_sequence f x n =
  let rec aux acc current k =
    if k < 0 then reverse acc
    else aux (current :: acc) (f current) (k - 1)
  in
  aux [] x n

let primer_apply_sequence_1 = apply_sequence (fun x -> x * x) 2 5
(* val primer_apply_sequence_1 : int list = [2; 4; 16; 256; 65536; 4294967296] *)

let primer_apply_sequence_2 = apply_sequence (fun x -> x * x) 2 (-5)
(* val primer_apply_sequence_2 : int list = [] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `filter`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `filter f list` vrne seznam elementov `list`, pri katerih funkcija `f`
  vrne vrednost `true`.
  Pri tem ne smete uporabiti vgrajene funkcije `List.filter`.
[*----------------------------------------------------------------------------*)

let rec filter f l = 
  match l with
  | [] -> []
  | x :: xs ->
    if f x then x :: filter f xs 
      (*če f x velja, je x v rezultatu; pred njega damo filtriran rep; x :: filter f xs*)
    else filter f xs
    (*če f x ne velja, preskočimo in vrnemo samo filtriran rep; filter f xs*)

let filter_tlrec f l =
  let rec aux acc = function 
  (*acc: elementi, ki so ustrezali, do sedaj zbrani;
  implicitni seznam: elementi, ki jih je treba še pregledati*)
    | [] -> reverse acc 
    (*elemente smo dodajali na začetek, zato so v obratnem vrstnem redu*)
    | x :: xs -> if f x then aux (x :: acc) xs else aux acc xs
    (*če f x velja, x dodamo na začetek akumulatorja in nadaljujemo, 
    sicer akumulator pustimo pri miru*)
  in
  aux [] l

let primer_filter = filter ((<)3) [0; 1; 2; 3; 4; 5]
(* val primer_filter : int list = [4; 5] *)

(*----------------------------------------------------------------------------*
 ## Funkcija `exists`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `exists` sprejme seznam in funkcijo, ter vrne vrednost `true` čim
  obstaja element seznama, za katerega funkcija vrne `true` in `false` sicer.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let rec exists f l =
  match l with
  | [] -> false (*v praznem seznamu ni nobenega elemeta, zato ni takega, ki bi ustrezal*)
  | x :: xs -> f x || exists f xs 
  (*rezultat je true, če ustreza glava x ali če obstaja ustrezen element v repu*)

let rec exists cond = function
  | [] -> false
  | x :: xs when cond x -> true (*glava ustreza pogoju, zato takoj vrnemo true*)
  | x :: xs -> exists cond xs (*sem pridemo le, če prejšnji vzorec ni ustrezal; nadaljujemo z repom*)

let primer_exists_1 = exists ((<) 3) [0; 1; 2; 3; 4; 5]
(* val primer_exists_1 : bool = true *)

let primer_exists_2 = exists ((<) 8) [0; 1; 2; 3; 4; 5]
(* val primer_exists_2 : bool = false *)

(*----------------------------------------------------------------------------*
 ## Funkcija `first`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `first f default list` vrne prvi element seznama, za katerega
  funkcija `f` vrne `true`. Če takšnega elementa ni, vrne `default`.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let rec first f default = function
  | [] -> default (*če pridemo do konca seznama, vrnemo default*)
  | x :: xs when f x -> x (*glava ustreza pogoju, zato jo takoj vrnemo*)
  | x :: xs -> first f default xs 
  (*glava ne ustreza pogoju (sicer bi se izbral prejšnji vzorec), zato iščemo naprej po repu;
  argumenta f in default ostaneta enaka*)

let rec first f default l =
  match l with
  | [] -> default
  | x :: xs -> if f x then x else first f default xs

let primer_first_1 = first ((<) 3) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_1 : int = 5 *)

let primer_first_2 = first ((<) 8) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_2 : int = 0 *)


(*----------------------------------------------------------------------------*)
(*Primeri iz predavanj*)

(*1. Vsota prvih n števil*)
let rec vsota_prvih n =
  if n = 0 then 0 
  else n + vsota_prvih (n - 1)

let rec vsota_prvih2 n =
  match n with
  | 0 -> 0
  | _ -> n + vsota_prvih2 (n - 1)

let rec vsota_prvih3 =
  function
  | 0 -> 0
  | n-> n + vsota_prvih3 (n - 1) 
  
  (* pri function poljubne vrednosti poimenujemo, da jih lahko uporabimo v nadaljevanju*)

(*2. Fakulteta*)
let rec fakulteta n =
  if n = 0 then 1
  else n * fakulteta (n - 1)

let rec fakulteta2 n =
  match n with
  | 0 -> 1
  | _ -> n * fakulteta2 (n - 1)

let rec fakulteta3 =
  function
  | 0 -> 1
  | n -> n * fakulteta3 (n - 1)

(*3.a n-to Fibonaccijevo število*)
let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n - 1) + fib (n - 2) (*seštejemo prejšnji dve Fibonaccijevi števili*)

let rec fib2 n = 
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib2 (n - 1) + fib2 (n - 2)

let rec fib3 =
  function
  | 0 -> 0
  | 1 -> 1
  | n -> fib3 (n - 1) + fib3 (n - 2)

(*3.b n-to Fibonaccijevo število s pomožno funkcijo*)
let hitri_fib n = 
  let rec aux n a b = 
    (*n - koliko korakov še moramo narediti, a je trenutno, b je naslednje Fibonaccijevo število*)
    if n = 0 then a 
    (* ko je n enak 0 smo naredili vse potrebne korake, zato vrnemo a*)
    else aux (n - 1) b (a + b) 
    (*v vsakem koraku n zmanjšamo za 1, a postane b in b postane a + b*)
  in aux n 0 1 (*z 'in' povemo, kaj naj zunanja funkcija izvede s pomožno definirano funkcijo*)

(*Rekurzivne funkcije na seznamih*)
(*1. Vsota elementov seznama*)
let rec vsota =
  function
  | [] -> 0
  | x::xs -> x + vsota xs

(*2. Dolžina seznama oz. število elementov seznama*)
let rec dolzina =
  function
  | [] -> 0
  | _::xs -> 1 + dolzina xs

(*3. List.map oz. uporaba funkcije na vseh elementih seznama*)
let rec map f =
  function
  | [] -> []
  | x::xs -> f x :: map f xs

(*4. Združevanje dveh seznamov v enega*)
let rec ( @ ) sez1 sez2 =
  match sez1 with
  | [] -> sez2 (*če je sez1 prazen, vrnemo sez2*)
  | x::xs -> x :: (xs @ sez2) (*dodamo prvi element sez1 na začetek rezultata, preostanek rekurzivno združimo*)
