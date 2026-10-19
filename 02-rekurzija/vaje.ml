(*----------------------------------------------------------------------------*
 # Rekurzija
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

(*Definiramo seznam:
type 'a list = [] | 'a :: 'a list
type intlist = Prazen | Sestavljen (int * intlist)
v Sestavljen moramo podati število in seznam;
Prazen in Sestavljen sta konstruktorja*)
let rec reverse l = match l with
  | [] -> []
  | x::xs -> (reverse xs) @ [x]

  (* variantni tip -> ločimo možnosti
  prvega damo na konec, preostanek obrnemo
  z @ staknemo sezname
  vsakič delamo rekurzijo na manjši stvari*)

(*Repno rekurzivna varianta: (zadnji klic v funkciji je spet funkcija sama)
torej zadnji klic je reverse
naredimo pomožno funkcijo, ki nosi tisto informacijo
seznam je kot stack: dajemo iz ene strani in jemljemo samo iz te strani
imamo dva kupa: tisti, ki je obrnjen (akumulator: acc) in tisti, ki ga moramo obrniti, ponavljamo, dokler ne zmanjka kupa za obrnit*)

let reverse2 l = 
  let rec reverse_aux l acc = match l with (*seznam za obrnit in akumulator*)
    | [] -> acc (*če smo konec, vrnemo, kar smo nabirali*)
    | x::xs -> reverse_aux xs (x::acc) (*x dodamo na začetek akumulatorja*)
  in
  reverse_aux l []

(*let reverse seznam =
  let rec aux seznam acc =
    match seznam with
    | [] -> acc
    | x :: xs -> aux xs (x :: acc)
  in
  aux seznam []*)

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

(*zadnja stvar v funkciji je konstrukcija seznama - v teoriji ni repno rekurzivno, 
v praksi je to poseben primer repne rekurzije - če je zadnja stvar konstruktor*)

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

let range n = 1
  (*if n < 0 then []*)



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

let rec map f l = match l with
  | [] -> []
  | x :: xs -> (f x) :: (map f xs)
(*je repno rekurzivno - zadnja stvar je konstruktor*)

let primer_map_1 =
  let plus_two = (+) 2 in
  map plus_two [0; 1; 2; 3; 4]
(* val primer_map_1 : int list = [2; 3; 4; 5; 6] *)

(*----------------------------------------------------------------------------*
 Napišite še funkcijo `map_tlrec`, ki naj bo repno rekurzivna različica funkcije
 `map`.
[*----------------------------------------------------------------------------*)

let map_tlrec f seznam =
  let rec aux seznam acc =
    match seznam with
    | [] -> reverse acc
    | x :: xs -> aux xs (f x :: acc)
  in
  aux seznam []

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


let mapi f l =
  let rec mapi_aux i l = match l with
    | [] -> []
    | x::xs -> (f i x) :: mapi_aux (i+1) xs
in
mapi_aux 0 l

(*je repno rekurzuvna*)

let mapi f seznam =
  let rec aux i seznam =
    match seznam with
    | [] -> []
    | x :: xs -> f i x :: aux (i+1) xs
  in
  aux 0 seznam


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

let rec zip l1 l2 = 1

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

let rec unzip seznam = 1
  (*match seznam with
  | [] -> ([],[])
  | (x, y) :: xs -> *)

let primer_unzip_1 = unzip [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_1 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)


(*let rec unzip seznam =
  match seznam with
  | [] -> ([], [])
  | (x, y) :: xs ->
      let (prvi, drugi) = unzip xs in
      (x :: prvi, y :: drugi)*)

(*----------------------------------------------------------------------------*
 Funkcija `unzip_tlrec` je repno rekurzivna različica funkcije `unzip`.
[*----------------------------------------------------------------------------*)

let unzip_tlrec _ = ()

let primer_unzip_2 = unzip_tlrec [(0,"a"); (1,"b"); (2,"c")]
(* val primer_unzip_2 : int list * string list = ([0; 1; 2], ["a"; "b"; "c"]) *)

(*let unzip_tlrec seznam =
  let rec aux seznam acc1 acc2 =
    match seznam with
    | [] -> (reverse acc1, reverse acc2)
    | (x, y) :: xs ->
        aux xs (x :: acc1) (y :: acc2)
  in
  aux seznam [] []*)

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

let rec loop _ _ _ = ()

let primer_loop = loop (fun x -> x < 10) ((+) 4) 4
(* val primer_loop : int = 12 *)

(*let rec loop condition f x =
  if condition x then loop condition f (f x)
  else x*)

(*----------------------------------------------------------------------------*
 ## Funkcija `fold_left`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `fold_left_no_acc f list` naj sprejme seznam `x0; x1; ...; xn` in
 funkcijo dveh argumentov `f` in vrne vrednost izračuna `f(... (f (f x0 x1) x2)
 ... xn)`. V primeru seznama z manj kot dvema elementoma naj vrne napako.
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc _ _ = ()

let primer_fold_left_no_acc =
  fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"]
(* val primer_fold_left_no_acc : string = "FICUS" *)

(*let rec fold_left_no_acc f seznam =
  match seznam with
  | x :: y :: xs ->
      let z = f x y in
      let rec aux acc seznam =
        match seznam with
        | [] -> acc
        | x :: xs -> aux (f acc x) xs
      in
      aux z xs
  | _ -> failwith "Seznam mora imeti vsaj dva elementa"*)

(*----------------------------------------------------------------------------*
 ## Funkcija `apply_sequence`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `apply_sequence f x n` naj vrne seznam zaporednih uporab funkcije `f`
 na vrednosti `x` do vključno `n`-te uporabe, torej `[x; f x; f (f x); ...; fⁿ
 x]`. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let apply_sequence _ _ _ = ()

let primer_apply_sequence_1 = apply_sequence (fun x -> x * x) 2 5
(* val primer_apply_sequence_1 : int list = [2; 4; 16; 256; 65536; 4294967296] *)

let primer_apply_sequence_2 = apply_sequence (fun x -> x * x) 2 (-5)
(* val primer_apply_sequence_2 : int list = [] *)

(*let apply_sequence f x n =
  let rec aux vrednost i acc =
    if i > n then reverse acc
    else aux (f vrednost) (i + 1) (vrednost :: acc)
  in
  if n < 0 then []
  else aux x 0 []*)

(*----------------------------------------------------------------------------*
 ## Funkcija `filter`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `filter f list` vrne seznam elementov `list`, pri katerih funkcija `f`
  vrne vrednost `true`.
  Pri tem ne smete uporabiti vgrajene funkcije `List.filter`.
[*----------------------------------------------------------------------------*)

let rec filter _ _ = ()

let primer_filter = filter ((<)3) [0; 1; 2; 3; 4; 5]
(* val primer_filter : int list = [4; 5] *)

(*let rec filter f seznam =
  match seznam with
  | [] -> []
  | x :: xs ->
      if f x then x :: filter f xs
      else filter f xs*)

(*----------------------------------------------------------------------------*
 ## Funkcija `exists`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `exists` sprejme seznam in funkcijo, ter vrne vrednost `true` čim
  obstaja element seznama, za katerega funkcija vrne `true` in `false` sicer.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let rec exists _ _ = ()

let primer_exists_1 = exists ((<) 3) [0; 1; 2; 3; 4; 5]
(* val primer_exists_1 : bool = true *)

let primer_exists_2 = exists ((<) 8) [0; 1; 2; 3; 4; 5]
(* val primer_exists_2 : bool = false *)

(*let rec exists f seznam =
  match seznam with
  | [] -> false
  | x :: xs ->
      if f x then true
      else exists f xs*)

(*----------------------------------------------------------------------------*
 ## Funkcija `first`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `first f default list` vrne prvi element seznama, za katerega
  funkcija `f` vrne `true`. Če takšnega elementa ni, vrne `default`.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let rec first _ _ _ = ()

let primer_first_1 = first ((<) 3) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_1 : int = 5 *)

let primer_first_2 = first ((<) 8) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_2 : int = 0 *)

(*let rec first f default seznam =
  match seznam with
  | [] -> default
  | x :: xs ->
      if f x then x
      else first f default xs*)
      
(*let range n =
  let rec aux i acc =
    if i > n then reverse acc
    else aux (i + 1) (i :: acc)
  in
  if n < 0 then []
  else aux 0 []*) 
  

(*let rec zip xs ys =
  match xs, ys with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys
  | _ -> failwith "Seznama nista enake dolzine"*)