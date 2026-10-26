(*----------------------------------------------------------------------------*
 # Uvod v OCaml
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Vektorji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razteg : float -> float list -> float list`, ki vektor,
 predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

(*List.map f [a; b; c] ... vrne [f a; f b; f c]*)
let razteg skalar seznam = List.map (fun x -> skalar *. x) seznam
let razteg1 skalar seznam = List.map ( ( *. ) skalar) seznam

let primer_vektorji_1 = razteg 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
[*----------------------------------------------------------------------------*)

(*List.map2 f [x1; x2; x3] [y1; y2; y3] ... vrne [f x1 y1; f x2 y2; f x3 y3]*)
let sestej u v = List.map2 (fun x y -> x +. y) u v

let sestej2 u v = List.map2 ( +. ) u v
let primer_vektorji_2 = sestej [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
[*----------------------------------------------------------------------------*)

(*List.fold_left f init list
Začne z začetno vrednostjo in za vsak element izračuna f akumulator element.
Seznam obdela od leve proti desni.
Njen tip je: ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
('acc -> 'a -> 'acc): 1. argument: funkcija f
-> 'acc: 2. argument: začetna vrednost akumulatorja, ima isti tip kot akumulator
-> 'a list: 3. argument: seznam
-> 'acc: rezultat, končni akumulator, ko so obdelani vsi elementi
*)

(*Naslednja zapisa sta enakovredna:
let vsota_seznama = List.fold_left (+.) 0.
let vsota_seznama sez = List.fold_left (+.) 0. sez*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt u v = vsota_seznama (
  List.map2 (fun x y -> x *. y) u v
)

let skalarni_produkt2 u v =
  List.map2 (fun x y -> x *. y) u v 
  |> vsota_seznama

let primer_vektorji_3 = skalarni_produkt [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
[*----------------------------------------------------------------------------*)

let norma u = 
  List.map (fun x -> x *. x) u
  |> vsota_seznama
  |> sqrt

let norma2 u = 
  sqrt (vsota_seznama (List.map (fun x -> x *. x) u))

let norma3 u = Float.sqrt (skalarni_produkt u u)

let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
[*----------------------------------------------------------------------------*)

let vmesni_kot u v = acos ((skalarni_produkt u v) /. ((norma u) *. (norma v)))

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
[*----------------------------------------------------------------------------*)

let normirani u = 
  let n = norma u in 
  (*normo shranimo v novo spremenljivko zato, da se ne izračuna vsakič znova, ampak samo enkrat*)
  List.map (fun x -> x /. n) u

let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projekcija u v = razteg (
  ((skalarni_produkt u v ) /. (norma v))
) v

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
[*----------------------------------------------------------------------------*)
(*^ je stik dveh nizov*)
let ovij oznaka vsebina = 
  "<" ^ oznaka ^ ">" ^ vsebina ^ "</" ^ oznaka ^ ">"

let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
[*----------------------------------------------------------------------------*)
(* 
- najprej razdelimo niz glede na presledke: String.split_on_char
- dodamo presledke: List.map
- združimo: String.concat
*)
let zamakni k niz = 
  String.split_on_char '\n' niz
  |> List.map (fun vrstica -> (String.make k ' ') ^ vrstica)
  |> String.concat "\n"

  let zamakni2 k niz = 
  let presledki = String.make k ' ' in
  let vrstice = String.split_on_char '\n' niz in
  let zamaknjene_vrstice = List.map ((^) presledki) vrstice in
  String.concat "\n" zamaknjene_vrstice

let primer_html_2 = zamakni 4 "Hello,\nworld!"
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
[*----------------------------------------------------------------------------*)

let ul seznam = 
  let vsebina =
    List.map (ovij "li") seznam
    |> String.concat "\n"
    |> zamakni 2
  in
  "<ul>" ^ vsebina ^ "</ul>"

let ul2 s =
  let oviti_elementi = List.map (ovij "li") s in
  let vrstice = String.concat "\n" oviti_elementi in
  let zamaknjeno = zamakni 2 vrstice in
  let notranjost = "\n" ^ zamaknjeno ^ "\n" in
  ovij
    "ul"
    notranjost

let primer_html_3 = ul ["ananas"; "banana"; "čokolada"]
(* val primer_html_3 : string =
  "<ul>\n  <li>ananas</li>\n  <li>banana</li>\n  <li>čokolada</li>\n</ul>" *)

(*----------------------------------------------------------------------------*
 ## Nakupovalni seznam
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme
 niz, ki vsebuje vejico, loči na del pred in del za njo.
[*----------------------------------------------------------------------------*)

let razdeli_vrstico niz = 
  let vejica = String.index niz ',' in
  let levi = String.sub niz 0 vejica in
  let desni = String.sub niz (vejica + 2) ((String.length niz) - vejica - 2) in
  (* vejica + 2 zato, ker vejica je vejica in naslednji znak je presledek 
  število znakov od začetka desnega dela do konca niza je String.length niz - (vejica + 2)*)
  levi, desni

let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz = 
  String.split_on_char '\n' niz
  |> List.map razdeli_vrstico

let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente f sez = 
  List.map (fun (a,b) -> a, f b) sez

let primer_seznam_3 =
  let seznam = [("ata", "mama"); ("teta", "stric")] in
  pretvori_druge_komponente String.length seznam
(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek nakup cenik = 
  let urejen_seznam =
    pretvori_v_seznam_parov nakup
    |> pretvori_druge_komponente Float.of_string 
  in
  let urejen_cenik =
    pretvori_v_seznam_parov cenik
    |> pretvori_druge_komponente Float.of_string
  in
  let cena_izdelka izdelek =
    List.map ((fun (i1, p1) (i2, p2) -> if i1 = i2 then p1 *. p2 else 0.) (fst izdelek, snd izdelek)) urejen_cenik
    |> vsota_seznama
  in
  urejen_seznam
  |> List.map cena_izdelka
  |> vsota_seznama


(* Resimo lahko tudi z uporabo delnih vrednosti (option type) *)
let izracunaj_skupni_znesek_opt cenik seznam =
  let urejen_cenik = 
    pretvori_v_seznam_parov cenik 
    |> pretvori_druge_komponente float_of_string
  in
  let urejen_seznam = 
    pretvori_v_seznam_parov seznam
    |> pretvori_druge_komponente float_of_string
  in 
  List.filter_map
    (fun (izd, kol) -> 
      List.find_map 
        (fun (izd_c, cena) -> if izd = izd_c then Some (cena *. kol) else None)
        urejen_cenik
    )
    urejen_seznam
  |> vsota_seznama

let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)
