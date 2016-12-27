open Unix;;

type 'a quad = Nul | Noeud of 'a * 'a quad * 'a quad * 'a quad * 'a quad;;


let creerArbre r fgg fgd fdg fdd = Noeud(r,fgg,fgd,fdg,fdd)

let rec sommex l res = match l with
	[] -> res
	|(x,y,z)::r -> sommex r (res + x);;
let rec sommey l res = match l with
	[] -> res
	|(x,y,z)::r -> sommey r (res + y) ;;
let rec sommez l res = match l with
	[] -> res
	|(x,y,z)::r -> sommez r (res + z);;

let moy l = [sommex l 0 / (List.length l), sommey l 0 / (List.length l) , sommez l 0 /(List.length l )];;


let separe l = List.map (fun x -> Str.split (Str.regexp " ") x) l;;


let stis l = let rec stis l n =
	match l with
	a::r -> stis r (if a = "" then n else (int_of_string a)::n)
	|[] -> n
	in
List.rev (stis l []);;

let rec man l = match l with
	 [] -> []
	|a::r -> a@(man r);;


let ajuste l =
let rec pixou b res = match b with
	    [] -> res
	|x::y::z::r -> pixou r ((x,y,z) ::res) in
        pixou l [];;


let rec clean l = match l with
	[] -> []
	|x::r when x = "" -> clean r
	|x::r when String.get x 0 = '#' -> clean r
	|x::r when x = " " -> clean r
	|x::r -> x::clean r;;

let read_file filename =
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines ;;

let lec fic =
	let a fic = (man(separe(clean(read_file fic)))) in
	let b l = match l with
		a::b::c::d::r -> (a,b,c,d,List.rev (ajuste (stis r)))
		|_ -> failwith "Error"in
	b (a fic);;

(*------------------------------decoupe l'image en 4 -------------------------------------------*)
let decoupeIMdd a longueur =
	let rec divise2 a c d res=  match a with
		[] -> res
		|(x,y,z)::b when c > (d/2) -> divise2 b (c+1) d ((x,y,z):: res)
		|(x,y,z)::b when c <= (d/2) -> divise2 b (c+1) d res
		|_ -> [] in
	let rec decoupeHoD a c longueur res = match a with
		[] -> res
		|(x,y,z)::r when c mod longueur = 0 -> decoupeHoD r (c+1) longueur res
		|(x,y,z)::r when c mod longueur <= (longueur/2)  -> decoupeHoD r (c+1) longueur ((x,y,z)::res)
		|(x,y,z)::r when c mod longueur > (longueur/2) -> decoupeHoD r (c+1) longueur res in
	decoupeHoD ( divise2 a 1 (List.length a) []) 1 ((List.length a)/4) [];;

let decoupeIMdg a longueur =
	let rec divise2 a c d res=  match a with
		[] -> res
		|(x,y,z)::b when c > (d/2) -> divise2 b (c+1) d ((x,y,z):: res)
		|(x,y,z)::b when c <= (d/2) -> divise2 b (c+1) d res
		|_ -> [] in
	let rec decoupeHOG a c longueur res = match a with
		[] -> res
		|(x,y,z)::r when c mod longueur = 0 -> decoupeHOG r (c+1) longueur ((x,y,z)::res)
		|(x,y,z)::r when c mod longueur <= (longueur/2) -> decoupeHOG r (c+1) longueur res
		|(x,y,z)::r when c mod longueur > (longueur/2) -> decoupeHOG r (c+1) longueur ((x,y,z)::res)in
	decoupeHOG ( divise2 a 1 (List.length a) [] ) 1 ((List.length a)/4) [];;


let decoupeIMgd a longueur =
	let rec divise2b a c d res = match a with
		[] -> res
		|(x,y,z)::r when c <= (d/2) -> divise2b r (c+1) d ((x,y,z)::res)
		|(x,y,z)::r when c > (d/2)-> res in
	let rec decoupeHOG a c longueur res = match a with
		[] -> res
		|(x,y,z)::r when c mod longueur = 0 -> decoupeHOG r (c+1) longueur ((x,y,z)::res)
		|(x,y,z)::r when c mod longueur <= (longueur/2) -> decoupeHOG r (c+1) longueur res
		|(x,y,z)::r when c mod longueur > (longueur/2) -> decoupeHOG r (c+1) longueur ((x,y,z)::res)in
	decoupeHOG ( divise2b a 1 (List.length a) [] ) 1 ((List.length a)/4) [];;

let decoupeIMgg a longueur =
	let rec divise2b a c d res = match a with
		[] -> res
		|(x,y,z)::r when c <= (d/2) -> divise2b r (c+1) d ((x,y,z)::res)
		|(x,y,z)::r when c > (d/2)-> res in
	let rec decoupeHoD a c longueur res = match a with
		[] -> res
		|(x,y,z)::r when c mod longueur = 0 -> decoupeHoD r (c+1) longueur res
		|(x,y,z)::r when (c mod longueur <= (longueur/2))  -> decoupeHoD r (c+1) longueur ((x,y,z)::res)
		|(x,y,z)::r when c mod longueur > (longueur/2) -> decoupeHoD r (c+1) longueur res
		|_-> decoupeHoD a (c+1) longueur res  in
	(decoupeHoD ( divise2b a 1 (List.length a) []) 1 ((List.length a)/4) []);;

(*------------------------------------------cree l'arbre------------------*)

let arbregg a = decoupeIMgg a (List.length a);;
let arbregd a = decoupeIMgd a (List.length a);;
let arbredg a = decoupeIMdg a (List.length a);;
let arbredd a = decoupeIMdd a (List.length a);;

let rec arbre a  = match a with
	|a when (List.length a) = 0 -> Nul
	|a when (List.length a) = 1 -> Noeud(a,Nul,Nul,Nul,Nul)
	|x::y::z::r when (List.length a) = 4 -> Noeud(moy a, arbre [x], arbre[y], arbre r, arbre [z])
	|_ -> Noeud (moy a, arbre (arbregd a), arbre (arbregg a), arbre (arbredd a), arbre (arbredg a));;

let rec arbre_to_list a = match a with
	Nul -> []
	|Noeud (a,Nul,Nul,Nul,Nul) -> a
	|Noeud (x,y,z,b,c) -> (arbre_to_list y)@(arbre_to_list z)@(arbre_to_list b)@(arbre_to_list c);;
