type 'a quad = Nul | Noeud of 'a * 'a quad * 'a quad * 'a quad * 'a quad;;

let creerArbre r fgg fgd fdg fdd = Noeud(r,fgg,fgd,fdg,fdd)

let rec sommex l = match l with
	[] -> 0
	|(x,y,z)::r -> x + sommex r ;;
let rec sommey l = match l with
	[] -> 0
	|(x,y,z)::r -> y + sommey r ;;
let rec sommez l = match l with
	[] -> 0
	|(x,y,z)::r -> z + sommez r ;;

let moy l = [sommex l / (List.length l), sommey l / (List.length l) , sommez l /(List.length l )];;

let separe l = List.map (fun x -> Str.split (Str.regexp " ") x) l;;

(* essayer en terminal*)

let sti l = List.map (fun x ->  try int_of_string x with Failure "int_of_string" -> print_string x; 0) l;;

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


let lec fic = let l = (man(separe(clean(read_file fic)))) in
	match l with
	a::b::c::d::r -> (a,b,c,d, ajuste(stis r))
	|_-> failwith "Error";;

(*------------------------------decoupe l'image en 4 -------------------------------------------*)
let decoupeIMdd a longueur =
	let rec divise2 a c d=  match a with
		[] -> []
		|(x,y,z)::b when c > (d/2) -> (x,y,z):: divise2 b (c+1) d
		|(x,y,z)::b when c <= (d/2) -> divise2 b (c+1) d
		|_ -> [] in
	let rec decoupeHoD a c longueur = match a with
		[] -> []
		|(x,y,z)::r when c mod longueur = 0 -> decoupeHoD r (c+1) longueur
		|(x,y,z)::r when c mod longueur <= (longueur/2)  -> (x,y,z)::decoupeHoD r (c+1) longueur
		|(x,y,z)::r when c mod longueur > (longueur/2) -> decoupeHoD r (c+1) longueur in
	decoupeHoD ( divise2 a 1 (List.length a)) 1 ((List.length a)/4);;

let decoupeIMdg a longueur =
	let rec divise2 a c d=  match a with
		[] -> []
		|(x,y,z)::b when c > (d/2) -> (x,y,z):: divise2 b (c+1) d
		|(x,y,z)::b when c <= (d/2) -> divise2 b (c+1) d
		|_ -> [] in
	let rec decoupeHOG a c longueur = match a with
		[] -> []
		|(x,y,z)::r when c mod longueur = 0 -> (x,y,z)::decoupeHOG r (c+1) longueur
		|(x,y,z)::r when c mod longueur <= (longueur/2) -> decoupeHOG r (c+1) longueur
		|(x,y,z)::r when c mod longueur > (longueur/2) -> (x,y,z)::decoupeHOG r (c+1) longueur in
	decoupeHOG ( divise2 a 1 (List.length a)) 1 ((List.length a)/4);;

let decoupeIMgd a longueur =
	let rec divise2b a c d = match a with
		[] -> []
		|(x,y,z)::r when c <= (d/2) -> (x,y,z)::divise2b r (c+1) d
		|(x,y,z)::r when c > (d/2)-> [] in
	let rec decoupeHOG a c longueur = match a with
		[] -> []
		|(x,y,z)::r when c mod longueur = 0 -> (x,y,z)::decoupeHOG r (c+1) longueur
		|(x,y,z)::r when c mod longueur <= (longueur/2) -> decoupeHOG r (c+1) longueur
		|(x,y,z)::r when c mod longueur > (longueur/2) -> (x,y,z)::decoupeHOG r (c+1) longueur in
	decoupeHOG ( divise2b a 1 (List.length a)) 1 ((List.length a)/4);;

let decoupeIMgg a longueur =
	let rec divise2b a c d = match a with
		[] -> []
		|(x,y,z)::r when c <= (d/2) -> (x,y,z)::divise2b r (c+1) d
		|(x,y,z)::r when c > (d/2)-> [] in
	let rec decoupeHoD a c longueur = match a with
		[] -> []
		|(x,y,z)::r when c mod longueur = 0 -> decoupeHoD r (c+1) longueur
		|(x,y,z)::r when c mod longueur <= (longueur/2)  -> (x,y,z)::decoupeHoD r (c+1) longueur
		|(x,y,z)::r when c mod longueur > (longueur/2) -> decoupeHoD r (c+1) longueur in
	decoupeHoD ( divise2b a 1 (List.length a)) 1 ((List.length a)/4);;

(*------------------------------------------cree l'arbre------------------*)

let arbregg a = decoupeIMgg a (List.length a);;
let arbregd a = decoupeIMgd a (List.length a);;
let arbredg a = decoupeIMdg a (List.length a);;
let arbredd a = decoupeIMdd a (List.length a);;

let rec arbre a  = match a with
	|a when (List.length a) = 0 -> Nul
	|a when (List.length a) = 1 -> Noeud(a,Nul,Nul,Nul,Nul)
	|x::y::z::r when (List.length a) = 4 -> Noeud(moy a, arbre [x], arbre [y], arbre [z], arbre r)
	|_ -> Noeud (moy a, arbre (arbregg a), arbre (arbregd a), arbre (arbredg a), arbre (arbredd a));;

let test = [(1,1,1);(2,2,2);(3,3,3);(4,4,4);(5,5,5);(6,6,6);(7,7,7);(8,8,8);(9,9,9);(10,10,10);(11,11,11);(12,12,12);(13,13,13);(14,14,14);(15,15,15);(16,16,16)];;

(*let conv_taux x = *)

let rec compression a t = match a with		(* t = taux de compression *)
	 Nul-> Nul
	|Noeud(x,y,z,c,h) when t <= 1 -> Noeud (x, compression y (t+1), compression z (t+1), compression c (t+1), compression h (t+1))
	|Noeud (a,b,c,d,e) -> Noeud(a,Nul,Nul,Nul,Nul) ;;

let get5 (e1,e2,e3,e4,e5) = e5;;

let rec arbre_to_list a = match a with
	Nul -> []
	|Noeud (a,Nul,Nul,Nul,Nul) -> [a]
	|Noeud (x,y,z,b,c) -> (arbre_to_list y)@(arbre_to_list z)@(arbre_to_list b)@(arbre_to_list c);;

let execu fic = arbre (get5 (lec fic));;
