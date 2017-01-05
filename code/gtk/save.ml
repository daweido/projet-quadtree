#load "str.cma";;

type 'a quad = Nul | Noeud of 'a * 'a quad * 'a quad * 'a quad * 'a quad;;

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

(* essayer en terminal*)

let get5 (e1,e2,e3,e4,e5) = e5;;

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

let lec fic = 
	let a fic = (man(separe(clean(read_file fic)))) in
	let b l = match l with
		a::b::c::d::r -> (a,b,c,d,List.rev (ajuste (stis r)))
		|_ -> failwith "Error"in
	b (a fic);; 

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

let test = [(1,1,1);(2,2,2);(6,6,6);(5,5,5);(3,3,3);(4,4,4);(8,8,8);(7,7,7);(9,9,9);(10,10,10);(14,14,14);(13,13,13);(11,11,11);(12,12,12);(16,16,16);(15,15,15)];;

let rec arbre_to_list a = match a with
	Nul -> []
	|Noeud (a,Nul,Nul,Nul,Nul) -> a
	|Noeud (x,y,z,b,c) -> (arbre_to_list y)@(arbre_to_list z)@(arbre_to_list b)@(arbre_to_list c);;

let rec divise2 a c d res=  match a with
	[] -> res
	|(x,y,z)::b when c > (d/2) -> divise2 b (c+1) d ((x,y,z):: res)
	|(x,y,z)::b when c <= (d/2) -> divise2 b (c+1) d res
	|_ -> [];;

let rec divise2b a c d res = match a with
	[] -> res
	|(x,y,z)::r when c <= (d/2) -> divise2b r (c+1) d ((x,y,z)::res)
	|(x,y,z)::r when c > (d/2)-> res;;

let rec reorga1 a b c res = match a with
	[] -> res
	|(x,y,z)::r when c mod b = 0 -> reorga1 r b (c+1) res 
	|(x,y,z)::r when c mod b <= (b/2) -> reorga1 r b (c+1) ((x,y,z)::res)
	|(x,y,z)::r when c mod b > (b/2) -> reorga1 r b (c+1) res;;

let rec reorga2 a b c res = match a with
	[] -> res
	|(x,y,z)::r when c mod b = 0 -> reorga2 r b (c+1) ((x,y,z)::res)
	|(x,y,z)::r when c mod b > (b/2) -> reorga2 r b (c+1) ((x,y,z)::res)
	|(x,y,z)::r when c mod b <= (b/2) -> reorga2 r b (c+1) res;;

let reorga1fin a b c res = (divise2b (reorga1 a b c res) c b [])@(divise2 (reorga1 a b c res) c b []);;

let reorga2fin a b c res = (List.rev (divise2 (reorga2 a b c res) c b []))@ (List.rev (divise2b (reorga2 a b c res) c b []));;


let test2 = [(1,1,1);(2,2,2);(6,6,6);(5,5,5);(3,3,3);(4,4,4);(8,8,8);(7,7,7)];;

let test3 = [(1,1,1);(2,2,2);(3,3,3);(4,4,4);(5,5,5);(6,6,6);(7,7,7);(8,8,8);(9,9,9);(10,10,10);(11,11,11);(12,12,12);(13,13,13);(14,14,14);(15,15,15);(16,16,16)];;

let ll a = int_of_float(sqrt (float_of_int (List.length a))) ;;

let reorgafin a = (reorga2 (divise2b (arbre_to_list a) 1 (List.length (arbre_to_list a)) []) (ll (arbre_to_list a)) 1 []) @ (reorga1fin (divise2b (arbre_to_list a) 1 (List.length (arbre_to_list a)) []) (ll (arbre_to_list a)) 1 []) @ (reorga2fin (divise2 (arbre_to_list a) 1 (List.length (arbre_to_list a)) []) (ll (arbre_to_list a)) 1 []) @ (List.rev(reorga1 (divise2 (arbre_to_list a) 1 (List.length (arbre_to_list a)) []) (ll (arbre_to_list a)) 1 []));;

let write_file filename l =
    let fecr = open_out filename in
    let rec write_line l = match l with
        [] -> close_out fecr
        | x::r -> begin output_string fecr (x^"\n"); write_line r end
    in write_line l;;

let test4 = (1,1,1);;

let convert (a,b,c) = (string_of_int a)^" "^(string_of_int b)^" "^(string_of_int c);;

let rec change a res = match a with
	[] -> res
	|(x,y,z)::r -> change r (convert(x,y,z)::res);;

let listfin a = List.rev (change (reorgafin a) []);;
	
