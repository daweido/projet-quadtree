open Str
(*Regroupe list 3 par 3*)
exception Arbre_vide;;

type 'a quad = Nul | Noeud of 'a * 'a quad * 'a quad * 'a quad * 'a quad;;

let creerArbre r fgg fgd fdg fdd = Noeud(r,fgg,fgd,fdg,fdd);;

let separe l = List.map (fun x -> Str.split (Str.regexp " ") x) l;;

(* essayer en terminal*)

let sti l = List.map (fun x ->  try int_of_string x with Failure "int_of_string" -> print_string x; 0) l;;

let stis l = List.map (fun x ->  int_of_string x) l;;

let rec man l = match l with
	 [] -> []
	|a::r -> a@(man r);;

let rec ajuste l = match l with
	 [] -> []
	|x::y::z::r -> (x,y,z)::(ajuste r)
	|_-> failwith "Error";;


let rec clean l = match l with
	 [] -> []
	|x::r when x = "" -> clean r
	|x::r when String.get x 0 = '#' -> clean r
	|x::r when x = " " -> clean r
	|x::r -> x::clean r;;




(*Programme Principal*)


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


let lec fic = let l = (man(separe((clean(read_file fic))))) in
	match l with
	a::b::c::d::r -> (a,b,c,d, ajuste(sti r))
	|_-> failwith "Error";;



let arbre = creerArbre 5 (creerArbre 2 Nul Nul Nul Nul) (creerArbre 3 Nul Nul Nul Nul) (creerArbre 4 Nul Nul Nul Nul) (creerArbre 6 Nul Nul Nul Nul);;

let view a =
	let rec repStr s n =
		if n = 0 then ""
		else s^(repStr s (n-1)) in
	let rec view_ a n = match a with
		Nul -> ""
		|Noeud (v,fgg,fgd,fdg,fdd) -> (view_ fgg (n+1))^(view_ fgd (n+1))^(repStr "   " n)^ (string_of_int v)^ "\n" ^ (view_ fdg (n+1))^(view_ fdd (n+1)) in
	print_string (view_ a 0);;

(*let grandArbre a =
	let noeud a n = match a with
	[] -> []
	|x::r when n = 0 -> Noeud (_,x,grandArbre r 1,Nul,Nul)
	|x::r when n = 1 -> Noeud (_,_,x,grandArbre r 2,Nul)
	|x::r when n = 2 -> Noeud (_,_,_,x,grandArbre r 3)
	|x::r when n = 3 -> Noeud ((a+b+c+x)/4,a,b,c,x)
	in
	let branche l = match l with
		[]-> []
		|x::y::z::h::r -> noeud x::y::z::h::l 0
	in
	creerArbre (branche a 0);;   *)

let rec nbre a = match a with
	[] -> 0
	|a::b -> 1 + nbre b;;

let get5et2 (e1,e2,e3,e4,e5) = ((int_of_string e2), e5);;

let carre (a,b) = if (a mod 4 = 0) || (b mod 4 = 0) then true else false;;

let decoupeIMgg (e2,a) =
	let rec divise2 a c d=  match a with
		[] -> []
		|(x,y,z)::b when c < (d/2) -> (x,y,z):: divise2 b (c+1) d
		|_ -> [] in
	let rec filsgaucheg a c=  match a with
		[]-> []
		|(x,y,z)::b when c mod e2 = 0 -> filsgaucheg b (c+1)
		|(x,y,z)::b when (c mod e2 <= (e2 /2))-> (x,y,z)::filsgaucheg b (c+1)
		|(x,y,z)::b when c mod e2 >= (e2 /2) -> filsgaucheg b (c+1) in
	filsgaucheg (divise2 a 0 (nbre a)) 1;;

let decoupeIMgd (e2,a) =
	let rec divise2 a c d=  match a with
		[] -> []
		|(x,y,z)::b when c < (d/2) -> (x,y,z):: divise2 b (c+1) d
		|_ -> [] in
	let rec filsgauched a c = match a with
		[]-> []
		|(x,y,z)::b when c mod e2 = 0 -> (x,y,z)::filsgauched b (c+1)
		|(x,y,z)::b when c mod (e2/2) = 0 -> filsgauched b (c+1)
		|(x,y,z)::b when c mod e2 >= (e2 /2) -> (x,y,z)::filsgauched b (c+1)
		|(x,y,z)::b when c mod e2 <= (e2 /2) -> filsgauched b (c+1) in
	filsgauched (divise2 a 0 (nbre a)) 1;;

let decoupeIMdg (e2,a)=
	let rec divise2 a c d=  match a with
		[] -> []
		|(x,y,z)::b when c >= (d/2) -> (x,y,z):: divise2 b (c+1) d
		|(x,y,z)::b when c < (d/2) -> divise2 b (c+1) d
		|_ -> [] in
	let rec filsgaucheg a c=  match a with
		[]-> []
		|(x,y,z)::b when c mod e2 = 0 -> filsgaucheg b (c+1)
		|(x,y,z)::b when (c mod e2 <= (e2 /2))-> (x,y,z)::filsgaucheg b (c+1)
		|(x,y,z)::b when c mod e2 >= (e2 /2) -> filsgaucheg b (c+1) in
	filsgaucheg (divise2 a 0 (nbre a)) 1;;

let decoupeIMdg (e2,a)=
	let rec divise2 a c d=  match a with
		[] -> []
		|(x,y,z)::b when c >= (d/2) -> (x,y,z):: divise2 b (c+1) d
		|(x,y,z)::b when c < (d/2) -> divise2 b (c+1) d
		|_ -> [] in
	let rec filsgauched a c = match a with
		[]-> []
		|(x,y,z)::b when c mod e2 = 0 -> (x,y,z)::filsgauched b (c+1)
		|(x,y,z)::b when c mod (e2/2) = 0 -> filsgauched b (c+1)
		|(x,y,z)::b when c mod e2 >= (e2 /2) -> (x,y,z)::filsgauched b (c+1)
		|(x,y,z)::b when c mod e2 <= (e2 /2) -> filsgauched b (c+1) in
	filsgauched (divise2 a 0 (nbre a)) 1;;

(*let grand_Arbre a = match a with
	a when (list.length a) = 0 -> Nul
	|a when (List.length a) =1 -> Noeud(a,Nul,Nul,Nul,Nul)
	|*)

let test = [(1,1,1);(2,2,2);(3,3,3);(4,4,4);(5,5,5);(6,6,6);(7,7,7);(8,8,8);(9,9,9);(10,10,10);(11,11,11);(12,12,12);(13,13,13);(14,14,14);(15,15,15);(16,16,16);(17,17,17);(18,18,18);(19,19,19);(20,20,20);(21,21,21);(22,22,22);(23,23,23);(24,24,8);(25,9,9);(26,10,10);(27,11,11);(28,12,12);(29,13,13);(30,14,14);(31,15,15);(32,16,16);(33,1,1);(2,2,2);(3,3,3);(4,4,4);(5,5,5);(6,6,6);(7,7,7);(8,8,8);(9,9,9);(10,10,10);(11,11,11);(12,12,12);(13,13,13);(14,14,14);(15,15,15);(16,16,16);(1,1,1);(2,2,2);(3,3,3);(34,4,4);(35,5,5);(36,6,6);(37,7,7);(38,8,8);(39,9,9);(40,10,10);(41,11,11);(42,12,12);(43,13,13);(44,14,14);(45,15,15);(46,16,16);(47,1,1);(48,2,2);(49,3,3);(50,4,4);(51,5,5);(52,6,6);(7,7,7);(8,8,8);(9,9,9);(10,10,10);(11,11,11);(12,12,12);(13,13,13);(14,14,14);(15,15,15);(16,16,16);(17,17,17);(18,18,18);(19,19,19);(20,20,20);(21,21,21);(22,22,22);(23,23,23);(24,24,8);(25,9,9);(26,10,10);(27,11,11);(28,12,12);(29,13,13);(30,14,14);(31,15,15);(32,16,16);(33,1,1);(2,2,2);(3,3,3);(4,4,4);(5,5,5);(6,6,6);(7,7,7);(8,8,8);(9,9,9);(10,10,10);(11,11,11);(12,12,12);(13,13,13);(14,14,14);(15,15,15);(16,16,16);(1,1,1);(2,2,2);(3,3,3);(34,4,4);(35,5,5);(36,6,6);(37,7,7);(38,8,8);(39,9,9);(40,10,10);(41,11,11);(42,12,12);(43,13,13);(44,14,14);(45,15,15);(46,16,16)];;

let get (e1,e2,e3,e4,e5) = e5;;
