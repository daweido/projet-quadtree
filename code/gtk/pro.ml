
(**----------------------Fonctions fichiers-----------------------------------**)

(*--------------------Lecture d'un fichier-------------------*)

let split=Str.split(Str.regexp_string" ");;

let readfile filename=
	let rec readline f = try
		let l = input_line f in l::readline f
		with
		    End_of_file->[]
		in
	readline(open_in filename);;



(*-----------Enlever les commentaires et les caractères spéciaux--------------*)

let rec filtre l =match l with
	[]->[]
	|x::r when x=""->filtre r
	|x::r when String.get x 0 = '#'-> filtre r
	|x::r when x<>"" && x<>"#"->x::filtre r
	|_->failwith"error";;

(*-----------------Regrouper en triplets------------------------*)

let rec regroupe l=match l with
	[]->[]
	|x::y::z::r->(int_of_string x,int_of_string y,int_of_string z)::regroupe r
	|_->failwith "pas de triplets";;

(*-----------------------Lecture finale-------------------------*)
let flat l=List.flatten l;;

let superlecture f=filtre(flat(List.map split (filtre (readfile f))));;


let rec sommel l =let plusTriplet (x1,x2,x3) (y1,y2,y3) = (x1+y1,x2+y2,x3+y3) in match l with
                        []->(0,0,0)
                        |x::r -> plusTriplet x (sommel r);;



let clean l=match l with
	[]->[]
	|a::b::c::d::r->regroupe r;;

let moyennel (x,y,z) l =
	let ll=clean l in
		(int_of_float(float x/. float (List.length (ll))),int_of_float(float y/. float (List.length (ll))),int_of_float(float z/. float (List.length (ll))));;

let moyenne f= moyennel (sommel (clean (superlecture f))) (superlecture f);;

(**-------------------------------Arbre----------------------------------------**)

(*------------------------Creer l'arbre------------------------*)

type 'a quad = Nul | Noeud of 'a * 'a quad * 'a quad * 'a quad * 'a quad;;
exception Arbre_vide;;
let creerArbre r fgg fgd fdg fdd = Noeud(r,fgg,fgd,fdg,fdd);;

(*-----------------------Vue Arbre----------------------------*)

let view a =
	let rec repStr s n =
		if n = 0 then ""
		else s^(repStr s (n-1)) in
	let rec view_ a n = match a with
		Nul -> ""
		|Noeud (v,fgg,fgd,fdg,fdd) -> (view_ fgg (n+1))^(view_ fgd (n+1))^(repStr "   " n)^ (string_of_int v)^ "\n" ^ (view_ fdg (n+1))^(view_ fdd (n+1)) in
	print_string (view_ a 0);;

(*-----------------Exemple d'un arbre quadtree-----------------*)

let arbre = creerArbre 10 (creerArbre 3 Nul Nul Nul Nul) (creerArbre 9 Nul Nul Nul Nul) (creerArbre 7 Nul Nul Nul Nul) (creerArbre 6 Nul Nul Nul Nul) ;;
let arbre1= creerArbre 10 (creerArbre 4 Nul Nul Nul Nul) (creerArbre 3 Nul Nul Nul Nul) (creerArbre 2 Nul Nul Nul Nul) (creerArbre 1 Nul Nul Nul Nul) ;;
(**----------------------Opérations simples-----------------------------------**)

(*-----------------------Rotation de 90 degrés-----------------*)

let rotated a = match a with
	 Nul -> raise Arbre_vide
	|Noeud(v,fgg,fgd,fdg,fdd) -> Noeud(v,fgd,fdg,fdd,fgg)
	|_ -> failwith "Error";;

let rotateg a = match a with
	 Nul -> raise Arbre_vide
	|Noeud(v,fgg,fgd,fdg,fdd) -> Noeud(v,fdd,fgg,fgd,fdg)
	|_ -> failwith "Error";;

(*-----------------------Mirroir--------------------*)

let mirroirgd a = match a with
	 Nul-> raise Arbre_vide
	|Noeud(v,fgg,fgd,fdg,fdd)->Noeud(v,fgd,fgg,fdd,fdg);;


let mirroirhb a = match a with
	 Nul-> raise Arbre_vide
	|Noeud(v,fgg,fgd,fdg,fdd)->Noeud(v,fdd,fdg,fgd,fgg)
	|_->failwith"Error";;

(*---------------Inversion des couleurs--------------*)

let rec inversion l= let triplet (x,y,z)=(255-x,255-y,255-z) in match l with
	[]->failwith"Error"
	|(x,y,z)::[]->triplet(x,y,z)::[]
	|(x,y,z)::r->triplet(x,y,z)::(inversion r);;

(**----------------------Opérations avancées-----------------------------------**)



(*--------------------Compression--------------------*)




(*--------------------Segmentation-------------------*)
