(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg quad.ml -o quadtree*)
open GMain
open GdkKeysyms

let _ = GMain.init ()

(*Definition de la fenetre*)
let window = GWindow.window
	~width:900
	~height:700
	~position:`CENTER
	~resizable:false
	~title:"PPMShop" ()

(* GtkMessageDialog - Court message à l'attention de l'utilisateur. *)
let confirm _ =
  let dlg = GWindow.message_dialog
		~message:"<b><big>Voulez-vous vraiment quitter ?</big></b>\n\n"
		~parent:window
		~destroy_with_parent:true
		~use_markup:true
		~message_type:`QUESTION
		~position:`CENTER_ON_PARENT
		~buttons:GWindow.Buttons.yes_no () in
	let res = dlg#run () = `NO in
	dlg#destroy ();
	res

(* GtkAboutDialog - Boîte de dialogue "À propos..." *)
let about_button = GWindow.about_dialog
	~comments: "PPMShop est une application permettant de manipuler des images en formats .ppm\n
Ce programme a été réalisé dans le cadre d'un projet informatique en deuxième année
à l'EISTI"
	~name: "PPMShop"
	~resizable: false
	~authors:["RIGAUX"]
	~copyright:"Copyright © 2016-2017 EISTI"
	~license:"This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version. \n

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.\n

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>."
	~version:"1.0"
	~website:"https://eisti.fr/fr"
	~website_label:"EISTI"
	~position:`CENTER_ON_PARENT
	~parent:window
	~destroy_with_parent:true ()

(*Definition de conteneur de window*)
let vbox = GPack.vbox
	~spacing:10
	~packing:window#add ()

(*TOOLBAR*)
let toolbar = GButton.toolbar
	~orientation: `HORIZONTAL
	~style:`BOTH
	~packing:(vbox#pack ~expand:false) ()

let _ =
	let packing = toolbar#insert in
	let abo = GButton.tool_button ~label: "À Propos" ~stock: `ABOUT ~packing () in
	ignore (GButton.separator_tool_item ~packing ());
	let qui = GButton.tool_button ~label: "Quitter" ~stock: `QUIT ~packing () in
	ignore (abo#connect#clicked (fun () -> ignore (about_button#run ()); ignore (about_button#misc#hide ())));
	qui#connect#clicked Main.quit;;

let quit =
	let button = GButton.button
				~stock:`QUIT () in
	ignore (button#connect#clicked ~callback:GMain.quit);
	button;;

(*DEBUT DE DEUXIEME INTERFACE*)

(*Définition des conteneurs*)
(*+ Grands conteneur*)
let hboxtwo = GPack.hbox
	~spacing:10
	~packing: vbox#add ()

let bboxtwo = GPack.button_box `HORIZONTAL
	~spacing:150
	~layout:`EDGE
	~border_width:5
	~child_width: 250
	~child_height: 50
	~packing:(vbox#pack ~expand:false) ()

(*Sous conteneurs*)
(*let notebook = GPack.notebook ~packing:hboxtwo#add ()

let vboxtwo = GPack.vbox
	~spacing:10
	~packing:hboxtwo#add ()*)

(*bouton retour*)
let returnB = GButton.button
	~label: "Retour" ()

(*Tableau avec informations sur image*)
let tableInfo = GPack.table
	~rows:3 ~columns:2
	~homogeneous:true
	~packing:hboxtwo#add ();;
(*Label du tableau Info*)
let nomInfo = GMisc.label
						~text:"Nom de l'image :" ();;
let dimInfo = GMisc.label
						~text:"Dimensions de l'image :" ();;
let moyInfo = GMisc.label
						~text:"Moyenne des couleurs :" ();;

(*Ajout dans noms dans tableau info*)
ignore (tableInfo#attach ~left:0 ~top:0 (nomInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:1 (dimInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:2 (moyInfo#coerce));

(*Info Fichier*)
let nomFichInfo = GMisc.label
						~text:"test.ppm" ();;
let dimFichInfo = GMisc.label
						~text:"800*800" ();;
let moyFichInfo = GMisc.label
						~text:"175" ();;

(*Ajout infos dans tableau*)
ignore (tableInfo#attach ~left:1 ~top:0 (nomFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:1 (dimFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:2 (moyFichInfo#coerce));


(*Creation du widget à mettre dans le TAB Simple du notebook*)
let labelSimple = GMisc.label
					~text:("Opérations Simple") ()

let hboxSimple = GPack.hbox
	~spacing:10
	~packing:hboxtwo#add ()

let tableSimple = GPack.table
	~rows:3
	~columns:2
	~row_spacings:10
	~col_spacings:5
	~homogeneous:true
	~packing:hboxSimple#add ()

(*Noms opérations Simple*)
let rotSimple = GMisc.label
						~text:"Rotation" ();;
let miroirSimple = GMisc.label
						~text:"Miroir" ();;
let invSimple = GMisc.label
						~text:"Inversion" ();;


(*Ajout dans tableSimple des noms d'oréprations*)
ignore (tableSimple#attach ~left:0 ~top:0 (rotSimple#coerce));
ignore (tableSimple#attach ~left:0 ~top:1 (miroirSimple#coerce));
ignore (tableSimple#attach ~left:0 ~top:2 (invSimple#coerce))

(*Bouton Arrows*)
(*Ajouter callback ici*)
let create_arrow_button ~kind ~shadow () =
	let button = GButton.button () in
	let arrow = GMisc.arrow ~kind ~shadow ~packing:button#add () in
	button

(*ROTATION*)
let tableRot = GPack.table
	~rows:1
	~columns:2
	~homogeneous:true ();;

ignore (tableSimple#attach ~left:1 ~top:0 (tableRot#coerce))
(*leftRot*)
let leftRot = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ();;
ignore (tableRot#attach ~left:0 ~top:0 (leftRot#coerce))

(*rightRot*)
let rightRot = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ();;
ignore (tableRot#attach ~left:1 ~top:0 (rightRot#coerce))


(*MIROIR*)
let tableMir = GPack.table
	~rows:3
	~columns:3
	~homogeneous:true ();;

ignore (tableSimple#attach ~left:1 ~top:1 (tableMir#coerce))
(*upMir*)
let upMir = create_arrow_button ~kind:`UP ~shadow:`IN ();;
ignore (tableMir#attach ~left:1 ~top:0 (upMir#coerce))

(*rightMir*)
let rightMir = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ();;
ignore (tableMir#attach ~left:2 ~top:1 (rightMir#coerce))

(*downMir*)
let downMir = create_arrow_button ~kind:`DOWN ~shadow:`OUT ();;
ignore (tableMir#attach ~left:1 ~top:2 (downMir#coerce))

(*leftMir*)
let leftMir = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ();;
ignore (tableMir#attach ~left:0 ~top:1 (leftMir#coerce))

(*INVERSION*)
let tableInv = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true ();;
ignore (tableSimple#attach ~left:2 ~top:2 (tableInv#coerce))

let invBUT = GButton.button
					~label: "Inverser" ();;
ignore (tableInv#attach ~left:0 ~top:0  (invBUT#coerce))


(*Affichage de fenetre*)
let _ =
	ignore (window#event#connect#delete confirm);
	window#show ();
	GMain.main ()
