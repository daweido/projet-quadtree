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

let hboxtwo = GPack.hbox ~spacing:10 ~packing: vbox#add ()

let bboxtwo = GPack.button_box `HORIZONTAL
	~layout:`START
	~border_width:5
	~child_width: 250
	~child_height: 50
	~packing:(vbox#pack ~expand:false) ();;

(*bouton retour*)
let returnB = GButton.button
	~label: "Retour"
	~packing:bboxtwo#add ();;

(*let alignSecondView = GBin.alignment
		~xalign:0.5
		~yalign:0.1
		~xscale:0.0
		~yscale:0.0
		~packing:hboxtwo#add ()
*)
let hboxSecondView = GPack.hbox ~packing:hboxtwo#add ()

let leftToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~packing:(hboxSecondView#pack ~expand:false) ()

(*Tableau avec informations sur image*)
let alignInfo = GBin.alignment
		~xalign:0.5
		~yalign:0.1
		~xscale:0.0
		~yscale:0.0 ();;

leftToolbarSecondView#insert_widget ~tooltip:"Info" alignInfo#coerce;;

let tableInfo = GPack.table
	~rows:3
	~columns:2
	~row_spacings:5
	~col_spacings:1
	~homogeneous:true
	~packing: alignInfo#add ()

(*Label du tableau Info*)
let nomInfo = GMisc.label ~markup: "<b>Nom :</b>" ();;
let dimInfo = GMisc.label ~markup: "<b>Dimensions :</b>" ();;
let moyInfo = GMisc.label ~markup: "<b>Moyenne :</b>" ();;

(*Ajout dans noms dans tableau info*)
ignore (tableInfo#attach ~left:0 ~top:1 (nomInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:2 (dimInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:3 (moyInfo#coerce))

(*Info Fichier*)
let nomFichInfo = GMisc.label ~markup:"test.ppm" ();;
let dimFichInfo = GMisc.label ~markup:"800*800" ();;
let moyFichInfo = GMisc.label ~markup:"175" ();;

(*Ajout infos dans tableau*)
ignore (tableInfo#attach ~left:1 ~top:1 (nomFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:2 (dimFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:3 (moyFichInfo#coerce));;


(*ViewImage*)
let aligneImageView = GBin.alignment
	~xalign:0.5
	~yalign:0.1
	~xscale:0.0
	~yscale:0.0
	~packing:hboxSecondView#add ()

let tableImageView = GPack.table
	~rows:1
	~columns:1
	~packing: aligneImageView#add ()

let viewImageAfficheSecond = GMisc.image
	~file: "test.ppm" ();;

ignore (tableImageView#attach ~left:0  ~top:0 (viewImageAfficheSecond#coerce))

(*RightToolbar*)
let rightToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(hboxSecondView#pack ~expand:false) ()

let toolbarNameRotation = GMisc.label ~markup: "<b>Rotation</b>" ();;

rightToolbarSecondView#insert_widget ~tooltip:"Rotation" toolbarNameRotation#coerce;;


(*Affichage de fenetre*)
let _ =
	ignore (window#event#connect#delete confirm);
	window#show ();
	GMain.main ()
