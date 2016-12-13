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




(*Affichage de fenetre*)
let _ =
	ignore (window#event#connect#delete confirm);
	window#show ();
	GMain.main ()
