open GMain
open GdkKeysyms

module Aux =
struct
	let load file =
		let ich = open_in file in
		let len = in_channel_length ich in
		let buf = Buffer.create len in
		Buffer.add_channel buf ich len;
		close_in ich;
		print_endline (Buffer.contents buf) (*Fonction de chargement*)

	let save file =
		let och = open_out file in
		output_string och ("lol"); (*Fonction de Sauvegarde*)
		close_out och
end


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

(*MENU BAR*)
let menubar = GMenu.menu_bar ~packing:vbox#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_menu = factory#add_submenu "Fichier"
let factory_fileMen = new GMenu.factory file_menu ~accel_group

let _ =
factory_fileMen#add_item "À propos"
								~callback: (fun () -> ignore (about_button#run ()); about_button#misc#hide ());
factory_fileMen#add_item "Sauvegarder"
								~key:_S
								~callback: (fun () -> print_endline "Sauvegarder");
factory_fileMen#add_item "Quitter"
								~key:_Q
								~callback: Main.quit;;

(*Titre*)
let titre = GMisc.label ~markup: "<span font_desc=\"Tahoma 35\">PPMShop - Manipulation d'image de format PPM</span>" ~packing:vbox#add ()


(*Bouttons*)
(*Conteneur de Bouttons*)
let bbox = GPack.button_box `VERTICAL
	~spacing:150
	~layout:`SPREAD
	~border_width:5
	~child_width: 250
	~child_height: 50
	~packing:(vbox#pack ~expand:false) ()

(* GtkFileChooserDialog - Boîte de dialogue d'ouverture et d'enregistrement. *)
let action_button stock event action =
	let dlg = GWindow.file_chooser_dialog
		~action:`OPEN
		~parent:window
		~position:`CENTER_ON_PARENT
		~destroy_with_parent:true () in
	dlg#add_button_stock `CANCEL `CANCEL;
	dlg#add_select_button_stock stock event;
	let btn = GButton.button ~stock ~packing:bbox#add () in
	GMisc.image ~stock ~packing:btn#set_image ();
	btn#connect#clicked (fun () ->
	if dlg#run () = `OPEN then Gaux.may action dlg#filename;
	dlg#misc#hide ());
 	btn


(*Boutton Ouvrir*)
let load = action_button `OPEN `OPEN (Aux.load);;

(*Boutton Quitter*)
let quit =
	let button = GButton.button
				~stock:`QUIT
				~packing:bbox#add () in
button#connect#clicked ~callback:GMain.quit;
button



(*Affichage de fenetre*)
let _ =
	window#event#connect#delete confirm;
	window#show ();
	GMain.main ()
