(*Compilation interface toute seule*)
(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg quad.ml -o quadtree*)


(*Compilation Interface+Fichier externe*)
(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg str.cma v2.ml quad.ml -o quadtree*)
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


let abo = GButton.tool_button ~label: "À Propos" ~stock: `ABOUT ~packing:toolbar#insert ();;
ignore (GButton.separator_tool_item ~packing:toolbar#insert ());;
let qui = GButton.tool_button ~label: "Quitter" ~stock: `QUIT ~packing:toolbar#insert ();;
ignore (abo#connect#clicked (fun () -> ignore (about_button#run ()); ignore (about_button#misc#hide ())));;
qui#connect#clicked Main.quit;;

(*CONFIRMATION PAGE*)
let alignConfirmation = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~show: false
	~packing:vbox#add()

let vboxConfirm = GPack.vbox ~spacing:100 ~packing: alignConfirmation#add ()
let titreConf = GMisc.label ~markup: "<span font_desc=\"Tahoma 20\">Veuillez confirmer le choix de votre fichier : test.ppm ?</span>" ~packing:vboxConfirm#add ()

(*BouttonsConf*)
(*Conteneur de BouttonsConf*)
let bboxConf = GPack.button_box `HORIZONTAL
	~spacing:150
	~layout:`SPREAD
	~border_width:5
	~child_width: 250
	~child_height: 50
	~packing:(vboxConfirm#add) ()

let conf = GButton.button
		~label: "Confirmer"
		~packing:bboxConf#add ()

let confAn = GButton.button
		~label: "Annuler"
		~packing:bboxConf#add ()





(*FIRST PAGE*)
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

module Aux =
struct
	let load file =
		let ich = open_in file in
		let len = in_channel_length ich in
		let buf = Buffer.create len in
		Buffer.add_channel buf ich len;
		close_in ich;
		print_endline (Buffer.contents buf);
		print_endline (Filename.current_dir_name)
		(*Fonction de chargement*)

	let save file =
		let och = open_out file in
		output_string och ("lol"); (*Fonction de Sauvegarde*)
		close_out och
end

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
	ignore (GMisc.image ~stock ~packing:btn#set_image ());
	ignore (btn#connect#clicked (fun () ->
	if dlg#run () = `OPEN then Gaux.may action dlg#filename;
	dlg#misc#hide ();bbox#misc#hide ();titre#misc#hide ();alignConfirmation#misc#show ()));
 	btn;;
(*BOUTON ANNULER SUR DEUXIEME PAGE*)
ignore (confAn#connect#clicked (fun () -> ignore (alignConfirmation#misc#show ()); ignore (titre#misc#show ()); ignore (bbox#misc#show ())));;

(*DEBUT DE TROISIEME INTERFACE*)
let ignore_apply f obj = ignore (f obj)
(*Définition des conteneurs*)
(*+ Grands conteneur*)
let hboxtwo = GPack.hbox
	~spacing:10
	~show: false
	~packing: vbox#add ()

let bboxtwo = GPack.button_box `HORIZONTAL
	~layout:`START
	~border_width:5
	~child_width: 250
	~child_height: 50
	~show:false
	~packing:(vbox#pack ~expand:false) ();;


(*Sous conteneurs*)

(*NOTEBOOK*)
let notebook = GPack.notebook
 			~border_width:5
			~homogeneous_tabs: true
			~tab_pos:`LEFT
			~packing:hboxtwo#add ();;

(*BOUTON CONFIMER SUR DEUXIEME PAGE*)
ignore (conf#connect#clicked (fun () -> ignore (alignConfirmation#misc#hide ()); ignore (hboxtwo#misc#show ()); ignore (bboxtwo#misc#show ());ignore (notebook#goto_page 0)));;

(*bouton retour*)
let returnB = GButton.button
	~label: "Retour"
	~packing:bboxtwo#add ();;

(*BOUTON RETOUR SUR TROISIEME PAGE*)
ignore (returnB#connect#clicked (fun () -> ignore (titre#misc#show ()); ignore (bbox#misc#show ()); ignore (hboxtwo#misc#hide ()); ignore (bboxtwo#misc#hide ())));;

(*Creation du widget à mettre dans le TAB Home du notebook*)
let labelHome = GMisc.label
					~text:"Home" ()

let vboxHome = GPack.vbox
				~packing:(ignore_apply (notebook#append_page ~tab_label:labelHome#coerce)) ()
(* Create a centering alignment object *)
let alignHome = GBin.alignment
			~xalign:0.5
			~yalign:0.1
			~xscale:0.0
			~yscale:0.0
			~packing:vboxHome#add ()

let tableHome = GPack.table
				~rows:2
				~columns:12
				~border_width:3
				~row_spacings:15
				~packing:alignHome#add ()
(*Table Titles*)
let titlePPM = GMisc.label ~markup:"<span font_desc=\"Tahoma 25\"><b>PPMShop</b></span>" ();;
let rot = GMisc.label ~markup:"<span font_desc=\"Tahoma 18\"><b>Rotation</b></span>" ~justify:`LEFT ();;
let rotG = GMisc.label ~markup:"<big>Rotation Gauche</big>" ~justify:`LEFT ();;
let rotD = GMisc.label ~markup:"<big>Rotation Droite</big>" ~justify:`LEFT ();;
let mir = GMisc.label ~markup:"<span font_desc=\"Tahoma 18\"><b>Miroir</b></span>" ~justify:`LEFT ();;
let mirHB = GMisc.label ~markup:"<big>Miroir Haut/Bas</big>" ~justify:`LEFT ();;
let mirGD = GMisc.label ~markup:"<big>Miroir Gauche/Droite</big>" ~justify:`LEFT ();;
let inv = GMisc.label ~markup:"<big>Inversion</big>" ~justify:`LEFT ();;
let com = GMisc.label ~markup:"<big>Compression</big>" ~justify:`LEFT ();;
let seg = GMisc.label ~markup:"<big>Segmentation</big>" ~justify:`LEFT ();;
let sav = GMisc.label ~markup:"<big>Sauvegarde</big>" ~justify:`LEFT ();;

(*Table Texts*)
let textInt = GMisc.label
~text:"Quadtree est un programme qui permet de charger des images en format .ppm et d'appliquer plusieurs opérations sur ces
dernières. En ce qui concerne les opérations on trouve : les opérations simples : Rotation, Mirroir et Inversion de couleurs;
les opérations avancées : Compression et Ségmentation. De plus, quadtree permet de sauvgarder les changements apportés à
l'image."
~justify:`CENTER ();;
let textRG = GMisc.label
~text: "Cette opération consiste à effectuer une rotaion de 90 degrés sur l'image vers la gauche." ~justify:`CENTER ();;
let textRD = GMisc.label
~text: " Cette opération consiste à effectuer une rotaion de 90 degrés sur l'image vers la droite." ~justify:`CENTER ();;
let textMHB = GMisc.label
~text: "Cette opération consiste à découper l'image en deux parties symetriques par rapport à un
axe de symétrie horizontale." ~justify:`CENTER ();;
let textMDG = GMisc.label
~text: "Cette opération consiste à découper l'image en deux parties symetriques par rapport à
un axe de symétrie verticale." ~justify:`CENTER ();;
let textI = GMisc.label
~text: "Cette opération consiste à effectuer une inversion des couleurs de l'image." ~justify:`CENTER ();;
let textC = GMisc.label
~text: "Cette opération consiste à effectuer une compression de l'image." ~justify:`CENTER ();;
let textS = GMisc.label
~text: "Cette opération consiste à effectuer une segmentation de l'image." ~justify:`CENTER ();;
let textSAV = GMisc.label
~text: "Cette opération consiste à effectuer un enregistrement-sous de l'image." ~justify:`CENTER ();;




(*Title and Text addition*)
ignore (tableHome#attach ~left:0 ~right:2 ~top:0 (titlePPM#coerce));
ignore (tableHome#attach ~left:0 ~right:2 ~top:1 (textInt#coerce));
ignore (tableHome#attach ~left:0 ~top:2 (rot#coerce));
ignore (tableHome#attach ~left:0 ~top:3 (rotG#coerce));
ignore (tableHome#attach ~left:1 ~top:3 (textRG#coerce));
ignore (tableHome#attach ~left:0 ~top:4 (rotD#coerce));
ignore (tableHome#attach ~left:1 ~top:4 (textRD#coerce));
ignore (tableHome#attach ~left:0 ~top:5 (mir#coerce));
ignore (tableHome#attach ~left:0 ~top:6 (mirHB#coerce));
ignore (tableHome#attach ~left:1 ~top:6 (textMHB#coerce));
ignore (tableHome#attach ~left:0 ~top:7 (mirGD#coerce));
ignore (tableHome#attach ~left:1 ~top:7 (textMDG#coerce));
ignore (tableHome#attach ~left:0 ~top:8 (inv#coerce));
ignore (tableHome#attach ~left:1 ~top:8 (textI#coerce));
ignore (tableHome#attach ~left:0 ~top:9 (com#coerce));
ignore (tableHome#attach ~left:1 ~top:9 (textC#coerce));
ignore (tableHome#attach ~left:0 ~top:10 (seg#coerce));
ignore (tableHome#attach ~left:1 ~top:10 (textS#coerce));
ignore (tableHome#attach ~left:0 ~top:11 (sav#coerce));
ignore (tableHome#attach ~left:1 ~top:11 (textSAV#coerce))

(*Tableau avec informations sur image*)
let labelInfo = GMisc.label
					~text:("Infos Fichier") ()

let hboxInfo = GPack.hbox
				~packing:(ignore_apply (notebook#append_page ~tab_label:labelInfo#coerce)) ()

let alignInfo = GBin.alignment
		~xalign:0.5
		~yalign:0.1
		~xscale:0.0
		~yscale:0.0
		~packing:hboxInfo#add ()

let tableInfo = GPack.table
	~rows:4
	~columns:2
	~row_spacings:20
	~col_spacings:15
	~homogeneous:true
	~packing: alignInfo#add ()

(*Label du tableau Info*)
let titreInfo = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Informations du fichier</b></span>" ();;
let nomInfo = GMisc.label ~markup: "<b><big>Nom de l'image :</big></b>" ();;
let dimInfo = GMisc.label ~markup: "<b><big>Dimensions de l'image :</big></b>" ();;
let moyInfo = GMisc.label ~markup: "<b><big>Moyenne des couleurs :</big></b>" ();;

(*Ajout dans noms dans tableau info*)
ignore (tableInfo#attach ~left:0 ~right:2 ~top:0 (titreInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:1 (nomInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:2 (dimInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:3 (moyInfo#coerce))

(*Info Fichier*)
let nomFichInfo = GMisc.label
						~markup:"<big>test.ppm</big>" ();;
let dimFichInfo = GMisc.label
						~markup:"<big>800*800</big>" ();;
let moyFichInfo = GMisc.label
						~markup:"<big>175</big>" ();;

(*Ajout infos dans tableau*)
ignore (tableInfo#attach ~left:1 ~top:1 (nomFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:2 (dimFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:3 (moyFichInfo#coerce));;

(*Creation du widget à mettre dans le TAB Simple du notebook*)
let labelSimple = GMisc.label
					~text:("Opérations Simple") ()

let hboxSimple = GPack.hbox
	~spacing:10
	~packing:(ignore_apply (notebook#append_page ~tab_label:labelSimple#coerce)) ()

let alignSimple = GBin.alignment
		~xalign:0.5
		~yalign:0.1
		~xscale:0.0
		~yscale:0.0
		~packing:hboxSimple#add ()

let tableSimple = GPack.table
	~rows:4
	~columns:2
	~row_spacings:10
	~col_spacings:50
	~homogeneous:true
	~packing:alignSimple#add ()

(*Noms opérations Simple*)
let titreSimple = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Opérations Simples</b></span>" ()
let rotSimple = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Rotation</span>" ()
let miroirSimple = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Miroir</span>" ()
let invSimple = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Inversion</span>" ();;


(*Ajout dans tableSimple des noms d'oréprations*)
ignore (tableSimple#attach ~left:0 ~right:2 ~top:0 (titreSimple#coerce));
ignore (tableSimple#attach ~left:0 ~top:1 (rotSimple#coerce));
ignore (tableSimple#attach ~left:0 ~top:2 (miroirSimple#coerce));
ignore (tableSimple#attach ~left:0 ~top:3 (invSimple#coerce))

(*Bouton Arrows*)
(*Ajouter callback ici*)
let create_arrow_button ~kind ~shadow ~packing () =
	let button = GButton.button ~packing () in
	let arrow = GMisc.arrow ~kind ~shadow ~packing:button#add () in
	button

(*ROTATION*)
let alignTableRot = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableSimple#attach ~left:1 ~top:1 (alignTableRot#coerce))
let tableRot = GPack.table
	~rows:1
	~columns:2
	~homogeneous:true
	~packing: alignTableRot#add ()

(*leftRot*)
let bboxleftRot = GPack.button_box `HORIZONTAL
		~border_width:3
		~child_width: 25
		~child_height:25 ();;

ignore (tableRot#attach ~left:0 ~top:0 (bboxleftRot#coerce))

let leftRot = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:bboxleftRot#add ()

(*rightRot*)
let bboxrightRot = GPack.button_box `HORIZONTAL
		~border_width:2
		~child_width: 25
		~child_height:25 ();;

ignore (tableRot#attach ~left:1 ~top:0 (bboxrightRot#coerce))

let rightRot = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:bboxrightRot#add ()

(*MIROIR*)
let alignTableMiroir = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableSimple#attach ~left:1 ~top:2 (alignTableMiroir#coerce))

let tableMir = GPack.table
	~rows:3
	~columns:3
	~homogeneous:true
	~packing:alignTableMiroir#add ();;

(*upMir*)
let bboxMiroirUp = GPack.button_box `HORIZONTAL
		~child_width:25
		~child_height:25 ();;

ignore (tableMir#attach ~left:1 ~top:0 (bboxMiroirUp#coerce))

let upMir = create_arrow_button ~kind:`UP ~shadow:`IN ~packing:bboxMiroirUp#add ();;

(*rightMir*)
let bboxMiroirRight = GPack.button_box `HORIZONTAL
		~child_width:25
		~child_height:25 ();;

ignore (tableMir#attach ~left:2 ~top:1 (bboxMiroirRight#coerce))

let rightMir = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:bboxMiroirRight#add ();;

(*downMir*)
let bboxMiroirDown = GPack.button_box `HORIZONTAL
		~child_width:25
		~child_height:25 ();;

ignore (tableMir#attach ~left:1 ~top:2 (bboxMiroirDown#coerce))

let downMir = create_arrow_button ~kind:`DOWN ~shadow:`OUT ~packing:bboxMiroirDown#add ();;

(*leftMir*)
let bboxMiroirLeft = GPack.button_box `HORIZONTAL
		~child_width:25
		~child_height:25 ();;

ignore (tableMir#attach ~left:0 ~top:1 (bboxMiroirLeft#coerce))

let leftMir = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:bboxMiroirLeft#add ();;


(*INVERSION*)
let alignTableInversion = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableSimple#attach ~left:1 ~top:3 (alignTableInversion#coerce))

let tableInv = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableInversion#add ()

let bboxInversion = GPack.button_box `HORIZONTAL
		~child_width:200
		~child_height:50 ();;

ignore (tableInv#attach ~left:0 ~top:0 (bboxInversion#coerce))

let invBUT = GButton.button ~label: "Inverser" ~packing:bboxInversion#add ();;

(*Creation du widget à mettre dans le TAB Avancées du notebook*)
let labelAdvanced = GMisc.label ~text:("Opérations Avancées") ()

let hboxAdvanced = GPack.hbox
	~spacing:10
	~packing:(ignore_apply (notebook#append_page ~tab_label:labelAdvanced#coerce)) ()

let alignAdvanced = GBin.alignment
			~xalign:0.5
			~yalign:0.1
			~xscale:0.0
			~yscale:0.0
			~packing:hboxAdvanced#add ()

let tableAdvanced = GPack.table
	~rows:3
	~columns:2
	~row_spacings:35
	~col_spacings:50
	~homogeneous:true
	~packing:alignAdvanced#add ()

(*Noms opérations Avancées*)
let titreAdvanced = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Opérations Avancées</b></span>" ()
let comprAdv = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Compression</span>" ()
let segmAdv = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Segmentation</span>" ();;

(*Ajout dans tableAdvanced des noms d'oréprations*)
ignore (tableAdvanced#attach ~left:0 ~right:2 ~top:0 (titreAdvanced#coerce));
ignore (tableAdvanced#attach ~left:0 ~top:1 (comprAdv#coerce));
ignore (tableAdvanced#attach ~left:0 ~top:2 (segmAdv#coerce))

(*COMPRESSION*)
let alignTableCompression = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableAdvanced#attach ~left:1 ~top:1 (alignTableCompression#coerce))

let tableCompression = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableCompression#add ()

let bboxCompression = GPack.button_box `HORIZONTAL
		~child_width:250
		~child_height:50 ();;

ignore (tableCompression#attach ~left:0 ~top:0  (bboxCompression#coerce));;

let advancedCompressionButton = GButton.button ~label: "Compresser" ~packing:bboxCompression#add ();;

(*SEGMENTATION*)
let alignTableSegmentation = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableAdvanced#attach ~left:1 ~top:2 (alignTableSegmentation#coerce))

let tableSegmentation = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableSegmentation#add ()

let bboxSegmentation = GPack.button_box `HORIZONTAL
		~child_width:250
		~child_height:50 ();;

ignore (tableSegmentation#attach ~left:0 ~top:0  (bboxSegmentation#coerce));;

let advancedSegmentationButton = GButton.button ~label: "Segmenter"  ~packing:bboxSegmentation#add ();;



(*Creation du widget à mettre dans le TAB Sauvegarder du notebook*)
let labelSave = GMisc.label ~text:("Sauvegarde") ()

let hboxSave = GPack.hbox
	~spacing:10
	~packing:(ignore_apply (notebook#append_page ~tab_label:labelSave#coerce)) ()

(* Create a centering alignment object *)
let alignTabSave = GBin.alignment
		~xalign:0.5
		~yalign:0.1
		~xscale:0.0
		~yscale:0.0
		~packing:hboxSave#add ()

let tableTabSave = GPack.table
	~rows:2
	~columns:1
	~row_spacings:50
	~homogeneous:true
	~packing:alignTabSave#add ()

(*Nom opération Sauvegarde*)
let nameSave = GMisc.label ~markup:"<span font_desc=\"Tahoma 25\"><b>Enregistrement</b></span>" ();;
(*Ajout dans tableSave du nom d'enregistrement*)
ignore (tableTabSave#attach ~left:0  ~top:0 (nameSave#coerce))

(*Enregistrement*)
let alignTableSave = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableTabSave#attach ~left:0 ~top:1 (alignTableSave#coerce))

let tableSave = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableSave#add ()

let savebbox = GPack.button_box `VERTICAL
	~child_width: 250
	~child_height:50 ();;

ignore (tableSave#attach ~left:0 ~top:0  (savebbox#coerce))

let saveBut= GButton.button ~label: "Sauvegarder" ~packing:savebbox#add ();;

let viewOneToggle = GButton.toggle_tool_button
	~label:"View 1"
	~packing:toolbar#insert ()

let viewTwoToggle = GButton.toggle_tool_button
	~label:"View 2"
	~packing:toolbar#insert ();;

viewOneToggle#set_active true;;
(*Boutton Ouvrir*)
let load = action_button `OPEN `OPEN (Aux.load);;

(*Boutton Quitter*)
let quit =
	let button = GButton.button
				~stock:`QUIT
				~packing:bbox#add () in
	ignore (button#connect#clicked ~callback:GMain.quit);
	button

(*Affichage de fenetre*)
let _ =
	ignore (window#event#connect#delete confirm);
	window#show ();
	GMain.main ()
