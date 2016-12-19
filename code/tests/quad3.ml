(*Compilation interface toute seule*)
(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg quad.ml -o quadtree*)


(*Compilation Interface+Fichier externe*)
(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg str.cma v2.ml quad.ml -o quadtree*)
open GMain
open GdkKeysyms

let _ = GMain.init ()

(*FONCTIONS DE BASES*)
module Aux =
struct
	let load file =
		let ich = open_in file in
		let len = in_channel_length ich in
		let buf = Buffer.create len in
		Buffer.add_channel buf ich len;
		close_in ich;
		print_endline (Buffer.contents buf)
		(*Fonction de chargement*)

	let save file =
		let och = open_out file in
		output_string och ("lol"); (*Fonction de Sauvegarde*)
		close_out och
end

let ignore_apply f obj = ignore (f obj)
(*MAIN CONTAINERS*)
(*Window*)
let window = GWindow.window
	~width:900
	~height:700
	~position:`CENTER
	~resizable:false
	~title:"PPMShop" ()

(*Biggest Window Container*)
let firstVbox = GPack.vbox
	~spacing:10
	~packing:window#add ()

let topToolbar = GButton.toolbar
	~orientation: `HORIZONTAL
	~style:`BOTH
	~packing:(firstVbox#pack ~expand:false) ()

(*First Page - Accueil*)
let firstPageTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 35\">PPMShop - Manipulation d'image de format PPM</span>" ~packing:firstVbox#add ()

let firstPageButtonBox = GPack.button_box `VERTICAL
	~spacing:150
	~layout:`SPREAD
	~border_width:5
	~child_width: 250
	~child_height: 50
	~packing:(firstVbox#pack ~expand:false) ()
(*End First Page*)

(*Second Page - Confirmation*)

let alignConfirmation = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~show: false
	~packing:firstVbox#add()

let confirmVerticalBox = GPack.vbox
	~spacing:100 ~packing: alignConfirmation#add ()

let titreConf = GMisc.label ~markup: "<span font_desc=\"Tahoma 20\">Veuillez confirmer le choix de votre fichier : test.ppm ?</span>" ~packing:confirmVerticalBox#add ()

let confirmButtonBox = GPack.button_box `HORIZONTAL
	~spacing:150
	~layout:`SPREAD
	~border_width:5
	~child_width: 250
	~child_height: 50
	~packing:(confirmVerticalBox#add) ()
(*End of Second Page*)

(************************Troisième Page****************************************)
let thirdPageHBoxView1 = GPack.hbox
	~spacing:10
	~show: false
	~packing: firstVbox#add ()

let thirdPageHBoxView2 = GPack.hbox
	~spacing:10
	~show: false
	~packing: firstVbox#add ()


let thridPageButtonBox = GPack.button_box `HORIZONTAL
	~layout:`START
	~border_width:5
	~child_width: 250
	~child_height: 50
	~show:false
	~packing:(firstVbox#pack ~expand:false) ();;
(*View 1*)
let notebook = GPack.notebook
 	~border_width:5
	~homogeneous_tabs: true
	~tab_pos:`LEFT
	~packing:thirdPageHBoxView1#add ();;
(********NOTEBOOK********)
(*1st Tab*)
let notebookTabHomeLabel = GMisc.label ~text:"Home" ()

let notebookVBoxHome = GPack.vbox
	~packing:(ignore_apply (notebook#append_page ~tab_label:notebookTabHomeLabel#coerce)) ()

let notebookAlignHome = GBin.alignment
	~xalign:0.5
	~yalign:0.1
	~xscale:0.0
	~yscale:0.0
	~packing:notebookVBoxHome#add ()

let notebookTableHome = GPack.table
	~rows:2
	~columns:12
	~border_width:3
	~row_spacings:15
	~packing:notebookAlignHome#add ()

(*2nd Tab*)
let notebookTabInfoLabel = GMisc.label ~text:("Infos Fichier") ()

let notebookHBoxInfo = GPack.hbox
	~packing:(ignore_apply (notebook#append_page ~tab_label:notebookTabInfoLabel#coerce)) ()

let notebookAlignInfo = GBin.alignment
	~xalign:0.5
	~yalign:0.1
	~xscale:0.0
	~yscale:0.0
	~packing:notebookHBoxInfo#add ()

let notebookTableInfo = GPack.table
	~rows:4
	~columns:2
	~row_spacings:20
	~col_spacings:15
	~homogeneous:true
	~packing: notebookAlignInfo#add ()

(*3rd Tab*)
let notebookTabSimpleLabel = GMisc.label ~text:("Opérations Simple") ()

let notebookHBoxSimple = GPack.hbox
	~spacing:10
	~packing:(ignore_apply (notebook#append_page ~tab_label:notebookTabSimpleLabel#coerce)) ()

let notebookAlignSimple = GBin.alignment
	~xalign:0.5
	~yalign:0.1
	~xscale:0.0
	~yscale:0.0
	~packing:notebookHBoxSimple#add ()

let notebookTableSimple = GPack.table
	~rows:4
	~columns:2
	~row_spacings:10
	~col_spacings:50
	~homogeneous:true
	~packing:notebookAlignSimple#add ()

let notebookTableRotAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let notebookTableRot = GPack.table
	~rows:1
	~columns:2
	~homogeneous:true
	~packing: notebookTableRotAlign#add ()

let notebookButtonBoxLeftRot = GPack.button_box `HORIZONTAL
	~border_width:3
	~child_width: 25
	~child_height:25 ();;

let notebookButtonBoxRightRot = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;

let notebookTableMirorAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let notebookTableMiror = GPack.table
	~rows:3
	~columns:3
	~homogeneous:true
	~packing:notebookTableMirorAlign#add ();;

let notebookButtonBoxMirorUp = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;

let notebookButtonBoxMirorRight = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;

let notebookButtonBoxMirorDown = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;

let notebookButtonBoxMirorLeft = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;

let notebookTableInversionAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let notebookTableInversion = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:notebookTableInversionAlign#add ()

let notebookButtonBoxInversion = GPack.button_box `HORIZONTAL
	~child_width:200
	~child_height:50 ();;

(*4th Tab*)
let notebookTabAdvancedLabel = GMisc.label ~text:("Opérations Avancées") ()

let notebookHBoxAdvanced = GPack.hbox
	~spacing:10
	~packing:(ignore_apply (notebook#append_page ~tab_label:notebookTabAdvancedLabel#coerce)) ()

let notebookAdvancedAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.1
	~xscale:0.0
	~yscale:0.0
	~packing:notebookHBoxAdvanced#add ()

let notebookAdvancedTable = GPack.table
	~rows:3
	~columns:2
	~row_spacings:35
	~col_spacings:50
	~homogeneous:true
	~packing:notebookAdvancedAlign#add ()

let notebookAlignTableCompression = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let notebookTableCompression = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:notebookAlignTableCompression#add ()

let notebookButtonBoxCompression = GPack.button_box `HORIZONTAL
	~child_width:250
	~child_height:50 ();;

let notebookAlignTableSegmentation = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let notebookTableSegmentation = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:notebookAlignTableSegmentation#add ()

let notebookButtonBoxSegmentation = GPack.button_box `HORIZONTAL
	~child_width:250
	~child_height:50 ();;

(*5th Tab*)
let notebookTabSaveLabel = GMisc.label ~text:("Sauvegarde") ()

let notebookHBoxSave = GPack.hbox
	~spacing:10
	~packing:(ignore_apply (notebook#append_page ~tab_label:notebookTabSaveLabel#coerce)) ()

let notebookAlignSave = GBin.alignment
	~xalign:0.5
	~yalign:0.1
	~xscale:0.0
	~yscale:0.0
	~packing:notebookHBoxSave#add ()

let notebookTableSave = GPack.table
	~rows:2
	~columns:1
	~row_spacings:50
	~homogeneous:true
	~packing:notebookAlignSave#add ()

let notebookTableAlignSave = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let notebookTabTableSave = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:notebookTableAlignSave#add ()

let notebookSaveButtonBox = GPack.button_box `VERTICAL
	~child_width: 250
	~child_height:50 ();;


(*View2*)
let leftToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(thirdPageHBoxView2#pack ~expand:false) ()

let tableLeftToolbar = GPack.table
	~rows:2
	~columns:1
	~row_spacings:20 ();;

let buttonBoxLeftToolbar = GPack.button_box `VERTICAL
	~layout:`SPREAD
	~border_width:3
	~child_width: 90
	~child_height: 25 ();;

let tableInfo = GPack.table
	~rows:4
	~columns:2
	~row_spacings:5
	~col_spacings:1
	~border_width:2
	~homogeneous:true ();;


(*ViewImage*)
let aligneImageView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:thirdPageHBoxView2#add ()

let tableImageView = GPack.table
	~rows:1
	~columns:1
	~packing: aligneImageView#add ()


(*RightToolbar*)
let rightToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(thirdPageHBoxView2#pack ~expand:false) ()

let tableRightToolbar = GPack.table
	~rows:6
	~columns:1
	~row_spacings:55
	~border_width:5 ();;

let alignTableRotToolbar = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableRot = GPack.table
	~rows:2
	~columns:2
	~homogeneous:true
	~packing: alignTableRotToolbar#add ()

let bboxleftRot = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;

let bboxrightRot = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;

let alignTableMiroir = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableMir = GPack.table
	~rows:4
	~columns:3
	~homogeneous:true
	~packing:alignTableMiroir#add ();;

let bboxMiroirUp = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;


let bboxMiroirRight = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;


let bboxMiroirDown = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;

let bboxMiroirLeft = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:25 ();;

let alignTableInversion = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableInv = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableInversion#add ()

let bboxInversion = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;

let alignTableCompression = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableCompression = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableCompression#add ()

let bboxCompression = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;


let alignTableSegmentation = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableSegmentation = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableSegmentation#add ()

let bboxSegmentation = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;

let alignTableSave = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableSave = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableSave#add ()

let savebbox = GPack.button_box `VERTICAL
	~child_width: 90
	~child_height:25 ();;

(*BUTTONS AND WIDNOWS*)
(*Top Toolbar*)
let topToolbarAbout = GButton.tool_button ~label: "À Propos" ~stock: `ABOUT ~packing:topToolbar#insert ();;
ignore (GButton.separator_tool_item ~packing:topToolbar#insert ());;
let topToolbarQuit = GButton.tool_button ~label: "Quitter" ~stock: `QUIT ~packing:topToolbar#insert ();;

let topToolbarViewsItem = GButton.tool_item
	~show:false
	~packing:topToolbar#insert ()
let topToolbarAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:topToolbarViewsItem#add ();;

let topToolbarTable = GPack.table
	~rows:1
	~columns:2
	~col_spacings:20
	~packing:topToolbarAlign#add();;


let topToolbarRadioView1 = GButton.radio_button
	~label:"View 1"
	~active:true ()

let topToolbarRadioView2 = GButton.radio_button
	~label:"View 2"
	~group:topToolbarRadioView1#group ()
(*About Dialog*)
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

let confirmButtonConfirmer = GButton.button
		~label: "Confirmer"
		~packing:confirmButtonBox#add ()

let confirmButtonAnnuler = GButton.button
		~label: "Annuler"
		~packing:confirmButtonBox#add ();;

let thridPageButtonReturn = GButton.button
	~label: "Retour"
	~packing:thridPageButtonBox#add ();;
(*View 1*)
let create_arrow_button ~kind ~shadow ~packing () =
	let button = GButton.button ~packing () in
	let arrow = GMisc.arrow ~kind ~shadow ~packing:button#add () in
	button

let notebookArrowLeftRot = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:notebookButtonBoxLeftRot#add ()
let notebookArrowRightRot = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:notebookButtonBoxRightRot#add ()
let notebookArrowMirorUp = create_arrow_button ~kind:`UP ~shadow:`IN ~packing:notebookButtonBoxMirorUp#add ();;
let notebookArrowMirorRight = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:notebookButtonBoxMirorRight#add ();;
let notebookArrowMirorDown = create_arrow_button ~kind:`DOWN ~shadow:`OUT ~packing:notebookButtonBoxMirorDown#add ();;
let notebookArrowMirorLeft = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:notebookButtonBoxMirorLeft#add ();;
let notebookButtonInversion = GButton.button ~label: "Inverser" ~packing:notebookButtonBoxInversion#add ();;
let notebookButtonCompression = GButton.button ~label: "Compresser" ~packing:notebookButtonBoxCompression#add ();;
let notebookButtonSegmentation = GButton.button ~label: "Segmenter"  ~packing:notebookButtonBoxSegmentation#add ();;
(*View2*)
let aboutLeftToolbarButton = GButton.button
	~label: "About"
	~packing:buttonBoxLeftToolbar#add ()

let rightToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(thirdPageHBoxView2#pack ~expand:false) ()

let leftRot = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:bboxleftRot#add ()
let rightRot = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:bboxrightRot#add ()
let upMir = create_arrow_button ~kind:`UP ~shadow:`IN ~packing:bboxMiroirUp#add ();;
let rightMir = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:bboxMiroirRight#add ();;
let downMir = create_arrow_button ~kind:`DOWN ~shadow:`OUT ~packing:bboxMiroirDown#add ();;
let leftMir = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:bboxMiroirLeft#add ();;
let invBUT = GButton.button ~label: "Inverser" ~packing:bboxInversion#add ();;
let advancedCompressionButton = GButton.button ~label: "Compresser" ~packing:bboxCompression#add ();;
let advancedSegmentationButton = GButton.button ~label: "Segmenter"  ~packing:bboxSegmentation#add ();;
let saveBut= GButton.button ~label: "Sauvegarder" ~packing:savebbox#add ();;



(*LOAD*)
let action_buttonLoad =
	let dlgLoad = GWindow.file_chooser_dialog
		~action:`OPEN
		~parent:window
		~position:`CENTER_ON_PARENT
		~destroy_with_parent:true () in
	dlgLoad#add_button_stock `CANCEL `CANCEL;
	dlgLoad#add_select_button_stock `OPEN `OPEN;
	let btn = GButton.button ~stock:`OPEN ~packing:firstPageButtonBox#add () in
	 GMisc.image ~stock:`OPEN ~packing:btn#set_image ();
	btn#connect#clicked (fun () -> if dlgLoad#run () = `OPEN then Gaux.may (Aux.load) dlgLoad#filename;
	dlgLoad#misc#hide ();firstPageButtonBox#misc#hide ();firstPageTitle#misc#hide ();alignConfirmation#misc#show ());
	btn

let firstPageQuitButton = GButton.button
				~stock:`QUIT
				~packing:firstPageButtonBox#add ();;

let quitConfirmationPopup _ =
	let quitConfirmationPopupWindow = GWindow.message_dialog
		~message:"<b><big>Voulez-vous vraiment quitter ?</big></b>\n\n"
		~parent:window
		~destroy_with_parent:true
		~use_markup:true
		~message_type:`QUESTION
		~position:`CENTER_ON_PARENT
		~buttons:GWindow.Buttons.yes_no () in
	let quitConfirmationPopupAnswer = quitConfirmationPopupWindow#run () = `NO in
	quitConfirmationPopupWindow#destroy ();
	quitConfirmationPopupAnswer
(*LABELS*)
(*Table Texts*)
let notebookHomeIntro = GMisc.label
~text:"Quadtree est un programme qui permet de charger des images en format .ppm et d'appliquer plusieurs opérations sur ces
dernières. En ce qui concerne les opérations on trouve : les opérations simples : Rotation, Mirroir et Inversion de couleurs;
les opérations avancées : Compression et Ségmentation. De plus, quadtree permet de sauvgarder les changements apportés à
l'image."
~justify:`CENTER ();;
let notebookHomeTextRotLeft = GMisc.label
~text: "Cette opération consiste à effectuer une rotaion de 90 degrés sur l'image vers la gauche." ~justify:`CENTER ();;
let notebookHomeTextRotRight = GMisc.label
~text: "Cette opération consiste à effectuer une rotaion de 90 degrés sur l'image vers la droite." ~justify:`CENTER ();;
let notebookHomeTextMirorUpDown = GMisc.label
~text: "Cette opération consiste à découper l'image en deux parties symetriques par rapport à un
axe de symétrie horizontale." ~justify:`CENTER ();;
let notebookHomeTextMirorLeftRight = GMisc.label
~text: "Cette opération consiste à découper l'image en deux parties symetriques par rapport à
un axe de symétrie verticale." ~justify:`CENTER ();;
let notebookHomeTextInversion = GMisc.label
~text: "Cette opération consiste à effectuer une inversion des couleurs de l'image." ~justify:`CENTER ();;
let notebookHomeTextCompression = GMisc.label
~text: "Cette opération consiste à effectuer une compression de l'image." ~justify:`CENTER ();;
let notebookHomeTextSegmentation = GMisc.label
~text: "Cette opération consiste à effectuer une segmentation de l'image." ~justify:`CENTER ();;
let notebookHomeTextSave = GMisc.label
~text: "Cette opération consiste à effectuer un enregistrement-sous de l'image." ~justify:`CENTER ();;
(*View1*)
(*Notebook*)
let notebookHomeTitle = GMisc.label ~markup:"<span font_desc=\"Tahoma 25\"><b>PPMShop</b></span>" ();;
let notebookHomeTitleRot = GMisc.label ~markup:"<span font_desc=\"Tahoma 18\"><b>Rotation</b></span>" ~justify:`LEFT ();;
let notebookHomeTitleRotLeft = GMisc.label ~markup:"<big>Rotation Gauche</big>" ~justify:`LEFT ();;
let notebookHomeTitleRotRight = GMisc.label ~markup:"<big>Rotation Droite</big>" ~justify:`LEFT ();;
let notebookHomeTitleMiror = GMisc.label ~markup:"<span font_desc=\"Tahoma 18\"><b>Miroir</b></span>" ~justify:`LEFT ();;
let notebookHomeTitleMirorUpDown = GMisc.label ~markup:"<big>Miroir Haut/Bas</big>" ~justify:`LEFT ();;
let notebookHomeTitleMirorLeftRight = GMisc.label ~markup:"<big>Miroir Gauche/Droite</big>" ~justify:`LEFT ();;
let notebookHomeTitleInversion = GMisc.label ~markup:"<big>Inversion</big>" ~justify:`LEFT ();;
let notebookHomeTitleCompression = GMisc.label ~markup:"<big>Compression</big>" ~justify:`LEFT ();;
let notebookHomeTitleSegmentation = GMisc.label ~markup:"<big>Segmentation</big>" ~justify:`LEFT ();;
let notebookHomeTitleSave = GMisc.label ~markup:"<big>Sauvegarde</big>" ~justify:`LEFT ();;

(*Label du tableau Info*)
let notebookInfoTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Informations du fichier</b></span>" ();;
let notebookInfoName = GMisc.label ~markup: "<b><big>Nom de l'image :</big></b>" ();;
let notebookInfoDimensions = GMisc.label ~markup: "<b><big>Dimensions de l'image :</big></b>" ();;
let notebookInfoMean = GMisc.label ~markup: "<b><big>Moyenne des couleurs :</big></b>" ();;

(*Info Fichier*)
let notebookInfoNameTest = GMisc.label ~markup:"<big>test.ppm</big>" ();;
let notebookInfoDimensionsTest = GMisc.label ~markup:"<big>800*800</big>" ();;
let notebookInfoMeanTest = GMisc.label ~markup:"<big>175</big>" ();;

(*Creation du widget à mettre dans le TAB Simple du notebook*)

(*Noms opérations Simple*)
let notebookSimpleTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Opérations Simples</b></span>" ()
let notebookSimpleRotation = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Rotation</span>" ()
let notebookSimpleMiror = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Miroir</span>" ()
let notebookSimpleInversion = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Inversion</span>" ();;
(*Noms opérations Avancées*)
let notebookAdvancedTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Opérations Avancées</b></span>" ()
let notebookAdvancedCompression = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Compression</span>" ()
let notebookAdvancedSegmentation = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Segmentation</span>" ();;
let notebookSaveTitle = GMisc.label ~markup:"<span font_desc=\"Tahoma 25\"><b>Enregistrement</b></span>" ();;

(*View2*)
let titreInfo = GMisc.label ~markup: "<b>Informations</b>" ();;
let titreFichierInfo = GMisc.label ~markup: "<b>Image</b>" ();;
let nomInfo = GMisc.label ~markup: "<b>Nom :</b>" ();;
let dimInfo = GMisc.label ~markup: "<b>Dimensions :</b>" ();;
let moyInfo = GMisc.label ~markup: "<b>Moyenne :</b>" ();;
let nomFichInfo = GMisc.label ~markup:"test.ppm" ();;
let dimFichInfo = GMisc.label ~markup:"800*800" ();;
let moyFichInfo = GMisc.label ~markup:"175" ();;
let toolbarNameRotation = GMisc.label ~markup: "<b>Rotation</b>" ();;
let toolbarNameMiroir = GMisc.label ~markup: "<b>Miroir</b>" ();;





(*ACTIONS & FUNCTIONS*)
(*ABOUT WINDOW*)
ignore (topToolbarAbout#connect#clicked (fun () -> ignore (about_button#run ()); ignore (about_button#misc#hide ())));;
topToolbarQuit#connect#clicked Main.quit;;

ignore (topToolbarTable#attach ~left:0 ~top:0 (topToolbarRadioView1#coerce));
ignore (topToolbarTable#attach ~left:1 ~top:0 (topToolbarRadioView2#coerce));

ignore (firstPageQuitButton#connect#clicked ~callback:GMain.quit);


ignore (confirmButtonAnnuler#connect#clicked (fun () -> ignore (alignConfirmation#misc#hide ()); ignore (firstPageTitle#misc#show ()); ignore (firstPageButtonBox#misc#show ())));;
ignore (notebookTableHome#attach ~left:0 ~right:2 ~top:0 (notebookHomeTitle#coerce));
ignore (notebookTableHome#attach ~left:0 ~right:2 ~top:1 (notebookHomeIntro#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:2 (notebookHomeTitleRot#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:3 (notebookHomeTitleRotLeft#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:3 (notebookHomeTextRotLeft#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:4 (notebookHomeTitleRotRight#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:4 (notebookHomeTextRotRight#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:5 (notebookHomeTitleMiror#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:6 (notebookHomeTitleMirorUpDown#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:6 (notebookHomeTextMirorUpDown#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:7 (notebookHomeTitleMirorLeftRight#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:7 (notebookHomeTextMirorLeftRight#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:8 (notebookHomeTitleInversion#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:8 (notebookHomeTextInversion#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:9 (notebookHomeTitleCompression#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:9 (notebookHomeTextCompression#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:10 (notebookHomeTitleSegmentation#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:10 (notebookHomeTextSegmentation#coerce));
ignore (notebookTableHome#attach ~left:0 ~top:11 (notebookHomeTitleSave#coerce));
ignore (notebookTableHome#attach ~left:1 ~top:11 (notebookHomeTextSave#coerce));
(*Ajout dans noms dans tableau info*)
ignore (notebookTableInfo#attach ~left:0 ~right:2 ~top:0 (notebookInfoTitle#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:1 (notebookInfoName#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:2 (notebookInfoDimensions#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:3 (notebookInfoMean#coerce));
(*Ajout infos dans tableau*)
ignore (notebookTableInfo#attach ~left:1 ~top:1 (notebookInfoNameTest#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:2 (notebookInfoDimensionsTest#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:3 (notebookInfoMeanTest#coerce));;

(*Ajout dans tableSimple des noms d'oréprations*)
ignore (notebookTableSimple#attach ~left:0 ~right:2 ~top:0 (notebookSimpleTitle#coerce));
ignore (notebookTableSimple#attach ~left:0 ~top:1 (notebookSimpleRotation#coerce));
ignore (notebookTableSimple#attach ~left:0 ~top:2 (notebookSimpleMiror#coerce));
ignore (notebookTableSimple#attach ~left:0 ~top:3 (notebookSimpleInversion#coerce));
ignore (notebookTableSimple#attach ~left:1 ~top:1 (notebookTableRotAlign#coerce));
ignore (notebookTableRot#attach ~left:0 ~top:0 (notebookButtonBoxLeftRot#coerce));
ignore (notebookTableRot#attach ~left:1 ~top:0 (notebookButtonBoxRightRot#coerce));
ignore (notebookTableSimple#attach ~left:1 ~top:2 (notebookTableMirorAlign#coerce));
ignore (notebookTableMiror#attach ~left:1 ~top:0 (notebookButtonBoxMirorUp#coerce));
ignore (notebookTableMiror#attach ~left:2 ~top:1 (notebookButtonBoxMirorRight#coerce));
ignore (notebookTableMiror#attach ~left:1 ~top:2 (notebookButtonBoxMirorDown#coerce));
ignore (notebookTableMiror#attach ~left:0 ~top:1 (notebookButtonBoxMirorLeft#coerce));
ignore (notebookTableSimple#attach ~left:1 ~top:3 (notebookTableInversionAlign#coerce));
ignore (notebookTableInversion#attach ~left:0 ~top:0 (notebookButtonBoxInversion#coerce));
(*Ajout dans tableAdvanced des noms d'oréprations*)
ignore (notebookAdvancedTable#attach ~left:0 ~right:2 ~top:0 (notebookAdvancedTitle#coerce));
ignore (notebookAdvancedTable#attach ~left:0 ~top:1 (notebookAdvancedCompression#coerce));
ignore (notebookAdvancedTable#attach ~left:0 ~top:2 (notebookAdvancedSegmentation#coerce));
(*COMPRESSION*)
ignore (notebookAdvancedTable#attach ~left:1 ~top:1 (notebookAlignTableCompression#coerce));
ignore (notebookTableCompression#attach ~left:0 ~top:0  (notebookButtonBoxCompression#coerce));;
ignore (notebookAdvancedTable#attach ~left:1 ~top:2 (notebookAlignTableSegmentation#coerce));
ignore (notebookTableSegmentation#attach ~left:0 ~top:0  (notebookButtonBoxSegmentation#coerce));;
ignore (notebookTableSave#attach ~left:0  ~top:0 (notebookSaveTitle#coerce));
ignore (notebookTableSave#attach ~left:0 ~top:1 (notebookTableAlignSave#coerce));
ignore (firstPageQuitButton#connect#clicked ~callback:GMain.quit);;

(*View2*)
let viewImageAfficheSecond = GMisc.image ~file: "test.ppm" ();;

leftToolbarSecondView#insert_widget ~tooltip:"Info" tableLeftToolbar#coerce;;
ignore (tableLeftToolbar#attach ~left:0 ~top:0 (titreInfo#coerce));
ignore (tableLeftToolbar#attach ~left:0  ~top:1 (buttonBoxLeftToolbar#coerce));;
ignore (tableLeftToolbar#attach ~left:0  ~top:2 (tableInfo#coerce));;
ignore (tableInfo#attach ~left:0 ~right:2 ~top:0 (titreFichierInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:1 (nomInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:2 (dimInfo#coerce));
ignore (tableInfo#attach ~left:0 ~top:3 (moyInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:1 (nomFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:2 (dimFichInfo#coerce));
ignore (tableInfo#attach ~left:1 ~top:3 (moyFichInfo#coerce));;
ignore (tableImageView#attach ~left:0  ~top:0 (viewImageAfficheSecond#coerce));;
ignore (rightToolbarSecondView#insert_widget ~tooltip:"Right Toolbar" (tableRightToolbar#coerce));
ignore (tableRightToolbar#attach ~left:0 ~top:0 (alignTableRotToolbar#coerce));;
ignore (tableRot#attach ~left:0 ~right:2 ~top:0 (toolbarNameRotation#coerce));;
ignore (tableRot#attach ~left:0 ~top:1 (bboxleftRot#coerce));;
ignore (tableRot#attach ~left:1 ~top:1 (bboxrightRot#coerce));;
ignore (tableRightToolbar#attach ~left:0 ~top:1 (alignTableMiroir#coerce));;
ignore (tableMir#attach ~left:0 ~right:3 ~top:0 (toolbarNameMiroir#coerce));;
ignore (tableMir#attach ~left:1 ~top:1 (bboxMiroirUp#coerce));;
ignore (tableMir#attach ~left:2 ~top:2 (bboxMiroirRight#coerce));;
ignore (tableMir#attach ~left:1 ~top:3 (bboxMiroirDown#coerce));;
ignore (tableMir#attach ~left:0 ~top:2 (bboxMiroirLeft#coerce));;
ignore (tableRightToolbar#attach ~left:0 ~top:2 (alignTableInversion#coerce));
ignore (tableInv#attach ~left:0 ~top:0 (bboxInversion#coerce));;
ignore (tableRightToolbar#attach ~left:0 ~top:3 (alignTableCompression#coerce));
ignore (tableCompression#attach ~left:0 ~top:0  (bboxCompression#coerce));;
ignore (tableRightToolbar#attach ~left:0 ~top:4 (alignTableSegmentation#coerce));
ignore (tableSegmentation#attach ~left:0 ~top:0  (bboxSegmentation#coerce));;
ignore (tableRightToolbar#attach ~left:0 ~top:5 (alignTableSave#coerce));
ignore (tableSave#attach ~left:0 ~top:0  (savebbox#coerce));






ignore (confirmButtonConfirmer#connect#clicked (fun () -> ignore (alignConfirmation#misc#hide ()); ignore (thirdPageHBoxView1#misc#show ()); ignore (thridPageButtonBox#misc#show ());ignore (notebook#goto_page 0);ignore (topToolbarViewsItem#misc#show ())));;
ignore (thridPageButtonReturn#connect#clicked (fun () -> ignore (thirdPageHBoxView1#misc#hide ());ignore (thirdPageHBoxView2#misc#hide ()); ignore (thridPageButtonBox#misc#hide ());ignore (topToolbarViewsItem#misc#hide ()); ignore (firstPageTitle#misc#show ());ignore (firstPageButtonBox#misc#show ());));;

ignore (topToolbarRadioView1#connect#toggled (fun () -> ignore (thirdPageHBoxView1#misc#show ());ignore (thirdPageHBoxView2#misc#hide ())));;
ignore (topToolbarRadioView2#connect#toggled (fun () -> ignore (thirdPageHBoxView1#misc#hide ());ignore (thirdPageHBoxView2#misc#show ())));;

let action_buttonSave =
	let dlgSave = GWindow.file_chooser_dialog
		~action:`OPEN
		~parent:window
		~position:`CENTER_ON_PARENT
		~destroy_with_parent:true () in
	dlgSave#add_button_stock `CANCEL `CANCEL;
	dlgSave#add_select_button_stock `SAVE `SAVE;
	let btn = GButton.button ~stock:`SAVE ~packing:notebookSaveButtonBox#add () in
	 GMisc.image ~stock:`SAVE ~packing:btn#set_image ();
	btn#connect#clicked (fun () -> if dlgSave#run () = `OPEN then Gaux.may (Aux.save) dlgSave#filename;
	dlgSave#misc#hide ());
	btn

(*Affichage de fenetre*)
let _ =
	ignore (window#event#connect#delete quitConfirmationPopup);
	window#show ();
	GMain.main ()
