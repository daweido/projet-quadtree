(*Compilation Interface+Fichier externe*)
(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg str.cma v2.ml quad.ml -o quadtree*)

open GMain
open GdkKeysyms

let _ = GMain.init ()
let fileGlobalPath = ref ""
let tmpFileGlobalPath = ref ""

let go_find_image =
	let path_to_exec = Sys.executable_name in
	(String.sub path_to_exec 0 (String.length(path_to_exec)-7))^"images/" ;;

(*FONCTIONS DE BASES*)
let viewImageAfficheFirst = GMisc.image ();;
let viewImageAfficheSecond = GMisc.image ();;
let notebookInfoName = GMisc.label ();;
let nomFichInfo = GMisc.label ();;
let dimFichInfoViewSecond = GMisc.label ();;
let notebookInfoDimensionsValue = GMisc.label ();;
let moyRouge = GMisc.label ();;
let moyVert = GMisc.label ();;
let moyBleu = GMisc.label ();;
let notebookInfoMeanRouge = GMisc.label ();;
let notebookInfoMeanVert = GMisc.label ();;
let notebookInfoMeanBleu = GMisc.label ();;


let string_dimensions_info dimX dimY =
	dimX^" x "^dimY;;



let ignore_apply f obj = ignore (f obj)
(*MAIN CONTAINERS*)
(*Window*)
let window = GWindow.window
	~width:900
	~height:700
	~position:`CENTER
	(*~resizable:false*)
	~title:"PPMShop" ();;

window#connect#destroy GMain.quit;;


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
let tmpNameFile filePath =
	let filePathNoExtension = String.sub filePath 0 ((String.length filePath)-4) in
	filePathNoExtension^"_tmp.ppm";;

(*GENERAL I/O*)
module Aux =
struct
	let file_name filePath = Filename.basename filePath

	let buffer_size = 8192

	let buffer = String.create buffer_size

	let file_copy input_name output_name =
		let file_in = open_in input_name in
		let file_out = open_out output_name in
		let channel_length = in_channel_length file_in in
		let buf = Buffer.create channel_length in
		Buffer.add_channel buf file_in channel_length;
		close_in file_in;
		output_string file_out (Buffer.contents buf);
		close_out file_out;;

	let moyRVB (_,_,_,_,l) =
		let moyenne l = (Fonctions.moy l) in
		let moyRed = string_of_int ((Fonctions.sommex l 0)/(List.length l)) in
		let moyGreen = string_of_int ((Fonctions.sommey l 0)/(List.length l)) in
		let moyBlue = string_of_int ((Fonctions.sommez l 0)/(List.length l)) in
		moyRouge#set_label (moyRed);
		moyVert#set_label (moyGreen);
		moyBleu#set_label (moyBlue);
		notebookInfoMeanRouge#set_label (moyRed);
		notebookInfoMeanVert#set_label (moyGreen);
		notebookInfoMeanBleu#set_label (moyBlue);;

	let verifppm filePath =
		let fileString path = match path with
			None -> ""
			|Some s -> s in
		let lecture_fichier sPath = Fonctions.lec sPath in
		let dimXFichier (_,x,_,_,_) = x in
		let dimYFichier (_,_,y,_,_) = y in
		let typePPM (type_image,_,_,_,_) = type_image in
		let maxValCouleurs (_,_,_,maxVal,_) = maxVal in
		if ((dimXFichier (lecture_fichier (fileString filePath))) = (dimYFichier (lecture_fichier (fileString filePath)))) &&
			(typePPM (lecture_fichier (fileString filePath)) = "P3") && ((int_of_string (maxValCouleurs (lecture_fichier (fileString filePath)))) <= 255)
			&& ((int_of_string (maxValCouleurs (lecture_fichier (fileString filePath)))) >= 1)
			then true else false;;

	let loadGeneral filePath =
		let lecture_fichier = Fonctions.lec filePath in
		let dimXFichier (_,x,_,_,_) = x in
		let dimYFichier (_,_,y,_,_) = y in
		fileGlobalPath := filePath;
		tmpFileGlobalPath := tmpNameFile !fileGlobalPath;
		print_endline (!fileGlobalPath); (*A Enlever*)
		print_endline (!tmpFileGlobalPath); (*A Enlever*)
		Sys.chdir (Filename.dirname filePath);
		file_copy (!fileGlobalPath) (!tmpFileGlobalPath);
		viewImageAfficheFirst#set_file filePath;
		viewImageAfficheSecond#set_file filePath;
		moyRVB lecture_fichier;
		notebookInfoName#set_label (file_name filePath); (*FileName*)
		nomFichInfo#set_label (file_name filePath); (*FileName*)
		dimFichInfoViewSecond#set_label (string_dimensions_info (dimXFichier lecture_fichier) (dimYFichier lecture_fichier));
		notebookInfoDimensionsValue#set_label (string_dimensions_info (dimXFichier lecture_fichier) (dimYFichier lecture_fichier));
		at_exit (fun _ -> (Sys.remove (!tmpFileGlobalPath)))



	let ajoutExt filepath =
		let length = String.length filepath in
		let threeLast nfilepath = String.sub nfilepath (length-4) 4 in
		if (threeLast filepath) = ".ppm" then filepath else filepath^".ppm"

	let saveAsGeneral newFilePath =
		let lecture_fichier = Fonctions.lec newFilePath in
		let newFilePathAfterVerif = ajoutExt newFilePath in
		file_copy (!tmpFileGlobalPath) newFilePathAfterVerif;
		Sys.remove (!tmpFileGlobalPath);
		fileGlobalPath := newFilePathAfterVerif;
		tmpFileGlobalPath := (tmpNameFile !fileGlobalPath);
		Sys.chdir (Filename.dirname newFilePathAfterVerif);
		file_copy (!fileGlobalPath) (!tmpFileGlobalPath);
		viewImageAfficheFirst#set_file (!fileGlobalPath);
		moyRVB lecture_fichier;
		viewImageAfficheSecond#set_file (!fileGlobalPath);
		notebookInfoName#set_label (file_name (!fileGlobalPath));
		nomFichInfo#set_label (file_name (!fileGlobalPath));
		at_exit (fun _ -> (Sys.remove (!tmpFileGlobalPath)))

	let saveGeneral filePath tmpFilePath =
		let lecture_fichier = Fonctions.lec filePath in
		Sys.remove (filePath);
		Sys.rename (tmpFilePath) (file_name filePath);
		file_copy (filePath) (tmpFilePath);
		moyRVB lecture_fichier;
		viewImageAfficheFirst#set_file filePath;
		viewImageAfficheSecond#set_file filePath

	(*let loadManipulationUse file =
		Fonctions.arbre(Fonctions.lec file)


	let saveManipulationUse file =
		let och = open_out file in
		output_string och ("TEST VERIFICATION OUTPUT"); (*OVERWRITE*)
		close_out och*)
end

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
	~yalign:0.2
	~xscale:0.0
	~yscale:0.0
	~packing:notebookHBoxInfo#add ()

let notebookTableInfo = GPack.table
	~rows:7
	~columns:2
	~row_spacings:20
	~col_spacings:15
	~homogeneous:true
	~packing: notebookAlignInfo#add ()

(*3rd Tab*)
let notebookTabImageViewLabel = GMisc.label ~text:("Image View") ()

let notebookHBoxImageView = GPack.hbox
	~packing:(ignore_apply (notebook#append_page ~tab_label:notebookTabImageViewLabel#coerce)) ()

let notebookAlignImageView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:notebookHBoxImageView#add ()

let notebookTableImageView = GPack.table
	~rows:1
	~columns:1
	~packing: notebookAlignImageView#add ()

(*4th Tab*)
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
	~rows:1
	~columns:2
	~homogeneous:true
	~packing:notebookTableMirorAlign#add ();;

let notebookButtonBoxMirorUpDown = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:65 ();;

let notebookButtonBoxMirorRightLeft = GPack.button_box `HORIZONTAL
	~child_width: 65
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

(*5th Tab*)
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

(*View2*)
let leftToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(thirdPageHBoxView2#pack ~expand:false) ()

let tableLeftToolbarSecondView = GPack.table
	~rows:2
	~columns:1
	~border_width:5
	~row_spacings:10 ();;

let tableInfoSecondView = GPack.table
	~rows:5
	~columns:2
	~row_spacings:10
	~col_spacings:1
	~border_width:2
	~homogeneous:true ();;


(*ViewImage*)
let alignImageViewSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:thirdPageHBoxView2#add ()

let tableImageViewSecondView = GPack.table
	~rows:1
	~columns:1
	~packing: alignImageViewSecondView#add ()


(*RightToolbar*)
let rightToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(thirdPageHBoxView2#pack ~expand:false) ()

let tableRightToolbarSecondView = GPack.table
	~rows:5
	~columns:1
	~row_spacings:50
	~border_width:5 ();;

let alignTableRotToolbarSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableRotSecondView = GPack.table
	~rows:2
	~columns:2
	~homogeneous:true
	~packing: alignTableRotToolbarSecondView#add ()

let bboxLeftRotSecondView = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;

let bboxRightRotSecondView = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;

let alignTableMiroirSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableMirSecondView = GPack.table
	~rows:3
	~columns:1
	~row_spacings:10
	~packing:alignTableMiroirSecondView#add ();;


let alignbboxMiroirUpDownSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let bboxMiroirUpDownSecondView = GPack.button_box `HORIZONTAL
	~child_width:10
	~child_height:40
	~packing:alignbboxMiroirUpDownSecondView#add ();;


let alignbboxMiroirRightLeftSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let bboxMiroirRightLeftSecondView = GPack.button_box `HORIZONTAL
	~child_width:40
	~child_height:10
	~packing:alignbboxMiroirRightLeftSecondView#add ();;


let alignTableInversionSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableInvSecondView = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableInversionSecondView#add ()

let bboxInversionSecondView = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;

let alignTableCompressionSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableCompressionSecondView = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableCompressionSecondView#add ()

let bboxCompressionSecondView = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;


let alignTableSegmentationSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

let tableSegmentationSecondView = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableSegmentationSecondView#add ()

let bboxSegmentationSecondView = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;

(*BUTTONS AND WIDNOWS*)
(*Top Toolbar*)
let topToolbarRadioView1 = GButton.radio_button
	~label:"View 1"
	~active:true ()

let topToolbarRadioView2 = GButton.radio_button
	~label:"View 2"
	~group:topToolbarRadioView1#group ()

let topToolbarAbout = GButton.tool_button ~stock: `ABOUT ~packing:topToolbar#insert ()
let topToolbarFirstSeparator = GButton.separator_tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarReturnButton = GButton.tool_button ~stock:`GO_BACK ~show:false ~packing:topToolbar#insert ()
let topToolbarSecondSeparator = GButton.separator_tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarSave = GButton.tool_button ~stock:`SAVE ~show:false ~packing:topToolbar#insert ()
let topToolbarSaveAs = GButton.tool_button ~stock:`SAVE_AS ~show:false ~packing:topToolbar#insert ()
let topToolbarThirdSeparator = GButton.separator_tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarViewsItem = GButton.tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarLastSeparator = GButton.separator_tool_item ~packing:topToolbar#insert ()
let topToolbarQuit = GButton.tool_button ~stock: `QUIT ~packing:topToolbar#insert ()



let topToolbarAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:topToolbarViewsItem#add ();;

let topToolbarTable = GPack.table
	~rows:1
	~columns:3
	~col_spacings:20
	~packing:topToolbarAlign#add();;



let openImageFilter () = GFile.filter
    ~name:"PPM Files"
    ~patterns:[ "*.ppm" ] ()

let saveImageFilter () = GFile.filter
	~name:"PPM Files"
	~patterns:[ "*.ppm" ] ()

let openAll_files () =
	let filt = GFile.filter ~name:"All Files" () in
	filt#add_pattern "*" ;
	filt

let saveAll_files () =
	let filt = GFile.filter ~name:"All Files" () in
	filt#add_pattern "*" ;
	filt

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

(*View 1*)
let create_arrow_button ~kind ~shadow ~packing () =
	let button = GButton.button ~packing () in
	let arrow = GMisc.arrow ~kind ~shadow ~packing:button#add () in
	button

let notebookArrowLeftRot = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:notebookButtonBoxLeftRot#add ()
let notebookArrowRightRot = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:notebookButtonBoxRightRot#add ()
let notebookArrowMirorUpDown = GButton.button ~packing:notebookButtonBoxMirorUpDown#add ()
let upArrowFirstView = GMisc.arrow ~kind:`UP ~shadow:`IN ()
let downArrowFirstView = GMisc.arrow ~kind:`DOWN ~shadow:`OUT ()

let alignUpDownMirFirstView= GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:notebookArrowMirorUpDown#add ();;

let upDownMirFirstViewTable = GPack.table
	~rows:2
	~columns:1
	~row_spacings:20
	~packing:alignUpDownMirFirstView#add ();;


let notebookArrowMirorRightLeft = GButton.button ~packing:notebookButtonBoxMirorRightLeft#add ()

let leftArrowFirstView = GMisc.arrow ~kind:`LEFT ~shadow:`IN ()
let rightArrowFirstView = GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ()

let alignRightLeftMirFirstView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:notebookArrowMirorRightLeft#add ();;

let rightLeftMirMirFirstViewTable = GPack.table
	~rows:1
	~columns:2
	~col_spacings:20
	~packing:alignRightLeftMirFirstView#add ();;


let notebookButtonInversion = GButton.button ~label: "Inverser" ~packing:notebookButtonBoxInversion#add ();;
let notebookButtonCompression = GButton.button ~label: "Compresser" ~packing:notebookButtonBoxCompression#add ();;
let notebookButtonSegmentation = GButton.button ~label: "Segmenter"  ~packing:notebookButtonBoxSegmentation#add ();;

(*View2*)

let leftRot = create_arrow_button ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:bboxLeftRotSecondView#add ()
let rightRot = create_arrow_button ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:bboxRightRotSecondView#add ()

let upDownMir = GButton.button ~packing:bboxMiroirUpDownSecondView#add ();;

let upArrowSecondView = GMisc.arrow ~kind:`UP ~shadow:`IN ()
let downArrowSecondView = GMisc.arrow ~kind:`DOWN ~shadow:`OUT ()

let alignUpDownMirSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:upDownMir#add ();;

let upDownMirSecondViewTable = GPack.table
	~rows:2
	~columns:1
	~row_spacings:15
	~packing:alignUpDownMirSecondView#add ();;

let rightLeftMir = GButton.button ~packing:bboxMiroirRightLeftSecondView#add ();;
let leftArrowSecondView = GMisc.arrow ~kind:`LEFT ~shadow:`IN ()
let rightArrowSecondView = GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ()

let alignRightLeftMirSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:rightLeftMir#add ();;

let rightLeftMirMirSecondViewTable = GPack.table
	~rows:1
	~columns:2
	~col_spacings:15
	~packing:alignRightLeftMirSecondView#add ();;



let invBUT = GButton.button ~label: "Inverser" ~packing:bboxInversionSecondView#add ();;
let advancedCompressionButton = GButton.button ~label: "Compresser" ~packing:bboxCompressionSecondView#add ();;
let advancedSegmentationButton = GButton.button ~label: "Segmenter"  ~packing:bboxSegmentationSecondView#add ();;

(*LOAD*)
let action_buttonLoad =
	let dlgLoad = GWindow.file_chooser_dialog
		~action:`OPEN
		~parent:window
		~position:`CENTER_ON_PARENT
		~destroy_with_parent:true () in
	let dialog = GWindow.message_dialog
		~title:"Erreur"
		~message_type:`ERROR
		~parent:dlgLoad
		~width:300
		~position:`CENTER_ON_PARENT
		~destroy_with_parent:true
		~message:"<b>Image non supportée</b>\nNous vous rappelons que l'image doit être carré et de format P3.\nVeuillez bien vouloir réessayer"
		~use_markup:true
		~buttons:GWindow.Buttons.ok () in

	dlgLoad#add_button_stock `CANCEL `CANCEL;
	dlgLoad#add_select_button_stock `OPEN `OPEN;
	dlgLoad#add_filter (openImageFilter ());
	dlgLoad#add_filter (openAll_files ());
	let btn = GButton.button ~stock:`OPEN ~packing:firstPageButtonBox#add () in
	GMisc.image ~stock:`OPEN ~packing:btn#set_image ();
	btn#connect#clicked (fun () -> if dlgLoad#run () = `OPEN then (if (Aux.verifppm (dlgLoad#filename)) then (Gaux.may (Aux.loadGeneral) dlgLoad#filename;
	dlgLoad#misc#hide ();firstPageButtonBox#misc#hide ();firstPageTitle#misc#hide ();ignore (thirdPageHBoxView1#misc#show ());
	ignore (notebook#goto_page 0);ignore (topToolbarViewsItem#misc#show ());ignore (topToolbarSaveAs#misc#show ());ignore (topToolbarSave#misc#show ());ignore (topToolbarFirstSeparator#misc#show ());
	ignore (topToolbarSecondSeparator#misc#show ());ignore (topToolbarReturnButton#misc#show ()))
	else
		let d = dialog#run () in
		if (d = `DELETE_EVENT) || (d =  `OK ) then dialog#misc#hide (); dlgLoad#misc#hide ())
 	else dlgLoad#misc#hide ());
	btn

let firstPageQuitButton = GButton.button
				~stock:`QUIT
				~packing:firstPageButtonBox#add ();;

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

(*Label du tableau Info*)
let notebookInfoTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Informations du fichier</b></span>" ();;
let notebookInfoNameLabel = GMisc.label ~markup: "<b><big>Nom de l'image :</big></b>" ();;
let notebookInfoDimensions = GMisc.label ~markup: "<b>Dimensions de l'image :</b>" ();;
let notebookInfoMean = GMisc.label ~markup: "<b>Moyenne des couleurs</b>" ();;
let notebookInfoRougeTitre = GMisc.label ~markup: "<b>Moyenne des rouges : </b>" ();;
let notebookInfoVertTitre = GMisc.label ~markup: "<b>Moyenne des verts :</b>" ();;
let notebookInfoBlueuTitre = GMisc.label ~markup: "<b>Moyenne des blueus :</b>" ();;

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

(*View2*)
let titreInfo = GMisc.label ~markup: "<b>Informations</b>" ();;
let nomInfo = GMisc.label ~markup: "<b>Nom :</b>" ();;
let dimInfo = GMisc.label ~markup: "<b>Dimensions :</b>" ();;
let moyInfo = GMisc.label ~markup: "<b>Moyenne</b>" ();;
let moyRougeTitre = GMisc.label ~markup: "<b>Rouge :</b>" ();;
let moyVertTitre = GMisc.label ~markup: "<b>Vert :</b>" ();;
let moyBleuTitre = GMisc.label ~markup: "<b>Blueu :</b>" ();;
let toolbarNameRotation = GMisc.label ~markup: "<b>Rotation</b>" ();;
let toolbarNameMiroir = GMisc.label ~markup: "<b>Miroir</b>" ();;





(*ACTIONS & FUNCTIONS*)
(*ABOUT WINDOW*)
ignore (topToolbarAbout#connect#clicked (fun () -> ignore (about_button#run ()); ignore (about_button#misc#hide ())));;

topToolbarQuit#connect#clicked GMain.quit;;

ignore (topToolbarTable#attach ~left:0 ~top:0 (topToolbarRadioView1#coerce));
ignore (topToolbarTable#attach ~left:1 ~top:0 (topToolbarRadioView2#coerce));

ignore (firstPageQuitButton#connect#clicked ~callback:GMain.quit);


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
ignore (notebookTableHome#attach ~left:1 ~top:11 (notebookHomeTextSave#coerce));
(*Ajout dans noms dans tableau info*)
ignore (notebookTableInfo#attach ~left:0 ~right:2 ~top:0 (notebookInfoTitle#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:1 (notebookInfoNameLabel#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:2 (notebookInfoDimensions#coerce));
ignore (notebookTableInfo#attach ~left:0 ~right:2 ~top:3 (notebookInfoMean#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:4 (notebookInfoRougeTitre#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:5 (notebookInfoVertTitre#coerce));
ignore (notebookTableInfo#attach ~left:0 ~top:6 (notebookInfoBlueuTitre#coerce));

(*Ajout infos dans tableau*)
ignore (notebookTableInfo#attach ~left:1 ~top:1 (notebookInfoName#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:2 (notebookInfoDimensionsValue#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:4 (notebookInfoMeanRouge#coerce));;
ignore (notebookTableInfo#attach ~left:1 ~top:5 (notebookInfoMeanVert#coerce));;
ignore (notebookTableInfo#attach ~left:1 ~top:6 (notebookInfoMeanBleu#coerce));;

(*4th TAB - IMAGE VIEW*)
ignore (notebookTableImageView#attach ~left:0 ~top:0 (viewImageAfficheFirst#coerce));;

(*Ajout dans tableSimple des noms d'oréprations*)
ignore (notebookTableSimple#attach ~left:0 ~right:2 ~top:0 (notebookSimpleTitle#coerce));
ignore (notebookTableSimple#attach ~left:0 ~top:1 (notebookSimpleRotation#coerce));
ignore (notebookTableSimple#attach ~left:0 ~top:2 (notebookSimpleMiror#coerce));
ignore (notebookTableSimple#attach ~left:0 ~top:3 (notebookSimpleInversion#coerce));
ignore (notebookTableSimple#attach ~left:1 ~top:1 (notebookTableRotAlign#coerce));
ignore (notebookTableRot#attach ~left:0 ~top:0 (notebookButtonBoxLeftRot#coerce));
ignore (notebookTableRot#attach ~left:1 ~top:0 (notebookButtonBoxRightRot#coerce));
ignore (notebookTableSimple#attach ~left:1 ~top:2 (notebookTableMirorAlign#coerce));

ignore (notebookTableMiror#attach ~left:0 ~top:0 (notebookButtonBoxMirorUpDown#coerce));
ignore (notebookTableMiror#attach ~left:1 ~top:0 (notebookButtonBoxMirorRightLeft#coerce));

ignore (upDownMirFirstViewTable#attach ~left:0 ~top:0 (upArrowFirstView#coerce));;
ignore (upDownMirFirstViewTable#attach ~left:0 ~top:1 (downArrowFirstView#coerce));;

ignore (rightLeftMirMirFirstViewTable#attach ~left:0 ~top:0 (leftArrowFirstView#coerce));;
ignore (rightLeftMirMirFirstViewTable#attach ~left:1 ~top:0 (rightArrowFirstView#coerce));;


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
ignore (firstPageQuitButton#connect#clicked ~callback:GMain.quit);;

(*View2*)
ignore (tableImageViewSecondView#attach ~left:0  ~top:0 (viewImageAfficheSecond#coerce));;
leftToolbarSecondView#insert_widget ~tooltip:"Info" tableLeftToolbarSecondView#coerce;;
ignore (tableLeftToolbarSecondView#attach ~left:0 ~top:0 (titreInfo#coerce));
ignore (tableLeftToolbarSecondView#attach ~left:0  ~top:1 (tableInfoSecondView#coerce));;
ignore (tableInfoSecondView#attach ~left:0 ~top:0 (nomInfo#coerce));
ignore (tableInfoSecondView#attach ~left:0 ~top:1 (dimInfo#coerce));
ignore (tableInfoSecondView#attach ~left:0 ~right:2 ~top:2 (moyInfo#coerce));
ignore (tableInfoSecondView#attach ~left:0 ~top:3 (moyRougeTitre#coerce));
ignore (tableInfoSecondView#attach ~left:0 ~top:4 (moyVertTitre#coerce));
ignore (tableInfoSecondView#attach ~left:0 ~top:5 (moyBleuTitre#coerce));

ignore (tableInfoSecondView#attach ~left:1 ~top:0 (nomFichInfo#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:1 (dimFichInfoViewSecond#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:3 (moyRouge#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:4 (moyVert#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:5 (moyBleu#coerce));;

ignore (rightToolbarSecondView#insert_widget ~tooltip:"Right Toolbar" (tableRightToolbarSecondView#coerce));
ignore (tableRightToolbarSecondView#attach ~left:0 ~top:0 (alignTableRotToolbarSecondView#coerce));;
ignore (tableRotSecondView#attach ~left:0 ~right:2 ~top:0 (toolbarNameRotation#coerce));;
ignore (tableRotSecondView#attach ~left:0 ~top:1 (bboxLeftRotSecondView#coerce));;
ignore (tableRotSecondView#attach ~left:1 ~top:1 (bboxRightRotSecondView#coerce));;
ignore (tableRightToolbarSecondView#attach ~left:0 ~top:1 (alignTableMiroirSecondView#coerce));;
ignore (tableMirSecondView#attach ~left:0 ~right:2 ~top:0 (toolbarNameMiroir#coerce));;
ignore (tableMirSecondView#attach ~left:0 ~top:1 (alignbboxMiroirUpDownSecondView#coerce));;
ignore (tableMirSecondView#attach ~left:0 ~top:2 (alignbboxMiroirRightLeftSecondView#coerce));;

ignore (upDownMirSecondViewTable#attach ~left:0 ~top:0 (upArrowSecondView#coerce));;
ignore (upDownMirSecondViewTable#attach ~left:0 ~top:1 (downArrowSecondView#coerce));;

ignore (rightLeftMirMirSecondViewTable#attach ~left:0 ~top:0 (leftArrowSecondView#coerce));;
ignore (rightLeftMirMirSecondViewTable#attach ~left:1 ~top:0 (rightArrowSecondView#coerce));;

ignore (tableRightToolbarSecondView#attach ~left:0 ~top:2 (alignTableInversionSecondView#coerce));
ignore (tableInvSecondView#attach ~left:0 ~top:0 (bboxInversionSecondView#coerce));;
ignore (tableRightToolbarSecondView#attach ~left:0 ~top:3 (alignTableCompressionSecondView#coerce));
ignore (tableCompressionSecondView#attach ~left:0 ~top:0  (bboxCompressionSecondView#coerce));;
ignore (tableRightToolbarSecondView#attach ~left:0 ~top:4 (alignTableSegmentationSecondView#coerce));
ignore (tableSegmentationSecondView#attach ~left:0 ~top:0  (bboxSegmentationSecondView#coerce));;

ignore (topToolbarReturnButton#connect#clicked (fun () -> ignore (thirdPageHBoxView1#misc#hide ());ignore (thirdPageHBoxView2#misc#hide ());Sys.remove (!tmpFileGlobalPath);
ignore (topToolbarViewsItem#misc#hide ());ignore (topToolbarSaveAs#misc#hide ());ignore (topToolbarSave#misc#hide ());ignore (topToolbarSecondSeparator#misc#hide ());ignore (topToolbarThirdSeparator#misc#hide ());
ignore (topToolbarFirstSeparator#misc#hide ()); ignore (firstPageTitle#misc#show ()); ignore (firstPageButtonBox#misc#show ());ignore (topToolbarReturnButton#misc#hide ())));;

ignore (topToolbarRadioView1#connect#toggled (fun () -> ignore (thirdPageHBoxView1#misc#show ());ignore (thirdPageHBoxView2#misc#hide ())));;
ignore (topToolbarRadioView2#connect#toggled (fun () -> ignore (thirdPageHBoxView1#misc#hide ());ignore (thirdPageHBoxView2#misc#show ())));;


let saveAsButton =
	let dlgSave = GWindow.file_chooser_dialog
		~action:`SAVE
		~title:"SAUVEGARDE DIALOG"
		~parent:window
		~position:`CENTER_ON_PARENT
		~width:450
		~height:350
		~destroy_with_parent:true () in
	dlgSave#add_button_stock `CANCEL `CANCEL;
	dlgSave#add_select_button_stock `SAVE_AS `SAVE_AS;
	dlgSave#do_overwrite_confirmation;
	dlgSave#add_filter (saveImageFilter ());
	dlgSave#add_filter (saveAll_files ());
	topToolbarSaveAs#connect#clicked (fun () -> if dlgSave#run () = `SAVE_AS then Gaux.may (Aux.saveAsGeneral) dlgSave#filename;
	dlgSave#misc#hide ());


ignore (topToolbarSave#connect#clicked ~callback: (fun _ -> Aux.saveGeneral (!fileGlobalPath) (!tmpFileGlobalPath)));;


(*Affichage de fenetre*)
let _ =
	window#show ();
	GMain.main ()
