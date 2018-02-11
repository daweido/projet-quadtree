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

let tmpNameFile filePath =
	let filePathNoExtension = String.sub filePath 0 ((String.length filePath)-4) in
	filePathNoExtension^"_tmp.ppm";;

(*GENERAL I/O*)
module Aux =
struct
	let file_name filePath = Filename.basename filePath

	let buffer_size = 8192

	let buffer = Bytes.create buffer_size

	let file_copy input_name output_name =
		let file_in = open_in input_name in
		let file_out = open_out output_name in
		let channel_length = in_channel_length file_in in
		let buf = Buffer.create channel_length in
		Buffer.add_channel buf file_in channel_length;
		close_in file_in;
		output_string file_out (Buffer.contents buf);
		close_out file_out;;

	let moyRVB l =
		let moyRed = string_of_int ((Fonctions.sommex l 0)/(List.length l)) in
		let moyGreen = string_of_int ((Fonctions.sommey l 0)/(List.length l)) in
		let moyBlue = string_of_int ((Fonctions.sommez l 0)/(List.length l)) in
		moyRouge#set_label (moyRed);
		moyVert#set_label (moyGreen);
		moyBleu#set_label (moyBlue);
		notebookInfoMeanRouge#set_label (moyRed);
		notebookInfoMeanVert#set_label (moyGreen);
		notebookInfoMeanBleu#set_label (moyBlue);;

	let lecture_fichier path = Fonctions.lec path

	let arb (_,_,_,_,a) = a

	let verifppm filePath =
		let fileString path = match path with
			None -> ""
			|Some s -> s in
		let verifDim x y = if x = y then true else false in
		let verifType p = if p = "P3" then true else false in
		let verifMaxVal maxVal = if (maxVal >= 1) && (maxVal <= 255) then true else false in
		let verifTotal (type_image,x,y,maxVal,_) =
			if (verifDim x y) && (verifType type_image) && (verifMaxVal (int_of_string maxVal)) then true else false in
		verifTotal (lecture_fichier (fileString filePath));;

	let loadGeneral filePath =
		let dimMoy (_,x,y,_,l) =
			moyRVB l;
			dimFichInfoViewSecond#set_label (string_dimensions_info x y);
			notebookInfoDimensionsValue#set_label (string_dimensions_info x y) in
		fileGlobalPath := filePath;
		tmpFileGlobalPath := tmpNameFile !fileGlobalPath;
		print_endline (!fileGlobalPath); (*A Enlever*)
		print_endline (!tmpFileGlobalPath); (*A Enlever*)
		Sys.chdir (Filename.dirname filePath);
		file_copy (!fileGlobalPath) (!tmpFileGlobalPath);
		viewImageAfficheFirst#set_file filePath;
		viewImageAfficheSecond#set_file filePath;
		dimMoy (lecture_fichier filePath);
		notebookInfoName#set_label (file_name filePath); (*FileName*)
		nomFichInfo#set_label (file_name filePath); (*FileName*)
		at_exit (fun _ -> (Sys.remove (!tmpFileGlobalPath)))

	let ajoutExt filepath =
		let length = String.length filepath in
		let threeLast nfilepath = String.sub nfilepath (length-4) 4 in
		if (threeLast filepath) = ".ppm" then filepath else filepath^".ppm"

	let saveAsGeneral newFilePath =
		let newFilePathAfterVerif = ajoutExt newFilePath in
		file_copy (!tmpFileGlobalPath) newFilePathAfterVerif;
		Sys.remove (!tmpFileGlobalPath);
		fileGlobalPath := newFilePathAfterVerif;
		tmpFileGlobalPath := (tmpNameFile !fileGlobalPath);
		Sys.chdir (Filename.dirname newFilePathAfterVerif);
		file_copy (!fileGlobalPath) (!tmpFileGlobalPath);
		viewImageAfficheFirst#set_file (!fileGlobalPath);
		moyRVB (arb (lecture_fichier !fileGlobalPath));
		viewImageAfficheSecond#set_file (!fileGlobalPath);
		notebookInfoName#set_label (file_name (!fileGlobalPath));
		nomFichInfo#set_label (file_name (!fileGlobalPath));
		at_exit (fun _ -> (Sys.remove (!tmpFileGlobalPath)))

	let saveGeneral filePath tmpFilePath =
		Sys.remove (filePath);
		Sys.rename (tmpFilePath) (file_name filePath);
		file_copy (filePath) (tmpFilePath);
		moyRVB (arb (lecture_fichier filePath));
		viewImageAfficheFirst#set_file filePath;
		viewImageAfficheSecond#set_file filePath


	(*let saveManipulationUse file =
		let och = open_out file in
		output_string och ("TEST VERIFICATION OUTPUT"); (*OVERWRITE*)
		close_out och*)

	(*let buttonsFonctionalit filePath func =
		let lectArbre (_,_,_,_,ar) = Fonctions.arbre ar in
		let type_image (t,_,_,_,_) = t in
		let dimX (_,x,_,_,_) = x in
		let dimY (_,_,y,_,_) = y in
		let maxVal (_,_,_,max,_) = max in
		Fonctions.write_file filePath (Fonctions.listfin (func (lectArbre (lecture_fichier filePath))));
		viewImageAfficheFirst#set_file filePath;
		viewImageAfficheSecond#set_file filePath*)


	let buttonsFonctionalit filePath =
		Fonctions.write_file filePath (Fonctions.listfin (lecture_fichier filePath));
		viewImageAfficheFirst#set_file filePath;
		viewImageAfficheSecond#set_file filePath

	(*let buttonsFonctionalitInver filePath  =
		let lectArbre (_,_,_,_,ar) = Fonctions.arbre ar in
		let tupleInvModif (p,x,y,v,ar) = (p,x,y,v,(Fonctions.inversion ar [])) in
		let type_image (t,_,_,_,_) = t in
		let dimX (_,x,_,_,_) = x in
		let dimY (_,_,y,_,_) = y in
		let maxVal (_,_,_,max,_) = max in
		Fonctions.write_file filePath (Fonctions.listfin (lectArbre (tupleInvModif (lecture_fichier filePath))));
		viewImageAfficheFirst#set_file filePath;
		viewImageAfficheSecond#set_file filePath*)
end

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


(*Top Toolbar*)
let topToolbarRadioView1 = GButton.radio_button
	~label:"View 1"
	~active:true ()

let topToolbarRadioView2 = GButton.radio_button
	~label:"View 2"
	~group:topToolbarRadioView1#group ()

let topToolbarAbout = GButton.tool_button ~stock: `ABOUT ~packing:topToolbar#insert ()

let topToolbarFirstSeparator = GButton.separator_tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarReturnButton = GButton.tool_button ~stock:`GO_BACK ~show:false ~packing:topToolbar#insert ();;


let topToolbarSecondSeparator = GButton.separator_tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarSave = GButton.tool_button ~stock:`SAVE ~show:false ~packing:topToolbar#insert ()
let topToolbarSaveAs = GButton.tool_button ~stock:`SAVE_AS ~show:false ~packing:topToolbar#insert ()
let topToolbarThirdSeparator = GButton.separator_tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarViewsItem = GButton.tool_item ~show:false ~packing:topToolbar#insert ()
let topToolbarAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:topToolbarViewsItem#add ()

let topToolbarTable = GPack.table
	~rows:1
	~columns:3
	~col_spacings:20
	~packing:topToolbarAlign#add();;
ignore (topToolbarTable#attach ~left:0 ~top:0 (topToolbarRadioView1#coerce));
ignore (topToolbarTable#attach ~left:1 ~top:0 (topToolbarRadioView2#coerce))



let topToolbarLastSeparator = GButton.separator_tool_item ~packing:topToolbar#insert ()

let topToolbarQuit = GButton.tool_button ~stock: `QUIT ~packing:topToolbar#insert ();;
topToolbarQuit#connect#clicked GMain.quit


let about_button = GWindow.about_dialog
	~comments: "PPMShop est une application permettant de manipuler des images en formats .ppm\n
Ce programme a été réalisé dans le cadre d'un projet informatique en deuxième année
à l'EISTI"
	~name: "PPMShop"
	~resizable: false
	~authors:["Interface Graphique : David RIGAUX\nFonctions de Manipulation : Amine et Mehdi DALAA"]
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
	~destroy_with_parent:true ();;

ignore (topToolbarAbout#connect#clicked (fun () -> ignore (about_button#run ()); ignore (about_button#misc#hide ())));;

(************************Troisième Page****************************************)
let thirdPageHBoxView1 = GPack.hbox
	~spacing:10
	~show: false
	~packing: firstVbox#add ()

(********NOTEBOOK********)
let notebook = GPack.notebook
 	~border_width:5
	~homogeneous_tabs: true
	~tab_pos:`LEFT
	~packing:thirdPageHBoxView1#add ();;
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

let notebookHomeTitle = GMisc.label ~markup:"<span font_desc=\"Tahoma 25\"><b>PPMShop</b></span>" ();;

ignore (notebookTableHome#attach ~left:0 ~right:2 ~top:0 (notebookHomeTitle#coerce))

let notebookHomeIntro = GMisc.label
	~text:"Quadtree est un programme qui permet de charger des images en format .ppm et d'appliquer plusieurs opérations sur ces
dernières. En ce qui concerne les opérations on trouve : les opérations simples : Rotation, Mirroir et Inversion de couleurs;
les opérations avancées : Compression et Ségmentation. De plus, quadtree permet de sauvgarder les changements apportés à
l'image."
	~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:0 ~right:2 ~top:1 (notebookHomeIntro#coerce))

let notebookHomeTitleRot = GMisc.label ~markup:"<span font_desc=\"Tahoma 18\"><b>Rotation</b></span>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:2 (notebookHomeTitleRot#coerce))

let notebookHomeTitleRotLeft = GMisc.label ~markup:"<big>Rotation Gauche</big>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:3 (notebookHomeTitleRotLeft#coerce))

let notebookHomeTextRotLeft = GMisc.label
	~text: "Cette opération consiste à effectuer une rotaion de 90 degrés sur l'image vers la gauche." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:3 (notebookHomeTextRotLeft#coerce))

let notebookHomeTitleRotRight = GMisc.label ~markup:"<big>Rotation Droite</big>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:4 (notebookHomeTitleRotRight#coerce))

let notebookHomeTextRotRight = GMisc.label
	~text: "Cette opération consiste à effectuer une rotaion de 90 degrés sur l'image vers la droite." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:4 (notebookHomeTextRotRight#coerce))

let notebookHomeTitleMiror = GMisc.label ~markup:"<span font_desc=\"Tahoma 18\"><b>Miroir</b></span>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:5 (notebookHomeTitleMiror#coerce))

let notebookHomeTitleMirorUpDown = GMisc.label ~markup:"<big>Miroir Haut/Bas</big>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:6 (notebookHomeTitleMirorUpDown#coerce))


let notebookHomeTextMirorUpDown = GMisc.label
	~text: "Cette opération consiste à découper l'image en deux parties symetriques par rapport à un
axe de symétrie horizontale." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:6 (notebookHomeTextMirorUpDown#coerce))

let notebookHomeTitleMirorLeftRight = GMisc.label ~markup:"<big>Miroir Gauche/Droite</big>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:7 (notebookHomeTitleMirorLeftRight#coerce))


let notebookHomeTextMirorLeftRight = GMisc.label
	~text: "Cette opération consiste à découper l'image en deux parties symetriques par rapport à
un axe de symétrie verticale." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:7 (notebookHomeTextMirorLeftRight#coerce))

let notebookHomeTitleInversion = GMisc.label ~markup:"<big>Inversion</big>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:8 (notebookHomeTitleInversion#coerce))

let notebookHomeTextInversion = GMisc.label
	~text: "Cette opération consiste à effectuer une inversion des couleurs de l'image." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:8 (notebookHomeTextInversion#coerce))

let notebookHomeTitleCompression = GMisc.label ~markup:"<big>Compression</big>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:9 (notebookHomeTitleCompression#coerce))

let notebookHomeTextCompression = GMisc.label
	~text: "Cette opération consiste à effectuer une compression de l'image." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:9 (notebookHomeTextCompression#coerce))

let notebookHomeTitleSegmentation = GMisc.label ~markup:"<big>Segmentation</big>" ~justify:`LEFT ();;
ignore (notebookTableHome#attach ~left:0 ~top:10 (notebookHomeTitleSegmentation#coerce))

let notebookHomeTextSegmentation = GMisc.label
	~text: "Cette opération consiste à effectuer une segmentation de l'image." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:10 (notebookHomeTextSegmentation#coerce))

let notebookHomeTextSave = GMisc.label
	~text: "Cette opération consiste à effectuer un enregistrement-sous de l'image." ~justify:`CENTER ();;
ignore (notebookTableHome#attach ~left:1 ~top:11 (notebookHomeTextSave#coerce))

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

let notebookInfoTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Informations du fichier</b></span>" ();;
ignore (notebookTableInfo#attach ~left:0 ~right:2 ~top:0 (notebookInfoTitle#coerce))

let notebookInfoNameLabel = GMisc.label ~markup: "<b><big>Nom de l'image :</big></b>" ();;
ignore (notebookTableInfo#attach ~left:0 ~top:1 (notebookInfoNameLabel#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:1 (notebookInfoName#coerce))

let notebookInfoDimensions = GMisc.label ~markup: "<b>Dimensions de l'image :</b>" ();;
ignore (notebookTableInfo#attach ~left:0 ~top:2 (notebookInfoDimensions#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:2 (notebookInfoDimensionsValue#coerce))

let notebookInfoMean = GMisc.label ~markup: "<b>Moyenne des couleurs</b>" ();;
ignore (notebookTableInfo#attach ~left:0 ~right:2 ~top:3 (notebookInfoMean#coerce))

let notebookInfoRougeTitre = GMisc.label ~markup: "<b>Moyenne des rouges : </b>" ();;
ignore (notebookTableInfo#attach ~left:0 ~top:4 (notebookInfoRougeTitre#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:4 (notebookInfoMeanRouge#coerce))

let notebookInfoVertTitre = GMisc.label ~markup: "<b>Moyenne des verts :</b>" ();;
ignore (notebookTableInfo#attach ~left:0 ~top:5 (notebookInfoVertTitre#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:5 (notebookInfoMeanVert#coerce))

let notebookInfoBlueuTitre = GMisc.label ~markup: "<b>Moyenne des blueus :</b>" ();;
ignore (notebookTableInfo#attach ~left:0 ~top:6 (notebookInfoBlueuTitre#coerce));
ignore (notebookTableInfo#attach ~left:1 ~top:6 (notebookInfoMeanBleu#coerce))

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
	~packing: notebookAlignImageView#add ();;

ignore (notebookTableImageView#attach ~left:0 ~top:0 (viewImageAfficheFirst#coerce))
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

let notebookSimpleTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Opérations Simples</b></span>" ();;
ignore (notebookTableSimple#attach ~left:0 ~right:2 ~top:0 (notebookSimpleTitle#coerce))

let notebookSimpleRotation = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Rotation</span>" ();;
ignore (notebookTableSimple#attach ~left:0 ~top:1 (notebookSimpleRotation#coerce))

let notebookTableRotAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (notebookTableSimple#attach ~left:1 ~top:1 (notebookTableRotAlign#coerce))

let notebookTableRot = GPack.table
	~rows:1
	~columns:2
	~homogeneous:true
	~packing: notebookTableRotAlign#add ()

let notebookButtonBoxLeftRot = GPack.button_box `HORIZONTAL
	~border_width:3
	~child_width: 25
	~child_height:25 ();;
ignore (notebookTableRot#attach ~left:0 ~top:0 (notebookButtonBoxLeftRot#coerce))

let notebookArrowLeftRot = GButton.button ~packing:notebookButtonBoxLeftRot#add ()
let notebookArrowLeftRotImage = GMisc.arrow ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:notebookArrowLeftRot#add ();;

notebookArrowLeftRot#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

let notebookButtonBoxRightRot = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;
ignore (notebookTableRot#attach ~left:1 ~top:0 (notebookButtonBoxRightRot#coerce))


let notebookArrowRightRot = GButton.button ~packing:notebookButtonBoxRightRot#add ()
let notebookArrowRightRotImage = GMisc.arrow ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:notebookArrowRightRot#add ();;
notebookArrowRightRot#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

let notebookSimpleMiror = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Miroir</span>" ();;
ignore (notebookTableSimple#attach ~left:0 ~top:2 (notebookSimpleMiror#coerce))

let notebookTableMirorAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (notebookTableSimple#attach ~left:1 ~top:2 (notebookTableMirorAlign#coerce))

let notebookTableMiror = GPack.table
	~rows:1
	~columns:2
	~homogeneous:true
	~packing:notebookTableMirorAlign#add ();;

let notebookButtonBoxMirorUpDown = GPack.button_box `HORIZONTAL
	~child_width:25
	~child_height:65 ();;
ignore (notebookTableMiror#attach ~left:0 ~top:0 (notebookButtonBoxMirorUpDown#coerce))


let notebookArrowMirorUpDown = GButton.button ~packing:notebookButtonBoxMirorUpDown#add ();;
notebookArrowMirorUpDown#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

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
	~packing:alignUpDownMirFirstView#add ()

let upArrowFirstView = GMisc.arrow ~kind:`UP ~shadow:`IN ();;
ignore (upDownMirFirstViewTable#attach ~left:0 ~top:0 (upArrowFirstView#coerce))

let downArrowFirstView = GMisc.arrow ~kind:`DOWN ~shadow:`OUT ();;
ignore (upDownMirFirstViewTable#attach ~left:0 ~top:1 (downArrowFirstView#coerce))

let notebookButtonBoxMirorRightLeft = GPack.button_box `HORIZONTAL
	~child_width: 65
	~child_height:25 ();;
ignore (notebookTableMiror#attach ~left:1 ~top:0 (notebookButtonBoxMirorRightLeft#coerce))


let notebookArrowMirorRightLeft = GButton.button ~packing:notebookButtonBoxMirorRightLeft#add ();;
notebookArrowMirorRightLeft#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

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
	~packing:alignRightLeftMirFirstView#add ()

let leftArrowFirstView = GMisc.arrow ~kind:`LEFT ~shadow:`IN ();;
ignore (rightLeftMirMirFirstViewTable#attach ~left:0 ~top:0 (leftArrowFirstView#coerce))

let rightArrowFirstView = GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ();;
ignore (rightLeftMirMirFirstViewTable#attach ~left:1 ~top:0 (rightArrowFirstView#coerce))

let notebookSimpleInversion = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Inversion</span>" ();;
ignore (notebookTableSimple#attach ~left:0 ~top:3 (notebookSimpleInversion#coerce))

let notebookTableInversionAlign = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;
ignore (notebookTableSimple#attach ~left:1 ~top:3 (notebookTableInversionAlign#coerce))


let notebookTableInversion = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:notebookTableInversionAlign#add ()

let notebookButtonBoxInversion = GPack.button_box `HORIZONTAL
	~child_width:200
	~child_height:50 ();;

ignore (notebookTableInversion#attach ~left:0 ~top:0 (notebookButtonBoxInversion#coerce))

let notebookButtonInversion = GButton.button ~label: "Inverser" ~packing:notebookButtonBoxInversion#add ();;
notebookButtonInversion#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath )

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

let notebookAdvancedTitle = GMisc.label ~markup: "<span font_desc=\"Tahoma 25\"><b>Opérations Avancées</b></span>" ();;
ignore (notebookAdvancedTable#attach ~left:0 ~right:2 ~top:0 (notebookAdvancedTitle#coerce))

let notebookAdvancedCompression = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Compression</span>" ();;
ignore (notebookAdvancedTable#attach ~left:0 ~top:1 (notebookAdvancedCompression#coerce))

let notebookAlignTableCompression = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (notebookAdvancedTable#attach ~left:1 ~top:1 (notebookAlignTableCompression#coerce))

let notebookTableCompression = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:notebookAlignTableCompression#add ()

let notebookButtonBoxCompression = GPack.button_box `HORIZONTAL
	~child_width:250
	~child_height:50 ();;

ignore (notebookTableCompression#attach ~left:0 ~top:0  (notebookButtonBoxCompression#coerce))


let notebookButtonCompression = GButton.button ~label: "Compresser" ~packing:notebookButtonBoxCompression#add ();;
notebookButtonCompression#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

let notebookAdvancedSegmentation = GMisc.label ~markup:"<span font_desc=\"Tahoma 20\">Segmentation</span>" ();;
ignore (notebookAdvancedTable#attach ~left:0 ~top:2 (notebookAdvancedSegmentation#coerce))

let notebookAlignTableSegmentation = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (notebookAdvancedTable#attach ~left:1 ~top:2 (notebookAlignTableSegmentation#coerce))

let notebookTableSegmentation = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:notebookAlignTableSegmentation#add ()

let notebookButtonBoxSegmentation = GPack.button_box `HORIZONTAL
	~child_width:250
	~child_height:50 ();;

ignore (notebookTableSegmentation#attach ~left:0 ~top:0  (notebookButtonBoxSegmentation#coerce))

let notebookButtonSegmentation = GButton.button ~label: "Segmenter"  ~packing:notebookButtonBoxSegmentation#add ();;
notebookButtonSegmentation#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath )

(*View 2*)
let thirdPageHBoxView2 = GPack.hbox
	~spacing:10
	~show: false
	~packing: firstVbox#add ();;

ignore (topToolbarReturnButton#connect#clicked (fun () -> ignore (thirdPageHBoxView1#misc#hide ());ignore (thirdPageHBoxView2#misc#hide ());Sys.remove (!tmpFileGlobalPath);
	ignore (topToolbarViewsItem#misc#hide ());ignore (topToolbarSaveAs#misc#hide ());ignore (topToolbarSave#misc#hide ());ignore (topToolbarSecondSeparator#misc#hide ());ignore (topToolbarThirdSeparator#misc#hide ());
	ignore (topToolbarFirstSeparator#misc#hide ()); ignore (firstPageTitle#misc#show ()); ignore (firstPageButtonBox#misc#show ());ignore (topToolbarReturnButton#misc#hide ())));

ignore (topToolbarRadioView1#connect#toggled (fun () -> ignore (thirdPageHBoxView1#misc#show ());ignore (thirdPageHBoxView2#misc#hide ())));
ignore (topToolbarRadioView2#connect#toggled (fun () -> ignore (thirdPageHBoxView1#misc#hide ());ignore (thirdPageHBoxView2#misc#show ())))

let leftToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(thirdPageHBoxView2#pack ~expand:false) ()

let tableLeftToolbarSecondView = GPack.table
	~rows:2
	~columns:1
	~border_width:5
	~row_spacings:10 ();;

leftToolbarSecondView#insert_widget ~tooltip:"Info" tableLeftToolbarSecondView#coerce

let titreInfo = GMisc.label ~markup: "<b>Informations</b>" ();;
ignore (tableLeftToolbarSecondView#attach ~left:0 ~top:0 (titreInfo#coerce))

let tableInfoSecondView = GPack.table
	~rows:5
	~columns:2
	~row_spacings:10
	~col_spacings:1
	~border_width:2
	~homogeneous:true ();;

ignore (tableLeftToolbarSecondView#attach ~left:0  ~top:1 (tableInfoSecondView#coerce))

let nomInfo = GMisc.label ~markup: "<b>Nom :</b>" ();;
ignore (tableInfoSecondView#attach ~left:0 ~top:0 (nomInfo#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:0 (nomFichInfo#coerce))

let dimInfo = GMisc.label ~markup: "<b>Dimensions :</b>" ();;
ignore (tableInfoSecondView#attach ~left:0 ~top:1 (dimInfo#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:1 (dimFichInfoViewSecond#coerce))

let moyInfo = GMisc.label ~markup: "<b>Moyenne</b>" ();;
ignore (tableInfoSecondView#attach ~left:0 ~right:2 ~top:2 (moyInfo#coerce))

let moyRougeTitre = GMisc.label ~markup: "<b>Rouge :</b>" ();;
ignore (tableInfoSecondView#attach ~left:0 ~top:3 (moyRougeTitre#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:3 (moyRouge#coerce))

let moyVertTitre = GMisc.label ~markup: "<b>Vert :</b>" ();;
ignore (tableInfoSecondView#attach ~left:0 ~top:4 (moyVertTitre#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:4 (moyVert#coerce))

let moyBleuTitre = GMisc.label ~markup: "<b>Blueu :</b>" ();;
ignore (tableInfoSecondView#attach ~left:0 ~top:5 (moyBleuTitre#coerce));
ignore (tableInfoSecondView#attach ~left:1 ~top:5 (moyBleu#coerce))

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
	~packing: alignImageViewSecondView#add ();;

ignore (tableImageViewSecondView#attach ~left:0  ~top:0 (viewImageAfficheSecond#coerce))


(*RightToolbar*)
let rightToolbarSecondView = GButton.toolbar
	~orientation: `VERTICAL
	~style:`BOTH
	~packing:(thirdPageHBoxView2#pack ~expand:false) ()

let tableRightToolbarSecondView = GPack.table
	~rows:5
	~columns:1
	~row_spacings:85
	~border_width:5 ()

let alignTableRotToolbarSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableRightToolbarSecondView#attach ~left:0 ~top:0 (alignTableRotToolbarSecondView#coerce));

ignore (rightToolbarSecondView#insert_widget ~tooltip:"Right Toolbar" (tableRightToolbarSecondView#coerce))

let tableRotSecondView = GPack.table
	~rows:2
	~columns:2
	~homogeneous:true
	~packing: alignTableRotToolbarSecondView#add ()

let toolbarNameRotation = GMisc.label ~markup: "<b>Rotation</b>" ();;
ignore (tableRotSecondView#attach ~left:0 ~right:2 ~top:0 (toolbarNameRotation#coerce))

let bboxLeftRotSecondView = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;
ignore (tableRotSecondView#attach ~left:0 ~top:1 (bboxLeftRotSecondView#coerce))

let leftRot = GButton.button ~packing:bboxLeftRotSecondView#add ()
let leftRotImage = GMisc.arrow ~kind:`LEFT ~shadow:`ETCHED_IN ~packing:leftRot#add ();;
leftRot#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)


let bboxRightRotSecondView = GPack.button_box `HORIZONTAL
	~border_width:2
	~child_width: 25
	~child_height:25 ();;
ignore (tableRotSecondView#attach ~left:1 ~top:1 (bboxRightRotSecondView#coerce))


let rightRot = GButton.button ~packing:bboxRightRotSecondView#add ()
let rightRotImage = GMisc.arrow ~kind:`RIGHT ~shadow:`ETCHED_OUT ~packing:rightRot#add ();;
rightRot#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)


let alignTableMiroirSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableRightToolbarSecondView#attach ~left:0 ~top:1 (alignTableMiroirSecondView#coerce))

let tableMirSecondView = GPack.table
	~rows:3
	~columns:1
	~row_spacings:10
	~packing:alignTableMiroirSecondView#add ()

let toolbarNameMiroir = GMisc.label ~markup: "<b>Miroir</b>" ();;
ignore (tableMirSecondView#attach ~left:0 ~right:2 ~top:0 (toolbarNameMiroir#coerce))

let alignbboxMiroirUpDownSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableMirSecondView#attach ~left:0 ~top:1 (alignbboxMiroirUpDownSecondView#coerce))

let bboxMiroirUpDownSecondView = GPack.button_box `HORIZONTAL
	~child_width:10
	~child_height:40
	~packing:alignbboxMiroirUpDownSecondView#add ()



let upDownMir = GButton.button ~packing:bboxMiroirUpDownSecondView#add ()
let alignUpDownMirSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:upDownMir#add ()

let upDownMirSecondViewTable = GPack.table
	~rows:2
	~columns:1
	~row_spacings:15
	~packing:alignUpDownMirSecondView#add ()

let upArrowSecondView = GMisc.arrow ~kind:`UP ~shadow:`IN ();;
ignore (upDownMirSecondViewTable#attach ~left:0 ~top:0 (upArrowSecondView#coerce))

let downArrowSecondView = GMisc.arrow ~kind:`DOWN ~shadow:`OUT ();;
ignore (upDownMirSecondViewTable#attach ~left:0 ~top:1 (downArrowSecondView#coerce));
upDownMir#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

let alignbboxMiroirRightLeftSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableMirSecondView#attach ~left:0 ~top:2 (alignbboxMiroirRightLeftSecondView#coerce))

let bboxMiroirRightLeftSecondView = GPack.button_box `HORIZONTAL
	~child_width:40
	~child_height:10
	~packing:alignbboxMiroirRightLeftSecondView#add ()

let rightLeftMir = GButton.button ~packing:bboxMiroirRightLeftSecondView#add ()

let alignRightLeftMirSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0
	~packing:rightLeftMir#add ()

let rightLeftMirMirSecondViewTable = GPack.table
	~rows:1
	~columns:2
	~col_spacings:15
	~packing:alignRightLeftMirSecondView#add ()

let leftArrowSecondView = GMisc.arrow ~kind:`LEFT ~shadow:`IN ();;
ignore (rightLeftMirMirSecondViewTable#attach ~left:0 ~top:0 (leftArrowSecondView#coerce))

let rightArrowSecondView = GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ();;
ignore (rightLeftMirMirSecondViewTable#attach ~left:1 ~top:0 (rightArrowSecondView#coerce));

rightLeftMir#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)


let alignTableInversionSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableRightToolbarSecondView#attach ~left:0 ~top:2 (alignTableInversionSecondView#coerce))

let bboxInversionSecondView = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ()

let invBUT = GButton.button ~label: "Inverser" ~packing:bboxInversionSecondView#add ();;
invBUT#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

let tableInvSecondView = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableInversionSecondView#add ();;

ignore (tableInvSecondView#attach ~left:0 ~top:0 (bboxInversionSecondView#coerce))

let alignTableCompressionSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableRightToolbarSecondView#attach ~left:0 ~top:3 (alignTableCompressionSecondView#coerce))

let tableCompressionSecondView = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableCompressionSecondView#add ()

let bboxCompressionSecondView = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;

ignore (tableCompressionSecondView#attach ~left:0 ~top:0  (bboxCompressionSecondView#coerce))

let advancedCompressionButton = GButton.button ~label: "Compresser" ~packing:bboxCompressionSecondView#add ();;
advancedCompressionButton#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)


let alignTableSegmentationSecondView = GBin.alignment
	~xalign:0.5
	~yalign:0.5
	~xscale:0.0
	~yscale:0.0 ();;

ignore (tableRightToolbarSecondView#attach ~left:0 ~top:4 (alignTableSegmentationSecondView#coerce))

let tableSegmentationSecondView = GPack.table
	~rows:1
	~columns:1
	~homogeneous:true
	~packing:alignTableSegmentationSecondView#add ()

let bboxSegmentationSecondView = GPack.button_box `HORIZONTAL
		~child_width:90
		~child_height:25 ();;

ignore (tableSegmentationSecondView#attach ~left:0 ~top:0  (bboxSegmentationSecondView#coerce))

let advancedSegmentationButton = GButton.button ~label: "Segmenter"  ~packing:bboxSegmentationSecondView#add ();;
advancedSegmentationButton#connect#clicked (fun () -> Aux.buttonsFonctionalit !tmpFileGlobalPath)

(*LOAD*)
let action_buttonLoad =
	let dlgLoad = GWindow.file_chooser_dialog
		~action:`OPEN
		~parent:window
		~title: "Ouvrir"
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
	ignore (GMisc.image ~stock:`OPEN ~packing:btn#set_image ());
	ignore (btn#connect#clicked (fun () -> if dlgLoad#run () = `OPEN then (if (Aux.verifppm (dlgLoad#filename)) then (Gaux.may (Aux.loadGeneral) dlgLoad#filename;
	dlgLoad#misc#hide ();firstPageButtonBox#misc#hide ();firstPageTitle#misc#hide ();ignore (thirdPageHBoxView1#misc#show ());
	ignore (notebook#goto_page 0);ignore (topToolbarViewsItem#misc#show ());ignore (topToolbarSaveAs#misc#show ());ignore (topToolbarSave#misc#show ());ignore (topToolbarFirstSeparator#misc#show ());
	ignore (topToolbarSecondSeparator#misc#show ());ignore (topToolbarReturnButton#misc#show ()))
	else
		let d = dialog#run () in
		if (d = `DELETE_EVENT) || (d =  `OK ) then dialog#misc#hide (); dlgLoad#misc#hide ())
 	else dlgLoad#misc#hide ()));
	btn

let firstPageQuitButton = GButton.button
				~stock:`QUIT
				~packing:firstPageButtonBox#add ();;

ignore (firstPageQuitButton#connect#clicked ~callback:GMain.quit)


let saveAsButton =
	let dlgSave = GWindow.file_chooser_dialog
		~action:`SAVE
		~title:"Enregistrement-sous..."
		~parent:window
		~position:`CENTER_ON_PARENT
		~width:450
		~height:350
		~destroy_with_parent:true () in
	dlgSave#add_button_stock `CANCEL `CANCEL;
	dlgSave#add_select_button_stock `SAVE_AS `SAVE_AS;
	dlgSave#set_do_overwrite_confirmation true;
	dlgSave#add_filter (saveImageFilter ());
	dlgSave#add_filter (saveAll_files ());
	ignore (topToolbarSaveAs#connect#clicked (fun () -> if dlgSave#run () = `SAVE_AS then (Gaux.may (Aux.saveAsGeneral) dlgSave#filename;
	dlgSave#misc#hide ()) else dlgSave#misc#hide ()));

ignore (topToolbarSave#connect#clicked ~callback: (fun _ -> Aux.saveGeneral (!fileGlobalPath) (!tmpFileGlobalPath)));;
(*Affichage de fenetre*)
let _ =
	window#show ();
	GMain.main ()
