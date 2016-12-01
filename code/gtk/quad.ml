open GMain
open GdkKeysyms

let _ = GMain.init ()

(*Definition de la fenetre*)
let window = GWindow.window
	~width:900
	~height:700
	~position:`CENTER
	~resizable:false
	~title:"Manipulation d'image" ()
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
factory_fileMen#add_item "Sauvegarder" ~key:_S ~callback: (fun () -> print_endline "Sauvegarder");
factory_fileMen#add_item "Quitter" ~key:_Q ~callback: Main.quit;;

(*Titre*)
let titre = GMisc.label ~markup: "<big>Manipulation d'Image</big>" ~text: "Manipulation d'image PPM" ~packing:vbox#add ()

(*Bouttons*)
(*Conteneur de Bouttons*)
let bbox = GPack.button_box `VERTICAL
	~spacing:150
	~layout:`SPREAD
	~child_width: 250
	~child_height: 50
	~packing:(vbox#pack ~expand:false) ()

(*Boutton Ouvrir*)
let load =
	let button = GButton.button
				~stock:`OPEN
				~packing:bbox#add () in
button#connect#clicked ~callback: (fun () -> print_endline "Boutton Charger");
button

(*Boutton Quitter*)
let quit =
	let button = GButton.button
				~stock:`QUIT
				~packing:bbox#add () in
button#connect#clicked ~callback:GMain.quit;
button


(*Affichage de fenetre*)
let _ =
	window#connect#destroy ~callback:GMain.quit;
	window#show ();
	GMain.main ()
