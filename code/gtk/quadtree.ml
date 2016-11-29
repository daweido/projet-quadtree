(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg quadtree.ml -o quadtree*)
open GMain
open GdkKeysyms

let _ = GMain.init ();;

let xpm_label_box ~file ~text ~packing () =
	Sys.chdir ("/Users/davidrigaux/Documents/GIT/projet_quadtree/code/gtk");
	if not (Sys.file_exists file) then failwith (file ^" does not exist");
	(*Create box for image and label and pack*)
	let box = GPack.hbox ~border_width:100 ~packing () in

	(*Now on to the image stuff and pack into box*)
	let pixmap = GDraw.pixmap_from_xpm ~file () in
	GMisc.pixmap pixmap ~packing:(box#pack ~padding:5) ()

	(*Create a label for the button and pack into box*)
	GMisc.label ~text ~packing:(box#pack ~padding:3) ()

let window = GWindow.window ~width:900 ~height:700
							~resizable:false
							~title:"Manipulation d'image" ();;

let vbox = GPack.vbox
	~spacing:10
	~border_width:10
	~packing:window#add ();;

let bbox = GPack.button_box `HORIZONTAL
	~layout:`EDGE
	~packing:(vbox#pack ~expand:false) ();;

(* Menu bar *)
let menubar = GMenu.menu_bar ~packing:vbox#pack ();;
let factory = new GMenu.factory menubar ();;
let accel_group = factory#accel_group ();;
let file_menu = factory#add_submenu "Fichier" ();;

(* File menu *)
let factory_fileMen = new GMenu.factory file_menu ~accel_group ();
factory_fileMen#add_item "Sauvegarder" ~key:_S;
factory_fileMen#add_item "Quitter" ~key:_Q ~callback: Main.quit;;

(* Button *)
(*Create a new button and pack*)
let button = GButton.button ~packing:vbox#add ();
(*Connect the clicked signal of the button to callback*)
button#connect#clicked ~callback: (fun () -> prerr_endline "Loading...");
(*Create box with xpm and label and pack into button*)
xpm_label_box ~file:"folder.xpm" ~text:"Charger..." ~packing:button#add ();;

(* Display the windows and enter Gtk+ main loop *)


let _ =
	window#add_accel_group accel_group;
	window#connect#destroy ~callback:Main.quit;
	window#event#connect#delete ~callback:(fun _ -> Main.quit (); true);
	window#show ();
	GMain.main ();;
