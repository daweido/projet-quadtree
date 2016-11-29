(*ocamlfind ocamlc -g -package lablgtk2 -linkpkg quadtree.ml -o quadtree*)
open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let xpm_label_box ~file ~text ~packing () =
	Sys.chdir ("/Users/davidrigaux/Documents/GIT/projet_quadtree/code/exemple gtk/2");
	if not (Sys.file_exists file) then failwith (file ^" does not exist");
	(*Create box for image and label and pack*)
	(* Create box for image and label and pack *)
 let box = GPack.hbox ~border_width:2 ~packing () in

 (* Now on to the image stuff and pack into box *)
 let pixmap = GDraw.pixmap_from_xpm ~file () in
 GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ();

 (* Create a label for the button and pack into box *)
 GMisc.label ~text ~packing:(box#pack ~padding:3) ()

let main () =
 (* Create a new window; set title and border_width *)
 let window = GWindow.window ~title:"Pixmap'd Buttons!" ~border_width:10 () in

 (* It's a good idea to do this for all windows. *)
 window#connect#destroy ~callback:Main.quit;
 window#event#connect#delete ~callback:(fun _ -> Main.quit (); true);

 (* Create a new button and pack *)
 let button = GButton.button ~packing:window#add () in

 (* Connect the "clicked" signal of the button to callback *)
 button#connect#clicked ~callback:
   (fun () -> print_endline "Hello again - cool button was pressed");

 (* Create box with xpm and label and pack into button *)
 xpm_label_box ~file:"folder.xpm" ~text:"cool button" ~packing:button#add ();

 (* Show the window and wait for the fun to begin! *)
 window#show ();
 Main.main ()

let _ = main ()
