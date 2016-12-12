open GMain
open GdkKeysyms

let _ = GMain.init ()

let window = GWindow.window
	~width:900
	~height:700
	~position:`CENTER
	~resizable:false
	~title:"Test" ()

let hboxtwo = GPack.hbox
	~spacing:10
	~packing: window#add ()

let notebook = GPack.notebook ~packing:hboxtwo#add ()

let fir = GMisc.label ~text:"Home" ()

let vboxtwo = GPack.vbox
	~spacing:10
	~packing:(notebook#append_page ~tab_label:fir#coerce) ()

let _ =
	ignore (window#event#connect#delete confirm);
	window#show ();
	GMain.main ()
