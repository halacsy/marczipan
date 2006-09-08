let _ =
let t = Timem.init () in
Timem.start t "reading inx";
let inp = open_in_bin "index.inx" in
let iix = InvIndex.read inp in
Timem.stop t ;

