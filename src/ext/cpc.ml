open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

class cpcVisitor = object
  inherit nopCilVisitor

end

let feature : featureDescr =
  { fd_name = "cpc";
    fd_enabled = ref false;
    fd_description = "cpc translation to C";
    fd_extraopt = [];
    fd_doit =
    (function (f: file) ->
      let vis = new cpcVisitor in
      visitCilFileSameGlobals vis f);
    fd_post_check = true;
  }

