open Cil
module E = Errormsg

  let rec check_instr cps = function
  | [] -> cps
  | (Set _) :: tl | (Asm _) :: tl ->
      (not cps) && check_instr cps tl
  | Call (_, Lval (Var f, NoOffset), _, _) :: tl when not f.vcps ->
      (not cps) && check_instr cps tl
  | Call (None, Lval (Var f, NoOffset), _, _) :: tl ->
        check_instr true tl
  | Call (Some _, Lval (Var f, NoOffset), _, _) :: _ ->
        E.s (E.unimp "cannot affect cps functions' return values")
  | i :: tl ->
        E.warn
          "I hope this has nothing to do with a cps function call: %a"
          dn_instr
          i;
        (not cps) && check_instr cps tl

let mark_cps s =
  match s.skind with
  | CpcYield _ | CpcDone _
  | CpcWait _ | CpcSleep _
  | CpcIoWait _ | Instr [] ->
      s.cps <- true;
      true
  | Instr l when check_instr false l ->
      s.cps <- true;
      true
  | _ -> false

let rec do_mark (s:stmt) =
  if (not s.cps) &&
     (match s.succs with
      | [] -> true
      | [x] -> x.cps
      | _ -> false) &&
     (mark_cps s)
  then match s.preds with
    | [s'] -> do_mark s'
    | _ -> ()

let do_check (s:stmt) =
  match s.skind with
  | CpcYield _ | CpcDone _
  | CpcWait _ | CpcSleep _
  | CpcIoWait _ | Instr []
      when not s.cps ->
        failwith "Pas en forme CPS"
  | Instr l 
      when (not s.cps) && check_instr false l ->
        failwith "Pas en forme CPS"
  | _ -> ()

class markCps = object(self)
  inherit nopCilVisitor

  method vfunc (f:fundec) : fundec visitAction =
    List.iter do_mark f.sallstmts;
    List.iter do_check f.sallstmts;
    DoChildren

end

let doit (f: file) =
  visitCilFileSameGlobals (new markCps) f

let feature : featureDescr =
  { fd_name = "cpc";
    fd_enabled = ref false;
    fd_description = "cpc translation to C";
    fd_extraopt = [];
    fd_doit = doit;
    fd_post_check = true;
  }

