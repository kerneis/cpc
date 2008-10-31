open Cil
module E = Errormsg

type instr_sort = PlainC | FullCps | PlainThenCps | Mixed

let isCps (i: instr) = match i with
  | Set _ | Asm _ -> false
  | Call (None, Lval (Var f, NoOffset), _, _) -> f.vcps
  | Call (Some _, Lval (Var f, NoOffset), _, _) ->
      if f.vcps
      then
        E.s (E.unimp "assignement of a cps function's return value")
      else
        false
  | _ ->
      E.warn
        "I hope this has nothing to do with a cps function call: %a"
        dn_instr i;
      false

let rec check_instr_list l =
  List.fold_left
    (fun sort i -> match sort with
    | PlainC ->
        if isCps i
        then PlainThenCps
        else PlainC
    | FullCps
    | PlainThenCps ->
        if isCps i
        then sort
        else Mixed
    | Mixed -> Mixed)
    (if isCps (List.hd l) then FullCps else PlainC)
    (List.tl l)


let mark_cps s =
  match s.skind with
  | CpcYield _ | CpcDone _
  | CpcWait _ | CpcSleep _
  | CpcIoWait _ | Instr []
  | Return _ ->
      s.cps <- true;
      true
  | Instr l -> begin
    match check_instr_list l with
      | PlainC | Mixed -> false
      | FullCps ->
          s.cps <- true;
          true
      | PlainThenCps ->
          s.cps <- true;
          false
      end
  | _ -> false

let rec do_mark (s:stmt) =
  if (not s.cps) &&
     (match s.succs with
      | [] -> true
      | [x] -> x.cps
      | _ -> false) &&
     (mark_cps s)
  then match s.preds with
    | [s'] ->
        do_mark s'
    | _ -> ()

let do_check (s:stmt) =
  match s.skind with
  | CpcYield _ | CpcDone _
  | CpcWait _ | CpcSleep _
  | CpcIoWait _ | Instr []
  | Return _
      when not s.cps ->
        E.s (E.error "Pas en forme CPS %a" d_stmt s)
  | Instr l
      when not (s.cps || check_instr_list l = PlainC) ->
        E.s (E.error "Pas en forme CPS %a" d_stmt s)
  | _ -> ()

class markCps = object(self)
  inherit nopCilVisitor

  method vstmt (s: stmt) : stmt visitAction =
    do_check s;
    DoChildren

  method vfunc (f:fundec) : fundec visitAction =
    ignore(Cfg.cfgFun f);
    List.iter do_mark f.sallstmts;
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

