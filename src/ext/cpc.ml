open Cil
module E = Errormsg

type instr_sort =
    PlainC
  | FullCps of varinfo option
  | PlainThenCps of varinfo option
  | Mixed

exception CpsAssign of varinfo

let isCps ?var (i: instr) =
  let check_var args = match var with
  | None -> true
  | Some v ->
    match args with
    | Lval (Var v', NoOffset)::_ -> v = v'
    | [] -> false
    | e::_ -> E.warn "%a should be a variable" dn_exp e; false
  in match i with
  | Set _ | Asm _ -> false
  (* Non cps call *)
  | Call (_, Lval (Var f, NoOffset), _, _) when not f.vcps -> false
  (* Cps call without assignment *)
  | Call (None, Lval (Var f, NoOffset), args, _) -> check_var args
  (* Cps call with assignment *)
  | Call (Some (Var v, NoOffset), Lval (Var f, NoOffset), args, _) ->
      check_var args && raise (CpsAssign v)
  | Call (Some l, Lval (Var f, NoOffset), _, _) ->
      E.warn "%a should be a variable" dn_lval l; false
  (* Weird call *)
  | Call _ ->
      E.warn
        "I hope this has nothing to do with a cps call: %a"
        dn_instr i;
      false

let isCpcSpawn s = match s.skind with
| CpcSpawn _ -> true
| _ -> false

let rec check_instr_list l =
  List.fold_left
    (fun sort i -> match sort with
    | PlainC -> begin
        try
          if isCps i
          then PlainThenCps None
          else PlainC
        with
          CpsAssign v' -> PlainThenCps (Some v')
        end
    | FullCps v -> begin
        try
          if isCps ?var:v i
          then FullCps None
          else Mixed
        with
          CpsAssign v' -> FullCps (Some v')
        end
    | PlainThenCps v -> begin
        try
          if isCps ?var:v i
          then PlainThenCps None
          else Mixed
        with
          CpsAssign v' -> PlainThenCps (Some v')
        end
    | Mixed -> Mixed)
    (try
      if isCps (List.hd l)
      then FullCps None
      else PlainC
     with
      CpsAssign v -> FullCps (Some v))
    (List.tl l)


let last_var = ref None

let mark_cps s =
  let extract_first_arg = function
  | Call(_,_,[],_) -> None
  | Call(_,_,Lval(Var v,NoOffset)::_,_) -> Some v
  | Call(_,_,e::_,_) ->
      E.warn "%a should be a variable" dn_exp e;
      None
  | _ -> assert false
  in match s.skind with
  | CpcYield _ | CpcDone _
  | CpcWait _ | CpcSleep _
  | CpcIoWait _ ->
      if !last_var = None
      then (s.cps <- true; true)
      else false
  | Instr []
  | Return (None, _) ->
      last_var := None;
      s.cps <- true;
      true
  | Return (Some (Lval (Var v, NoOffset)), _) ->
      last_var := Some v;
      s.cps <- true;
      true
  | Return (Some e, _) ->
      E.warn "%a should be a variable" dn_exp e;
      false
  | Instr l -> begin
    match check_instr_list l with
      | FullCps v when !last_var = v ->
          s.cps <- true;
          last_var := extract_first_arg (List.hd l);
          true
      | PlainThenCps v when !last_var = v ->
          s.cps <- true;
          false
      | PlainC | Mixed
      | FullCps _ | PlainThenCps _ -> false
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
    | _ -> last_var := None
    else
      last_var := None

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

  val mutable checking = false;

  method vstmt (s: stmt) : stmt visitAction =
    if checking then do_check s;
    if isCpcSpawn s
    then
      let c = checking in
      ChangeDoChildrenPost
        ((checking <- true;s), fun s -> checking <- c; s)
    else
      DoChildren

  method vfunc (f:fundec) : fundec visitAction =
    ignore(Cfg.cfgFun f);
    if
      f.svar.vcps ||
      List.exists isCpcSpawn f.sallstmts
    then
      List.iter do_mark f.sallstmts;
    let c = checking in
    ChangeDoChildrenPost
      ((checking <- f.svar.vcps;f), fun f -> checking <- c; f)

end

let do_convert return s =
  (* dummy converter, just reverse the stack *)
  mkStmt (Block (mkBlock (s@[return])))

class cpsConverter = object(self)
  inherit nopCilVisitor

  val mutable stack = []

  method vstmt (s: stmt) : stmt visitAction =
    if s.cps
    then begin match s.skind with
    | CpcYield _ | CpcDone _
    | CpcWait _ | CpcSleep _
    | CpcIoWait _ | Instr _
    | CpcSpawn _ ->
        stack <- s::stack;
        s.skind <- Instr [];
        SkipChildren
    | Return _ ->
        let res = do_convert s stack in
        stack <- [];
        ChangeTo res
    | _ -> assert false
    end
    else DoChildren

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

