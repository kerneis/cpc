open Cil
module E = Errormsg

type mark_context = {
  mutable cps_fun : bool; (* is it a cps function *)
  mutable cps_con : bool; (* is it in cps context *)
  mutable next_stmt : stmt; (* next stmt in cps context *)
  mutable last_var : varinfo option; (* last (cps) assigned variable *)
  }

let copy c =
  {cps_fun = c.cps_fun;
   cps_con = c.cps_con;
   next_stmt = c.next_stmt;
   last_var = c.last_var}

class markCps = object(self)
  inherit nopCilVisitor

  val mutable c =
    { cps_fun = false; cps_con = false;
      next_stmt = dummyStmt; last_var = None}

  method private set_next (s: stmt) : unit =
    if c.cps_con then match s.succs with
    | [] ->
        c.next_stmt <- dummyStmt;
        E.warn "return expected after %a" d_stmt s
    | [x] ->
        (*E.log "set_next %a -> %a\n" dn_stmt s dn_stmt x;*)
        c.next_stmt <- x
    | _ -> c.next_stmt <- dummyStmt;
        E.s (E.bug "cpc construct with several successors.\
       Please report this bug with the file causing the error.")

  method private is_cps (i: instr) =
    let check_var args = match c.last_var with
    | None -> true
    | Some v ->
      match args with
      | Lval (Var v', NoOffset)::_ -> v = v'
      | [] -> false
      | e::_ -> E.warn "%a should be a variable" dn_exp e; false
    in match i with
    | Set _ | Asm _ -> c.last_var <- None; false
    (* Non cps call *)
    | Call (_, Lval (Var f, NoOffset), _, _) when not f.vcps ->
        c.last_var <- None; false
    (* Cps call without assignment *)
    | Call (None, Lval (Var f, NoOffset), args, _) ->
        let res = check_var args in
        c.last_var <- None; res
    (* Cps call with assignment *)
    | Call (Some (Var v, NoOffset), Lval (Var f, NoOffset), args, _) ->
        check_var args && (c.last_var <- Some v; true)
    | Call (Some l, Lval (Var f, NoOffset), _, _) ->
        E.warn "%a should be a variable" dn_lval l; c.last_var <- None; false
    (* Weird call *)
    | Call _ ->
        E.warn
          "I hope this has nothing to do with a cps call: %a"
          dn_instr i;
        c.last_var <- None;
        false

  method vinst (i: instr) : instr list visitAction =
    match (self#is_cps i, c.cps_fun) with
    | true, true ->
        c.cps_con <- true;
        SkipChildren
    | true, false ->
        E.s (E.error "cps call not allowed here: %a" d_instr i)
    | false, _ when c.cps_con ->
        E.s (E.error "Not allowed in CPS context: %a" d_instr i)
    | false, _ -> SkipChildren

  method vstmt (s: stmt) : stmt visitAction =
    if c.cps_con && c.next_stmt != s
    then E.s (
      E.error "control flow broken in cps context: %a instead of %a"
      d_loc (get_stmtLoc s.skind)
      d_loc (get_stmtLoc c.next_stmt.skind)
    )
    else match s.skind with
    (* XXX accepted even in cps context ? XXX*)
    | CpcSpawn _ ->
        let context = copy c in
        self#set_next s;
        ChangeDoChildrenPost (
          (c.cps_fun <- true; c.cps_con <- false; s),
          (fun s ->
            c <- context;
            s.cps <- c.cps_con;
            s)
        )
    | CpcYield _ | CpcDone _ | CpcWait _ | CpcSleep _
    | CpcIoWait _
        when c.cps_fun -> (* beware, order matters! *)
          c.cps_con <- true; (* must be set first *)
          s.cps <- true;
          self#set_next s;
          SkipChildren
    | CpcYield _ | CpcDone _ | CpcWait _ | CpcSleep _
    | CpcIoWait _ ->
        E.s (E.error "CPC construct not allowed here: %a" d_stmt s)
    | CpcFun _ ->
        ChangeDoChildrenPost (s, fun s -> s.cps <- c.cps_con; self#set_next s; s)
    | Instr [] ->
        self#set_next s;
        s.cps <- c.cps_con;
        SkipChildren
    | Instr _ ->
        ChangeDoChildrenPost (s, fun s ->
          self#set_next s;
          s.cps <- c.cps_con;
          s
        )
    | Return _ when c.cps_fun ->
        s.cps <- true;
        c.cps_con <- false;
        c.next_stmt <- dummyStmt;
        SkipChildren
    | Block _ -> ChangeDoChildrenPost (s, fun s ->
        s.cps <- c.cps_con; s)
    | _ when c.cps_con ->
        E.s (E.error "Not allowed in CPS context: %a" d_stmt s)
    | _ -> ChangeDoChildrenPost (s, fun s ->
        if c.next_stmt != dummyStmt then
          E.s (E.error "escaping cps context in %a" dn_stmt s)
        else s)

  method vfunc (f:fundec) : fundec visitAction =
    ignore(Cfg.cfgFun f);
    c.next_stmt <- dummyStmt;
    let context = copy c in
    ChangeDoChildrenPost (
      (c.cps_fun <- f.svar.vcps; c.cps_con <- false; f),
      (fun f -> c <- context; f)
    )

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

