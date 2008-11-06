open Cil
module E = Errormsg

(* Split an instruction list given an instruction *)
exception SplitInstr of stmt * instr

(* Eliminate break and continue in a switch/loop *)
exception TrivializeStmt of stmt

(*****************************************************************************)

(* Context used in the markCps visitor *)
type mark_context = {
  mutable cps_fun : bool; (* is it a cps function *)
  mutable cps_con : bool; (* is it in cps context *)
  mutable last_stmt : stmt; (* last stmt in cps context *)
  mutable next_stmt : stmt; (* next stmt in cps context *)
  mutable enclosing_stmt : stmt; (* nearest loop or switch *)
  mutable last_var : varinfo option; (* last (cps) assigned variable *)
  }

let fresh_context () =
  { cps_fun = false; cps_con = false;
    last_stmt = dummyStmt;
    next_stmt = dummyStmt;
    enclosing_stmt = dummyStmt;
    last_var = None}

let copy c =
  {cps_fun = c.cps_fun;
   cps_con = c.cps_con;
   last_stmt = c.last_stmt;
   next_stmt = c.next_stmt;
   enclosing_stmt = c.enclosing_stmt;
   last_var = c.last_var}

(* add a goto from last_stmt to next_stmt *)
exception AddGoto of mark_context


class markCps = object(self)
  inherit nopCilVisitor

  val mutable c = fresh_context ()

  method private set_next (s: stmt) : unit =
      (* set next_stmt in cps context only *)
    if c.cps_con then begin
    c.last_stmt <- s;
    match s.succs with
    | [] ->
        c.next_stmt <- dummyStmt;
        E.warn "return expected after %a" d_stmt s
    | [x] ->
        (*E.log "set_next %a -> %a\n" dn_stmt s dn_stmt x;*)
        c.next_stmt <- x
    | _ -> c.next_stmt <- dummyStmt;
        E.s (E.bug "cpc construct with several successors.\
       Please report this bug with the file causing the error.")
    end

  method private is_cps (i: instr) =
      (* check that last_var is the first argument of args *)
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
    match self#is_cps i, c.cps_fun with
    | true, true ->
        c.cps_con <- true;
        SkipChildren
    | true, false ->
        E.s (E.error "cps call not allowed here: %a" d_instr i)
    | false, _ when c.cps_con ->
        raise (SplitInstr (c.last_stmt,i))
    | false, _ -> SkipChildren

  method vstmt (s: stmt) : stmt visitAction =
    if c.cps_con && c.next_stmt != s
    then begin
      (*E.log "control flow broken in cps context: %a\ninstead of: %a\n***\n"
      d_loc (get_stmtLoc s.skind)
      d_loc (get_stmtLoc c.next_stmt.skind);*)
    raise (AddGoto c) end
    else match s.skind with
    (* XXX accepted even in cps context ? XXX*)
    | CpcSpawn _ ->
        let context = copy c in
        c <- {(fresh_context ()) with cps_fun = true};
        self#set_next s;
        ChangeDoChildrenPost (s, fun s -> c <- context; s.cps <- c.cps_con; s)
    | CpcYield _ | CpcWait _ | CpcSleep _
    | CpcIoWait _
        when c.cps_fun -> (* beware, order matters! *)
          c.cps_con <- true; (* must be set first *)
          s.cps <- true;
          self#set_next s;
          SkipChildren
    |CpcDone _ when c.cps_fun ->
        (* just like a Return *)
        s.cps <- true;
        c.cps_con <- false;
        c.last_stmt <- dummyStmt;
        c.next_stmt <- dummyStmt;
        SkipChildren
    | CpcYield _ | CpcDone _ | CpcWait _ | CpcSleep _
    | CpcIoWait _ ->
        E.s (E.error "CPC construct not allowed here: %a" d_stmt s)
    | CpcFun _ ->
        ChangeDoChildrenPost
          (s, fun s -> s.cps <- c.cps_con; self#set_next s; s)
    | Instr [] ->
        self#set_next s;
        s.cps <- c.cps_con;
        SkipChildren
    | Instr _ ->
        c.last_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          self#set_next s;
          s.cps <- c.cps_con;
          s
        )
    | Return _ ->
        if c.cps_fun
        then begin
          s.cps <- true;
          c.cps_con <- false;
          c.last_stmt <- dummyStmt;
          c.next_stmt <- dummyStmt;
        end;
        SkipChildren

    (* Control flow in cps context *)
    | Goto _ when c.cps_con ->
        E.s (E.unimp "functionnalize goto")
    | Break _ | Continue _ when c.cps_con ->
        raise (TrivializeStmt c.enclosing_stmt);
    | If _ | Switch _ | Loop _ when c.cps_con ->
        raise (AddGoto c)

    (* Control flow otherwise *)
    | Goto _ | Break _ | Continue _ -> SkipChildren
    | If _ -> ChangeDoChildrenPost (s, fun s ->
        if c.next_stmt != dummyStmt then begin
          (*E.log "escaping cps context in if statement %a\n***\n" dn_stmt s;*)
          raise (AddGoto c) end
        else s)
    | Switch _ ->
        c.enclosing_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          if c.next_stmt != dummyStmt then begin
            (* E.log "escaping cps context in switch statement %a\n***\n" dn_stmt s; *)
            raise (AddGoto c) end
          else s)
    | Loop _ ->
        c.enclosing_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          if c.next_stmt != dummyStmt then begin
            (* E.log "escaping cps context in loop statement %a\n***\n" dn_stmt s; *)
            assert(c.next_stmt == s);
            raise (TrivializeStmt s) end
          else s)

    | Block _ ->
        ChangeDoChildrenPost
          (s, fun s -> s.cps <- c.cps_con; s)

    | TryFinally _ | TryExcept _ ->
        E.s (E.unimp "try/except/finally not supported by CPC")

  method vfunc (f:fundec) : fundec visitAction =
    Cfg.clearCFGinfo f;
    ignore(Cfg.cfgFun f);
    (*XXX c.next_stmt <- dummyStmt;*)
    let context = copy c in
    c <- {(fresh_context ()) with cps_fun = f.svar.vcps};
    ChangeDoChildrenPost (f, fun f -> c <- context; f)

end

(*****************************************************************************)

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

(*****************************************************************************)

let eliminate_switch_loop s =
  xform_switch_stmt s ~remove_loops:true
    (fun () -> failwith "break with no enclosing loop")
    (fun () -> failwith "continue with no enclosing loop") (-1)

let make_label =
  let i = ref 0 in
  fun () -> incr i; Printf.sprintf "cpc_label_%d" !i

let add_goto src dst =
  assert (src != dummyStmt && dst != dummyStmt);
  let (src_loc,dst_loc) = (get_stmtLoc src.skind, get_stmtLoc dst.skind) in
  let src' = mkStmt src.skind in
  src.skind <- Block (mkBlock ([
    src';
    mkStmt (Goto (ref dst, src_loc))]));
  dst.labels <- [Label (make_label(), dst_loc, false)]

(*****************************************************************************)

let rec doit (f: file) =
  try
    visitCilFileSameGlobals (new markCps) f;
    visitCilFile (new cpsConverter) f
  with
  | TrivializeStmt s when s = dummyStmt ->
      E.s (E.error "break or continue with no enclosing loop")
  | TrivializeStmt s ->
      (*E.log "TrivializeStmt %a\n" d_stmt s;*)
      eliminate_switch_loop s;
      doit f
  | AddGoto c ->
      let (src,dst) = (c.last_stmt, c.next_stmt) in
      add_goto src dst;
      doit f
  | SplitInstr (s, i) -> begin match s.skind with
    | Instr l ->
        let rec split_instr acc = function
          | [] -> raise Not_found
          | t::q when t==i -> (List.rev acc, l)
          | t::q -> split_instr (t::acc) q in
        let (l1,l2) = split_instr [] l in
        let (s1, s2) = (mkStmt (Instr l1), mkStmt (Instr l2)) in
        (*E.log "SplitInstr %a\n" d_instr i;*)
        s.skind <- Block (mkBlock ([s1; s2]));
        add_goto s1 s2;
        doit f
    | _ -> E.s (E.bug "SplitInstr raised with wrong argument %a" d_stmt s)
  end

let feature : featureDescr =
  { fd_name = "cpc";
    fd_enabled = ref false;
    fd_description = "cpc translation to C";
    fd_extraopt = [];
    fd_doit = doit;
    fd_post_check = true;
  }

