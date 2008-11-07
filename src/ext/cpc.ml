open Cil
module E = Errormsg

(* Split an instruction list given an instruction *)
exception SplitInstr of stmt * instr

(* Eliminate break and continue in a switch/loop *)
exception TrivializeStmt of stmt

(* Functionalize a statement with a label *)
exception FunctionalizeGoto of stmt

let is_label = function Label _ -> true | _ -> false

(******************** CPS Marking ********************************************)

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

let copy_context c =
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

  method private set_next ?(set_last=true) (s: stmt) : unit =
      (* set next_stmt in cps context only *)
    if c.cps_con then begin
    if set_last then c.last_stmt <- s;
    match s.succs with
    | [] ->
        c.next_stmt <- dummyStmt;
        (*E.warn "return expected after %a" d_stmt s*)
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
        | e::_ -> (*E.warn "%a should be a variable" dn_exp e;*) false
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
        (*E.warn "%a should be a variable" dn_lval l;*) c.last_var <- None; false
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
    (* Broken control flow *)
    if c.cps_con && c.next_stmt != s
    then begin
      E.log "control flow broken in cps context: %a\ninstead of: %a\n***\n"
      d_stmt s
      d_stmt c.next_stmt;
    raise (AddGoto c) end

    (* Potential goto into cps context *)
    else if c.cps_con && (List.exists is_label s.labels) then
      (E.log "label in cps context! ";
      raise (FunctionalizeGoto s))
    else match s.skind with

    (* CPC Constructs *)
    (* XXX accepted even in cps context ? XXX*)
    | CpcSpawn _ ->
        let context = copy_context c in
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
    | CpcDone _ when c.cps_fun ->
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
          (s, fun s -> s.cps <- c.cps_con; self#set_next ~set_last:false s; s)

    (* Instructions and return *)
    | Instr [] ->
        self#set_next ~set_last:false s;
        s.cps <- c.cps_con;
        SkipChildren
    | Instr (hd::_) when c.cps_con && not(self#is_cps hd) ->
        E.log "not cps instruction in cps context\n";
        raise (AddGoto c)
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
    | Goto (g, _) when c.cps_con ->
        raise (FunctionalizeGoto !g)
    | Break _ | Continue _ when c.cps_con ->
        raise (TrivializeStmt c.enclosing_stmt);
    | If _ | Switch _ | Loop _ when c.cps_con ->
        E.log "control flow in cps context\n";
        raise (AddGoto c)

    (* Control flow otherwise *)
    | Goto _ | Break _ | Continue _ -> SkipChildren
    | If _ -> ChangeDoChildrenPost (s, fun s ->
        if c.next_stmt != dummyStmt then begin
          E.log "escaping cps context in if statement %a\n***\n" dn_stmt s;
          raise (AddGoto c) end
        else s)
    | Switch _ ->
        c.enclosing_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          if c.next_stmt != dummyStmt then begin
            E.log "escaping cps context in switch statement %a\n***\n" dn_stmt s;
            raise (AddGoto c) end
          else s)
    | Loop _ ->
        c.enclosing_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          if c.next_stmt != dummyStmt then begin
            E.log "escaping cps context in loop statement %a\n***\n" dn_stmt s;
            assert(c.next_stmt == s);
            raise (TrivializeStmt s) end
          else s)

    (* Blocks *)
    | Block _ ->
        self#set_next ~set_last:false s;
        ChangeDoChildrenPost
          (s, fun s -> s.cps <- c.cps_con; s)

    | TryFinally _ | TryExcept _ ->
        E.s (E.unimp "try/except/finally not supported by CPC")

  method vfunc (f:fundec) : fundec visitAction =
    Cfg.clearCFGinfo f;
    ignore(Cfg.cfgFun f);
    let context = copy_context c in
    c <- {(fresh_context ()) with cps_fun = f.svar.vcps};
    ChangeDoChildrenPost (f, fun f -> c <- context; f)

end

(******************* CPS Conversion ******************************************)

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

(********************* Cleaning **********************************************)

class cleaner = object(self)
  inherit nopCilVisitor

  val mutable stack = []

  method vblock (b: block) : block visitAction =
    ChangeDoChildrenPost (b, fun b ->
      b.bstmts <- compactStmts b.bstmts;
      b)
end

(************** Functionnalize Goto ******************************************)

exception GotoContent of stmt list

let make_function_name =
  let i = ref 0 in
  fun base -> incr i; Printf.sprintf "__cpc_%s_%d" base !i

class functionalizeGoto start =
      let Label(label,_,_) = List.find is_label start.labels in
      let fd = emptyFunction (make_function_name label) in

      let fd_block last =
        [ mkStmt (CpcFun (fd, locUnknown));
          mkStmt (Instr[Call(None,Lval(Var fd.svar, NoOffset),[],locUnknown)]);
          last ] in
      let () = fd.svar.vcps <- true in
      object(self)
        inherit nopCilVisitor

        val mutable acc = false
        val mutable stack = []

        method private unstack last =
          acc <- false;
          fd.sbody <- mkBlock (List.rev stack);
          stack <- [];
          mkStmt (Block (mkBlock (compactStmts (fd_block last))))

        method private unstack_fun ({skind=CpcFun(f,_)} as s) =
          acc <- false;
          fd.sbody <- mkBlock (List.rev stack);
          stack <- [];
          f.sbody.bstmts <- compactStmts
            (f.sbody.bstmts
            @ (fd_block (mkStmt (CpcDone locUnknown))));
          s

        method vstmt (s: stmt) : stmt visitAction =
        if s == start then acc <- true;
        match s.skind with
        | Goto (g, loc) when !g == start ->
            let s' = (mkStmt ( Instr
              [Call(None,Lval(Var fd.svar, NoOffset),[],loc)])) in
            if acc then begin
              stack <- s' :: stack;
              ChangeTo (mkEmptyStmt())
            end else  ChangeTo s'
        | Return _ | CpcDone _ when acc ->
            ChangeTo(self#unstack s)
        | _ when acc -> ChangeDoChildrenPost(
            (acc <- false;s),
            (fun s -> acc <- true;
              stack <- s :: stack;
              mkEmptyStmt ()))
        | Block _ -> DoChildren
        | CpcFun _ ->
            ChangeDoChildrenPost(s, fun s ->
            if acc
            then self#unstack_fun s
            else s
          )
        | _ ->
            ChangeDoChildrenPost(s, fun s ->
            if acc
            then self#unstack (mkStmt (CpcDone locUnknown))
            else s
          )
      end

(*****************************************************************************)

let eliminate_switch_loop s =
  xform_switch_stmt s ~remove_loops:true
    (fun () -> failwith "break with no enclosing loop")
    (fun () -> failwith "continue with no enclosing loop") (-1)

let make_label =
  let i = ref 0 in
  fun () -> incr i; Printf.sprintf "__cpc_label_%d" !i

let add_goto src dst =
  assert (src != dummyStmt && dst != dummyStmt);
  E.log "add goto from %a\n to %a\n" d_stmt src d_stmt dst;
  let (src_loc,dst_loc) = (get_stmtLoc src.skind, get_stmtLoc dst.skind) in
  let src' = mkStmt src.skind in
  src.skind <- Block (mkBlock ([
    src';
    mkStmt (Goto (ref dst, src_loc));
    mkStmt (CpcDone locUnknown)
    ]));
  dst.labels <- [Label (make_label(), dst_loc, false)]


(*****************************************************************************)

let init () = lineDirectiveStyle := None

let rec doit (f: file) =
  try
    E.log "********************* doit ******************\n";
    visitCilFileSameGlobals (new cleaner) f;
    let r = read_line () in
    if r = "q" then E.log "quit!\n" else
    if r = "d" then (dumpFile defaultCilPrinter stdout "" f; doit f)
    else begin
    visitCilFileSameGlobals (new markCps) f;
    (*visitCilFile (new cpsConverter) f;*)
    visitCilFileSameGlobals (new cleaner) f;
    E.log "Finished!\n";
    end
  with
  | TrivializeStmt s when s = dummyStmt ->
      E.s (E.error "break or continue with no enclosing loop")
  | TrivializeStmt s ->
      E.log "TrivializeStmt %a\n" d_stmt s;
      eliminate_switch_loop s;
      doit f
  | AddGoto {last_stmt = src; next_stmt = dst} ->
      add_goto src dst;
      doit f
  | SplitInstr ({skind = Instr l} as s, i) ->
      let rec split_instr acc = function
        | [] -> raise Not_found
        | t::q when t==i -> (List.rev acc, l)
        | t::q -> split_instr (t::acc) q in
      let (l1,l2) = split_instr [] l in
        begin
        let (s1, s2) = (mkStmt (Instr l1), mkStmt (Instr l2)) in
        E.log "SplitInstr %a\n at point:\n%a\n" d_stmt s d_instr i;
        s.skind <- Block (mkBlock ([s1; s2]));
        add_goto s1 s2;
        end;
      doit f
  | SplitInstr (s, _) ->
      E.s (E.bug "SplitInstr raised with wrong argument %a" d_stmt s)
  | FunctionalizeGoto start ->
      E.log "functionalize goto\n";
      visitCilFileSameGlobals (new functionalizeGoto start) f;
      doit f
  | Exit -> E.log "Exit\n";()

let feature : featureDescr =
  { fd_name = "cpc";
    fd_enabled = ref false;
    fd_description = "cpc translation to C";
    fd_extraopt = [];
    fd_doit = (fun f -> init() ; doit f);
    fd_post_check = true;
  }

