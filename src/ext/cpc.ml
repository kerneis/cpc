open Cil
module E = Errormsg

(* Split an instruction list given an instruction *)
exception SplitInstr of stmt * instr

(* Eliminate break and continue in a switch/loop *)
exception TrivializeStmt of stmt

(* Functionalize a statement with a label *)
exception FunctionalizeGoto of stmt

let is_label = function Label _ -> true | _ -> false

exception FoundFun of fundec

(*************** Utility functions *******************************************)

let fst4 (x,_,_,_) = x

class replaceGotos start replace_with =
  object(self)
  inherit nopCilVisitor

  method vstmt s = match s.skind with
  | Goto (g, loc) when !g == start ->
      s.skind <- (match replace_with loc with
      | [] -> Instr []
      | [{labels=[];skind=x}] -> x
      | l -> Block (mkBlock l));
      SkipChildren
  | _ -> DoChildren
end

(* Return a copy of statement, clear the initial one and update gotos
accordingly *)
let copyClearStmt s file stack =
  let res = {(mkEmptyStmt()) with skind = s.skind; labels = s.labels} in
  let new_goto loc = [mkStmt (Goto (ref res, loc))] in
  if (List.exists is_label s.labels)
  then (
    let vis = new replaceGotos s new_goto in
    visitCilFileSameGlobals vis file;
    List.iter (fun s -> ignore(visitCilStmt vis s)) stack);
  s.skind <- Instr [];
  s.labels <- [];
  res

(* FIXME: every loop/switch is removed, although only the first one
would be necessary *)
let eliminate_switch_loop s =
  xform_switch_stmt s ~remove_loops:true
    (fun () -> failwith "break with no enclosing loop")
    (fun () -> failwith "continue with no enclosing loop") (-1)

let make_label =
  let i = ref 0 in
  fun () -> incr i; Printf.sprintf "__cpc_label_%d" !i

let add_goto src dst =
  assert (src != dummyStmt && dst != dummyStmt);
  E.log "add goto from %a\nto %a\n" d_stmt src d_stmt dst;
  let (src_loc,dst_loc) = (get_stmtLoc src.skind, get_stmtLoc dst.skind) in
  let src' = mkStmt src.skind in
  src.skind <- Block (mkBlock ([
    src';
    mkStmt (Goto (ref dst, src_loc));
    (*mkStmt (CpcDone locUnknown)*)
    ]));
  dst.labels <- [Label (make_label(), dst_loc, false)]

let add_goto_after src enclosing file stack =
  E.log "add_goto_after: enclosing is %a\n" d_stmt enclosing;
  let dst = mkEmptyStmt() in
  let copy = copyClearStmt enclosing file stack in
  add_goto src dst;
  enclosing.skind <- Block (mkBlock ([
    copy;
    dst]));

(*** Stolen from src/ext/partial.ml ***
(* Sets of {c goto}-targets *)
module LabelSet =
  Set.Make (struct
              type t = label * stmt
              let compare x y =
                match (fst x), (fst y) with
                    Label (name1, _, _), Label (name2, _, _) ->
                      String.compare name1 name2
                  | _, _ -> 0
            end)
(* Answer whether block [b] contains labels that are
   alive.  "Live" labels are actually targets of
   [goto]-instructions {b outside} of [b]. *)
let live_labels b =
  let gather_labels acc stmt =
    List.fold_left (fun a x -> LabelSet.add (x,stmt) a) acc stmt.labels in
  let rec visit_block stmt_fun acc blk =
    List.fold_left
      (fun a x ->
        let y = stmt_fun a x in
          match x.skind with
              Instr _
            | Return _ | Goto _ | Break _ | Continue _ -> y
            | If (_expr, then_block, else_block, _loc) ->
                visit_block
                  stmt_fun
                  (visit_block stmt_fun y then_block)
                  else_block
            | Switch (_expr, block, _stmt_list, _loc) ->
                visit_block stmt_fun y block
            | Loop (block, _loc, _opt_stmt1, _opt_stmt2) ->
                visit_block stmt_fun y block
            | Block block ->
                visit_block stmt_fun y block
            | TryFinally (block1, block2, _loc)
            | TryExcept (block1, _, block2, _loc) ->
                visit_block
                  stmt_fun
                  (visit_block stmt_fun y block1)
                  block2
            | CpcDone _ | CpcYield _ | CpcWait _
            | CpcSleep _ | CpcIoWait _ -> y
            | CpcSpawn (s, _) ->
                visit_block stmt_fun y (mkBlock [s])
            | CpcFun ({sbody = block},_) ->
                visit_block stmt_fun y block)
      acc
      blk.bstmts
  and gather_gotos acc stmt =
    match stmt.skind with
        Goto (stmt_ref, _loc) -> gather_labels acc !stmt_ref
      | _ -> acc
  and transitive_closure ini_stmt =
    let rec iter trace acc stmt =
      List.fold_left
        (fun (a_trace, a_stmt) s ->
          if List.mem s.sid a_trace then (a_trace, a_stmt)
          else iter (s.sid :: a_trace) (s :: a_stmt) s)
        (trace, acc) (stmt.preds @ stmt.succs) in
      List.sort (* sorting is unnecessary, but nice *)
        (fun a b -> a.sid - b.sid)
        (snd (iter [] [] ini_stmt)) in
  let block_labels = visit_block gather_labels LabelSet.empty b
  and block_gotos = visit_block gather_gotos LabelSet.empty b
  and all_gotos =
    List.fold_left
      (fun a x ->
        match x.skind with
            Goto (stmt_ref, _loc) -> gather_labels a !stmt_ref
          | Block block -> visit_block gather_gotos a block
          | _ -> a)
      LabelSet.empty
      (if b.bstmts = [] then []
        else transitive_closure (List.hd b.bstmts))
  in
    (LabelSet.inter
      (LabelSet.diff all_gotos block_gotos)
      block_labels)
*)

(* return the nearest enclosing function of a statement *)
class enclosingFunction = fun start -> object(self)
  inherit nopCilVisitor

  val mutable ef = None

  method vstmt s =
    if s == start
    then (match ef with Some f -> raise (FoundFun f) | None -> assert false)
    else DoChildren

  method vfunc f =
    let f' = ef in
    ef <- Some f;
    ChangeDoChildrenPost(f, fun f ->
      ef <- f';f)
end

let enclosing_function start file =
  try
    visitCilFileSameGlobals (new enclosingFunction start) file;
    E.log "enclosing function not found for: %a\n" d_stmt start;
    raise Not_found
  with FoundFun f -> f

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


class markCps = fun file -> object(self)
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
      | Some v -> true (* XXX Cannot be checked at this level --- should be done
                        * by switching variables after lambda-lifting  and
                        * optimizations.
        match args with
        | Lval (Var v', NoOffset)::_ -> v = v'
        | [] -> false
        | e::_ -> (*E.warn "%a should be a variable" dn_exp e;*) false *)
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
    | Goto (g, _)
        when enclosing_function s file != enclosing_function !g file ->
          E.log "live goto!\n";
          raise (FunctionalizeGoto !g)
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

  method vblock (b: block) : block visitAction =
    ChangeDoChildrenPost (b, fun b ->
      b.bstmts <- compactStmts b.bstmts;
      b)
end

(********************* Lambda-lifting ****************************************)

module S = Set.Make(
  struct
    type t = varinfo
    let compare x y = compare x.vid y.vid
  end)

class replaceVars = fun map -> object(self)
  inherit nopCilVisitor

  method vvrbl (v:varinfo) : varinfo visitAction =
    if not (v.vglob || isFunctionType v.vtype)
    then ChangeTo(List.assoc v map)
    else SkipChildren
end

class insertArgs = fun map -> object(self)
  inherit nopCilVisitor

  method vinst = function
    | Call(lval, Lval ((Var f, NoOffset) as l), args, loc)
        when List.mem_assoc f map ->
          let args' = List.assoc f map in
          assert(args = []);
          E.log "inserting in %a\n" d_lval l;
          ChangeTo([Call(lval, Lval(Var f, NoOffset), args', loc)])
    | _ -> SkipChildren
end

class lambdaLifter = object(self)
  inherit nopCilVisitor

  val mutable free_vars = S.empty
  val mutable local_funs = []
  val mutable local_proto = []
  val mutable local_map = [] (* maps local functions to their free vars *)

  method vvrbl (v:varinfo) : varinfo visitAction =
    if not (v.vglob || isFunctionType v.vtype)
    then free_vars <- S.add v free_vars;
    SkipChildren

  method vstmt (s: stmt) : stmt visitAction = match s.skind with
  | CpcFun (f, _) ->
      assert(f.sformals = []);
      let old_fv = free_vars in
      free_vars <- S.empty;
      ChangeDoChildrenPost (s, fun s ->
        let f_fv = S.elements free_vars in
        let f_formals = List.map (fun v -> copyVarinfo v v.vname) f_fv in
        let f_args = List.map (fun v -> Lval(Var v, NoOffset)) f_fv in
        let map = List.combine f_fv f_formals in
        setFormals f f_formals;
        f.sbody <- visitCilBlock (new replaceVars map) f.sbody;
        (* FIXME Ensure name uniqueness *)
        local_funs <- GFun(f,locUnknown) :: local_funs;
        local_proto <- GVarDecl(f.svar,locUnknown) :: local_proto;
        local_map <- (f.svar,f_args) :: local_map;
        free_vars <- S.union free_vars old_fv;
        mkEmptyStmt())
  | _ -> DoChildren

  method vglob (g: global) : global list visitAction = match g with
  | GFun _ -> ChangeDoChildrenPost ([g], fun g ->
      (* TODO: use CLists instead? *)
      let res = local_proto @ local_funs @ g in
      let insert_vis = new insertArgs local_map in
      let insert_args g = match visitCilGlobal insert_vis g with
        | [g] -> g
        | _ -> assert false in
      local_funs <- [];
      local_proto <- [];
      local_map <- [];
      free_vars <- S.empty;
      List.map insert_args res)
  | _ -> SkipChildren

end

(************** Functionalize Goto *******************************************)

exception GotoContent of stmt list

let make_function_name =
  let i = ref 0 in
  fun base -> incr i; Printf.sprintf "__cpc_%s_%d" base !i

exception FoundType of typ

class hasReturn = object(self)
  inherit nopCilVisitor

  method vstmt s = match s.skind with
  | Return (Some e, _) -> raise (FoundType (typeOf e))
  | Return (None, _) -> raise (FoundType voidType)
  | _ -> DoChildren
end

(* return the type of the first Return in a statement *)
let has_return s =
  try
    ignore(visitCilStmt (new hasReturn) s);
    None
  with FoundType t -> Some t

class functionalizeGoto start file =
      let enclosing_fun = enclosing_function start file in
      let label = match List.find is_label start.labels with
        | Label(l,_,_) -> l | _ -> assert false in
      let fd = emptyFunction (make_function_name label) in
      let ret_type = fst4 (splitFunctionTypeVI enclosing_fun.svar) in
      let ret_var =
        if typeSig ret_type = typeSig voidType
        then None
        else Some (makeTempVar enclosing_fun ~name:"ret_var" ret_type) in
      let () = fd.svar.vcps <- true in
      object(self)
        inherit nopCilVisitor

        val mutable acc = false (* are we in an accumulating phase?*)
        val mutable stack = []
        val mutable do_return = false (* does the stack contain return
        statements *)
        val mutable last_stmt = dummyStmt; (* last stmt processed -- the
        final goto must be added there when we're done accumulating *)
        val mutable new_start = dummyStmt; (* copy of start in the stack
        -- probably useless, should be equal to last stmt in stack *)

        method private unstack_block b =
          let return_val, return_exp =
            begin match ret_var with
              | Some ret_var when do_return ->
              let ret_val = Var ret_var,NoOffset in
              setFunctionType fd (TFun (ret_type,Some [],false,[]));
              (Some ret_val, Some (Lval ret_val))
              | _ -> None, None
            end in
          let call_fun loc = [
            mkStmt (Instr
              [Call(return_val,Lval(Var fd.svar, NoOffset),[],loc)]);
            mkStmt (Return (return_exp, loc))] in
          acc <- false;
          fd.sbody <- mkBlock (List.rev stack);
          stack <- [];
          b.bstmts <- compactStmts (
            [ mkStmt (Block (mkBlock(b.bstmts)));
              mkStmt (CpcFun (fd, locUnknown))]
            @ call_fun locUnknown);
          assert(new_start != dummyStmt);
          assert(new_start == List.hd fd.sbody.bstmts);
          visitCilFileSameGlobals (new replaceGotos new_start call_fun) file

        method vstmt (s: stmt) : stmt visitAction =
          last_stmt <- s;
          if s == start then acc <- true;
          (if acc then match has_return s with
          | None -> ()
          | Some r when typeSig ret_type = typeSig r ->
              do_return <- true
          | _ -> E.s (E.error "conflicting return types\n"));
          ChangeDoChildrenPost(s,
          (fun s ->
            if acc then
              ( let copy = copyClearStmt s file stack in
                stack <- copy :: stack;
                if s == start then new_start <- copy;
               s)
            else s))

        method vblock (b: block) : block visitAction =
          let old_acc = acc in
          let enclosing = last_stmt in
          ChangeDoChildrenPost(
            (acc <- false; b),
            (fun b ->
              last_stmt <- enclosing;
              if acc
              then begin match (compactStmts stack), enclosing.succs with
              | [], _ -> assert false
              (*XXX BULLSHIT!!!!!!!
               * | [x], _ -> (* only stacked the labeled statement, at
                           * the end of the block: substitute in place *)
                  let subst _loc = [x] in
                  acc <- false;
                  visitCilFileSameGlobals (new replaceGotos start subst) file;
                  b*)
              | _, []
              | {skind=Return _} :: _ , _
              | {skind=CpcDone _} :: _ , _ ->
                  self#unstack_block b; b
              | last_in_stack :: _, _ ->
                  (* XXX this works only because we do not modify *any*
                   * statement while visiting the file --- otherwise
                   * the destination label is somehow deleted when
                   * returning from vblock. *)
                  self#unstack_block b;
                  add_goto_after last_in_stack enclosing file stack;
                  b
              end
              else (acc <- old_acc ; b)))
end

exception BreakContinue of stmt

(*exception LiveStmt of stmt

let rec choose_stmt set start =
  try
    let ((_,s) as x) = LabelSet.choose set in
    if s == start
    then choose_stmt (LabelSet.remove x set) start
    else Some s
  with Not_found -> None
*)

(* Look for break/continue in statements to be functionalized ---
XXX code must be kept in sync with functionalizeGotos !!! ---
and return the nearest enclosing loop/switch statement *)
class findEnclosing = fun start -> object(self)
  inherit nopCilVisitor

  val mutable nearest_loop = dummyStmt
  val mutable seen_start = false
  val mutable check_loops = false

  method vstmt s =
    (* check that (check_loops => seen_start) holds *)
    assert((not check_loops) || seen_start);
    if s == start then (seen_start <- true; check_loops <- true);
    match s.skind with
    | Break _ | Continue _ when check_loops ->
        raise (BreakContinue nearest_loop)
    | Switch _ | Loop _ ->
      let e = nearest_loop in
      let cl = check_loops in
        ChangeDoChildrenPost(
          (nearest_loop <- s; check_loops <- false; s),
          (fun s ->
            nearest_loop <- e;
            check_loops <- cl;
            s))
    | _ -> DoChildren

  method vblock b =
    let seen = seen_start in
    let cl = check_loops in
    ChangeDoChildrenPost(b, fun b ->
      (*let live_stmt =
        if seen_start
        then choose_stmt (live_labels b) start
        else None in
      match live_stmt with
      | Some s -> raise (LiveStmt s)
      | None ->*) (seen_start <- seen; check_loops <- cl; b))

end

let rec functionalize start f =
  begin try
    visitCilFileSameGlobals (new findEnclosing start) f;
    visitCilFileSameGlobals (new functionalizeGoto start f) f;
  with
  | BreakContinue s ->
      assert( s != dummyStmt);
      E.log "found escaping break or continue: trivializing\n%a\n" d_stmt s;
      eliminate_switch_loop s;
      E.log "***\nresult:\n%a\n" d_stmt s;
  (*| LiveStmt s ->
      E.log "found a live label: functionalizing it\n%a\n" d_stmt s;
      functionalize s f*)
  | Not_found -> E.s (E.bug "no enclosing function found\n")
  end

(*****************************************************************************)

let init () = lineDirectiveStyle := None

let pause = ref false

let rec doit (f: file) =
  try
    E.log "********************* doit ******************\n";
    visitCilFileSameGlobals (new cleaner) f;
    let r = if !pause then read_line () else "" in
    if r = "q" then E.log "quit!\n" else
    if r = "d" then (dumpFile defaultCilPrinter stdout "" f; doit f)
    else if r = "r" then (pause := false; doit f)
    else begin
    visitCilFileSameGlobals (new markCps f) f;
    E.log "Lambda-lifting\n";
    visitCilFile (new lambdaLifter) f;
    (*visitCilFile (new cpsConverter) f;*)
    E.log "Cleaning things a bit\n";
    visitCilFileSameGlobals (new cleaner) f;
    uniqueVarNames f; (* just in case *)
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
        | t::q as l when t==i -> (List.rev acc, l)
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
      functionalize start f;
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

