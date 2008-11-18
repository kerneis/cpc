open Cil
module E = Errormsg

(* Split an instruction list given an instruction *)
exception SplitInstr of stmt * instr

(* Eliminate break and continue in a switch/loop *)
exception TrivializeStmt of stmt

let is_label = function Label _ -> true | _ -> false

exception FoundFun of fundec
exception FoundVar of varinfo

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
  let res = {(mkEmptyStmt()) with skind = s.skind; labels = s.labels; cps=s.cps} in
  let new_goto loc = [mkStmt (Goto (ref res, loc))] in
  if (List.exists is_label s.labels)
  then (
    let vis = new replaceGotos s new_goto in
    visitCilFileSameGlobals vis file;
    List.iter (fun s -> ignore(visitCilStmt vis s)) stack);
  s.skind <- Instr [];
  s.labels <- [];
  s.cps <- false;
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

let add_goto src dst file =
  assert (src != dummyStmt && dst != dummyStmt);
  E.log "add goto from %a\nto %a\n" d_stmt src d_stmt dst;
  let (src_loc,dst_loc) = (get_stmtLoc src.skind, get_stmtLoc dst.skind) in
  (* XXX DO NOT USE copyClearStmt HERE --- we want to keep the labels *)
  let src' = {(mkEmptyStmt()) with skind=src.skind; cps = src.cps} in
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
  add_goto src dst file;
  enclosing.skind <- Block (mkBlock ([
    copy;
    dst]))

(* return the cps var just before a statement *)
let rec find_var = function
  | [Call (Some (Var v, NoOffset), Lval (Var f, NoOffset), _, _)]
      when f.vcps ->
        FoundVar v
  | [] | [_] -> Not_found
  | hd::tl -> find_var tl

(* find one of the following patterns:
  <cps call>; goto l; label l: <start>;
or
  <cps call>; cpc_yield; goto l; label l: <start>;
and extract the last var of <cps call>.
*)
class lastVar = fun start -> object(self)
  inherit nopCilVisitor

  method vstmt s = match s.skind, s.succs with
  | Goto(g, _), [succ] when !g == start && !g == succ -> begin
      match s.preds with
      | [{skind=Instr l} as p] when p.cps ->
          raise (find_var l)
      | [{skind=CpcYield _} as p] when p.cps -> begin
        match p.preds with
        | [{skind=Instr l} as p'] when p'.cps ->
            raise (find_var l)
        | _ -> raise Not_found
      end
      | _ -> raise Not_found
  end
  | _ -> DoChildren

end

let last_var start file =
  try
    visitCilFileSameGlobals (new lastVar start) file;
    None
  with
  | FoundVar v ->
      E.log "found last_var %a\n" d_lval (Var v, NoOffset);
      Some v
  | Not_found -> None

(* return the nearest enclosing function of a statement *)
class enclosingFunction = fun fd_opt -> object(self)
  inherit nopCilVisitor

  val mutable ef = fd_opt

  method vfunc f =
    let f' = ef in
    ef <- Some f;
    ChangeDoChildrenPost(f, fun f ->
      ef <- f';f)
end

let enclosing_function start file =
  let visitor = object(self)
    inherit (enclosingFunction None)

    method vstmt s =
      if s == start
      then (match ef with Some f -> raise (FoundFun f) | None -> assert false)
      else DoChildren
  end in
  try
    visitCilFileSameGlobals visitor file;
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

(* Functionalize a statement with a label *)
exception FunctionalizeGoto of stmt * mark_context

exception SplitYield of stmt

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
      raise (FunctionalizeGoto (s,c)))
    else begin
      if c.cps_con then assert(s.labels = []); (* no Case or Default in cps
                                                * context *)
      let lv = c.last_var in match s.skind with

    (* Instructions *)
    (* Must be first, to catch last_var if necessary *)
    | Instr [] ->
        self#set_next ~set_last:false s;
        s.cps <- c.cps_con;
        SkipChildren
    | Instr (hd::_) when c.cps_con && not(self#is_cps hd) ->
        E.log "not cps instruction in cps context: %a\n" d_instr hd;
        raise (AddGoto c)
    | Instr _ ->
        (* if in cps_context, c.last_var has been updated by the former
         * pattern-matching, so we must restore it *)
        if c.cps_con then c.last_var <- lv;
        c.last_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          self#set_next s;
          s.cps <- c.cps_con;
          s
        )

    (* Return, cpc_done and cpc_yield *)

    | Return _ ->
        if c.cps_fun
        then begin
          s.cps <- true;
          c.cps_con <- false;
          c.last_stmt <- dummyStmt;
          c.next_stmt <- dummyStmt;
        end;
        SkipChildren
    | CpcDone _ when c.cps_fun ->
        (* just like a Return *)
        s.cps <- true;
        c.cps_con <- false;
        c.last_stmt <- dummyStmt;
        c.next_stmt <- dummyStmt;
        SkipChildren

      (* cpc_yield should be split, except if it has already been done *)
    | CpcYield _ when s.cps ->
        c.cps_con <- true;
        self#set_next s;
        SkipChildren
    | CpcYield _ ->
        s.cps <- true; (* leave a mark for next time *)
        raise (SplitYield s)

    (* Control flow in cps context *)
    | Goto (g, _) when c.cps_con ->
        raise (FunctionalizeGoto (!g,c))
    | Break _ | Continue _ when c.cps_con ->
        raise (TrivializeStmt c.enclosing_stmt);
    | If _ | Switch _ | Loop _ when c.cps_con ->
        E.log "control flow in cps context\n";
        raise (AddGoto c)
    | _ when c.last_var != None ->
        E.log "return variable ignored in cps context\n";
        raise (AddGoto c)

    (* Control flow otherwise *)
    | Goto (g, _)
        when enclosing_function s file != enclosing_function !g file ->
          E.log "live goto!\n";
          raise (FunctionalizeGoto (!g,c))
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

    (* CPC Constructs *)
    | CpcSpawn _ ->
        s.cps <- c.cps_con;
        self#set_next s;
        SkipChildren
    | CpcWait _ | CpcSleep _ | CpcIoWait _
        when c.cps_fun -> (* Beware, order matters! *)
          c.cps_con <- true; (* must be set first *)
          s.cps <- true;
          self#set_next s;
          SkipChildren
    | CpcDone _ | CpcWait _ | CpcSleep _
    | CpcIoWait _ ->
        E.s (E.error "CPC construct not allowed here: %a" d_stmt s)
    | CpcFun _ -> (* saving and restoring context is done in vfunc *)
        ChangeDoChildrenPost
          (s, fun s -> s.cps <- c.cps_con; self#set_next ~set_last:false s; s)

    (* Blocks *)
    | Block _ ->
        self#set_next ~set_last:false s;
        ChangeDoChildrenPost
          (s, fun s -> s.cps <- c.cps_con; s)

    | TryFinally _ | TryExcept _ ->
        E.s (E.unimp "try/except/finally not supported by CPC")

    end

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

module H = Hashtbl

let h = H.create 32

let get_map fd =
  try H.find h fd with Not_found -> []

let extend_map fd map =
  let l = get_map fd in
  (* we shall not overwrite any previous binding *)
  assert(List.for_all (fun (x,_) -> not (List.mem_assq x l)) map);
  H.replace h fd (map @ l)

(* extend the map associated to fd and replace variables accordingly *)
let replace_vars fd map =
  let visitor = object(self)
    inherit nopCilVisitor

    val mutable current_map = map

    method vvrbl (v:varinfo) : varinfo visitAction =
      try
        ChangeTo(
          let r = List.assoc v current_map in
          E.log "%s(%d)->%s(%d)\n" v.vname v.vid r.vname r.vid;
          r)
    with
    Not_found -> SkipChildren

    method vstmt s = match s.skind with
    | CpcFun(fd, _) ->
        let m = current_map in
        current_map <- (get_map fd)@m;
        ChangeDoChildrenPost(s, fun s ->
          current_map <- m; s)
    | _ -> DoChildren
  end in
  extend_map fd map;
  fd.sbody <- visitCilBlock visitor fd.sbody

class replaceVisitor = fun enclosing -> object(self)
    inherit (enclosingFunction (Some enclosing))

    method vvrbl (v:varinfo) : varinfo visitAction =
      let map = match ef with Some f -> get_map f | None -> assert false in
      try
        ChangeTo(
          let r = List.assoc v map in
          E.log "%s(%d)->%s(%d)\n" v.vname v.vid r.vname r.vid;
          r)
      with Not_found -> SkipChildren

  end

(* return a list of local cps functions in a function *)
let collect_local_fun fd =
  let fun_list = ref [] in
  let collector = object(self)
    inherit nopCilVisitor

    method vstmt s = match s.skind with
    | CpcFun (fd, _) ->
        fun_list := fd :: !fun_list;
        DoChildren
    | _ -> DoChildren
  end in
  ignore(visitCilFunction collector fd);
  !fun_list

(* remove local functions *)
let remove_local_fun fd =
  visitCilFunction (object(self)
    inherit nopCilVisitor

    method vstmt s = match s.skind with
    | CpcFun (fd, _) ->
        ChangeTo(mkEmptyStmt())
    | _ -> DoChildren
  end) fd

(* return a set of free variables in a function *)
let free_vars fd =
  let args = List.fold_left (fun s x -> S.add x s) S.empty fd.sformals in
  let bounded = List.fold_left (fun s x -> S.add x s) args fd.slocals in
  let vars = ref S.empty in
  let collector = object(self)
    inherit nopCilVisitor

    method vvrbl v =
      if not (v.vglob || isFunctionType v.vtype)
      then vars := S.add v !vars;
      SkipChildren

    method vstmt s = match s.skind with
    | CpcFun _ -> SkipChildren
    | _ -> DoChildren
  end in
  ignore(visitCilFunction collector fd);
  S.diff !vars bounded

(* remove free variable thanks to an iterated lambda-lifting technique:
  - find free vars of a local function
  - add them as parameters of this function and insert them at every call point
  - proceed with the next function until none has free vars left
  Return the list of globals made out of (removed) local functions *)
let remove_free_vars enclosing_fun loc =
  let rec make_globals gfuns = function
    | [] -> GFun(remove_local_fun enclosing_fun,loc) :: (* List.rev *) gfuns
    | fd :: tl ->
        GVarDecl(fd.svar,locUnknown) ::
          make_globals (GFun(remove_local_fun fd,locUnknown) :: gfuns) tl in
  let introduce_new_vars fd fv =
    let new_formals = List.map (fun v -> copyVarinfo v v.vname) fv in
    let new_args = List.map (fun v -> Lval(Var v, NoOffset)) fv in
    let map = List.combine fv new_formals in
    let insert =
      object(self)
        inherit (replaceVisitor enclosing_fun)

        method vinst = function
          | Call(lval, Lval ((Var f, NoOffset) as l), args, loc)
          when f == fd.svar ->
            let args' = args @ new_args in
            E.log "inserting in %a\n" d_lval l;
            ChangeDoChildrenPost([Call(lval, Lval(Var f, NoOffset), args',
            loc)], fun x -> x)
          | _ -> SkipChildren

        method vstmt s = match s.skind with
          | CpcSpawn(Lval ((Var f, NoOffset) as l), args, loc)
          when f == fd.svar ->
            let args' = args @ new_args in
            E.log "inserting in %a\n" d_lval l;
            s.skind <- CpcSpawn(Lval(Var f, NoOffset), args', loc);
            DoChildren
          | _ -> DoChildren
      end in
    setFormals fd (fd.sformals @ new_formals);
    (* XXX Beware, order matters! replaceVar must be called BEFORE insert, to
     * update the global map (h) which is then used by insert *)
    replace_vars fd map;
    enclosing_fun.sbody <- visitCilBlock insert enclosing_fun.sbody;
    E.log "%a\n" d_global (GVarDecl(fd.svar,locUnknown)) in
  let rec iter = function
  | fd :: tl ->
      let fv = free_vars fd in
      if S.is_empty fv
      then iter tl
      else begin
        E.log "free variables in %s:\n" fd.svar.vname;
        S.iter (fun v -> E.log "- %s(%d)\n" v.vname v.vid) fv;
        introduce_new_vars fd (S.elements fv);
        (* XXX It is necessary to restart from scratch with a fresh list of
         * local functions, for they have changed when introducing new vars *)
        E.log "Result:\n%a\n**************\n" d_global (GFun (enclosing_fun,loc));
        iter (collect_local_fun enclosing_fun)
      end
  | [] -> make_globals [] (collect_local_fun enclosing_fun) in
  iter (collect_local_fun enclosing_fun)

class lambdaLifter = object(self)
  inherit nopCilVisitor

  method vglob (g: global) : global list visitAction = match g with
  | GFun (fd,loc) -> ChangeTo(remove_free_vars fd loc)
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
      let last_var = last_var start file in
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
          let args, map = match last_var with
          | None -> [], []
          | Some v ->
              let v' = copyVarinfo v v.vname in
              setFormals fd [v'];
              [Lval(Var v, NoOffset)], [(v, v')] in
          let call_fun loc = [
            mkStmt (Instr
              [Call(return_val,Lval(Var fd.svar, NoOffset),args,loc)]);
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
          assert(List.for_all is_label new_start.labels); (*No Case or Default*)
          (* XXX Beware, order matters! replaceVars must be called AFTER
           * replaceGotos, for fd.sbody might contain recursive calls to itself *)
          visitCilFileSameGlobals (new replaceGotos new_start call_fun) file;
          replace_vars fd map;
          new_start.labels <- []

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
              then begin match (compactStmts stack), enclosing.succs,
              enclosing.skind with
              (* Loops should have been trivialized first, and stack should
               * contain at least the <start> stmt *)
              | [], _, _ | _, _, Loop _ -> assert false
              | _, [], _
              | _, _, CpcFun _
              | {skind=Return _} :: _ , _, _
              | {skind=CpcDone _} :: _ , _, _ ->
                  self#unstack_block b; b
              | last_in_stack :: _, _, _ ->
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
      add_goto src dst f;
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
        add_goto s1 s2 f;
        end;
      doit f
  | SplitInstr (s, _) ->
      E.s (E.bug "SplitInstr raised with wrong argument %a" d_stmt s)
  | FunctionalizeGoto (start,c) ->
      E.log "functionalize goto\n";
      begin match c.enclosing_stmt.skind with
      | Switch _ | Loop _ ->
          E.log "enclosing is a switch or a loop: trivializing first\n";
          eliminate_switch_loop c.enclosing_stmt;
          doit f
      | _ -> functionalize start f; doit f
      end
  | SplitYield ({skind=CpcYield l} as s) ->
      let (s1,s2) = (copyClearStmt s f [], mkStmt (Instr [])) in
      E.log "SplitYield\n";
      s.skind <- Block (mkBlock ([s1; s2]));
      add_goto s1 s2 f;
      doit f
  | SplitYield s ->
      E.s (E.bug "SplitYield raised with wrong argument %a" d_stmt s)
  | Exit -> E.log "Exit\n";()

let feature : featureDescr =
  { fd_name = "cpc";
    fd_enabled = ref false;
    fd_description = "cpc translation to C";
    fd_extraopt = [];
    fd_doit = (fun f -> init() ; doit f);
    fd_post_check = true;
  }

