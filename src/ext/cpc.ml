(*
 * Copyright (c) 2008-2010,
 *  Gabriel Kerneis     <kerneis@pps.jussieu.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open Cil
open Cilint
open Pretty
module E = Errormsg

module S = Usedef.VS

(* Split an instruction list given an instruction
 * the boolean tells whether a goto should be added *)
exception SplitInstr of stmt * instr * bool

(* Eliminate break and continue in a switch/loop *)
exception TrivializeStmt of stmt

let is_label = function Label _ -> true | _ -> false

exception FoundFun of fundec
exception FoundVar of varinfo

let external_patch = ref true

let aligned_continuations = ref true

(* Avoid stack-overflow on recursive structures *)
let (=) x y = (compare x y) = 0

let (=>) a b = (not a)||b

let trace = Trace.trace "cpc"
let stats = Trace.trace "cpc_stats"

(* recursing into vtype is very expensive in terms of memory allocation,
   and thus time spent in the GC. We try as much as possible to use this
   customized base visitor, which avoids this pitfall.
   WARNING: vtype goes into attributes and TArray, both of them
   containing expressions, and thus variables. So, do NOT use this
   visitor if you need to inspect and/or change expressions or
   variables. *)
class mynopCilVisitor = object(self)
    inherit nopCilVisitor
    method vtype v = SkipChildren
end

(*************** Utility functions *******************************************)

(* From src/cil.ml, modified to transform loops as well *)
let switch_count = ref (-1)
let get_switch_count () =
  incr switch_count;
  !switch_count

let switch_label = ref (-1)

let break_dest = ref dummyStmt
let cont_dest = ref dummyStmt

let labelTbl = ref []
let recordGoto s =
  match s.skind with
  | Goto(g, _) ->
      labelTbl := (!g,s)::!labelTbl;
      trace (dprintf "recorded:\n%a\n" d_stmt !g);
      s
  | _ -> s

class initLabelTbl = object(self)
    inherit mynopCilVisitor
    method vstmt s =
      ignore(recordGoto s);
      DoChildren
end

module VS = Set.Make (struct
  type t = varinfo
  let compare v v' = compare v.vid v'.vid
end)

module StringSet = Set.Make (struct
  type t = string
  let compare = compare
end)

let ampSet = ref VS.empty
let add_amp v = ampSet := VS.add v !ampSet
let safe_functions = ref StringSet.empty
let add_safe f = safe_functions := StringSet.add f !safe_functions


class initSafeFunctions = object(self)
    inherit nopCilVisitor
    method vglob = function
    | GPragma(Attr("cpc_no_retain", l), _) ->
        let sf = String.concat ", " (List.map (function
          | AStr f -> add_safe f; f
          | p -> E.s (E.error "wrong parameter in cpc_no_retain: %a" d_attrparam p))
          l) in
        ChangeTo [GText (Printf.sprintf "/* safe functions: %s*/" sf)]
    | _ -> SkipChildren
end


let is_safe f =
      hasAttribute "cpc_no_retain" f.vattr ||
      StringSet.mem f.vname !safe_functions


class initAmpSet = object(self)
    inherit mynopCilVisitor

    val mutable record = false
    val mutable call_name = ""

    method vinst :  instr -> instr list visitAction = function
    | Call(_, Lval(Var f, NoOffset), args, _) when not f.vcps && is_safe f ->
        assert(not record && call_name = "");
        call_name <- f.vname;
        ignore(List.map (visitCilExpr (self:>cilVisitor)) args);
        call_name <- "";
        SkipChildren
    | Call(_, Lval(Var f, NoOffset), args, _) when not f.vcps ->
        assert(not record && call_name = "");
        record <- true;
        call_name <- f.vname;
        ignore(List.map (visitCilExpr (self:>cilVisitor)) args);
        call_name <- "";
        record <- false;
        SkipChildren
    | Call(_, _, args, _) ->
        assert(not record && call_name = "");
        record <- true;
        ignore(List.map (visitCilExpr (self:>cilVisitor)) args);
        record <- false;
        SkipChildren
    | Set(_, e, _) ->
        assert(not record);
        record <- true;
        ignore(visitCilExpr (self:>cilVisitor) e);
        record <- false;
        SkipChildren
    | Asm _ as i ->
        assert(not record);
        record <- true;
        ChangeDoChildrenPost([i], fun i ->
          record <- false; i)

    method vexpr = function
    | AddrOf(Var v, _) when record && not v.vglob ->
        if(not(call_name = "")) then
          ignore(warn "boxing %s, consider making %s a safe function."
          v.vname call_name);
        trace (dprintf "[boxing] registering %s\n" v.vname);
        add_amp v;
        DoChildren
    | AddrOf(Var v, _) ->
        if v.vglob then
          trace(dprintf "[boxing] skipped global variable %s (%t).\n"
          v.vname d_thisloc)
        else if(not(call_name = "")) then
          trace(dprintf "[boxing] skipped %s, because %s is a safe function. (%t)\n"
          v.vname call_name d_thisloc)
        else
          trace(dprintf "[boxing] skipped %s because it cannot be retained (%t).\n"
          v.vname d_thisloc);
        DoChildren
    | _ -> DoChildren
    
    method vfunc fd =
      (* No CpcFun thanks to initial lambda-lifting *)
      if fd.svar.vcps then begin
        trace(dprintf "[boxing] %s %d\n" fd.svar.vname (List.length fd.sformals + List.length
        fd.slocals));
        List.iter (fun v -> if isArrayType v.vtype then begin 
          trace (dprintf "[boxing] boxing array %s unconditionnally\n" v.vname);
          add_amp v
        end) fd.slocals;
        if(List.filter (fun v -> isArrayType v.vtype) fd.sformals != []) then
          E.s (E.bug "Array found in function parameters, I'm lost!\n");
        DoChildren
      end else
        SkipChildren
end

let rec xform_switch_stmt s = begin
  if not(!break_dest == dummyStmt) then
  s.labels <- Util.list_map (fun lab -> match lab with
    Label _ -> lab
  | Case(e,l) ->
      let suffix =
	match getInteger e with
	| Some value ->
	    if compare_cilint value zero_cilint < 0 then
	      "neg_" ^ string_of_cilint (neg_cilint value)
	    else
	      string_of_cilint value
	| None ->
	    incr switch_label;
	    "exp_" ^ string_of_int !switch_label
      in
      let str = Pretty.sprint !lineLength
	  (Pretty.dprintf "switch_%d_%s" !switch_count suffix) in
      (Label(str,l,false))
  | Default(l) -> (Label(Printf.sprintf
                  "switch_%d_default" !switch_count,l,false))
  ) s.labels ;
  match s.skind with
  | Instr _ | Return _ | Goto _ -> ()
  | Break(l) -> if (not(!break_dest == dummyStmt)) then
                s.skind <- Goto(ref !break_dest,l);
                ignore(recordGoto s)
  | Continue(l) -> assert(not(!cont_dest == dummyStmt));
                s.skind <- Goto(ref !cont_dest,l);
                ignore(recordGoto s)
  | If(e,b1,b2,l) -> xform_switch_block b1;
                     xform_switch_block b2
  | Switch(_,b,_,_) when not(!cont_dest == dummyStmt)->
      let s = !break_dest in
      break_dest := dummyStmt;
      xform_switch_block b;
      break_dest := s;
  | Switch _ | Loop _ -> ()
  | Block(b) -> xform_switch_block b

  | TryExcept _ | TryFinally _ ->
      failwith "xform_switch_statement: structured exception handling not implemented"
  | CpcFun _ | CpcSpawn _ -> ()

end and xform_switch_block b =
  try
    let rec link_succs sl = match sl with
    | [] -> ()
    | hd :: tl -> (if hd.succs = [] then hd.succs <- tl) ; link_succs tl
    in
    link_succs b.bstmts ;
    List.iter (fun stmt -> xform_switch_stmt stmt) b.bstmts ;
  with e ->
    List.iter (fun stmt -> ignore
      (warn "(cpc) xform_switch_block: %a@!" d_stmt stmt)) b.bstmts ;
    raise e

let eliminate_switch_loop s = match s.skind with
  | Switch(e,b,sl,l) -> begin
      let i = get_switch_count () in
      let break_stmt = mkStmt (Instr []) in
      break_stmt.labels <-
				[Label((Printf.sprintf "switch_%d_break" i),l,false)] ;
      let break_block = mkBlock [ break_stmt ] in
      let body_block = b in
      let body_if_stmtkind = (If(zero,body_block,break_block,l)) in

      (* The default case, if present, must be used only if *all*
      non-default cases fail [ISO/IEC 9899:1999, §6.8.4.2, ¶5]. As a
      result, we sort the order in which we handle the labels (but not the
      order in which we print out the statements, so fall-through still
      works as expected). *)
      let compare_choices s1 s2 = match s1.labels, s2.labels with
      | (Default(_) :: _), _ -> 1
      | _, (Default(_) :: _) -> -1
      | _, _ -> 0
      in

      let rec handle_choices sl = match sl with
        [] -> body_if_stmtkind
      | stmt_hd :: stmt_tl -> begin
        let rec handle_labels lab_list = begin
          match lab_list with
            [] -> handle_choices stmt_tl
          | Case(ce,cl) :: lab_tl ->
              let pred = BinOp(Eq,e,ce,intType) in
              let then_block = mkBlock [ recordGoto(mkStmt (Goto(ref stmt_hd,cl))) ] in
              let else_block = mkBlock [ mkStmt (handle_labels lab_tl) ] in
              If(pred,then_block,else_block,cl)
          | Default(dl) :: lab_tl ->
              (* ww: before this was 'if (1) goto label', but as Ben points
              out this might confuse someone down the line who doesn't have
              special handling for if(1) into thinking that there are two
              paths here. The simpler 'goto label' is what we want. *)
              Block(mkBlock [ recordGoto(mkStmt (Goto(ref stmt_hd,dl))) ;
                              mkStmt (handle_labels lab_tl) ])
          | Label(_,_,_) :: lab_tl -> handle_labels lab_tl
        end in
        handle_labels stmt_hd.labels
      end in
      s.skind <- handle_choices (List.sort compare_choices sl) ;
      break_dest := break_stmt;
      xform_switch_block b;
      break_dest := dummyStmt;
    end
  | Loop(b,l,_,_) ->
          let i = get_switch_count () in
          let break_stmt = mkStmt (Instr []) in
          break_stmt.labels <-
						[Label((Printf.sprintf "while_%d_break" i),l,false)] ;
          let cont_stmt = mkStmt (Instr []) in
          cont_stmt.labels <-
						[Label((Printf.sprintf "while_%d_continue" i),l,false)] ;
          b.bstmts <- cont_stmt :: b.bstmts @ [mkStmt(Continue l); break_stmt] ;
          break_dest := break_stmt;
          cont_dest := cont_stmt;
          xform_switch_block b;
          break_dest := dummyStmt;
          cont_dest := dummyStmt;
          s.skind <- Block b
  | _ -> assert false

let fst4 (x,_,_,_) = x

(* override Cil.sizeOf *)
let sizeOf t = SizeOf(t)

let addr_of v =
  v.vaddrof <- true;
  mkAddrOrStartOf (Var v, NoOffset)

let cut_last l = let l' = List.rev l in
  List.rev (List.tl l'), List.hd l'

let first_instr s = match s.skind with
  | Instr (hd::_) -> hd
  | _ -> raise (Invalid_argument "first_instr")

let replaceGotos start replace_with =
  let perform_replace (_,s) = match s.skind with
  | Goto (g, loc) -> (
      assert(!g == start);
      s.skind <- (match replace_with s with
      | [] -> Instr []
      | l -> Block (mkBlock l))
  )
   | _ -> assert false  in
  let (gotos, others) = List.partition (fun (x,_) -> x==start) !labelTbl in
  labelTbl := others;
  List.iter perform_replace gotos

(* Return a copy of statement, clear the initial one and update gotos
accordingly *)
let copyClearStmt s =
  trace (dprintf "CLEARING %a\n" d_stmt s);
  let res = {(mkEmptyStmt()) with skind = s.skind; labels = s.labels; cps=s.cps} in
  let new_goto s = [recordGoto(mkStmt (Goto (ref res, get_stmtLoc s.skind)))] in
  (if List.exists is_label s.labels
  then replaceGotos s new_goto);
  (match s.skind with Goto (g, _) ->
    labelTbl := List.map (fun (dst,src) -> if src == s then
      (assert(!g == dst); (dst,res)) else (dst,src)) !labelTbl
   | _ -> ());
  s.skind <- Instr [];
  s.labels <- [];
  s.cps <- false;
  res

let timestamp () =
  let t = Unix.gettimeofday() in
  let sec = int_of_float t in
  let msec = int_of_float ((t -. floor(t)) *. 1000.) in
  Printf.sprintf "%u%u" sec msec

let make_label =
  let i = ref 0 in
  fun () -> incr i; Printf.sprintf "add_goto%d" !i

let add_goto src dst =
  assert (src != dummyStmt && dst != dummyStmt);
  trace (dprintf "add goto from %a\nto %a\n" d_stmt src d_stmt dst);
  let (src_loc,dst_loc) = (get_stmtLoc src.skind, get_stmtLoc dst.skind) in
  (* XXX DO NOT USE copyClearStmt HERE --- we want to keep the labels *)
  let src' = {(mkEmptyStmt()) with skind=src.skind; cps = src.cps} in
  src.skind <- Block (mkBlock ([
    src';
    recordGoto(mkStmt (Goto (ref dst, src_loc)));
    ]));
  dst.labels <- [Label (make_label(), dst_loc, false)]

let add_goto_after src enclosing =
  trace (dprintf "add_goto_after: enclosing is %a\n" d_stmt enclosing);
  let dst = mkEmptyStmt() in
  let copy = copyClearStmt enclosing in
  add_goto src dst;
  enclosing.skind <- Block (mkBlock ([
    copy;
    dst]))

(* Make a variable to hold the return value of a given function *)
(* Do NOT make a single variable per function anymore, it could prevent
   them from being optimized by percolating. *)
let make_ret_var fd typ =
  makeTempVar fd ~name:"__retres" typ

(* return the cps var just before a statement *)
let rec find_var = function
  | [Call (Some (Var v, NoOffset), Lval (Var f, NoOffset), _, _)]
      when f.vcps ->
        FoundVar v
  | [] | [_] -> Not_found
  | hd::tl -> find_var tl

(* find the following pattern:
  <cps call>; goto l; label l: <start>;
and extract the last var assigned in <cps call>.
*)
class lastVar = fun start -> object(self)
  inherit mynopCilVisitor

  method vstmt s = match s.skind, s.succs with
  | Goto(g, _), [succ] when !g == start && !g == succ -> begin
      match s.preds with
      | [{skind=Instr l} as p] when p.cps ->
          raise (find_var l)
      | _ -> raise Not_found
  end
  | _ -> DoChildren

end

let find_last_var start file =
  try
    visitCilFileSameGlobals (new lastVar start) file;
    None
  with
  | FoundVar v ->
      trace (dprintf "found last_var %a\n" d_lval (Var v, NoOffset));
      Some v
  | Not_found -> None

(* return the nearest enclosing function of a statement *)
class enclosingFunction = fun fd_opt -> object(self)
  inherit mynopCilVisitor

  val mutable ef = fd_opt

  method vfunc f =
    let f' = ef in
    ef <- f;
    ChangeDoChildrenPost(f, fun f ->
      ef <- f';f)
end

let enclosing_function start file =
  let visitor = object(self)
    inherit (enclosingFunction dummyFunDec)

    method vstmt s =
      if s == start
      then begin
        assert(not(ef == dummyFunDec));
        raise (FoundFun ef)
      end
      else DoChildren
  end in
  try
    visitCilFileSameGlobals visitor file;
    trace (dprintf "enclosing function not found for: %a\n" d_stmt start);
    raise Not_found
  with FoundFun f -> f

let stmt_in_fun stmt fd =
    let visitor = object(self)
      inherit mynopCilVisitor
      method vstmt s = if s == stmt then raise Not_found; 
      match s.skind with CpcFun _ -> SkipChildren | _ -> DoChildren
    end in
    try ignore(visitCilFunction visitor fd); false
    with Not_found -> true

exception FoundCompinfo of compinfo

let find_struct name file =
  let visitor = object(self)
    inherit mynopCilVisitor

    method vglob = function
      | GCompTag ({cname = n} as c,_) when n = name ->
          raise (FoundCompinfo c)
      | GCompTagDecl ({cname = n} as c,_) when n = name ->
          raise (FoundCompinfo c)
      | _ -> DoChildren
  end in
  try
    visitCilFileSameGlobals visitor file;
    E.s (E.bug "compinfo not found for: %s\n" name)
  with FoundCompinfo c -> c

exception FoundField of fieldinfo

let find_field struct_name field_name file =
  let visitor = object(self)
    inherit mynopCilVisitor

    method vglob = function
      | GCompTag ({cname = n} as c,_) when n = struct_name ->
          raise (FoundField (getCompField c field_name))
      | _ -> DoChildren
  end in
  try
    visitCilFileSameGlobals visitor file;
    E.s (E.bug "compinfo not found for: %s\n" struct_name)
  with
  | FoundField f -> f
  | Not_found ->
      E.s (E.bug "fieldinfo not found for: %s in %s\n" field_name struct_name)

exception FoundType of typ

let find_type name file =
  let visitor = object(self)
    inherit mynopCilVisitor

    method vglob = function
      | GType ({tname = n} as typeinfo,_) when n = name ->
          raise (FoundType (TNamed (typeinfo, [])))
      | _ -> DoChildren
  end in
  try
    visitCilFileSameGlobals visitor file;
    E.s (E.bug "typeinfo not found for: %s\n" name)
  with FoundType t -> t

let find_function name file =
  let visitor = object(self)
    inherit mynopCilVisitor

    method vglob = function
      | GVarDecl(v,_)
      | GFun ({svar=v},_) when v.vname = name && isFunctionType v.vtype ->
          raise (FoundVar v)
      | _ -> SkipChildren
  end in
  try
    visitCilFileSameGlobals visitor file;
    E.s (E.bug "function not found: %s\n" name)
  with FoundVar v -> v

(******************** CPS Marking ********************************************)

(* Context used in the markCps visitor *)
type mark_context = {
  mutable cps_fun : bool; (* is it a cps function *)
  mutable cps_con : bool; (* is it in cps context *)
  ef : fundec;
  mutable last_stmt : stmt; (* last stmt in cps context *)
  mutable next_stmt : stmt; (* next stmt in cps context *)
  mutable enclosing_stmt : stmt; (* nearest loop or switch *)
  mutable last_var : varinfo option; (* last (cps) assigned variable *)
  }

let fresh_context ef =
  { cps_fun = false; cps_con = false;
    ef = ef;
    last_stmt = dummyStmt;
    next_stmt = dummyStmt;
    enclosing_stmt = dummyStmt;
    last_var = None}

let copy_context c =
  {cps_fun = c.cps_fun;
   cps_con = c.cps_con;
   ef = c.ef;
   last_stmt = c.last_stmt;
   next_stmt = c.next_stmt;
   enclosing_stmt = c.enclosing_stmt;
   last_var = c.last_var}

(* add a goto from last_stmt to next_stmt *)
exception AddGoto of mark_context

(* Functionalize a statement with a label *)
exception FunctionalizeGoto of stmt * mark_context

class markCps = fun file -> object(self)
  inherit mynopCilVisitor

  val mutable c = fresh_context dummyFunDec

  method private set_next ?(set_last=true) (s: stmt) : unit =
      (* set next_stmt in cps context only *)
    if c.cps_con then begin
    if set_last then c.last_stmt <- s;
    match s.succs with
    | [] ->
        c.next_stmt <- dummyStmt;
        (*E.warn "return expected after %a" d_stmt s*)
    | [x] ->
        trace (dprintf "set_next %a -> %a\n" dn_stmt s dn_stmt x);
        c.next_stmt <- x
    | _ -> c.next_stmt <- dummyStmt;
        E.s (E.bug "cpc construct with several successors.\
       Please report this bug with the file causing the error.")
    end

  method private is_cps (i: instr) =
      (* check that last_var is the LAST argument of args *)
    let check_var args = match c.last_var with
      | None -> true
      | Some v ->
        match List.rev args with
        | Lval (Var v', NoOffset)::_ -> v = v'
        | [] -> false
        | e::_ -> E.warn "%a should be a variable (but I'll do my best)" dn_exp e; false
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
        E.s (E.bug "%a should be a variable (and this is REALLY annoying)" dn_lval l)
    (* Weird call *)
    | Call _ ->
        if c.cps_fun then E.warn
          "I hope this has nothing to do with a cps call: %a"
          dn_instr i;
        c.last_var <- None;
        false

  method vinst (i: instr) : instr list visitAction =
    match self#is_cps i, c.cps_fun with
    | true, true when not (c.cps_con || first_instr c.last_stmt == i) ->
      (* split if the first cps instruction is not the first of the statement *)
      (* do not add any goto , we're not in cps context yet *)
        raise (SplitInstr (c.last_stmt,i,false))
    | true, true ->
        c.cps_con <- true;
        SkipChildren
    | true, false ->
        E.s (E.error "cps call not allowed here: %a" d_instr i)
    | false, _ when c.cps_con ->
        raise (SplitInstr (c.last_stmt,i, true))
    | false, _ -> SkipChildren

  method vstmt (s: stmt) : stmt visitAction =
    (* Broken control flow *)
    if c.cps_con && c.next_stmt != s
    then begin
      trace (dprintf "control flow broken in cps context: %a\ninstead of: %a\n***\n"
      d_stmt s
      d_stmt c.next_stmt);
    raise (AddGoto c) end

    (* Potential goto into cps context *)
    else if c.cps_con && (List.exists is_label s.labels) then
      (trace (dprintf "label in cps context! ");
      raise (FunctionalizeGoto (s,c)))
    else begin
      if c.cps_con && s.labels <> [] then (* Case or Default *)
        raise (TrivializeStmt c.enclosing_stmt);
      let lv = c.last_var in match s.skind with

    (* Instructions *)
    (* Must be first, to catch last_var if necessary *)
    | Instr [] ->
        self#set_next ~set_last:false s;
        s.cps <- c.cps_con;
        SkipChildren
    | Instr (hd::_) when c.cps_con && not(self#is_cps hd) ->
        trace (dprintf "not cps instruction in cps context: %a\n" d_instr hd);
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

    (* Return *)

    (* In a regular function *)
    | Return _ when not c.cps_fun -> SkipChildren
    (* In a cps function: *)
    (* 1. with no cps instruction before *)
    | Return _ when not c.cps_con ->
        s.cps <- true;
        c.last_stmt <- dummyStmt;
        c.next_stmt <- dummyStmt;
        c.last_var <- None;
        SkipChildren
    (* 2. with cps instructions before but returning nothing *)
    | Return (None, _) ->
        s.cps <- true;
        c.last_stmt <- dummyStmt;
        c.next_stmt <- dummyStmt;
        c.last_var <- None;
        c.cps_con <- false;
        SkipChildren
    (* 3. with cps instructions before and returning the last var *)
    | Return (Some (Lval (Var v,NoOffset)), _) when Some v = c.last_var ->
        s.cps <- true;
        c.last_stmt <- dummyStmt;
        c.next_stmt <- dummyStmt;
        c.last_var <- None;
        c.cps_con <- false;
        SkipChildren
    (* 4. with cps instructions before and returning something else:
          this can't be done directly, we have to split it *)
    | Return (Some _, _) ->
        raise (AddGoto c)

    (* Cpc constructs *)

    (* cpc_spawn must NOT appear in cps context *)
    | CpcSpawn _ when c.cps_con ->
        raise (AddGoto c)
    | CpcSpawn _ ->
        self#set_next s;
        SkipChildren
    (* Internal functions can appear anywhere *)
    | CpcFun _ -> (* saving and restoring context is done in vfunc *)
        ChangeDoChildrenPost
          (s, fun s -> s.cps <- c.cps_con; self#set_next ~set_last:false s; s)

    (* Control flow in cps context *)
    | Goto (g, _) when c.cps_con ->
        raise (FunctionalizeGoto (!g,c))
    | Break _ | Continue _ when c.cps_con ->
        raise (TrivializeStmt c.enclosing_stmt);
    | If _ | Switch _ | Loop _ when c.cps_con ->
        trace (dprintf "control flow in cps context\n");
        raise (AddGoto c)
    | _ when c.cps_con && c.last_var != None ->
        trace (dprintf "return variable %s ignored in cps context:\n%a\n"
        (match c.last_var with None -> assert false
        | Some v -> v.vname) d_stmt s);
        raise (AddGoto c)

    (* Control flow otherwise *)
    | Goto (g, _) ->
        assert (c.ef != dummyFunDec) ;
        if stmt_in_fun !g c.ef then
        (
        SkipChildren)
        else (
          trace (dprintf "live goto!\n");
          raise (FunctionalizeGoto (!g,c)))
    | Break _ | Continue _ -> SkipChildren
    | If _ -> ChangeDoChildrenPost (s, fun s ->
        if c.next_stmt != dummyStmt then begin
          trace (dprintf "escaping cps context in if statement %a\n***\n" dn_stmt s);
          raise (AddGoto c) end
        else s)
    | Switch _ ->
        c.enclosing_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          if c.next_stmt != dummyStmt then begin
            trace (dprintf "escaping cps context in switch statement %a\n***\n"
             dn_stmt
             s);
            raise (AddGoto c) end
          else s)
    | Loop _ ->
        c.enclosing_stmt <- s;
        ChangeDoChildrenPost (s, fun s ->
          if c.next_stmt != dummyStmt then begin
            trace (dprintf "escaping cps context in loop statement %a\n***\n" dn_stmt s);
            assert(c.next_stmt == s);
            raise (TrivializeStmt s) end
          else s)

    (* CPC Constructs *)
    (* Blocks *)
    | Block _ ->
        self#set_next ~set_last:false s;
        DoChildren

    | TryFinally _ | TryExcept _ ->
        E.s (E.unimp "try/except/finally not supported by CPC")

    end

  method vfunc (f:fundec) : fundec visitAction =
    Cfg.clearCFGinfo f;
    ignore(Cfg.cfgFun f);
    let context = copy_context c in
    c <- {(fresh_context f) with cps_fun = f.svar.vcps};
    ChangeDoChildrenPost (f, fun f -> c <- context; f)

end

(******************* CPS Conversion ******************************************)

let extract var name = match !var with
  | Some x -> x
  | None -> E.s (E.bug "couldn't find %s in the runtime\n" name)

let biggest_alignment = !Machdep.theMachine.Machdep.alignof_aligned

class cpsConverter = fun file ->
  (* Extraction of types, struct, and functions from runtime *)
  let cpc_cont_ptr =
    TPtr(TComp(find_struct "cpc_continuation" file,[]),[]) in
  let cpc_fun_ptr = TPtr(find_type "cpc_function" file,[]) in
  let cpc_alloc = find_function "cpc_alloc" file in
  let cpc_dealloc =  find_function "cpc_dealloc" file in
  let cpc_push = find_function "cpc_continuation_push" file in
  let patch cont value f =
    let typ = typeOf value in
    let temp = makeTempVar f ~name:"patch" typ in
    (* typ temp = value *)
    (Set((Var temp, NoOffset), value, locUnknown)) ::
    if !external_patch then
      let cpc_patch = find_function "cpc_continuation_patch" file in
       (* cpc_patch(cont, sizeof(typ), &temp); *)
      [ Call(None,Lval(Var cpc_patch, NoOffset), [
        Lval(Var cont, NoOffset);
        mkCast (sizeOf typ) !typeOfSizeOf;
        mkCast (addr_of temp) voidPtrType],
        locUnknown)]
    else
      let memcpy = find_function "__builtin_memcpy" file in
      let cpc_arg = makeTempVar f ~name:"cpc_arg" voidPtrType in [
      (* cpc_arg = cont->c + cont->length - __BIGGEST_ALIGNMENT__ -
                     ((sizeof(typ) - 1) / __BIGGEST_ALIGNMENT__ + 1) * __BIGGEST_ALIGNMENT__ *)
        Set((Var cpc_arg, NoOffset),
        (if !aligned_continuations then
        Formatcil.cExp
        "cont->c + cont->length - %d:biggestalign - ((%e:sizetyp - 1) / %d:biggestalign + 1) * %d:biggestalign"
          [("cont", Fv cont); ("sizefp", Fe (sizeOf cpc_fun_ptr));
          ("sizetyp", Fe (sizeOf typ)); ("biggestalign", Fd biggest_alignment)]
        else
        (* compact continuations *)
        Formatcil.cExp
        "cont->c + cont->length - %e:sizefp - %e:sizetyp"
          [("cont", Fv cont); ("sizefp", Fe (sizeOf cpc_fun_ptr));
          ("sizetyp", Fe (sizeOf typ)); ("biggestalign", Fd biggest_alignment)]
        ),
        locUnknown);
      (* memcpy(cpc_arg, &temp, sizeof(typ)) *)
        Call(None, Lval(Var memcpy, NoOffset), [
        Lval(Var cpc_arg, NoOffset);
        mkCast (addr_of temp) voidPtrType;
        mkCast (sizeOf typ) !typeOfSizeOf],
        locUnknown)]
    in
  let cpc_spawn = find_function "cpc_prim_spawn" file in
  let spawn cc context =
    (* cpc_prim_spawn(cc, context); *)
    [Call(None,Lval(Var cpc_spawn, NoOffset), [
      Lval(Var cc, NoOffset);
      context
      ],locUnknown)] in
  (* XXX DEBUGING *)
  let debug =
    if !E.debugFlag then
      let print =
        findOrCreateFunc file "printf"
        (TFun(intType, Some ["format", charConstPtrType, []], true, [])) in
      fun s -> [Call(None,Lval(Var print,
      NoOffset),[mkString (Printf.sprintf "** %s\n" s)],locUnknown)]
    else fun _ -> [] in
  object(self)
  inherit (enclosingFunction dummyFunDec)

  val mutable stack = []
  val mutable struct_map = []
  val mutable newarglist_map = []
  val mutable current_continuation = makeVarinfo false "dummyVar" voidType
  val mutable cps_function = false


  method private convert_instr ?(apply_later=false) i =
    let cc,pre,post =
      if apply_later
      then
        let var = (makeTempVar ef ~name:"cpc_apply_later" cpc_cont_ptr) in
        var,
        [(* apply_later = (void* ) 0; *)
        Set((Var var, NoOffset),
          mkCast (integer 0) cpc_cont_ptr,locUnknown)],
        spawn
          var
          (if cps_function
          then Lval(Var current_continuation, NoOffset)
          else mkCast (mkCast (integer 0) voidPtrType) cpc_cont_ptr)
      else (current_continuation,[],[]) in
    match i with
    (* Cps call with or without assignment (we don't care at this level) *)
    | Call (_, Lval (Var f, NoOffset), args, _) -> pre @ [
      (* cc = new_arglist_fun(... args ..., cc);
         cc = cpc_continuation_push(cpc_cc,((cpc_function* )f)); *)
        Call (
          Some(Var cc , NoOffset),
          (try List.assoc f newarglist_map
          with Not_found -> E.s (E.bug "newarglist for \
          function %s not found" f.vname)),
          args@[Lval(Var cc, NoOffset)],
          locUnknown
        );
        Call (
          Some(Var cc , NoOffset),
          Lval(Var cpc_push, NoOffset),
          [Lval(Var cc, NoOffset);
          mkCast (addr_of f) cpc_fun_ptr],
          locUnknown
        )] @ debug "continuation_push" @ post
    | _ -> E.s (E.bug "Unexpected instruction in cps conversion: %a\n"
        d_instr i)

  method private do_convert return =
    (* convert cps calls to cpc_push *)
    let convert l =
      stats (dprintf "tails:%d\n" (List.length l));
      List.flatten
        (List.rev_map
            (function
            | {skind=Instr l} ->List.flatten (List.rev_map self#convert_instr l)
            | s -> E.s (E.bug "Unexpected statement in cps conversion: %a\n"
                d_stmt s))
            l
        ) in
    let is_last_var v = match stack with
    | {skind=Instr l} :: _ ->
        assert (l!=[]);
        begin match List.hd(List.rev l) with
        | Call (Some (Var v', NoOffset),_,_,_) -> v = v'
        | _ -> false
        end
    | [] -> false
    | _ -> assert false in
    let patch_instr = match return with
      | None -> []
      | Some (Lval(Var v, NoOffset)) when is_last_var v -> []
      | Some ret_exp ->
      (* patch must occur only on an empty stack, or something went
         wrong earlier in cps marking *)
      if (stack <> []) then
          E.s (E.bug "Stack must be empty in order to patch.\n");
      (* XXX DEBUGING *)
      (debug ("patching before exiting "^ef.svar.vname) @
      patch current_continuation ret_exp ef) in
    mkStmt (Instr (convert (List.rev stack) @ patch_instr))

  method vinst = function
  (* Special case: some functions need to be given the current continuation *)
  | Call(r, (Lval (Var f, NoOffset) as e), args, loc)
      when hasAttribute "cpc_need_cont" f.vattr ->
         ChangeTo [Call(r, e, Lval (Var current_continuation, NoOffset) :: args, loc)] 
  | _ -> SkipChildren

  method vstmt (s: stmt) : stmt visitAction = match s.skind with
  | Instr _ when s.cps ->
      (* If the labels are not empty, this must be first cps statement,
         so the stack has to be empty. *)
      assert(s.labels = [] || stack = []);
      let l = s.labels in
      stack <- (copyClearStmt s)::stack;
      (* Restore the labels, if any. *)
      s.labels <- l;
      SkipChildren
  | Return (ret_exp,loc) when s.cps ->
      (* Do not use ChangeTo to preserve the labels. *)
      assert(s.labels = [] || stack = []);
      s.skind <- Block (mkBlock [
          self#do_convert ret_exp;
          mkStmt (Return
            (Some (Lval(Var current_continuation, NoOffset)), loc))]);
      stack <- [];
      SkipChildren
  | CpcSpawn (f, args, _) ->
      s.skind <- Instr (self#convert_instr ~apply_later:true
        (Call(None,f,args,locUnknown)));
      SkipChildren
  | _ -> DoChildren

  method vglob = function
  (* Special case: some functions need to be given the current continuation *)
  | (GVarDecl ({vtype=TFun(ret, Some args,va,attr)} as v, l)) when
      hasAttribute "cpc_need_cont" v.vattr ->
        v.vtype <- TFun(ret, Some (("c", cpc_cont_ptr, [])::args), va, attr);
        DoChildren
  | (GVarDecl ({vtype=TFun(_,args,va,attr) ; vcps = true} as v, _) as g) ->
      (* do not deal with a declaration twice *)
      if List.mem_assq v struct_map then ChangeTo [] else begin
      let args = argsToList args in
      (* XXX copy the attributes too? Should be empty anyway *)
      let rec build_struct = function
        | [] -> []
        | [name, typ, attr] when !aligned_continuations ->
            (* alignment trick *)
            [name, typ, None, [Attr("aligned",[AInt biggest_alignment])], locUnknown]
        | (name, typ, attr) :: q ->
            (name, typ, None, [], locUnknown) :: build_struct q in
      let fields =  build_struct args in
      let arglist_struct =
        mkCompInfo true (v.vname^"_arglist")
          (fun _ -> fields)
          (if !aligned_continuations then []
           else [Attr("packed",[])]) in
      let comptag = GCompTag (arglist_struct, locUnknown) in
      let new_arglist_fun = emptyFunction (v.vname^"_new_arglist") in
      let cont_name = Printf.sprintf "cpc__continuation_%d" (newVID()) in
      let new_args = Some (args@[cont_name,cpc_cont_ptr,[]]) in
      let new_arglist_type = TFun (cpc_cont_ptr, new_args, false, []) in
      let temp_arglist =
        makeTempVar new_arglist_fun ~name:"cpc_arglist"
        (TPtr(TComp(arglist_struct, []),[])) in
      (* Beware, order matters! One must set function type before getting the
       * sformals (when setting new_arglist_fun.sbody) *)
      setFunctionTypeMakeFormals new_arglist_fun new_arglist_type;
      new_arglist_fun.svar.vinline <- true;
      new_arglist_fun.svar.vstorage <- Static;
      new_arglist_fun.sbody <- (
        let field_args, last_arg = cut_last new_arglist_fun.sformals in
        mkBlock ([mkStmt(Instr (
          (* XXX DEBUGING *)
          (debug ("Entering "^new_arglist_fun.svar.vname)) @ (
          (* temp_arglist = cpc_alloc(&last_arg,(int)sizeof(arglist_struct)) *)
          Call(
            Some (Var temp_arglist, NoOffset),
            Lval (Var cpc_alloc, NoOffset),
            [addr_of last_arg;
            mkCast (SizeOf (TComp(arglist_struct,[]))) intType],
            locUnknown
            ) ::
          (* temp_arglist -> field = field *)
          List.map2 (fun v f ->
            Set(
              (mkMem (Lval (Var temp_arglist,NoOffset)) (Field (f, NoOffset))),
              Lval(Var v, NoOffset),
              locUnknown
            )
          ) field_args arglist_struct.cfields)));
        (* return last_arg *)
        mkStmt(Return (
          Some (Lval(Var last_arg,NoOffset)),
          locUnknown))
      ]));
      struct_map <- (v, arglist_struct) :: struct_map;
      newarglist_map <-
        (v, Lval (Var new_arglist_fun.svar, NoOffset)) :: newarglist_map;
      if v.vstorage = Extern then begin
        (* mark it as completed (will be done later for non-extern function *)
        v.vcps <- false;
        v.vtype <- TFun(cpc_cont_ptr,
          Some ["cpc_current_continuation", cpc_cont_ptr, []], va, attr)
        end;
      ChangeTo [comptag;GFun (new_arglist_fun, locUnknown);g]
      end
  | GFun ({svar={vtype=TFun(ret_typ, _, va, attr) ; vcps =
  true};sformals = args} as fd, _) as g ->
      let new_arg = ["cpc_current_continuation", cpc_cont_ptr, []] in
      let arglist_struct = List.assoc fd.svar struct_map in
      let cpc_arguments =
        makeLocalVar fd "cpc_arguments"
        (TPtr(TComp(arglist_struct, []),[])) in
      (* add former arguments to slocals *)
      fd.slocals <- args @ fd.slocals;
      fd.sformals <- [];
      setFunctionTypeMakeFormals fd (TFun(cpc_cont_ptr, Some new_arg, va, attr));
      current_continuation <- begin match fd.sformals with
        | [x] -> x
        | _ -> assert false end;
      cps_function <- true;

      fd.sbody.bstmts <-
        mkStmt(Instr (
          (* XXX DEBUGING *)
          (debug ("Entering "^fd.svar.vname)) @ (
          (* cpc_arguments = cpc_dealloc(cpc_current_continuation,
                (int)sizeof(arglist_struct)) *)
          Call(
            Some (Var cpc_arguments, NoOffset),
            Lval (Var cpc_dealloc, NoOffset),
            [Lval (Var current_continuation, NoOffset);
            mkCast (SizeOf (TComp(arglist_struct,[]))) intType],
            locUnknown
            ) ::
          (* field = cpc_arguments -> field *)
          List.map2 (fun v f ->
            Set(
              (Var v, NoOffset),
              Lval (mkMem (Lval (Var cpc_arguments,NoOffset)) (Field (f, NoOffset))),
              locUnknown
            )
          )  args arglist_struct.cfields)))
        :: fd.sbody.bstmts;
      fd.svar.vcps <- false; (* this is not a cps function anymore *)
      ChangeDoChildrenPost([g], fun g -> stack <- []; g)
  | GFun _ ->
      cps_function <- false;
      DoChildren
  | _ -> DoChildren
end

(********************* Avoid ampersand on local variables ********************)

(* return a set of free variables in a function *)
let free_vars fd =
  let args = List.fold_left (fun s x -> S.add x s) S.empty fd.sformals in
  let bounded = List.fold_left (fun s x -> S.add x s) args fd.slocals in
  let vars = ref S.empty in
  let collector = object(self)
    inherit nopCilVisitor (* do not use mynopCilVisitor, variables can
    be hidden in types *)

    method vvrbl v =
      if not (v.vglob || isFunctionType v.vtype)
      then begin
        if isArrayType v.vtype && not (S.mem v bounded) then
          E.s (E.error "Free array in function %s: %s (%a)"
            fd.svar.vname v.vname d_loc !currentLoc)
        else
          vars := S.add v !vars;
      end;
      SkipChildren

    method vstmt s = match s.skind with
    | CpcFun _ -> SkipChildren
    | _ -> DoChildren
  end in
  ignore(visitCilFunction collector fd);
  S.diff !vars bounded

let must_be_boxed v = VS.mem v !ampSet && not v.vglob

class avoidAmpersand f =
    let malloc = Lval(Var (findOrCreateFunc f "malloc" (TFun(voidPtrType, Some
    ["size", !typeOfSizeOf, []], false, []))), NoOffset) in
    let free = Lval(Var (findOrCreateFunc f "free" (TFun(voidType, Some ["ptr",
    voidPtrType, []], false, []))), NoOffset) in
    let insert_free oldnew retTyp fd = visitCilStmt
      (object(self)
       inherit mynopCilVisitor
       method vstmt s = match s.skind with
       | Return (retval, loc) ->
          let rv, assign = match retval with
          | None -> None, []
          | Some rval -> let r = make_ret_var fd retTyp in
              Some (Lval (Var r, NoOffset)), [Set((Var r, NoOffset), rval, loc)] in
          s.skind <- Block (mkBlock [
          mkStmt (Instr (assign @ Util.list_map (fun (_, v) ->
          Call(None,free,[mkCast (Lval(Var v, NoOffset))
          voidPtrType],locUnknown)) oldnew));
          mkStmt (Return (rv,loc))]);
          SkipChildren
       | CpcFun _ -> assert(false); (* Thanks to initial lambda-lifting *)
       | _ -> DoChildren
       end
      )  in
  let malloc_init oldnew =
    let mallocs = mkStmt (Instr (Util.list_map (fun (_, v) ->
      let mem_v = mkMem (Lval (Var v, NoOffset)) NoOffset in
      Call(Some (Var v, NoOffset), malloc, [sizeOf (typeOfLval
      mem_v)],locUnknown)) oldnew)) in
    let inits =
      mkStmt (Instr (Util.list_map (fun (v,v') ->
        Set(mkMem (Lval (Var v', NoOffset)) NoOffset, Lval(Var v, NoOffset), locUnknown))
        (* Do not initialise a (local) variable with itself *)
        (List.filter (fun (v1, v2) -> v1.vid != v2.vid) oldnew))) in
    [mallocs ; inits]
  in
  let add_stars oldnew = visitCilStmt
    (object(self)
      inherit nopCilVisitor (* do not use mynopCilVisitor, lval can be
      hidden in types *)
      
      method vlval = function
      | (Var v, _) as lval when must_be_boxed v ->
          ChangeDoChildrenPost (lval, fun lval -> match lval with
          | Var v, offset ->
              let v' = try List.assoc v oldnew with Not_found -> assert false in
              mkMem (Lval (Var v', NoOffset)) offset
          | _ -> assert false)
      | _ -> DoChildren

      method vstmt s = match s.skind with
      | CpcFun _ -> assert(false); (* Thanks to initial lambda-lifting *)
      | _ -> DoChildren
    end) in
  object(self)
  inherit mynopCilVisitor

  method vfunc fd =
    if fd.svar.vcps then begin
      let retTyp =
        match fd.svar.vtype with
        | TFun(rt, _, _, _) -> rt
        | _ -> E.s (E.bug "Function %s does not have a function type\n" fd.svar.vname) in
      let locals = List.filter must_be_boxed fd.slocals in
      let formals = List.filter must_be_boxed fd.sformals in
      let fv = List.filter must_be_boxed (S.elements (free_vars fd)) in
      assert(fv = []); (* Thanks to an initial lambda-lifting *)
      let oldnew =
        (Util.list_map (fun v -> (* Change the type of locals directly *)
          v.vtype <- TPtr (v.vtype, []);
          (v,v))
        locals) @
        (Util.list_map (fun v -> (* Copy the formals as locals and avoid name clash *)
          let name = v.vname in
          v.vname <- "__"^name;
          (v, makeLocalVar fd name (TPtr (v.vtype, []))))
        formals) @
        (Util.list_map (fun v -> (* Make a local copy of free variables *)
          (v, makeLocalVar fd v.vname (TPtr (v.vtype, []))))
        fv) in
      if oldnew <> [] then begin
        trace (dprintf "avoidAmpersand: %d\n" (List.length oldnew));
        List.iter (fun (v, v') -> trace (dprintf "[amp] %s(%d) -> %s (%d)\n"
          v.vname v.vid v'.vname v'.vid)) oldnew;
        fd.sbody <- {
          fd.sbody with bstmts =
            malloc_init oldnew @
            (Util.list_map (fun s -> insert_free oldnew retTyp fd (add_stars oldnew s))
            fd.sbody.bstmts)
        };
      end;
    end;
    SkipChildren (* No CpcFun thanks to initial lambda-lifting *)
end



(******************************************************************************)
(*                                                                            *)
(*                            Creating environment                            *)
(*                                                                            *)
(******************************************************************************)

(*** First step ***)
(* Some global variables to be used between passes *)

(* [f, f_env] *)
let envList (* : (fundec, varinfo) list ref *) = ref []

class addEnvStruct f =
  (* free function declaration *)
  let free : Cil.exp =
    let fun_type =
      let fun_args_type = Some ["ptr", voidPtrType, []]
      and fun_ret_type = voidType in
      TFun (fun_ret_type, fun_args_type, false, [])
    in
    Lval( Var(findOrCreateFunc f "free" fun_type), NoOffset)
  in
  let modified_functions = ref [] in
  let setAsModified vid = modified_functions := vid::!modified_functions in
  let hasBeenModified vid = List.mem vid !modified_functions in
  let null_ptr = mkCast (integer 0) voidPtrType in
  let notVoid ret_typ = match ret_typ with
    | TVoid _ -> false
    | _ -> true
  in


(* We will just create a void pointer. His type will be change in the future. *)
object (self)
  inherit mynopCilVisitor

  method vglob = function
    (* special case for extern functions *)
    | (GVarDecl ({vtype=TFun(ret_typ, param_type, va, attr);
                  vcps = true;
                  vstorage = Extern;} as v,
                 _)) when notVoid ret_typ ->
      let new_ret_type = TPtr (ret_typ, []) in
      let new_param_type : (string * Cil.typ * Cil.attributes) list option =
        let params = match param_type with
          | None -> []
          | Some lst -> lst
        in
        Some (("cpc_ret_addr", new_ret_type, []) :: params)
      in
      let new_fun_type = TFun (voidType, new_param_type, va, attr) in
      v.vtype <- new_fun_type;
      setAsModified v.vid;
      SkipChildren


    (* change function body *)
    | GFun ({svar     = {vtype = TFun(ret_typ, param_type, va, attr);
                         vcps  = true};
             sformals = args} as fd,
            _) ->
      (* create the structure local variable *)
      let cpc_env = makeLocalVar fd "cpc_env" (TPtr(voidPtrType,[])) in
      (* XXX would voidPtrType be enough? *)
      envList := (fd, cpc_env) :: !envList;

      (* add return arg *)
      let (lazy_FRV, lazy_SFRV) = if notVoid ret_typ then begin
        (* remove return value type *)
        let new_ret_type = TPtr (ret_typ, []) in
        let new_param_type : (string * Cil.typ * Cil.attributes) list option =
          let params = match param_type with
            | None -> []
            | Some lst -> lst
          in
          Some (("cpc_ret_addr", new_ret_type, []) :: params)
        in
        let new_fun_type = TFun (voidType, new_param_type, va, attr) in
        fd.svar.vtype <- new_fun_type;
        setAsModified fd.svar.vid;

        (* add new return value as first argument *)
        let fd_ret_val =
          makeFormalVar fd ~where:"^" "cpc_ret_addr" new_ret_type
        in
        let star_fd_ret_val=mkMem (Lval (Var fd_ret_val, NoOffset)) NoOffset in
        (lazy fd_ret_val, lazy star_fd_ret_val)
      end else let lazy_false = lazy (assert false) in (lazy_false, lazy_false)
      in


      (* add free statements and convert Returns *)
      let free_instr = (* free((void* ) env); *)
        let arg = [mkCast (Lval (Var cpc_env, NoOffset)) voidPtrType] in
        Call (None, free, arg, locUnknown)
      in
      let add_free stmt =
        let free_visitor = (
          object (self)
            inherit mynopCilVisitor

            method vstmt s = match s.skind with
              | Return (retval, loc) ->
                  let return_stmt = mkStmt (Return(None, loc))
                  and free_stmt = mkStmt (Instr [free_instr]) in
                  let stmts = match retval with
                    | None -> [free_stmt; return_stmt]
                        (* return; yields:
                           free(cpc_env);
                           return;
                        *)
                    | Some rval ->
                        (* return x; yields:
                           if(cpc_ret != NULL) {
                               *cpc_ret = rval;
                           }
                           free(cpc_env);
                           return;
                        *)
                        let retval_assign_instr =
                          Set(Lazy.force lazy_SFRV, rval, loc)
                        in
                        let if_stmt =
                          let if_cond =
                            BinOp (Ne,
                                   Lval (Var (Lazy.force lazy_FRV), NoOffset),
                                   null_ptr,
                                   intType)
                          and if_then =
                            mkBlock [mkStmt (Instr [retval_assign_instr])]
                          and if_else = mkBlock [] in
                          mkStmt (If (if_cond, if_then, if_else, locUnknown))
                        in
                        [if_stmt; free_stmt; return_stmt]
                  in
                  s.skind <- Block (mkBlock stmts);
                  SkipChildren
              | _ -> DoChildren

          end)
        in
        visitCilStmt free_visitor stmt
      in
      let bstmt_with_frees = List.map add_free fd.sbody.bstmts in

      (* complete function *)
      fd.sbody.bstmts <- bstmt_with_frees;

      (* ok, next job... *)
      DoChildren

    | _ -> DoChildren


  method vinst = function
    | Call (lval_opt,
            (Lval(Var({vtype=TFun(ret_typ, arg_lst_typ,_,_);
                       vcps = true;} as vinfo),_)as f),
            arg_lst,
            loc) ->
        (* y = f(x); with f a cps function, yields:
           f(&y, x);
        *)
        let change retVal = ChangeTo [Call(None, f, retVal::arg_lst, loc)] in
        begin match lval_opt, ret_typ with
          | None, TVoid _ ->
              if hasBeenModified vinfo.vid
              then change null_ptr
              else DoChildren
          | None, _       ->
              change null_ptr
          | Some v, _     ->
              change (mkAddrOf v)
        end
    | _ -> DoChildren

end

(*** Second step ***)
class createEnv2 f =
  (* malloc function declaration *)
  let malloc : Cil.exp =
    let fun_type =
      let fun_args_type = Some ["size", !typeOfSizeOf, []]
      and fun_ret_type = voidPtrType in
      TFun (fun_ret_type, fun_args_type, false, [])
    in
    Lval( Var(findOrCreateFunc f "malloc" fun_type), NoOffset)
  in
  (* provide a solution to remplace a variable by another in a statement.
     (used for cpc_env) *)
  let variable_remplacer old_var new_var stmt =
    let vr_visitor = (
      object (self)
        inherit mynopCilVisitor

        method vstmt s = match s.skind with
          | CpcFun _ -> SkipChildren
          | _ -> DoChildren

        method vvrbl v =
          if v.vid = old_var.vid
          then ChangeTo new_var
          else SkipChildren
      end)
    in
    visitCilStmt vr_visitor stmt
  in
  (* avoid "Not Found" without any other information ! *)
  let safeGetCompField comp_info vname =
    try
      getCompField comp_info vname
    with
      | e -> E.s (E.bug "getCompField %s %s failed (%s)\n" comp_info.cname vname
        (if e = Not_found then "not found" else "unknown error"))
  in

object (self)
  inherit mynopCilVisitor

  method vglob = function
    | GFun ({svar     = {vtype = TFun(ret_typ, _, va, attr);
                         vcps  = true};
             sformals = args} as fd,
            _) as g ->
      (* retreive environment from Env1's step *)
      let cpc_env =
        try List.assoc fd !envList
        with Not_found ->
          E.s (E.bug "Env2: missmatching env for function %s" fd.svar.vname)
      in

      (* get all variables (locals and formals), reset locals *)
      (* TODO: il faut mieux sélectionner les variables... et comme je ne
         descends pas dans les sous-fonctions, j'en oublie sans aucun doute
         ici. *)
      let filter_cpc_env lst v = if v.vid = cpc_env.vid then lst else v::lst in
      let vars : Cil.varinfo list =
        List.fold_left filter_cpc_env args fd.slocals
      in
      let vars_id = List.map (fun v -> v.vid) vars in
      let is_var vid = List.mem vid vars_id in
      fd.slocals <- [cpc_env];

      (* create the structure*)
      let rec build_struct = function
        | [] -> []
        | var :: q ->
            (var.vname, var.vtype, None, [], locUnknown) :: build_struct q
      in
      let fields = build_struct vars in
      let env_struct : Cil.compinfo =
        mkCompInfo
          true
          (fd.svar.vname ^ "_env")
          (fun _ -> fields)
          []
      in
      (* create the structure declaration and local variable *)
      let comptag = GCompTag (env_struct, locUnknown) in
      cpc_env.vtype <- TPtr(TComp(env_struct, []),[]);

      (* malloc and inits the environment structure *)
      (* create the malloc instruction *)
      let malloc_instr =
        let starEnv = mkMem (Lval (Var cpc_env, NoOffset)) NoOffset in
        let ret_val = Some(Var cpc_env, NoOffset) in
        (* env = malloc(sizeof(typeof( *env ))) *)
        Call(ret_val, malloc, [sizeOf (typeOfLval starEnv)], locUnknown)
      in

      (* create the initialisation instructions *)
      let fieldnames = List.map (fun x -> x.vname) args in
      let fieldinfos = List.map (fun x -> safeGetCompField env_struct x) fieldnames
      in
      let make_init v f =
        (* f_env -> field = field *)
        Set(
          mkMem (Lval (Var cpc_env, NoOffset)) (Field (f, NoOffset)),
          Lval (Var v, NoOffset),
          locUnknown
        )
      in
      let init_instrs = List.map2 make_init args fieldinfos in
      let instructions = Instr (malloc_instr::init_instrs) in

      (* put some indirections *)
      let add_struct_access stmt =
        let asa_visitor = (
          object (self)
            inherit nopCilVisitor

            method vlval = function
              | (Var v, voffset) when is_var (v.vid) ->
                  let v_field = safeGetCompField env_struct v.vname in
                  let env_lval = Lval (Var cpc_env, NoOffset) in
                  let v' = mkMem env_lval (Field (v_field, voffset)) in
                  ChangeTo v'
              | _ -> DoChildren

          end)
        in
        visitCilStmt asa_visitor stmt
      in
      let bstmt_with_struct_access =
        List.map add_struct_access fd.sbody.bstmts in

      (* replace sub-functions cpc_env, and add it as argument. *)
      (* find sub-functions ID *)
      let funId_list = ref [] in
      let funId_finder stmt =
        let funId_visitor = (
          object (self)
            inherit mynopCilVisitor

            method vstmt s = match s.skind with
              | CpcFun (local_fd, _) ->
                  funId_list := local_fd.svar.vid :: !funId_list;
                  DoChildren
              | _ -> DoChildren
          end)
        in
        visitCilStmt funId_visitor stmt
      in
      (* now, just update the mutable variable *)
      let _ = List.map funId_finder bstmt_with_struct_access in


      (* add env arguments to sub-functions-call *)
      let add_args stmt =
        let isOurFunction nfos = (* is a sub-function *)
          let varFromLhost = function
            | Var v -> List.mem v.vid !funId_list
            | _ -> false
          in
          match nfos with
          | Lval l -> varFromLhost (fst l)
          | _ -> assert false
        in
        let fun_visitor = (
          object (self)
            inherit mynopCilVisitor

            method vinst i = match i with
              | Call (ret, nfos, args, loc) ->
                  if isOurFunction nfos
                  then begin
                    assert (args = []); (* because i'm sad if not *)
                    let new_args = [Lval (Var cpc_env, NoOffset)] in
                    ChangeTo [Call (ret, nfos, new_args, loc)]
                  end else SkipChildren
              | _ -> DoChildren

          end)
        in
        visitCilStmt fun_visitor stmt
      in
      let bstmt_with_env_in_args = List.map add_args bstmt_with_struct_access in


      (* replace... *)
      let add_params stmt =
        let fun_visitor = (
          object (self)
            inherit mynopCilVisitor

            method vstmt s = match s.skind with
              | CpcFun (local_fd, _) ->
                  (* I think that it's true: *)
                  assert (local_fd.sformals = []);
                  (* create the new env param *)
                  let local_cpc_env =
                    makeFormalVar
                      local_fd
                      cpc_env.vname
                      (TPtr(TComp(env_struct, []),[]));
                  in
                  (* replace the old env var by the new *)
                  let replace = variable_remplacer cpc_env local_cpc_env in
                  let new_stmts = List.map replace local_fd.sbody.bstmts in
                  local_fd.sbody.bstmts <- new_stmts;
                  DoChildren
              | _ -> DoChildren

          end)
        in
        visitCilStmt fun_visitor stmt
      in
      let bstmt_with_params = List.map add_params bstmt_with_env_in_args in


      (* complete function *)
      fd.sbody.bstmts <- (mkStmt instructions :: bstmt_with_params);

      (* Complete global déclarations *)
      ChangeTo [comptag; g]

    | _ -> SkipChildren

end



(********************* Assignment of cps return values ***********************)

class cpsReturnValues = object(self)
  inherit (enclosingFunction dummyFunDec)

  method vinst = function
  | Call (r, Lval (Var f, NoOffset), args, loc) as i when f.vcps ->
      let typ = fst4 (splitFunctionTypeVI f) in
      begin match r with
      | Some _ when (typeSig typ = typeSig voidType) -> (* Wrong assignment *)
          E.s (E.bug "Assignment of a function returning void: %a" d_instr i);
      | Some (Var _, NoOffset) -> SkipChildren (* Simple good assignment *)
      | Some l -> (* Some other valid but complex assignment *)
          let v = makeTempVar ef typ in
          ChangeTo [
            Call(Some(Var v, NoOffset), Lval (Var f, NoOffset), args, loc);
            Set(l, Lval(Var v, NoOffset), loc)
          ]
      | None when  (typeSig typ <> typeSig voidType) -> (* Missing assignment *)
          let v = makeTempVar ef typ in
          trace (dprintf "Ignoring a cps return value: %a\n" d_instr i);
          ChangeTo [
          Call(Some(Var v, NoOffset), Lval (Var f, NoOffset), args, loc)]
      | None -> SkipChildren (* No assignement (void function) *)
      end
  | _ -> SkipChildren

end

(********************* Remove nasty expressions in cps calls *****************)

(* If one of the arguments is nasty, rebind every argument ---
percolating variables will prevent the bindings from being free and the
compiler should optimize it pretty well. *)

exception ContainsNasty

let contains_nasty args =
  let visitor = object(self)
    inherit nopCilVisitor (* do not use mynopCilVisitor, lval can be
    hidden in types *)

    method vvrbl v =
      (* global variables are nasty *)
      if v.vglob then raise ContainsNasty;
      (* if a local variable has its address retained,
         avoidAmpersand has boxed it, and vlval has raised an exception
         before we get here, detecting the box. *)
      (* assert(not (must_be_boxed v)); XXX No more true with environments *)
      DoChildren

    method vlval = function
    | (Mem _, _) -> raise ContainsNasty
    | _ -> DoChildren
     end in
  ignore(Util.list_map (visitCilExpr visitor) args)

let rebind ef ftype args =
  let argstype =
    match ftype with
    | TFun (_,x,_,_) -> argsToList x
    | _ -> assert false in
  let args' = Util.list_map (fun (s,t,_) -> makeTempVar ~name:s ef t) argstype in
   let bind_list =
     List.map2 (fun  v e -> Set((Var v,NoOffset), e,locUnknown)) args' args in
   bind_list, Util.list_map (fun v -> Lval(Var v, NoOffset)) args'

class removeNastyExpressions = object(self)
  inherit (enclosingFunction dummyFunDec)

  method vinst = function
  | Call(ret, Lval(Var f, NoOffset), args, loc) when f.vcps ->
      (try contains_nasty args; SkipChildren
      with ContainsNasty ->
        trace (dprintf "nasty variables in call to %s\n" f.vname);
        let (bind_list, args') = rebind ef f.vtype args in
        ChangeTo(bind_list @[Call(ret, Lval(Var f, NoOffset),args',loc)]))
  | _ -> SkipChildren
end

(********** Add defaults arguments to cpc primitives *************************)

class addDefaultArgs file =
  let condvar_null_ptr =
    mkCast (mkCast (integer 0) voidPtrType)
    (TPtr(find_type "cpc_condvar" file,[])) in
  let cpc_sleep = find_function "cpc_sleep" file in
  object(self)
  inherit mynopCilVisitor

  method vinst = function
  | Call(ret, Lval(Var ({vname = "cpc_io_wait"} as v), NoOffset), [fd; dir], loc) ->
      ChangeTo [Call(ret, Lval(Var v, NoOffset),
      [fd; dir; condvar_null_ptr], loc)]
  | Call(ret, Lval(Var ({vname = "cpc_sleep"} as v), NoOffset), [s], loc) ->
      ChangeTo [Call(ret, Lval(Var v, NoOffset),
      [s; zero; condvar_null_ptr], loc)]
  | Call(ret, Lval(Var ({vname = "cpc_sleep"} as v), NoOffset), [s; ms], loc) ->
      ChangeTo [Call(ret, Lval(Var v, NoOffset),
      [s; ms; condvar_null_ptr], loc)]
  (* cpc_wait is handled via cpc_sleep when it has several arguments *)
  | Call(ret, Lval(Var {vname = "cpc_wait"}, NoOffset), [c; s], loc) ->
      ChangeTo [Call(ret, Lval(Var cpc_sleep, NoOffset),
      [s; zero; c], loc)]
  | Call(ret, Lval(Var {vname = "cpc_wait"}, NoOffset), [c; s; ms], loc) ->
      ChangeTo [Call(ret, Lval(Var cpc_sleep, NoOffset),
      [s; ms; c], loc)]
  | _ -> SkipChildren
end

(******** Insert goto after cps assignment and returns before cpc_done *******)

let rec insert_gotos il =
  let rec split acc l = match l with
  | Call(_, Lval(Var f, NoOffset), _, _)::_ when f.vcps ->
      (mkStmt (Instr (List.rev (List.hd l :: acc))), List.tl l, true)
  | hd :: tl -> split (hd :: acc) tl
  | [] -> (mkStmt (Instr (List.rev acc)), [], false) in
  (* the boolean indicates whether we have to include a goto or not *)
  match split [] il with
  | s, rem, false -> assert(rem=[]); [s]
  | s, rem, true ->
    let dst, tl = match insert_gotos rem with
    | [] -> mkEmptyStmt(), []
    | hd :: tl ->  hd, tl in
    add_goto s dst;
    s :: dst :: tl

class insertGotos = object(self)
  inherit mynopCilVisitor

  method vstmt s = match s.skind with
  | Instr il ->
      s.skind <- Block (mkBlock (compactStmts (insert_gotos il)));
      SkipChildren
  | _ -> DoChildren

end

(********************* Remove identity function ******************************)

let removeIdentity = fun file ->
  let replaced = ref [] in
  let replaceVar (fd,fd') = visitCilFile (
    object(self)
      inherit nopCilVisitor (* do not use mynopCilVisitor, variables can
      be hidden in types *)

      method vvrbl v =
        if v = fd then ChangeTo fd' else SkipChildren

      (* Add the protoype of fd' whenever we meet fd, in case the former
         is defined after the latter in the file. *)
      method vglob = function
      | GVarDecl (v,loc) when v = fd ->
        ChangeTo [GVarDecl (v, loc); GVarDecl (fd', loc)];
      | GFun (f, loc) when f.svar = fd ->
        ChangeDoChildrenPost (
          [GVarDecl (fd', loc);
           GFun (f, loc)
          ],
          fun x -> x)
      | _ -> DoChildren
    end)
    file in
  visitCilFileSameGlobals (object(self)
  inherit mynopCilVisitor

  method vfunc fd =
    if not fd.svar.vcps then SkipChildren
    else match fd.sbody.bstmts with
    | {skind=Instr [Call(None,Lval(Var fd',NoOffset),args,_)]} ::
      {skind=Return(None,_)} :: _ when fd'.vcps &&
      args = Util.list_map (fun v -> Lval(Var v,NoOffset)) fd.sformals ->
        replaced :=  (fd.svar,fd')::!replaced;
        (* Do not remove fd.sbody since it might be used outside. *)
        SkipChildren
    | {skind=Instr [Call(Some(l),Lval(Var fd',NoOffset),args,_)]} ::
      {skind=Return(Some(Lval l'),_)} :: _ when fd'.vcps && l=l' &&
      args = Util.list_map (fun v -> Lval(Var v,NoOffset)) fd.sformals ->
        replaced :=  (fd.svar,fd')::!replaced;
        (* Do not remove fd.sbody since it might be used outside. *)
        SkipChildren
    | _ -> SkipChildren
   end)
   file;
   (* Delaying the replacement is necessary, otherwise the globals are
      not inserted properly. *)
   List.iter replaceVar !replaced

(********************* Cleaning **********************************************)

class cleaner = object(self)
  inherit mynopCilVisitor

  method vblock (b: block) : block visitAction =
    ChangeDoChildrenPost (b, fun b ->
      b.bstmts <- compactStmts b.bstmts;
      b)
end

class folder = object(self)
  inherit mynopCilVisitor

  method vstmt s = match s.skind with
  | If(e,b1,b2,_) -> begin match isInteger e with
    | Some 0L -> ChangeDoChildrenPost(mkStmt (Block b2), fun x -> x)
    | Some _ -> ChangeDoChildrenPost(mkStmt (Block b1), fun x -> x)
    | None -> DoChildren
    end
  | _ -> DoChildren
end

(************** Percolate local variables ************************************)

(* If a local variable is used in a single local function, it should be
  local to that funtion, in order not to be lambda-lifted.
  This is a particular case of a more general optimization (that we do
  not perform). *)

let percolateLocals file =
  let l = ref [] in
  let decl = ref [] in
  (* in the association list ref l, the values are either:
     - Some fd if the key is present in a single function
     - None if it present in several functions or if it is a formal. *)
  let record_var var fd =
    try
      match List.assq var !l with
      | Some fd' when not(fd == fd') ->
          l := (var, None) :: List.remove_assq var !l
      | _ -> ()
    with Not_found -> l := (var, Some fd) :: !l in
  let visitor = object(self)
      inherit (enclosingFunction dummyFunDec)

      method vvrbl v =
        if not (v.vglob || isFunctionType v.vtype) then
            record_var v ef;
        SkipChildren

    (* We need to make sure that we won't record any formal var, and to
    keep track of the function were locals are declared. *)
     method vvdec v =
        if not (isFunctionType v.vtype) then begin
        (if List.memq v ef.sformals then
          try
            match List.assq v !l with
            | Some _ -> l := (v, None) :: List.remove_assq v !l
            | None -> ()
          with Not_found -> l := (v, None) :: !l);
        (if List.memq v ef.slocals then
          try
            if not(List.assq v !decl == ef) then
              E.s (E.bug "percolate: missmatching declaration for %s" v.vname)
          with Not_found -> decl := (v, ef) :: !decl);
        end;
        SkipChildren

    end in
    visitCilFileSameGlobals visitor file;
    List.iter (fun (var, fd) -> match fd with
    | None -> ()
    | Some fd ->
      trace (dprintf "%s optimized\n" var.vname);
      let fdecl = try List.assq var !decl with Not_found ->
       E.s (E.bug "percolate: cannot find variable %s, declared in function %s"
        var.vname fd.svar.vname) in
      fdecl.slocals <- List.filter (fun v -> not(v == var)) fdecl.slocals;
      fd.slocals <- var :: fd.slocals) !l

(********************* Lambda-lifting ****************************************)

let make_fresh_varinfo fd =
  let args = fd.sformals in
  let new_args = Util.list_map (fun v -> copyVarinfo v v.vname) args in
  setFormals fd new_args;
  List.combine args new_args

class uniqueVarinfo = object(self)
    inherit nopCilVisitor (* do not use mynopCilVisitor, variables can
    be hidden in types *)

    val mutable current_map = []

    method vvrbl (v:varinfo) : varinfo visitAction =
      try
        ChangeTo(
          let r = List.assoc v current_map in
          trace (dprintf "%s(%d)->%s(%d)\n" v.vname v.vid r.vname r.vid);
          r)
    with
    Not_found -> SkipChildren

    method vstmt s = match s.skind with
    | CpcFun _ ->
        E.s (E.bug "uniqueVarinfo should be called after lambda-lifting\n")
    | _ -> DoChildren

  method vglob = function
  | GFun (fd, loc) as g when fd.svar.vcps ->
      let map = make_fresh_varinfo fd in
      current_map <- map;
      ChangeDoChildrenPost([g], fun g ->
        current_map <- []; g)
  | _ -> SkipChildren

  end

(* return a list of local cps functions in a function *)
let collect_local_fun fd =
  let fun_list = ref [] in
  let collector = object(self)
    inherit mynopCilVisitor

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
    inherit mynopCilVisitor

    method vstmt s = match s.skind with
    | CpcFun (fd, _) ->
        ChangeTo(mkEmptyStmt())
    | _ -> DoChildren
  end) fd

(* remove free variable thanks to an iterated lambda-lifting technique:
  - find free vars of a local function
  - add them as parameters of this function and insert them at every call point
  - proceed with the next function until none has free vars left
  Return the list of globals made out of (removed) local functions *)
let remove_free_vars enclosing_fun loc =
  let rec make_globals gfuns = function
    | [] ->
        GVarDecl(enclosing_fun.svar,locUnknown) ::
        List.rev_append gfuns [GFun(remove_local_fun enclosing_fun,loc)]
    | fd :: tl ->
        GVarDecl(fd.svar,locUnknown) ::
          make_globals (GFun(remove_local_fun fd,locUnknown) :: gfuns) tl in
  let introduce_new_vars fd fv =
    let new_args = Util.list_map (fun v -> Lval(Var v, NoOffset)) fv in
    let insert =
      object(self)
        inherit mynopCilVisitor

        method vinst = function
          | Call(lval, Lval ((Var f, NoOffset) as l), args, loc)
          when f == fd.svar ->
            let args' = new_args @ args in
            trace (dprintf "inserting in %a\n" d_lval l);
            ChangeTo([Call(lval, Lval(Var f, NoOffset), args', loc)])
          | _ -> SkipChildren

        method vstmt s = match s.skind with
          | CpcSpawn(Lval ((Var f, NoOffset) as l), args, loc)
          when f == fd.svar ->
            let args' = new_args @ args in
            trace (dprintf "inserting in %a\n" d_lval l);
            s.skind <- CpcSpawn(Lval(Var f, NoOffset), args', loc);
            SkipChildren
          | _ -> DoChildren
      end in
    setFormals fd (fv @ fd.sformals);
    enclosing_fun.sbody <- visitCilBlock insert enclosing_fun.sbody;
    trace (dprintf "%a\n" d_global (GVarDecl(fd.svar,locUnknown))) in
  let local_funs = collect_local_fun enclosing_fun in
  let rec iter = function
  | fd :: tl ->
      let fv = free_vars fd in
      if S.is_empty fv
      then iter tl
      else begin
        trace (dprintf "free variables in %s:\n" fd.svar.vname);
        S.iter (fun v -> trace (dprintf "[fv] %s(%d)\n" v.vname v.vid)) fv;
        introduce_new_vars fd (S.elements fv);
        iter local_funs
      end
  | [] -> make_globals [] local_funs in
  iter local_funs

class lambdaLifter = object(self)
  inherit mynopCilVisitor

  method vglob (g: global) : global list visitAction = match g with
  | GFun (fd,loc) -> ChangeTo(remove_free_vars fd loc)
  | _ -> SkipChildren

end

(************** Functionalize Goto *******************************************)

exception GotoContent of stmt list

let make_function_name base =
  Printf.sprintf "__cpc_%s_%s" base (timestamp ())

exception FoundType of typ

class hasReturn = object(self)
  inherit mynopCilVisitor

  method vstmt s = match s.skind with
  | Return (Some e, _) -> raise (FoundType (typeOf e))
  (*| Return (None, _) -> raise (FoundType voidType)*)
  | _ -> DoChildren
end


(***** From src/frontc/cabs2cil.ml ******)

let rec stmtFallsThrough (s: stmt) : bool = 
  match s.skind with
    Instr _ -> true (* conservative, ignoring exit() etc. *)
  | Return _ | Break _ | Continue _ -> false
  | Goto _ -> false
  | If (_, b1, b2, _) -> 
      blockFallsThrough b1 || blockFallsThrough b2
  | Switch (e, b, targets, _) -> 
       (* See if there is a "default" case *)
       if not 
          (List.exists (fun s -> 
             List.exists (function Default _ -> true | _ -> false)
                          s.labels)
                       targets) then begin
          true (* We fall through because there is no default *)
       end else begin
          (* We must examine all cases. If any falls through, 
           * then the switch falls through. *)
          blockFallsThrough b || blockCanBreak b
       end
  | Loop (b, _, _, _) -> 
      (* A loop falls through if it can break. *)
      blockCanBreak b
  | Block b -> blockFallsThrough b
  | TryFinally (b, h, _) -> blockFallsThrough h
  | TryExcept (b, _, h, _) -> true (* Conservative *)
  | CpcSpawn _ | CpcFun _ -> true
and blockFallsThrough b = 
  let rec fall = function
      [] -> true
    | s :: rest -> 
        if stmtFallsThrough s then begin
            fall rest
        end else begin
          (* If we are not falling thorough then maybe there 
          * are labels who are *)
            labels rest
        end
  and labels = function
      [] -> false
        (* We have a label, perhaps we can jump here *)
      | s :: rest when s.labels <> [] -> 
         fall (s :: rest)
      | _ :: rest -> labels rest
  in
  let res = fall b.bstmts in
  res
(* will we leave this statement or block with a break command? *)
and stmtCanBreak (s: stmt) : bool = 
  match s.skind with
    Instr _ | Return _ | Continue _ | Goto _ -> false
  | Break _ -> true
  | If (_, b1, b2, _) -> 
      blockCanBreak b1 || blockCanBreak b2
  | Switch _ | Loop _ -> 
      (* switches and loops catch any breaks in their bodies *)
      false
  | Block b -> blockCanBreak b
  | TryFinally (b, h, _) -> blockCanBreak b || blockCanBreak h
  | TryExcept (b, _, h, _) -> blockCanBreak b || blockCanBreak h
  | CpcSpawn _ | CpcFun _ -> false
and blockCanBreak b = 
  List.exists stmtCanBreak b.bstmts
(***** End of code from cabs2cil.ml *****)

(* goto_method:
   0 = add gotos everywhere at the end functionalized blocks
   1 = add gotos only when the last block can be fallen through (DEFAULT)
   2 = make functionalized blocks as short as possible, stopping at the
       first we can't be fallen through. *)
let goto_method = ref 1

class functionalizeGoto start enclosing_fun file =
      let last_var = find_last_var start file in
      let label = match List.find is_label start.labels with
        | Label(l,_,_) -> l | _ -> assert false in
      let fd = emptyFunction (make_function_name label) in
      (* the return type of the functionalized chunk *)
      let ret_type = fst4 (splitFunctionTypeVI enclosing_fun.svar) in
      let () = fd.svar.vcps <- true in
      object(self)
        inherit mynopCilVisitor

        val mutable acc = false (* are we in an accumulating phase?*)
        val mutable stack = []
        val mutable last_stmt = dummyStmt; (* last stmt processed -- the
        final goto must be added there when we're done accumulating *)

        method private unstack_block b =
          let compute_returns s =
            if typeSig ret_type = typeSig voidType
            then None, None
            else
              let f = enclosing_function s file in
              let v = make_ret_var f ret_type in
              let ret_val = Var v, NoOffset in
              (Some ret_val, Some (Lval ret_val))
            in
          let args =
          setFunctionType fd (TFun (ret_type,Some [],false,[]));
          match last_var with
          | None -> []
          | Some v -> setFormals fd [v]; [Lval(Var v, NoOffset)] in
          (* s is the statement to be replaced by a call to fd *)
          let call_fun s =
            let loc = get_stmtLoc s.skind in
            let (return_val,return_exp) = compute_returns s in
            [mkStmt (Instr
              [Call(return_val,Lval(Var fd.svar, NoOffset),args,loc)]);
            mkStmt (Return (return_exp, loc))] in

          acc <- false;
          fd.sbody <- mkBlock (List.rev stack);
          stats (dprintf "goto_stack:%d\n" (List.length stack));
          stack <- [];
          start.skind <- Block (mkBlock (call_fun start));
          b.bstmts <- compactStmts
             [mkStmt (Block (mkBlock(b.bstmts)));
              mkStmt (CpcFun (fd, locUnknown))];
          let start_copy = List.hd fd.sbody.bstmts in
          (* Replace every goto to the start of fd by a call to fd. *)
          replaceGotos start_copy call_fun;
          assert(List.for_all is_label start_copy.labels); (* No Case or Default *)
          start_copy.labels <- []

        method vstmt (s: stmt) : stmt visitAction =
          last_stmt <- s;
          if s == start then (assert(stack = []); acc <- true);
          if acc then begin
            let copy = copyClearStmt s in
            stack <- copy :: stack;
            (* Do NOT stop accumulating as early as possible: we want
            bigger functions to maximize the odds to keep gotos and
            labels in the same function, preventing live gotos which
            would create still more functions.
            So the following code is actually a BAD idea, and the user
            should chose it explicitly with --goto: *)
            if !goto_method = 2 && not (stmtFallsThrough copy)
            then acc <- false;
            SkipChildren end
          else
            DoChildren

        method vblock (b: block) : block visitAction =
          assert(acc = false);
          let enclosing = last_stmt in
          ChangeDoChildrenPost(
            b,
            (fun b ->
              last_stmt <- enclosing;
              if stack <> []
              (* Do NOT compactStmts the stack!
                 [i1;i2];[i3;i4] becomes [i3;i4];[i1;i2] on the stack,
                 and is compacted to [i3;i4;i1;i2] which messes with
                 add_goto_after of course. *)
              then begin match stack, enclosing.succs,
              enclosing.skind with
              (* Loops should have been trivialized first, and stack should
               * contain at least the <start> stmt *)
              | [], _, _ -> assert false
              | _, _, Loop _ -> assert false
              | _, _, CpcFun _
              | _, [], _ ->
                  (* if we don't have any successor, or if we are at the
                     end of a local function, adding a goto is
                     impossible. *)
                  self#unstack_block b
              | s :: _ , _, _ when (!goto_method = 1 || !goto_method = 2) &&
                  not (stmtFallsThrough s) ->
                  (* not falling through the end of stack, so adding a
                  goto is useless. --goto 0 disables this crucial
                  optimization *)
                  self#unstack_block b
              | last_in_stack :: _, _, _ ->
                  (* XXX this works only because we do not change *any*
                   * statement while visiting the file --- otherwise
                   * the destination label is somehow deleted when
                   * returning from vblock. Updating in place is OK of
                   * course. *)
                  self#unstack_block b;
                  add_goto_after last_in_stack enclosing;
              end;
              assert(acc = false && stack = []);
              b
            )
         )
end

exception BreakContinue of stmt

(* Look for break/continue in statements to be functionalized ---
XXX code must be kept in sync with functionalizeGotos !!! ---
and return the nearest enclosing loop/switch statement *)

let rec functionalize start f =
  begin try
    let enclosing = ref dummyFunDec in
    let findEnclosing = 
      object(self)
        inherit (enclosingFunction dummyFunDec)

        val mutable nearest_loop = dummyStmt
        val mutable seen_start = false
        val mutable check_loops = false

        method vstmt s =
          assert(check_loops =>  seen_start);
          if s == start then (seen_start <- true; check_loops <- true;
          enclosing := ef);
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
              (seen_start <- seen; check_loops <- cl; b))

      end
    in  visitCilFileSameGlobals findEnclosing f;
    visitCilFileSameGlobals (new functionalizeGoto start !enclosing f) f;
  with
  | BreakContinue s ->
      assert( s != dummyStmt);
      trace (dprintf "found escaping break or continue: trivializing\n%a\n" d_stmt s);
      eliminate_switch_loop s;
      trace (dprintf "***\nresult:\n%a\n" d_stmt s);
  | Not_found -> E.s (E.bug "no enclosing function found\n")
      end

(*****************************************************************************)

class printCfg = object(self)
  inherit mynopCilVisitor

  method vfunc fd =
  if fd.svar.vcps then begin
    Cfg.clearCFGinfo fd;
    ignore(Cfg.cfgFun fd);
    Cfg.printCfgFilename ("cfg/"^fd.svar.vname^".dot") fd
  end; DoChildren
end

(*****************************************************************************)

let pause = ref false
let stage = ref max_int
let set_stage x = stage := x
let dumpcfg = ref false
let set_goto x = goto_method := min (max x 0) 2

let rec cps_marking f =
  try
    trace (dprintf "New turn\n");
    visitCilFileSameGlobals (new cleaner) f;
    let r = if !pause then read_line () else "" in
    if r = "q" then raise Exit else
    if r = "d" then (dumpFile defaultCilPrinter stdout "" f; cps_marking f)
    else if r = "r" then (pause := false; cps_marking f)
    else
    visitCilFileSameGlobals (new markCps f) f;
  with
  | TrivializeStmt s when s = dummyStmt ->
      E.s (E.error "break or continue with no enclosing loop")
  | TrivializeStmt s ->
      trace (dprintf "TrivializeStmt %a\n" d_stmt s);
      eliminate_switch_loop s;
      cps_marking f
  | AddGoto {last_stmt = src; next_stmt = dst} ->
      add_goto src dst;
      cps_marking f
  | SplitInstr ({skind = Instr l} as s, i, shall_add_goto) ->
      let rec split_instr acc = function
        | [] -> raise Not_found
        | t::q as l when t==i -> (List.rev acc, l)
        | t::q -> split_instr (t::acc) q in
      let (l1,l2) = split_instr [] l in
        begin
        let (s1, s2) = (mkStmt (Instr l1), mkStmt (Instr l2)) in
        trace (dprintf "SplitInstr %a\n:\n%a\n***\n%a\n" d_stmt s d_stmt s1 d_stmt s2);
        s.skind <- Block (mkBlock ([s1; s2]));
        if shall_add_goto
        then add_goto s1 s2
        else
          (* avoid fusion of s1 and s2 *)
          s2.cps <- true;
        end;
      cps_marking f
  | SplitInstr (s, _, _) ->
      E.s (E.bug "SplitInstr raised with wrong argument %a" d_stmt s)
  | FunctionalizeGoto (start,c) ->
      trace (dprintf "functionalize goto\n");
      begin match c.enclosing_stmt.skind with
      | Switch _ | Loop _ ->
          trace (dprintf "enclosing is a switch or a loop: trivializing first\n");
          eliminate_switch_loop c.enclosing_stmt;
          cps_marking f
      | _ -> functionalize start f; cps_marking f
      end

let stages = [
  ("Folding if-then-else\n", fun file ->
  visitCilFileSameGlobals (new folder) file);
  ("Initialize safe functions\n", fun file ->
  visitCilFile (new initSafeFunctions) file);
  ("Add defaults arguments\n", fun file ->
  visitCilFileSameGlobals (new addDefaultArgs file) file);
  ("Lambda-lifting\n", fun file ->
  visitCilFile (new lambdaLifter) file;
  visitCilFileSameGlobals (new uniqueVarinfo) file);
  ("Initialize label table\n", fun file ->
  visitCilFileSameGlobals (new initLabelTbl) file);
  (*
  ("Initialize ampersand table\n", fun file ->
  visitCilFileSameGlobals (new initAmpSet) file);
  (* WARNING: do not call uniqueVarinfo between building and using
   * the ampersand table, since it will deprecate any recorded varinfo!
   *)
  ("Avoid ampersand\n", fun file ->
  visitCilFileSameGlobals (new avoidAmpersand file) file);
  *)
  ("adding an empty environment with frees and mallocs\n", fun file ->
     visitCilFileSameGlobals (new addEnvStruct file) file);
  ("Remove nasty expressions\n", fun file ->
  visitCilFileSameGlobals (new removeNastyExpressions) file);
  ("Handle assignment cps return values\n", fun file ->
  visitCilFileSameGlobals (new cpsReturnValues) file);
  ("Insert gotos after cps assignments and returns after cpc_done\n",
  fun file -> visitCilFileSameGlobals (new insertGotos) file);
  ("Cps marking\n", fun file ->
  cps_marking file);
  ("fill environment, indirect...\n", fun file ->
     visitCilFile (new createEnv2 file) file);
  ("Percolating local variables\n", fun file ->
  percolateLocals file);
  ("Lambda-lifting\n", fun file ->
  visitCilFile (new lambdaLifter) file;
  visitCilFileSameGlobals (new uniqueVarinfo) file);
  ("Remove identity functions\n", fun file ->
  removeIdentity file);
  ("Dumping Cfg\n", fun file -> if !dumpcfg then begin
  (try Unix.mkdir "cfg" 0o750 with Unix.Unix_error (Unix.EEXIST,_,_) -> ());
  visitCilFileSameGlobals (new printCfg) file end);
  ("Cps conversion\n", fun file ->
  visitCilFile (new cpsConverter file) file);
  ("Alpha-conversion\n", fun file ->
  uniqueVarNames file);
  ("Removing unused variables\n", fun file ->
  Rmtmps.keepUnused := false; Rmtmps.removeUnusedTemps file);
]

let rec doit (f: file) =
  try
    ignore(List.fold_left (fun n (descr,step) ->
        if !stage < n then raise Exit;
        trace (dprintf "Stage %d: %s" n descr);
        Stats.time descr step f;
        Stats.time "Cleaning things a bit\n"
          (visitCilFileSameGlobals (new cleaner)) f;
        n+1) 0 stages);
    trace (dprintf "Finished\n")
  with Exit -> E.log "Exit\n"

let feature : featureDescr =
  let is_default = function
    | true -> " (default)"
    | false -> "" in
  { fd_name = "cpc";
    fd_enabled = ref true;
    fd_description = "cpc translation to C";
    fd_extraopt =
      [
       ("--stage",Arg.Int set_stage,"<n> how far you want to go");
       ("--pause",Arg.Set pause," step by step execution");
       ("--dumpcfg",Arg.Set dumpcfg," dump the cfg of cps functions in cfg/");
       ("--goto", Arg.Int set_goto, "<n> how to convert gotos (0-2)");
       ("--external-patch",Arg.Set external_patch," call \
       cpc_continuation_patch from the runtime library" ^ is_default(!external_patch));
       ("--noexternal-patch",Arg.Clear external_patch," generate inline \
       patching" ^ is_default(not !external_patch));
       ("--packed", Arg.Clear aligned_continuations, " compact continuations");
      ];
    fd_doit = doit;
    fd_post_check = true;
  }

