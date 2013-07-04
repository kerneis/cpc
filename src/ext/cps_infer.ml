(*
 * Copyright (c) 2013
 *  Gabriel Kerneis     <kerneis@pps.univ-paris-diderot.fr>
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
open Pretty
module E = Errormsg

(* Copied from cpc.ml *)
let visitReturnType visitor t = match unrollType t with
  | TFun(rt, args, isva, a) ->
      TFun(visitCilType visitor rt, args, isva, visitCilAttributes visitor a)
  | _ -> E.s (E.bug "visitReturnType called on a non-function type")

let is_cps_type ft =
  let cps_attr = ref false in
  ignore(visitReturnType
    (object(self)
      inherit nopCilVisitor
      method vtype t = match t with
      | TFun _ -> SkipChildren
      | _ -> DoChildren
      method vattr (Attr (a, _)) = if a = "cps" then cps_attr := true; SkipChildren
    end)
    ft);
  !cps_attr

(* Common operations on varinfo *)
module V = struct
  type t = varinfo
  let compare x y = compare x.vid y.vid
  let hash x = x.vid
  let equal x y = (=) x.vid y.vid
end

(* Directed graph of varinfo *)
module G = Graph.Imperative.Digraph.ConcreteBidirectional(V) ;;

(* Reachability analysis for varinfo.
 * If you initialize with a set of trusted (external for instance)
 * cps functions, and build called-by edges, the reachable varinfo
 * are the functions that should be cps *)
module Reachability = Graph.Fixpoint.Make(G)
(struct
    type vertex = G.E.vertex
    type label = G.E.label
    type cfg = G.t
    type data = bool
    let direction = Graph.Fixpoint.Forward
    let equal = (=)
    let join = (||)
    let analyze _ = (fun x -> x)
end)

let g = G.create()

(* Sets of declared and defined functions *)
module VS = Set.Make(V)
let decl = ref VS.empty
let def = ref VS.empty

(* vertex collecter *)
let vregister var set =
  E.log "vregister: %a\n" d_type var.vtype;
  G.add_vertex g var;
  set := VS.add var !set

class vcollect =
  object(self)
  inherit nopCilVisitor

  method vstmt s = match s.skind with
  | CpcFun ({svar = v}, _) -> vregister v def; DoChildren
  | _ -> DoChildren

  method vglob = function
  | GVarDecl(v,_) when isFunctionType v.vtype -> vregister v decl; SkipChildren
  | GFun ({svar=v},_) -> vregister v def; DoChildren
  | _ -> SkipChildren
end

(* edge collecter *)
class ecollect =
  object(self)
  inherit nopCilVisitor
  val mutable ef = dummyFunDec

  method vfunc f =
    let f' = ef in ef <- f;
    ChangeDoChildrenPost(f, fun f -> ef <- f';f)

  method vinst = function
  | Call(_, Lval(Var v, NoOffset), _, _) ->
      G.add_edge g v ef.svar;
      SkipChildren
  | Call(_, e, _, _) ->
      let v = makeVarinfo false "indirect-call" (typeOf e) in
      vregister v decl;
      G.add_edge g v ef.svar;
      SkipChildren
  | _ -> SkipChildren
end

let should_be_cps = ref (fun _ -> failwith "run analysis first")

(* Graphviz rendering *)
module ColoredG = struct
  include G
  let graph_attributes g = []
  let default_vertex_attributes g = []
  let vertex_name v =
    v.vname
  let vertex_attributes v =
    let shape =
      if !should_be_cps v then `Box else `Ellipse in
    let color =
      if !should_be_cps v != is_cps_type v.vtype
      then 0xff0000 else 0x000000 in
    let style = 
      if !should_be_cps v != is_cps_type v.vtype
      then `Dashed else `Solid in
    [`Shape shape; `Color color; `Style style]
  let get_subgraph v = None
  let default_edge_attributes g = []
  let edge_attributes (src, dst) = []
end

module Draw = Graph.Graphviz.Dot(ColoredG) ;;

let draw filename g =
  let chan = open_out filename in
  Draw.output_graph chan g;
  close_out chan

let doit file =
  (* work on a copy of the file, with unused variables removed *)
  let file = { (file) with fileName = file.fileName } in
  Rmtmps.removeUnusedTemps file;
  visitCilFileSameGlobals (new vcollect) file;
  visitCilFileSameGlobals (new ecollect) file;
  let no_def = VS.diff !decl !def in
  (* start with cps functions that are declared but not defined *)
  let init_cps v = VS.mem v no_def && is_cps_type v.vtype in
  should_be_cps := Reachability.analyze init_cps g;
  draw "cps.dot" g

let feature : featureDescr =
  { fd_name = "cpsInference";
    fd_enabled = ref false;
    fd_description = "infer needed and spurious cps annotations";
    fd_extraopt = [ ];
    fd_doit = doit;
    fd_post_check = true;
  }

