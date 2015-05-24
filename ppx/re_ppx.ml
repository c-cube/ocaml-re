open Ast_mapper
module AH = Ast_helper
module AT = Asttypes
module P = Parsetree
open Longident

let rec build_mark_list = function
  | [] -> [], []
  | re :: l ->
    let rel, fl = build_mark_list l in
    let id, re = Re.mark re in
    re::rel, id::fl
 
let rec find_mark subs = function
  | [] ->
    (* Invariant: At least one of the regexp of the alternative matches. *)
    assert false
  | id :: l ->
    if Re.marked subs id then id
    else find_mark subs l

let rec compile_re_cases re_list acc l = match l with
  | [] -> Re.alt re_list, List.rev acc
  | (re, var_list, branch) :: tail ->
    compile_re_cases
      (re :: re_list)
      (assert false) (* TODO *)
      tail
 
let match_  ~default l =
  let rel, fl = build_mark_list l in
  let re = Re.(compile @@ whole_string @@ alt rel) in
  fun ?pos ?len s ->
    try
      let subs = Re.exec ?pos ?len re s in
      find_mark subs fl
    with
        Not_found -> default s

(* check a list of patterns is a list of variables *)
let is_list_of_vars l =
  List.for_all
    (function
      | {P.pattern_desc=P.Ppat_var _ | P.Ppat_any; _} -> true
      | _ -> false
    ) l

(* transforms "s" (string pattern)
   into " (try Some (Re.exec re s) witn Not_found -> None), s"
  *)
let wrap_re_exec re s =
  [%expr (try Some (Re.exec [%e re] [%e s]) with Not_found -> None), [%e s]]

(* splits [l] (a list of patterns, cases) into [prefix, suffix].
   Prefix is composed of pairs
   [re, var list, branch], suffix is regular patterns (matched against the raw string) *)
let rec map_re_cases l = match l with
  | [] -> [], []
  | {P.pattern_desc=P.Pat_tuple
      [{ P.pattern_desc=P.Ppat_tuple
       ({P.pattern_desc=P.Ppat_constant (AT.Const_string (s, None)); _}::vars)
       } as pat
      ; branch
      ]
    } :: tail ->
    if is_list_of_vars vars
    then
      (* is indeed "string, var1, var2...", continue looking for patterns *)
      let prefix, suffix = map_re_cases tail in
      (re, vars, branch) :: prefix, suffix
    else [], pat::tail (* stop recursion *)
  | pat :: tail -> [], pat :: tail (* stop recursion *)

let mapper _argv =
  { default_mapper with
    expr= fun self e -> match e.P.pexp_desc with
      | P.Pexp_extension
          ({AT.txt="re"; _},
           P.PStr [{P.pstr_desc=
                      P.Pstr_eval ({P.pexp_desc=P.Pexp_match (pattern, cases); _}, _); _}]
          ) ->
        (* "match%re pattern with cases"
           the cases of the form "regex, var1, var2, ..." are updated so that
           they use a regex *)
        let re_cases, other_cases = map_re_cases cases in
        if re_cases = [] then fail "expect at least one regex pattern";
        (* merge all regex, and unsugar the corresponding cases *)
        let re, re_cases = compile_re_cases re_cases in
        (* generate pattern matches *)
        [%expr
          let re = Re.compile (Re.whole_string [%e re]) in
          try
            let substring = Re.exec re [%e pattern] in
            [% assert false] (* TODO *)
          with Not_found ->
            (* default *)
            [%e AH.Exp.match_ pattern other_cases]
        ]
      | _ -> self.expr self e (* recurse *)
  }

let () =
  register "re" mapper

