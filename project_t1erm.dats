(* ****** ****** *)
#staload "./project.sats"
#include "share/atspre_staload.hats"
#staload "./../../../mylib/mylib.dats"
#include "./../../HATS/libxatsopt.hats"

(* ****** ****** *)
implement fprint_val<t1ype> = fprint_t1ype
implement fprint_val<t1val> = fprint_t1val
implement fprint_val<t1erm> = fprint_t1erm
implement fprint_val<t1dcl> = fprint_t1dcl

extern fun print_t1env(t1env): void
extern fun fprint_t1env(out:FILEref, tvs:t1env): void
implement fprint_val<t1env> = fprint_t1env
overload print with print_t1env
overload fprint with fprint_t1env

(* ****** ****** *)
implement print_t1erm(t1m0) =
  fprint_t1erm(stdout_ref, t1m0)
implement print_t1dcl(tdcl) =
  fprint_t1dcl(stdout_ref, tdcl)
implement print_t1env(env) = 
  fprint_t1env(stdout_ref, env)
implement fprint_t1env(out, env) = 
  let
    val () = fprint(out, "[\n")
    val () = loop(env)
  in () end
  where {
    fun loop(tvs: t1env) = 
      case+ tvs of 
      | mylist_nil() => fprint(out, "]\n")
      | mylist_cons(tv, tvs) => 
        let
          val-(a, b) = tv
          val () = fprint(out, a)
          val () = fprint(out, ": ")
          val () = fprint(out, b)
          val () = fprint(out, ", ")
          val () = fprint(out, "\n")
          val () = loop(tvs)
        in () end
  }
overload fprint with fprint_t1env
overload print with print_t1env

(* ****** ****** *)
implement fprint_t1erm(out, t1m0) = (
  case+ t1m0 of
  | T1Mnil() => fprint!(out, "()")
  | T1Mint(int) => fprint!(out, "int(", int, ")")
  | T1Mbtf(btf) => fprint!(out, "btf(", btf, ")")
  | T1Mstr(str) => fprint!(out, "str(", str, ")")
  | T1Mvar(t1v1) => fprint!(out, "var(", t1v1, ")")
  | T1Mlam(t1vx, targ, t1m1) => fprint!(out, "lam(", t1vx, ", ", targ, ", ", t1m1, ")")
  | T1Mapp(t1m1, t1m2) => fprint!(out, "app(", t1m1, ", ", t1m2, ")")
  | T1Mfst(t1m1) => fprint!(out, t1m1, ".fst")
  | T1Msnd(t1m1) => fprint!(out, t1m1, ".snd")
  | T1Mtup(t1m1, t1m2) => fprint!(out, "(", t1m1, ", ", t1m2, ")")
  | T1Mseq(t1ms) => fprint!(out, "seq(", t1ms, ")")
  | T1Mopr(topr, t1ms) => fprint!(out, "opr(", topr, ": ", t1ms, ")")
  | T1Mif0(t1m1, t1m2, opt3) => fprint!(out, "if0(", t1m1, "; ", t1m2, " : ", opt3, ")")
  | T1Mlet(dcls, t1m1) => fprint!(out, "let(", dcls, " in ", t1m1, ")end")
  | T1Mfix(t1vf, t1vx, targ, t1m1, tres) => 
      fprint!(out, "fix(", t1vf, ", ", t1vx, ", ", targ, ", ", t1m1, ", ", tres, ")")
  | T1Mlazy(t1m1) => fprint!(out, "lazy(", t1m1, ")")
  | T1Manno(t1m1, t1p2) => fprint!(out, "anno(", t1m1, ": ", t1p2, ")")
  // HX: it is for error indcation:
  | T1Mnone(d1e1) => fprint!(out, "T1Mnone(", d1e1, ")")
) (*case+*) // end of [fprint_t1erm(out, t1m0)]

(* ****** ****** *)
implement fprint_t1dcl(out, tdcl) = (
  case+ tdcl of
  | T1DCLbind(t1v1, t1m2) => fprint!(out, "bind(", t1v1, ": ",  t1m2, ")")
  (*
  | // HX: it is for error indcation:
  T1DCLnone(d1cl) => fprint!(out, "T1DCLnone(", d1cl, ")")
  *)
) (*case+*) // end of [fprint_t1dcl(out, tdcl)]

(* end of [CS525-2022-Fall/Final_project_t1erm.dats] *)
