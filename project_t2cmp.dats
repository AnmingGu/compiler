(* ****** ****** *)
#staload "./project.sats"
#include "share/atspre_staload.hats"
#staload "./../../../mylib/mylib.dats"
#include "./../../HATS/libxatsopt.hats"

(* ****** ****** *)
implement fprint_val<t2box> = fprint_t2box
implement fprint_val<t2ins> = fprint_t2ins
implement fprint_val<t2bnd> = fprint_t2bnd
implement fprint_val<t2cmp> = fprint_t2cmp
implement fprint_val<c3env> = fprint_c3env

(* ****** ****** *)
implement print_t2box(t1x0) =
  fprint_t2box(stdout_ref, t1x0)
implement print_t2ins(ins) =
  fprint_t2ins(stdout_ref, ins)
implement print_t2bnd(tbnd) =
  fprint_t2bnd(stdout_ref, tbnd)
implement print_t2cmp(tcmp) =
  fprint_t2cmp(stdout_ref, tcmp)
implement print_c3env(c3env) = 
  fprint_c3env(stdout_ref, c3env)

(* ****** ****** *)
implement fprint_t2box(out, t2x0) = (
  case+ t2x0 of
  | T2Vnil() => fprint!(out, "()")
  | T2Vint(int) => fprint!(out, "int(", int, ")")
  | T2Vbtf(btf) => fprint!(out, "btf(", btf, ")")
  | T2Vstr(str) => fprint!(out, "str(", str, ")")
  | T2Varg(_, arg) => fprint!(out, "arg(0, ", arg, ")")
  | T2Vreg(reg) => fprint!(out, "reg", reg)
  | T2Vfun(fnm, arg, tcmp, ins) => fprint!(out, "fun(", fnm, "(", arg ,"): ", "cmp", "; ", ins, ")")
  | T2Vfnm(nme) => fprint!(out, "fnm(", nme, ")")
  | T2Vlzy(nme, arg, tcmp, ins) => fprint!(out, "lzy(", nme, "(", arg, "): ", "cmp", "; ", ins, ")")
) // end of [fprint_t2box]

(* ****** ****** *)
implement fprint_t2ins(out, tins) = (
  case+ tins of
  | T2Iopr(topr, t2xs) =>
      fprint!(out, "opr(", topr, ": ", t2xs, ")")
  | T2Ifst(t2x1) => fprint!(out, t2x1, ".fst")
  | T2Isnd(t2x1) => fprint!(out, t2x1, ".snd")
  | T2Itup(t2x1, t2x2) => 
      fprint!(out, "(", t2x1, ", ", t2x2, ")")
  | T2Iif0(t2x1, tbs1, tbs2) =>
      fprint!(out, "if0(", t2x1, "; ", tbs1, " : ", tbs2, ")")
  | T2Ical(t2x1, t2x2) =>
      fprint!(out, "cal(", t2x1, ", ", t2x2, ")")
  | T2Ienv(int, c3env) => fprint!(out, "T2Ienv(", int, ", ", c3env, ")")
) // end of [fprint_t2ins]

(* ****** ****** *)
implement fprint_t2bnd(out, tbnd) = (
  case+ tbnd of
  | T2BND(treg, tins) =>
      fprint!(out, "BND(", treg, ": ", tins, ")")
  | T2BOX(bx) => fprint!(out, "BOX(", bx, ")")
) // end of [fprint_t2bnd]

(* ****** ****** *)
implement fprint_t2cmp(out, tcmp) = (
  case+ tcmp of
  | T2CMP(bnds, t2x1) =>
      fprint!(out, "CMP(", bnds, ": ", t2x1, ")")
) // end of [fprint_t2cmp]


(* ****** ****** *)
implement fprint_c3env(out, env) = (
  fprint!(out, "C3ENV(", env.0, ", ", env.1, ")")
) // end of [fprint_t2cmp]

(* ****** ****** *)

(* end of [CS525-2022-Fall/Final_project_t2cmp.dats] *)
