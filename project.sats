(*
** For your
** Final project
*)

(* ****** ****** *)
#staload "./../../../mylib/mylib.dats"
#include "./../../HATS/libxatsopt.hats"

(* ****** ****** *)
datatype t1ype =
| T1Pnil of ()
| T1Pbas of tpbas
| T1Pext of tpVar
| T1Pfun of (t1ype, t1ype)
| T1Ptup of (t1ype, t1ype)
| T1Plist of (t1ype(*elt*))
| T1Plazy of (t1ype(*elt*))
| T1Pstrm of (t1ype(*elt*))
| T1Pnone of (s1exp(*unsupported*))
where tpbas = string
  and tpVar = ref(myoptn(t1ype))
  and t1ypelst = mylist(t1ype)
  and t1ypeopt = myoptn(t1ype)

(* ****** ****** *)
val T1Pint: t1ype
val T1Pbool: t1ype
val T1Punit: t1ype
val T1Pstring: t1ype

(* ****** ****** *)
fun print_t1ype(t1ype): void
fun fprint_t1ype(out:FILEref, t1p0:t1ype): void
overload print with print_t1ype
overload fprint with fprint_t1ype

(* ****** ****** *)
(*
HX-2022-11-01:
[ctype] for primitive operators
*)
datatype ctype = CTYPE of (t1ypelst, t1ype)

(* ****** ****** *)
fun print_ctype(ctype): void
fun fprint_ctype(out:FILEref, ct1p:ctype): void
overload print with print_ctype
overload fprint with fprint_ctype

(* ****** ****** *)
typedef t1opr = string
typedef t1var = string
(* ****** ****** *)
datatype t1dcl =
| T1DCLbind of (t1var, t1erm)
// | T1DCLnone of (d1ecl(*unsupported*))
and t1erm =
| T1Mnil of ()
| T1Mint of int
| T1Mbtf of bool
| T1Mstr of string
| T1Mvar of t1var
| T1Mlam of (t1var, t1ypeopt, t1erm)
| T1Mapp of (t1erm, t1erm)
| T1Mopr of (t1opr, t1ermlst(*arg*))
| T1Mif0 of (t1erm, t1erm, t1ermopt)
| T1Mfst of (t1erm)
| T1Msnd of (t1erm)
| T1Mtup of (t1erm, t1erm)
| T1Mseq of (t1ermlst)
| T1Mlet of (t1dclist, t1erm)
| T1Mfix of (t1var, t1var, t1ypeopt(*arg*), t1erm, t1ypeopt(*res*))
| T1Manno of (t1erm, t1ype(*anno*))
| T1Mlazy of (t1erm)
| T1Mnone of (d1exp(*unsupported*))
where t1dclist = mylist(t1dcl)
  and t1ermlst = mylist(t1erm)
  and t1ermopt = myoptn(t1erm)

(* ****** ****** *)
fun print_t1dcl(t1dcl): void
fun fprint_t1dcl(out:FILEref, d1cl:t1dcl): void
overload print with print_t1dcl
overload fprint with fprint_t1dcl

(* ****** ****** *)
fun print_t1erm(t1erm): void
fun fprint_t1erm(out:FILEref, t1m0:t1erm): void
overload print with print_t1erm
overload fprint with fprint_t1erm

(* ****** ****** *)
typedef t1ctx = mylist(@(t1var, t1ype))
(* ****** ****** *)
datatype t1val =
| T1Vnil of ()
| T1Vint of int
| T1Vbtf of bool
| T1Vstr of string
| T1Vtup of (t1val, t1val)
| T1Vlam of (t1erm, t1env)
| T1Vfix of (t1erm, t1env)
// for constructors
| T1Vcons of (int(*tag*), t1valist)
| T1Vstrm of (myoptn(t1val), t1val)
// for thunkification
| T1Vlazy of (t1erm, t1env)
where t1env = mylist(@(t1var, t1val))
  and t1valist = mylist(t1val)

(* ****** ****** *)
fun print_t1val(t1val): void
fun fprint_t1val(out:FILEref, t1v0:t1val): void
overload print with print_t1val
overload fprint with fprint_t1val

(* ****** ****** *)
(*
HX-2022-10-19:
For A-normal form
*)
datatype t2cmp = 
  T2CMP of (t2bndlst, t2box)
and t2box =
| T2Vnil of ()
| T2Vint of int
| T2Vbtf of bool
| T2Vstr of string
| T2Varg of (t2arg, t1var)
| T2Vreg of (t2reg)
| T2Vfun of (t1var, t1var, t2cmp, t2ins)
| T2Vfnm of (t1var)
| T2Vlzy of (t1var, t1var, t2cmp, t2ins)
and t2ins =
| T2Iopr of (t1opr, t2boxlst)
| T2Ifst of (t2box) // first projection
| T2Isnd of (t2box) // second projection
| T2Itup of (t2box, t2box) // pair formation
| T2Iif0 of (t2box, t2bndlst, t2bndlst)
| T2Ical of (t2box, t2box)
| T2Ienv of (int, c3env) // closure environment
and t2bnd = 
| T2BOX of (t2box)
| T2BND of (t2reg, t2ins)
where t2arg = int 
  and t2reg = int 
  and t2boxlst = mylist(t2box) 
  and t2bndlst = mylist(t2bnd) 
  and t2inslst = mylist(t2ins)
  and t2env = mylist(@(t1var, t2cmp))
  and t3env = mylist(@(t1var, t2box))
  and c3env = @(t1var, t3env)

(* ****** ****** *)
fun print_t2box(t2box): void
fun fprint_t2box(out:FILEref, t1p0:t2box): void
overload print with print_t2box
overload fprint with fprint_t2box
fun print_t2bnd(t2bnd): void
fun fprint_t2bnd(out:FILEref, t1p0:t2bnd): void
overload print with print_t2bnd
overload fprint with fprint_t2bnd
fun print_t2ins(t2ins): void
fun fprint_t2ins(out:FILEref, t1p0:t2ins): void
overload print with print_t2ins
overload fprint with fprint_t2ins
fun print_t2cmp(t2cmp): void
fun fprint_t2cmp(out:FILEref, t1p0:t2cmp): void
overload print with print_t2cmp
overload fprint with fprint_t2cmp
fun print_c3env(c3env): void
fun fprint_c3env(out:FILEref, t1p0:c3env): void
overload print with print_c3env
overload fprint with fprint_c3env

(* ****** ****** *)
fun trans1m_s1exp: s1exp -> t1ype
fun trans1m_s1explst: s1explst -> t1ypelst
fun trans1m_s1expopt: s1expopt -> t1ypeopt

(* ****** ****** *)
fun trans1m_d1exp: d1exp -> t1erm
fun trans1m_d1explst: d1explst -> t1ermlst
fun trans1m_d1expopt: d1expopt -> t1ermopt

(* ****** ****** *)
fun trans1m_d1ecl: d1ecl -> t1dcl
fun trans1m_d1eclist: d1eclist -> t1dclist

(* ****** ****** *)
fun trans1m_main_fun: d1eclist -> t1ermopt

(* ****** ****** *)
// interpreter
fun t1erm_interp0(t1erm): t1val
fun t1dclist_interp0(t1dclist): t1env

(* ****** ****** *)
// type-checker
fun t1erm_oftype0(t1erm): t1ype
fun t1dclist_oftype0(t1dclist): t1ctx

(* ****** ****** *)
// a normal form
fun t1erm_atrans0(t1erm): t2cmp
fun t1dclist_atrans0(t1dclist): t2env

(* ****** ****** *)
// emitting c
fun t2env_emit0(FILEref, t2env): void

(* ****** ****** *)
fun project_main0{n:int | n >= 1}
  (int(n), !argv(n)): void

(* ****** ****** *)

(* end of [CS525-2022-Fall/Final_project.sats] *)
