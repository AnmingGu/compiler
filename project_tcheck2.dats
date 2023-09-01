(* ****** ****** *)
#staload "./project.sats"
#include "share/atspre_staload.hats"
#staload "./../../../mylib/mylib.dats"

(* ****** ****** *)
(*
typedef t1ctx = mylist(@(t1var, t1ype))
*)
(* ****** ****** *)
extern fun tpVar_new(): tpVar
extern fun t1ype_new_ext(): t1ype
extern fun t1ype_new_fun(): t1ype
extern fun t1ype_new_tup(): t1ype
extern fun t1ype_eval(t1ype): t1ype

(* ****** ****** *)
implement tpVar_new() = ref(myoptn_nil())
implement t1ype_new_ext() = T1Pext(tpVar_new())
implement t1ype_new_fun() = T1Pfun(t1ype_new_ext(), t1ype_new_ext())
implement t1ype_new_tup() = T1Ptup(t1ype_new_ext(), t1ype_new_ext())

(* ****** ****** *)
implement t1ype_eval(tp0) =
  case+ tp0 of
  | T1Pext(X0) => (
    case+ !X0 of
    | myoptn_nil() => tp0
    | myoptn_cons(tp1) => t1ype_eval(tp1)
    )
  | _ (*non-T1Pext*) => tp0

(* ****** ****** *)
extern fun tpVar_equal_ref (X1: tpVar, X2: tpVar): bool
overload == with tpVar_equal_ref

implement tpVar_equal_ref(X1, X2) = (ref_get_ptr(X1) = ref_get_ptr(X2))

(* ****** ****** *)
extern fun tpVar_occurs_t1ype(X1: tpVar, tp2: t1ype): bool

(* ****** ****** *)
extern fun t1ype_unify(tp1: t1ype, tp2: t1ype): bool
overload unify with t1ype_unify
extern fun t1ype_unify_var1(tpVar, t1ype): bool
extern fun t1ype_unify_var2(t1ype, tpVar): bool

(* ****** ****** *)
implement tpVar_occurs_t1ype(X1, tp2) =
  let
    val tp2 = t1ype_eval(tp2)
  in
    case+ tp2 of
    | T1Pnil _ => false
    | T1Pbas _ => false
    | T1Pnone _ => false
    | T1Pext(X2) => (X1 == X2)
    | T1Pfun(tp21, tp22) =>
      (tpVar_occurs_t1ype(X1, tp21) || tpVar_occurs_t1ype(X1, tp22))
    | T1Ptup(tp21, tp22) =>
      (tpVar_occurs_t1ype(X1, tp21) || tpVar_occurs_t1ype(X1, tp22))
    | T1Plist(tp3) => tpVar_occurs_t1ype(X1, tp3)
    | T1Plazy(tp3) => tpVar_occurs_t1ype(X1, tp3)
    | T1Pstrm(tp3) => tpVar_occurs_t1ype(X1, tp3)
  end // end of [tpVar_occurs_t1ype]

(* ****** ****** *)
implement t1ype_unify(tp1, tp2) =
  let
    val tp1 = t1ype_eval(tp1)
    val tp2 = t1ype_eval(tp2)
  in
    case (tp1, tp2) of
    | (T1Pext(X1), _) => t1ype_unify_var1(X1, tp2)
    | (_, T1Pext(X2)) => t1ype_unify_var2(tp1, X2)
    | (T1Pbas nm1, T1Pbas nm2) => (nm1 = nm2)
    | (T1Pnil _, T1Pnil _) => true
    | (T1Pbas("unit"), T1Pnil _) => true
    | (T1Pnil _, T1Pbas("unit")) => true
    | (T1Pfun(tp11, tp12), T1Pfun(tp21, tp22)) =>
      (t1ype_unify(tp11, tp21) && t1ype_unify(tp12, tp22))
    | (T1Ptup(tp11, tp12), T1Ptup(tp21, tp22)) =>
      (t1ype_unify(tp11, tp21) && t1ype_unify(tp12, tp22))
    | (T1Plist(tp11), T1Plist(tp21)) => t1ype_unify(tp11, tp21)
    | (T1Plazy(tp11), T1Plazy(tp21)) => t1ype_unify(tp11, tp21)
    | (T1Pstrm(tp11), T1Pstrm(tp21)) => t1ype_unify(tp11, tp21)
    | (_, _) => false // HX: covering the rest of cases
  end // end of [t1ype_unify(tp1, tp2)]

(* ****** ****** *)
implement t1ype_unify_var1(X1, tp2) =
  // HX-2022-10-12:
  // [tp2] is already evaluated!
  case+ tp2 of
  | T1Pext(X2) => 
    if X1 == X2 then true 
    else (!X1 := myoptn_cons(tp2); true)
  | _ =>
    if tpVar_occurs_t1ype(X1, tp2) then false 
    else (!X1 := myoptn_cons(tp2); true)
  // end of [t1ype_unify_var1(X1, tp2)]

(* ****** ****** *)
implement t1ype_unify_var2(tp1, X2) =
  if tpVar_occurs_t1ype(X2, tp1) then false 
  else (!X2 := myoptn_cons(tp1); true)
  // end of [t1ype_unify_var2(tp1, X2)]

(* ****** ****** *)
extern fun t1erm_oftype0(t1erm): t1ype
extern fun t1erm_oftype1(t1erm, t1ctx): t1ype
extern fun t1erm_oftype1_var(t1erm, t1ctx): t1ype
extern fun t1erm_oftype1_opr(t1erm, t1ctx): t1ype
extern fun t1ermlst_oftype1 (tms: t1ermlst, xts: t1ctx): t1ypelst
extern fun t1dclist_oftype1(t1dclist, t1ctx): t1ctx

(* ****** ****** *)
implement t1erm_oftype0(tm0) = 
  t1erm_oftype1(tm0, mylist_nil())
implement t1erm_oftype1(tm0, xts) = (
  case+ tm0 of
  | T1Mnil _ => T1Pnil
  | T1Mint _ => T1Pint
  | T1Mbtf _ => T1Pbool
  | T1Mstr _ => T1Pstring
  | T1Mvar _ => t1erm_oftype1_var(tm0, xts)
  | T1Mlam(x1, tp_opt, tm1) => (
    case tp_opt of 
    | myoptn_nil() =>
      let 
        val tp1 = t1ype_new_ext()
        val tp2 = t1erm_oftype1(tm1, 
          mylist_cons(@(x1, tp1), xts))
      in 
        T1Pfun(tp1, tp2) 
      end
    | myoptn_cons(tp1) =>
      let
        val tp2 = t1erm_oftype1(tm1, 
          mylist_cons(@(x1, tp1), xts))
      in 
        T1Pfun(tp1, tp2) 
      end
    )
  | T1Mfix(fnm, x1, arg_tp_opt, tm1, res_tp_opt) => (
    case (arg_tp_opt, res_tp_opt) of
    | (myoptn_nil(), myoptn_nil()) => 
      let
        val tp_fun = t1ype_new_fun()
        val-T1Pfun(tp11, tp12) = tp_fun
        val tp2 = t1erm_oftype1(tm1, mylist_cons(
          @(fnm, tp_fun), mylist_cons(@(x1, tp11), xts)))
        val-true = t1ype_unify(tp12, tp2)
      in 
        tp_fun 
      end
    | (myoptn_cons(tp11), myoptn_nil()) => 
      let
        val tp_fun = T1Pfun(tp11, t1ype_new_ext())
        val-T1Pfun(_, tp12) = tp_fun
        val tp2 = t1erm_oftype1(tm1, mylist_cons(
          @(fnm, tp_fun), mylist_cons(@(x1, tp11), xts)))
        val-true = t1ype_unify(tp12, tp2)
      in 
        tp_fun 
      end
    | (myoptn_nil(), myoptn_cons(tp12)) => 
      let
        val tp_fun = T1Pfun(t1ype_new_ext(), tp12)
        val-T1Pfun(tp11, _) = tp_fun
        val tp2 = t1erm_oftype1(tm1, mylist_cons(
          @(fnm, tp_fun), mylist_cons(@(x1, tp11), xts)))
        val-true = t1ype_unify(tp12, tp2)
      in 
        tp_fun 
      end
    | (myoptn_cons(tp11), myoptn_cons(tp12)) => 
      let
        val tp_fun = T1Pfun(tp11, tp12)
        val tp2 = t1erm_oftype1(tm1, mylist_cons(
          @(fnm, tp_fun), mylist_cons(@(x1, tp11), xts)))
        val-true = t1ype_unify(tp12, tp2)
      in 
        tp_fun 
      end
    ) // end of [T1Mfix(fnm, x, _, bdy, _)]
  | T1Mapp(tm1, tm2) => 
    let
      val tp1 = t1erm_oftype1(tm1, xts)
      val tp2 = t1erm_oftype1(tm2, xts)
      val tp1 =
        let 
          val tp1 = t1ype_eval(tp1)
        in
          case+ tp1 of
          | T1Pext(X1) =>
            let
              val tp1 = t1ype_new_fun()
            in
              !X1 := myoptn_cons(tp1); tp1
            end
          | _ (*non-T1Pext*) => tp1
        end : t1ype // end-val
      val-T1Pfun(tp11, tp12) = tp1
      val-true = t1ype_unify(tp11, tp2) 
    in 
      tp12 
    end // end of [T1Mapp(tm1, tm2)]
  | T1Mopr _ => t1erm_oftype1_opr(tm0, xts)
  | T1Mif0(tm1, tm2, tm3_opt) => 
    let
      val tp1 = t1erm_oftype1(tm1, xts)
      val tp2 = t1erm_oftype1(tm2, xts)
      val-true = t1ype_unify(tp1, T1Pbool)
    in 
      case+ tm3_opt of 
      | myoptn_nil() =>
        let
          val-true = t1ype_unify(tp2, T1Pnil())
        in 
          T1Pnil() 
        end
      | myoptn_cons(tm3) =>
        let
          val tp3 = t1erm_oftype1(tm3, xts)
          val-true = t1ype_unify(tp2, tp3)
        in 
          tp2 
        end
    end // end of [T1Mif0(tm1, tm2, tm3_opt)]
  | T1Mtup(tm1, tm2) => 
    let 
      val tp1 = t1erm_oftype1(tm1, xts)
      val tp2 = t1erm_oftype1(tm2, xts)
    in 
      T1Ptup(tp1, tp2) 
    end // end of [T1Mtup(tm1, tm2)]
  | T1Mfst(tm1) =>
    let 
      val tp1 = t1erm_oftype1(tm1, xts)
      val tp1 = t1ype_eval(tp1)
    in
      case- tp1 of  
      | T1Ptup(tp11, _) => tp11
      | _ => t1ype_new_ext()
    end
  | T1Msnd(tm1) =>
    let 
      val tp1 = t1erm_oftype1(tm1, xts)
      val tp1 = t1ype_eval(tp1)
    in
      case- tp1 of  
      | T1Ptup(_, tp12) => t1ype_eval(tp12)
      | _ => t1ype_new_ext()
    end
  | T1Mlet(tdcs, tm1) => 
    t1erm_oftype1(tm1, t1dclist_oftype1(tdcs, xts))
  | T1Mseq(tms) => 
    loop(tms, t1ype_new_ext())
    where {
      fun loop(tms: t1ermlst, tp: t1ype): t1ype = 
        case tms of 
        | mylist_nil() => tp
        | mylist_cons(tm1, tms) => 
          let
            val tp1 = t1erm_oftype1(tm1, xts)
          in 
            loop(tms, tp1) 
          end
    } // end of [T1Mseq(tms)]
  | T1Manno(tm1, tp) => 
    let
      val tp1 = t1erm_oftype1(tm1, xts)
      val-true = t1ype_unify(tp1, tp)
    in 
      tp 
    end // end of [T1Manno(tm1, tp)]
  | T1Mlazy(tm1) =>
      let
        val tp1 = t1erm_oftype1(tm1, xts)
      in
        T1Plazy(tp1)
      end
  | T1Mnone _ => T1Pnil()
) // end of [t1erm_oftype1]

(* ****** ****** *)
implement t1erm_oftype1_var(tm0, xts) =
  let
    val-T1Mvar(x) = tm0
  in 
    find(xts) 
    where {
      fun find(xts: t1ctx): t1ype =
        case- xts of
        | mylist_cons(xt, xts) => 
          if x = xt.0 then xt.1 
          else find(xts)
    }
  end // end of [t1erm_oftype1_var]

(* ****** ****** *)
implement t1erm_oftype1_opr(tm0, xts) =
  let
    val-T1Mopr(opr, tms) = tm0
    val tps = t1ermlst_oftype1(tms, xts)
  in (
    case- opr of
    | "~" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
      in 
        T1Pint 
      end // end of [~]
    | "+" =>
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pint 
      end // end of [+]
    | "-" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pint 
      end // end of [-]
    | "*" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pint 
      end // end of [*]
    | "%" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pint 
      end // end of [%]
    | "<" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pbool 
      end // end of [<]
    | ">" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pbool 
      end // end of [>]
    | "=" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pbool 
      end // end of [=]
    | "<=" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pbool 
      end // end of [<=]
    | ">=" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pbool 
      end // end of [>=]
    | "!=" => 
      let
        val-mylist_cons(tp1, tvs) = tps
        val-mylist_cons(tp2, tvs) = tps
        val-true = t1ype_unify(tp1, T1Pint)
        val-true = t1ype_unify(tp2, T1Pint)
      in 
        T1Pbool 
      end // end of [!=]
    | "show" => T1Punit
    | "print" => T1Punit
    | "showval" => T1Punit
    | "list_nil" => T1Plist(t1ype_new_ext())
    | "list_cons" =>
      let
        val-mylist_cons(tp1, tps) = tps
        val-mylist_cons(tp2, tps) = tps
      in 
        case- tp2 of
        | T1Plist(tp21) => (
          let 
            val-true = t1ype_unify(tp1, tp21)
          in
            T1Plist(tp1)
          end
          )
        | T1Pext(tp_ref) => (
          let
            // check that tp_ref is nil
            val-true = t1ype_unify(tp2, t1ype_new_ext())
          in
            T1Plist(tp1)
          end
          )
      end // end of [list_cons]
    | "list_nilq" => 
      let
        val-mylist_cons(tp1, tps) = tps
      in
        case- tp1 of
        | T1Plist _ => T1Pbool
        | T1Pext _ => T1Pbool
      end
    | "list_consq" => 
      let
        val-mylist_cons(tp1, tps) = tps
      in
        case- tp1 of
        | T1Plist _ => T1Pbool
        | T1Pext _ => T1Pbool
      end
    | "list_uncons1" => 
      let
        val-mylist_cons(tp1, tps) = tps
      in
        case- tp1 of
        | T1Plist(tp11) => tp11
        | T1Pext(tp_ref) => (
          let
            // check that tp_ref is nil
            val-true = t1ype_unify(tp1, t1ype_new_ext())
          in
            t1ype_new_ext()
          end
          )
      end // end of [list_uncons1]
    | "list_uncons2" => 
      let
        val-mylist_cons(tp1, tps) = tps
      in
        case- tp1 of
        | T1Plist _ => tp1
        | T1Pext(tp_ref) => (
          let
            // check that tp_ref is nil
            val-true = t1ype_unify(tp1, t1ype_new_ext())
          in
            T1Plist(t1ype_new_ext())
          end
          )
      end // end of [list_uncons2]
    | "strm_nil" => T1Pstrm(t1ype_new_ext())
    | "strm_cons" =>
      let
        val-mylist_cons(tp1, tps) = tps
        val-mylist_cons(tp2, tvs) = tps
      in
        case- tp2 of
        | T1Plazy(T1Pstrm(tp21)) => (
          let
            val-true = t1ype_unify(tp1, tp21)
          in
            T1Pstrm(tp1)
          end
          )
        | T1Pext(tp_ref) => (
          let
            // check that tp_ref is nil
            val-true = t1ype_unify(tp1, t1ype_new_ext())
          in
            T1Pstrm(tp1)
          end
          )
      end
    | "strm_nilq" => 
      let 
        val-mylist_cons(tp1, tps) = tps
        val-T1Pstrm(_) = tp1
      in
        T1Pbool
      end // end of [strm_nilq]
    | "strm_consq" =>
      let 
        val-mylist_cons(tp1, tps) = tps
        val-T1Pstrm(_) = tp1
      in
        T1Pbool
      end // end of [strm_consq]
    | "strm_uncons1" => 
      let
        val-mylist_cons(tp1, tps) = tps
      in
        case- tp1 of
        | T1Pstrm(tp11) => tp11
        | T1Pext(tp_ref) => (
          let
            // check that tp_ref is nil
            val-true = t1ype_unify(tp1, t1ype_new_ext())
          in
            t1ype_new_ext()
          end
          )
      end // end of [strm_uncons1]
    | "strm_uncons2" => 
      let
        val-mylist_cons(tp1, tps) = tps
      in
        case- tp1 of
        | T1Pstrm _ => T1Plazy(tp1)
        | T1Pext(tp_ref) => (
          let
            // check that tp_ref is nil
            val-true = t1ype_unify(tp1, t1ype_new_ext())
          in
            T1Plazy(T1Pstrm(t1ype_new_ext()))
          end
          )
      end // end of [strm_uncons2]
    | "strm_uncons1" =>
      let 
        val-mylist_cons(tp1, tps) = tps
        val () = println!(tp1)
        val-T1Pstrm(tp11) = tp1
      in
        tp11
      end // end of [strm_uncons1]
    | "strm_uncons2" =>
      let 
        val-mylist_cons(tp1, tps) = tps
        val-T1Pstrm(_) = tp1
      in
        T1Plazy(tp1)
      end // end of [strm_uncons2]
    | "$lazy" =>
      let 
        val-mylist_cons(tp1, tps) = tps
      in
        tp1
      end
    | "$eval" =>
      let 
        val-mylist_cons(tp1, tps) = tps
      in
        case- tp1 of
        | T1Plazy(tp11) => tp11
        | T1Pext(tp_ref) => (
          let
            // check that tp_ref is nil
            val-true = t1ype_unify(tp1, t1ype_new_ext())
          in
            t1ype_new_ext()
          end
          )
      end // end of [$eval]
    )
  end // end of [t1erm_oftype1_opr]

(* ****** ****** *)
implement t1ermlst_oftype1(tms, xts) = 
  mylist_reverse(loop(tms, xts, mylist_nil()))
  where {
    fun loop(tms: t1ermlst, xts: t1ctx, tps: t1ypelst): t1ypelst = 
      case+ tms of
      | mylist_nil() => tps
      | mylist_cons(tm1, tms) => 
          let
            val tp = t1erm_oftype1(tm1, xts)
          in 
            loop(tms, xts, mylist_cons(tp, tps)) 
          end
  } // end of [t1ermlst_oftype1]

(* ****** ****** *)
implement t1dclist_oftype0(tlst0) = 
  let
    val lst = mylist_reverse(t1dclist_oftype1(tlst0, mylist_nil()))
    fun remove_dup(lst0: t1ctx, last: t1var, res: t1ctx): t1ctx =
      case lst0 of
      | mylist_nil() => mylist_reverse(res)
      | mylist_cons(@(var0, val0), lst0) => (
          if last = var0 then remove_dup(lst0, last, res)
          else remove_dup(lst0, var0, mylist_cons(@(var0, val0), res))
        )
  in
    remove_dup(lst, "__placeholder__", mylist_nil())
  end
implement t1dclist_oftype1(tlst0, xts) = 
  case tlst0 of 
  | mylist_nil() => xts
  | mylist_cons(t1dcl0, tlst1) =>
      let
        val-T1DCLbind(tvar0, tm0) = t1dcl0
        val tp0 = t1erm_oftype1(tm0, xts)
        val xts = (
          case tm0 of
          | T1Mfix(fnm, _, _, _, _) => mylist_cons(@(fnm, tp0), xts)
          | _ => xts
          )
      in
        t1dclist_oftype1(tlst1, mylist_cons(@(tvar0, tp0), xts))
      end
  // end of [t1dclist_oftype1]

(* ****** ****** *)

(* end of [CS525-2022-Fall/Final_project_tcheck2.dats] *)
