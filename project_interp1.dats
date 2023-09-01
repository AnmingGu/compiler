(* ****** ****** *)
#staload "./project.sats"
#include "share/atspre_staload.hats"
#staload "./../../../mylib/mylib.dats"
#staload "./project_t1erm.dats"

(* ****** ****** *)
extern fun t1erm_interp1(tm0: t1erm, xvs: t1env): t1val
extern fun t1erm_interp1_var(tm0: t1erm, xvs: t1env): t1val
extern fun t1erm_interp1_opr(tm0: t1erm, xvs: t1env): t1val
extern fun t1ermlst_interp1(tm0: t1ermlst, xvs: t1env): t1valist
extern fun t1dclist_interp1(tlst0: t1dclist, xvs: t1env): t1env

(* ****** ****** *)
implement t1erm_interp0(tm0) = 
  t1erm_interp1(tm0, mylist_nil())
implement t1erm_interp1(tm0, xvs) = (
  case+ tm0 of
  | T1Mnil() => T1Vnil()
  | T1Mint(i0) => T1Vint(i0)
  | T1Mbtf(b0) => T1Vbtf(b0)
  | T1Mstr(s0) => T1Vstr(s0)
  | T1Mvar _ => t1erm_interp1_var(tm0, xvs)
  | T1Mlam _ => T1Vlam(tm0, xvs)
  | T1Mfix _ => T1Vfix(tm0, xvs)
  | T1Mapp(tm1, tm2) =>
    let
      val tv1 = t1erm_interp1(tm1, xvs)
      val tv2 = t1erm_interp1(tm2, xvs)
    in
      case- tv1 of
      | T1Vlam(T1Mlam(xnm, _, tbd), xvs) =>
          t1erm_interp1(tbd, mylist_cons(@(xnm, tv2), xvs))
      | T1Vfix(T1Mfix(fnm, xnm, _, tbd, _), xvs) =>
          t1erm_interp1(tbd, mylist_cons(@(fnm, tv1), 
            mylist_cons(@(xnm, tv2), xvs)))
    end // end of [T1Mapp(tm1, tm2)]
  | T1Mopr(opr, tms) => t1erm_interp1_opr(tm0, xvs)
  | T1Mif0(tm1, tm2, tm3_opt) =>
    let
      val tv1 = t1erm_interp1(tm1, xvs)
    in
      case- tv1 of
      | T1Vbtf(b1) =>
        if b1 then t1erm_interp1(tm2, xvs)
        else (
          case tm3_opt of
          | myoptn_nil() => T1Vnil()
          | myoptn_cons(tm3) => t1erm_interp1(tm3, xvs)
        )
    end // end of [T1Mif0(tm1, tm2, tm3_opt)]
  | T1Mtup(tm1, tm2) =>
    let
      val tv11 = t1erm_interp1(tm1, xvs)
      val tv12 = t1erm_interp1(tm2, xvs)
    in 
      T1Vtup(tv11, tv12) 
    end // end of [T1Mtup(tm1, tm2)]
  | T1Mfst(tm1) =>
    let  
      val tv1 = t1erm_interp1(tm1, xvs)
    in
      case- tv1 of  
      | T1Vtup(tv11, _) => tv11
    end
  | T1Msnd(tm1) =>
    let 
      val tv1 = t1erm_interp1(tm1, xvs)
    in
      case- tv1 of  
      | T1Vtup(_, tv12) => tv12
    end
  | T1Mlet(dcls1, tm1) =>
    let
      fun loop(dcls: t1dclist, xvs: t1env): t1env =
        case+ dcls of 
        | mylist_nil() => xvs
        | mylist_cons(dcl, dcls) => 
          let
            val-T1DCLbind(var0, tm0) = dcl
            val tv0 = t1erm_interp1(tm0, xvs)
            val xvs1 = mylist_cons(@(var0, tv0), xvs)
          in
            loop(dcls, xvs1)
          end
    in
      t1erm_interp1(tm1, loop(dcls1, xvs))
    end // end of [T1Mlet(t1lst, tm1)]
  | T1Mseq(tmlst) => 
    let 
      val tvs = mylist_reverse(t1ermlst_interp1(tmlst, xvs))
    in
      case tvs of 
      | mylist_nil() => T1Vnil()
      | mylist_cons(tv, _) => tv
    end
  | T1Manno(tm1, _) => t1erm_interp1(tm1, xvs)
  | T1Mlazy(tm1) => T1Vlazy(tm1, xvs)
  | T1Mnone _ => T1Vnil()
) // end of [t1erm_interp1]
  
(* ****** ****** *)
implement t1erm_interp1_var(tm0, xvs) =
  find(xvs)
  where {
    val-T1Mvar(x) = tm0
    fun find(xvs: t1env): t1val =
      case- xvs of
      | mylist_cons(xv1, xvs) => (
        if x = xv1.0 then xv1.1 
        else find(xvs)
        )
  } // end of [t1erm_interp1_var]

(* ****** ****** *)
implement t1erm_interp1_opr(tm0, xvs) =
  let
    val-T1Mopr(opr, tms) = tm0
  in 
    case- opr of
    | "~" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vint(i1) = tv1
      in
        T1Vint(~i1)
      end // end of [~]
    | "+" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2
      in 
        T1Vint(i1 + i2) 
      end // end of [+]
    | "-" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vint(i1 - i2) 
      end // end of [-]
    | "%" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vint(i1 % i2) 
      end // end of [%]
    | "*" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vint(i1 * i2) 
      end // end of [*]
    | "<" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vbtf(i1 < i2) 
      end // end of [<]
    | ">" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vbtf(i1 > i2) 
      end // end of [>]
    | "=" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vbtf(i1 = i2) 
      end // end of [=]
    | "<=" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vbtf(i1 <= i2) 
      end // end of [<=]
    | ">=" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vbtf(i1 >= i2) 
      end // end of [>=]
    | "!=" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vint(i1) = tv1 
        and T1Vint(i2) = tv2 
      in 
        T1Vbtf(i1 != i2) 
      end // end of [!=]
    | "show" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
      in
        case- tv1 of
        | T1Vstr(str) => 
          let val () = print(str) in T1Vnil() end
      end // end of [show]
    | "print" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
      in
        case- tv1 of
        | T1Vint(i0) => 
          let val () = print!(i0) in T1Vnil() end
        | T1Vbtf(b0) => 
          let val () = print!(b0) in T1Vnil() end
        | T1Vstr(s0) => 
          let val () = print!(s0) in T1Vnil() end
        | _ => let val () = print!(tv1) in T1Vnil() end
      end // end of [print]
    | "showval" =>
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
      in 
        print(tv1); T1Vnil() 
      end // end of [showval]
    | "list_nil" => T1Vcons(0, mylist_sing(T1Vnil()))
    | "list_cons" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
      in
        T1Vcons(1, mylist_pair(tv1, tv2))
      end // end of [list_cons]
    | "list_nilq" => 
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vcons(len, _) = tv1
      in
        if len = 0 then T1Vbtf(true) 
        else T1Vbtf(false)
      end // end of [list_nilq]
    | "list_consq" =>
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vcons(l, _) = tv1
      in
        if l = 1 then T1Vbtf(true) 
        else T1Vbtf(false)
      end // end of [list_consq]
    | "list_uncons1" =>
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vcons(_, lst) = tv1
        val-mylist_cons(tv, _) = lst
      in 
        tv 
      end // end of [list_uncons1]
    | "list_uncons2" =>
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vcons(_, lst) = tv1
        val-mylist_cons(_, lst) = lst
        val-mylist_cons(tv, _) = lst
      in 
        tv 
      end // end of [list_uncons2]
    | "strm_nil" => T1Vstrm(myoptn_nil(), T1Vnil())
    | "strm_cons" =>
      let
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-mylist_cons(tv2, tvs) = tvs
        val-T1Vlazy(_, _) = tv2 // type checking
      in
        T1Vstrm(myoptn_cons(tv1), tv2)
      end // end of [strm_cons]
    | "strm_nilq" => 
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vstrm(optn, _) = tv1
      in
        case optn of
        | none() => T1Vbtf(true)
        | _ => T1Vbtf(false)
      end // end of [strm_nilq]
    | "strm_consq" =>
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vstrm(optn, _) = tv1
      in
        case optn of
        | none() => T1Vbtf(false)
        | _ => T1Vbtf(true)
      end // end of [strm_consq]
    | "strm_uncons1" =>
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vstrm(tv2_optn, _) = tv1
        val-myoptn_cons(tv2) = tv2_optn
      in 
        tv2 
      end // end of [strm_uncons1]
    | "strm_uncons2" =>
      let 
        val tvs = t1ermlst_interp1(tms, xvs)
        val-mylist_cons(tv1, tvs) = tvs
        val-T1Vstrm(_, lazy) = tv1
      in 
        lazy
      end // end of [strm_uncons2]
    | "$lazy" =>
      let 
        val-mylist_cons(tm1, tms) = tms
      in
        T1Vlazy(tm1, xvs)
      end // end of [$lazy]
    | "$eval" =>
      let
        val-mylist_cons(tm1, tms) = tms
        val-T1Vlazy(tm2, xvs1) = t1erm_interp1(tm1, xvs)
      in
        t1erm_interp1(tm2, xvs1)
      end // end of [$eval]
  end // end of [t1erm_interp1_opr]

(* ****** ****** *)
implement t1ermlst_interp1(tms: t1ermlst, xvs: t1env) = 
  loop(tms, xvs, mylist_nil())
  where {
    fun loop(tms: t1ermlst, xvs: t1env, tvs: t1valist): t1valist = 
      case+ tms of 
      | mylist_nil() => mylist_reverse(tvs)
      | mylist_cons(tm1, tms) => 
        let
          val tv1 = t1erm_interp1(tm1, xvs)
        in
          loop(tms, xvs, mylist_cons(tv1, tvs))
        end
  } // end of [t1ermlst_interp1]

(* ****** ****** *)
implement t1dclist_interp0(tlst0) = 
  mylist_reverse(t1dclist_interp1(tlst0, mylist_nil()))
implement t1dclist_interp1(tlst0, xvs) = 
  case tlst0 of 
  | mylist_nil() => xvs
  | mylist_cons(t1dcl0, tlst1) => 
    let
      val-T1DCLbind(tvar0, tm0) = t1dcl0
      val tv0 = t1erm_interp1(tm0, xvs)
      val xvs = (
        case tv0 of 
        | T1Vfix(T1Mfix(fnm, _, _, _, _), _) => 
            mylist_cons(@(fnm, tv0), xvs)
        | _ => xvs
      )
    in
      t1dclist_interp1(tlst1, mylist_cons(@(tvar0, tv0), xvs))
    end
  // end of [t1dclist_interp1]

(* end of [CS525-2022-Fall/Final_project_interp1.dats] *)
