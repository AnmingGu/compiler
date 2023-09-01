(* ****** ****** *)
#staload "./project.sats"
#include "share/atspre_staload.hats"
#staload "./../../../mylib/mylib.dats"
#staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)
overload + with mylist_extend

(* ****** ****** *)
// HX: for the a-norm-trans
extern fun t1erm_atrans1(t1m0: t1erm, env0: t2env, c3env0: 
  c3env): t2cmp
extern fun t1erm_atrans1_var(t1m0: t1erm, env0: t2env, 
  c3env0: c3env): t2cmp
extern fun t1erm_atrans1_opr(t1m0: t1erm, env0: t2env, 
  c3env0: c3env): t2cmp
extern fun t1ermlst_atrans1_for_opr(t1m0: t1ermlst, env0: 
  t2env, c3env0: c3env): @(t2bndlst, t2boxlst)
extern fun t1dclist_atrans1(dcls: t1dclist, env0: t2env, 
  c3env0: c3env): t2env
extern fun t2cmp_val(t2box): t2cmp
extern fun t2prm_val(t2box): t2cmp
extern fun t2reg_new(): int
extern fun t2fun_new(): int
extern fun t2lzy_new(): int
extern fun t2env_new(): int
extern fun t3env_extend(@(t1var, t2box), t3env): t3env
extern fun t3env_extend1(@(t1var, t2box), t3env, t3env): t3env

(* ****** ****** *)
implement t1erm_atrans0(t1m0) = 
  t1erm_atrans1(t1m0, mylist_nil(), @("_~_", mylist_nil()))
implement t1erm_atrans1(t1m0, env0, c3env0) = (
  case+ t1m0 of 
  | T1Mnil() => t2prm_val(T2Vnil())
  | T1Mint(int) => t2prm_val(T2Vint(int))
  | T1Mbtf(btf) => t2prm_val(T2Vbtf(btf))
  | T1Mstr(str) => t2prm_val(T2Vstr(str))
  | T1Mvar _ => t1erm_atrans1_var(t1m0, env0, c3env0)
  | T1Mlam(targ, _, t1m1) => (
    t2cmp_val(fun1)) 
    where {
      val t2c0 = t2cmp_val(T2Varg(0, targ))
      val env1 = mylist_cons(@(targ, t2c0), env0)
      val nme = "anon" + int2str(t2fun_new())
      val t3env1 = 
        t3env_extend(@(targ, T2Varg(0, targ)), c3env0.1)
      val c3env1 = @(nme, t3env1)
      val body = t1erm_atrans1(t1m1, env1, c3env1) 
      val tenv = t2env_new()
      val fun1 = T2Vfun(nme, targ, body, T2Ienv(tenv, @(nme, c3env0.1)))
    } // end of [T1Mlam(targ, _, t1m1)]
  | T1Mfix(f0, targ, _, t1m1, _) => (
    t2cmp_val(fun1))
    where {
      val t2c0 = t2cmp_val(T2Varg(0, targ))
      val env1 = 
        mylist_cons(
          @(f0, t2prm_val(T2Vfnm(f0))),
          mylist_cons(@(targ, t2c0), env0)
        )
      val t3env1 = t3env_extend(@(targ, T2Varg(0, targ)), c3env0.1)
      val c3env1 = @(f0, t3env1)
      val body = t1erm_atrans1(t1m1, env1, c3env1) 
      val tenv = t2env_new()
      val fun1 = T2Vfun(f0, targ, body, T2Ienv(tenv, @(f0, c3env0.1)))
    } // end of [T1Mfix(f0, targ, _, t1m1, _)]
  | T1Mapp(t1m1, t1m2) =>
    let
      val T2CMP(bnds1, t1x1) = t1erm_atrans1(t1m1, env0, c3env0)
      val T2CMP(bnds2, t1x2) = t1erm_atrans1(t1m2, env0, c3env0)
      val treg = t2reg_new()
      val tbnd = T2BND(treg, T2Ical(t1x1, t1x2))
      val bnds = mylist_append(bnds1, bnds2)
    in
      T2CMP(bnds + tbnd, T2Vreg(treg))
    end // end of [T1Mapp(t1m1, t1m2)]
  | T1Mopr _ => t1erm_atrans1_opr(t1m0, env0, c3env0)
  | T1Mif0(t1m1, t1m2, t1m3_opt) => 
    let
      val T2CMP(bnds1, t1x1) = t1erm_atrans1(t1m1, env0, c3env0)
      val T2CMP(bnds2, t1x2) = t1erm_atrans1(t1m2, env0, c3env0)
      val treg = t2reg_new()
    in
      case t1m3_opt of 
      | myoptn_nil() => 
        let
          val bnds3 = mylist_nil()
          val tbnd = T2BND(treg, T2Iif0(t1x1, bnds2, bnds3))
        in
          T2CMP(bnds1 + tbnd, T2Vreg(treg))
        end
      | myoptn_cons(t1m3) =>
        let
          val T2CMP(bnds3, t1x3) = t1erm_atrans1(t1m3, env0, c3env0)
          val tbnd = T2BND(treg, T2Iif0(t1x1, bnds2, bnds3))
        in
          T2CMP(bnds1 + tbnd, T2Vreg(treg))
        end
    end // end of [T1Mif0(t1m1, t1m2, t1m3_opt)]
  | T1Mtup(t1m1, t1m2) =>
    let
      val T2CMP(bnds1, t2x1) = t1erm_atrans1(t1m1, env0, c3env0)
      val T2CMP(bnds2, t2x2) = t1erm_atrans1(t1m2, env0, c3env0)
      val treg = t2reg_new()
      val tbnd = T2BND(treg, T2Itup(t2x1, t2x2))
      val bnds = mylist_append(bnds1, bnds2)
    in
      T2CMP(bnds + tbnd, T2Vreg(treg))
    end // end of [T1Mtup(t1m1, t1m2)]
  | T1Mfst(t1m1) => 
    let
      val T2CMP(bnds1, t1x1) = t1erm_atrans1(t1m1, env0, c3env0)
      val treg = t2reg_new()
      val tbnd = T2BND(treg, T2Ifst(t1x1))
    in
      T2CMP(bnds1 + tbnd, T2Vreg(treg))
    end // end of [T1Mfst(t1m1)]
  | T1Msnd(t1m1) => 
    let
      val T2CMP(bnds1, t1x1) = t1erm_atrans1(t1m1, env0, c3env0)
      val treg = t2reg_new()
      val tbnd = T2BND(treg, T2Isnd(t1x1))
    in
      T2CMP(bnds1 + tbnd, T2Vreg(treg))
    end // end of [T1snd(t1m1)]
  | T1Mlet(t1lst, t1m1) => 
    let
      fun loop(dcls: t1dclist, env0: t2env, bnds0: t2bndlst, t3env0: t3env): 
          @(t2env, t2bndlst, t3env) = (
        case+ dcls of 
        | mylist_nil() => @(env0, bnds0, t3env0)
        | mylist_cons(dcl, dcls) => 
          let
            val-T1DCLbind(var0, tm0) = dcl
            val t3env_nm = c3env0.0
            val cmp0 = t1erm_atrans1(tm0, env0, @(t3env_nm, t3env0))
            val T2CMP(bnds, box) = cmp0
            val t3env1 = t3env_extend(@(var0, box), t3env0)
          in
            loop(dcls, mylist_cons(@(var0, cmp0), env0),
              mylist_append(bnds0, bnds), t3env1)  
          end
      ) // end of [loop]
      val (env1, bnds1, t3env1) = loop(t1lst, env0, mylist_nil(), c3env0.1)
      val T2CMP(bnds0, box0) = t1erm_atrans1(t1m1, env1, @(c3env0.0, t3env1))
    in
      T2CMP(mylist_append(bnds1, bnds0), box0)
    end // end of [T1Mlet(t1lst, t1m1)]
  | T1Manno(t1m1, _) => t1erm_atrans1(t1m1, env0, c3env0)
  | T1Mseq(t1ms) => (
    loop(t1ms, mylist_nil(), T2Vnil()))
    where {
      fun loop(t1ms: t1ermlst, bnds0: t2bndlst, box0: t2box) = 
        case t1ms of 
        | mylist_nil() => T2CMP(bnds0, box0)
        | mylist_cons(t1m1, t1ms) => (
          let
            val T2CMP(bnds1, box1) = t1erm_atrans1(t1m1, env0, c3env0)
          in
            loop(t1ms, mylist_append(bnds0, bnds1), box1)
          end
        )
    } // end of [T1Mseq(t1ms)]
  | T1Mlazy(t1m1) => // TODO
    let
      val tenv = t2env_new()
      val nme = "lzy" + int2str(t2lzy_new())
      val targ = "_~_"
      // val t3env1 = t3env_extend(@(targ, T2Varg(0, targ)), c3env0.1)
      val c3env1 = @(nme, c3env0.1)
      val body = t1erm_atrans1(t1m1, env0, c3env1) 
      val fun1 = T2Vlzy(nme, targ, body, T2Ienv(tenv, @(nme, c3env0.1)))
      // val fun1 = T2Vlzy(nme, targ, body, T2Ienv(tenv, c3env0))
    in
      t2cmp_val(fun1)
    end // end of [T1Mlazy(t1m1)]
  | T1Mnone _ => T2CMP(mylist_nil(), T2Vnil())
) // end of [t1erm_atrans1]

(* ****** ****** *)
implement t1erm_atrans1_var(t1m0, env0, c3env0) =
  find(env0)
  where {
    val-T1Mvar(x) = t1m0
    fun find(env0: t2env): t2cmp =
      case- env0 of
      | mylist_cons(x_cmp1, env0) => (
        if x = x_cmp1.0 then (
          let 
            val T2CMP(_, box1) = x_cmp1.1
          in 
            t2prm_val(box1)
          end
        )
        else find(env0)
        )
    } // end of [t1erm_atrans1_var]

(* ****** ****** *)
implement t1erm_atrans1_opr(t1m0, env0, c3env0) =
  let
    val-T1Mopr(opr, tms) = t1m0
    val treg = t2reg_new()
    val (bndlst, boxlst) = t1ermlst_atrans1_for_opr(tms, env0, c3env0)
    val bnd = T2BND(treg, T2Iopr(opr, boxlst))
    val bnds = bndlst + bnd
  in 
    T2CMP(bnds, T2Vreg(treg))
  end // end of [t1erm_atrans1_for_opr]

(* ****** ****** *)
implement t1ermlst_atrans1_for_opr(tms: t1ermlst, xvs: t2env, c3env0) = 
  loop(tms, xvs, mylist_nil(), mylist_nil())
  where {
    fun loop(tms: t1ermlst, xvs: t2env, bnds: t2bndlst, boxs: t2boxlst): 
        @(t2bndlst, t2boxlst) = 
      case+ tms of 
      | mylist_nil() => @(bnds, boxs)
      | mylist_cons(tm1, tms) => 
        let
          val T2CMP(bnds1, box1) = t1erm_atrans1(tm1, xvs, c3env0)
        in
          loop(tms, xvs, mylist_append(bnds, bnds1), boxs + box1)
        end
  } // end of [t1ermlst_atrans1_for_opr]

(* ****** ****** *)
implement t2cmp_val(t2v0: t2box): t2cmp =
  T2CMP(mylist_nil(), t2v0)

(* ****** ****** *)
implement t2prm_val(t2v0: t2box): t2cmp =
  T2CMP(mylist_sing(T2BOX(t2v0)), t2v0)

(* ****** ****** *)
local
  val mycount = ref<int>(1)
in//local
  implement t2reg_new() = n0 
  where {
    val n0 = mycount[]
    val () = (mycount[] := n0 + 1)
  }
end // end of [local(t2reg_new)]

(* ****** ****** *)
local
  val mycount = ref<int>(1)
in//local
  implement t2fun_new() = n0 
  where {
    val n0 = mycount[]
    val () = (mycount[] := n0 + 1)
  }
end // end of [local(t2fun_new)]

(* ****** ****** *)
local
  val mycount = ref<int>(1)
in//local
  implement t2lzy_new() = n0 
  where {
    val n0 = mycount[]
    val () = (mycount[] := n0 + 1)
  }
end // end of [local(t2lzy_new)]

(* ****** ****** *)
local
  val mycount = ref<int>(1)
in//local
  implement t2env_new() = n0 
  where {
    val n0 = mycount[]
    val () = (mycount[] := n0 + 1)
  }
end // end of [local(t2fun_new)]

(* ****** ****** *)
implement t3env_extend(x0, t3env0) = 
  t3env_extend1(x0, t3env0, mylist_nil())
implement t3env_extend1(x0, t3env0, acc) = 
  case t3env0 of
  | mylist_nil() => mylist_extend(mylist_reverse(acc), x0)
  | mylist_cons(x1, t3env1) => (
    if x1.0 = x0.0 then mylist_append(mylist_extend(
      mylist_reverse(acc), @(x0.0, x0.1)), t3env1)
    else t3env_extend1(x0, t3env1, mylist_cons(x1, acc))
    )
// end of [t3env_extend1]

(* ****** ****** *)
implement t1dclist_atrans0(dcls) =
  let
    val env0 = mylist_nil()
    val c3env0 = @("_~_", mylist_nil())
    val env1 = mylist_reverse(t1dclist_atrans1(dcls, env0, c3env0))
    // prevent duplicate declaration and implementation of recursive functions
    fun equal(box1: t2box, box2: t2box): bool = 
      case (box1, box2) of
      | (T2Vfun(_, _, T2CMP(_, T2Vreg(reg1)), _), 
        T2Vfun(_, _, T2CMP(_, T2Vreg(reg2)), _)) => 
          reg1 = reg2
      | (_, _) => false
    fun remove_dup(env: t2env, last: t2box, res: t2env): t2env =
      case env of
      | mylist_nil() => mylist_reverse(res)
      | mylist_cons(@(var0, T2CMP(nm, box)), env) => (
        if equal(box, last) then remove_dup(env, last, res)
        else remove_dup(env, box, mylist_cons(@(var0, T2CMP(nm, box)), res))
        )
  in
    remove_dup(env1, T2Vnil(), mylist_nil()) 
  end // end of [t1dclist_atrans0]

(* ****** ****** *)
implement t1dclist_atrans1(dcls, env0, c3env0) = (
  case+ dcls of 
  | mylist_nil() => env0
  | mylist_cons(dcl, dcls) => 
    let
      val-T1DCLbind(var0, tm0) = dcl
      val cmp0 = t1erm_atrans1(tm0, env0, c3env0)
      // add fix name to environment
      val env0 = (
        case tm0 of 
        | T1Mfix(fnm, _, _, _, _) => mylist_cons(@(fnm, cmp0), env0)
        | _ => env0
      )
    in
      t1dclist_atrans1(dcls, mylist_cons(@(var0, cmp0), env0), c3env0)
    end
) // end of [t1dclist_atrans1]

(* ****** ****** *)

(* end of [CS525-2022-Fall/Final_project_atrans3.dats] *)
