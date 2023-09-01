(* ****** ****** *)
#staload "./project.sats"
#include "share/atspre_staload.hats"
#staload "./../../../mylib/mylib.dats"

(* ****** ****** *)
// implement main function
extern fun emit2_main(FILEref, string, t2env): void
// find all functions
extern fun emit2_fun_dec0(FILEref, t2env, t2boxlst): t2boxlst
extern fun emit2_fun_dec1(FILEref, t2box, t2boxlst): t2boxlst
// implement functions
extern fun emit2_fun_imp(FILEref, string, t2boxlst): void
extern fun emit2_fun(FILEref, string, t2box): void
extern fun emit2_lzy(FILEref, string, t2box): void
// handle using an environment multiple times in a function
extern fun find_env(int): bool
extern fun reset_env_list(): void
// declare and implement environments
extern fun emit2_env_dec(FILEref, t2boxlst): void
extern fun emit2_env_imp(FILEref, string, string, string, t2box): void
extern fun handle_env(FILEref, string, t2box, c3env): void
// declare registers
extern fun emit2_reg_dec(FILEref, string, t2bndlst): void
// implement each datatype
extern fun emit2_cmp(FILEref, string, t2cmp, string, c3env): void
extern fun emit2_box(FILEref, string, t2box, string, c3env): void
extern fun emit2_boxs(FILEref, string, t2boxlst, string, c3env): void
extern fun emit2_bnd(FILEref, string, t2bnd, string, c3env): void
extern fun emit2_bnds(FILEref, string, t2bndlst, string, c3env): void
extern fun emit2_bnds_for_if0(FILEref, string, t2bndlst, string, c3env): t2box
// helper functions
extern fun emit2_str(FILEref, string): void
extern fun opr_to_string(string): string

(* ****** ****** *)
implement t2env_emit0(out, env0) = 
  let
    val () = fprint!(out, "#include \"runtime.h\"\n\n")
    val () = fprint!(out, "// Declare functions\n")
    val funs = emit2_fun_dec0(out, env0, mylist_nil())
    // print functions to stdout for testing
    // val () = fprint!(stdout_ref, "/* \nClosure converted functions: \n")
    // fun loop_(boxs: t2boxlst) =
    //   case boxs of 
    //   | mylist_cons(box, boxs) => (
    //       fprint!(stdout_ref, "\nFUNCTION:\n\t", box, ", \n"); loop_(boxs)
    //     )
    //   | mylist_nil() => ()
    // val () = loop_(funs)
    // val () = fprint!(stdout_ref, "\n*/\n")
    val () = fprint!(out, "\n// Declare environments\n")
    val () = emit2_env_dec(out, funs)
    val () = fprint!(out, "\n// Implement functions\n")
    val () = emit2_fun_imp(out, "\t", mylist_reverse(funs))
    val () = fprint!(out, "\n")
    val () = fprint!(out, "// Implement main function\n")
    val () = fprint!(out, "int main(){\n")
    val () = emit2_main(out, "\t", env0)
    val () = fprint!(out, "\treturn 0;\n}\n")
  in 
    ()
  end // end of [t2env_emit0]

(* ****** ****** *)
implement emit2_main(out, tabs, env0) = (
  case env0 of
  | mylist_nil() => ()
  | mylist_cons(@(_, cmp), env0) => (
    let
      val T2CMP(bnds, box) = cmp
    in
      case box of 
      | T2Vfun _ => emit2_main(out, tabs, env0)
      | T2Vlzy _ => emit2_main(out, tabs, env0)
      | _ => (
        emit2_reg_dec(out, tabs, bnds);
        emit2_cmp(out, tabs, cmp, "_", @("_~_", mylist_nil()));
        fprint!(out, "\n");
        emit2_main(out, tabs, env0)
        )
    end
    ) 
) // end of [emit2_main]

(* ****** ****** *)
implement emit2_fun_dec0(out, env0, funs) = (
  case env0 of
  | mylist_nil() => funs
  | mylist_cons((_, T2CMP(bnds0, f)), env0) => (
    case f of 
    | T2Vfun(_, _, T2CMP(bnds1, box), _) => 
      emit2_fun_dec0(out, env0, 
      emit2_fun_dec1(out, f, 
      bnds_(bnds0, bnds_(bnds1, 
      boxes_(mylist_sing(box), funs)))))
    | T2Vlzy(_, _, T2CMP(bnds1, box), _) => 
      emit2_fun_dec0(out, env0, 
      emit2_fun_dec1(out, f, 
      bnds_(bnds1, bnds_(bnds0, 
      boxes_(mylist_sing(box), funs)))))
    | _ => (emit2_fun_dec0(out, env0, bnds_(bnds0, funs)))
    ) 
    where {
      fun boxes_(boxs: t2boxlst, funs: t2boxlst) = 
        case boxs of 
        | mylist_cons(box, boxs) => boxes_(boxs, 
            emit2_fun_dec1(out, box, funs))
        | _ => funs
        // end of [boxes_]
      and bnd_(bnd: t2bnd, funs: t2boxlst) =
        case bnd of
        | T2BOX(box) => emit2_fun_dec1(out, box, funs)
        | T2BND(_, ins) => ins_(ins, funs)
        // end of [bnd_]
      and bnds_(bnds: t2bndlst, funs: t2boxlst) =
          case bnds of 
          | mylist_nil() => funs
          | mylist_cons(bnd, bnds) => bnds_(bnds, bnd_(bnd, funs))
        // end of [bnds_]
      and ins_(ins: t2ins, funs: t2boxlst) = 
        case ins of
        | T2Iopr(_, boxs) => boxes_(boxs, funs)
        | T2Ifst(box) => emit2_fun_dec1(out, box, funs)
        | T2Isnd(box) => emit2_fun_dec1(out, box, funs)
        | T2Itup(box1, box2) => emit2_fun_dec1(out, box2, 
            emit2_fun_dec1(out, box1, funs))
        | T2Iif0(box, bnds1, bnds2) => bnds_(bnds2,  bnds_(bnds1, 
            emit2_fun_dec1(out, box, funs)))
        | T2Ical(box1, box2) => emit2_fun_dec1(out, box2, 
            emit2_fun_dec1(out, box1, funs))
        | T2Ienv(_, _) => funs
        // end of [ins_]
      }
) // end of [t2env_fun_dec0]

implement emit2_fun_dec1(out, box, funs: t2boxlst) = 
  let
    fun find_(x: string, funs: t2boxlst) = (
      case funs of 
      | mylist_nil() => false
      | mylist_cons(fun0, funs) =>
        case- fun0 of
        | T2Vfun(v, _, _, _) => 
          if x = v then true 
          else find_(x, funs)
        | T2Vlzy(v, _, _, _) => 
          if x = v then true 
          else find_(x, funs)
    ) // end of [find_]
  in
  case- box of
  | T2Vfun(fnm, _, tcmp, _) => (
    if find_(fnm, funs) then funs
    else 
      let
        val () = fprint!(out, "lamval1 LAM_FUN_", fnm, 
          "(lamval1, void*);\n")
      in
        emit2_fun_dec0(out, mylist_sing(@("_", tcmp)), 
          mylist_cons(box, funs))
      end
    ) // end of [T2Vfun]
  | T2Vlzy(fnm, _, _, _) => (
    if find_(fnm, funs) then funs
    else 
      let
        val () = fprint!(out, "lamval1 LAM_LAZY_", fnm, 
          "(lamval1, void*);\n")
      in
        mylist_cons(box, funs)
      end
    ) // end of [T2VlZy]
  | _ => funs
end
// end of [emit2_fun_dec1]

(* ****** ****** *)
implement emit2_fun_imp(out, tabs, funs) = (
  case funs of
  | mylist_nil() => ()
  | mylist_cons(f, funs) => (
    case f of 
    | T2Vfun _ => (
      emit2_fun(out, tabs, f);
      fprint!(out, "\n");
      emit2_fun_imp(out, tabs, funs)
      )
    | T2Vlzy _ => (
      emit2_lzy(out, tabs, f);
      fprint!(out, "\n");
      emit2_fun_imp(out, tabs, funs)
      )
    | _ => emit2_fun_imp(out, tabs, funs)
  )
) // end of [emit2_fun_imp]

(* ****** ****** *)
implement emit2_env_dec(out, funs) = 
  let
    fun emit_vars(vars: t3env) = (
      let
        fun loop1(xs: t3env, i0: int): void = (
          case+ xs of
          | mylist_nil() => ()
          | mylist_cons(x1, xs) => (
            if i0 > 0 then fprint!(out, ", "); 
            fprint!(out, x1.0); loop1(xs, i0+1)
            )
          ) // end of [loop1]
        val len = mylist_length(vars)
      in 
        if len > 0 then (
          fprint!(out, "\tlamval1 "); 
          loop1(vars, 0); fprint!(out, ";\n")
        )
        else ()
      end // end of [emit_vars]
    )
  in (
    case funs of
    | mylist_nil() => ()
    | mylist_cons(f, funs) => (
      case f of 
      | T2Vfun(_, _, _, ins) => 
        let
          val-T2Ienv(int, c3env0) = ins
        in
          if c3env0.0 = "_~_" then emit2_env_dec(out, funs)
          else (
            fprint!(out, "typedef struct{\n");
            emit_vars(c3env0.1);
            fprint!(out, "} env0_", c3env0.0, ";\n");
            fprint!(out, "typedef env0_", c3env0.0, " *env1_", 
              c3env0.0, ";\n");
            fprint!(out, "\n");
            emit2_env_dec(out, funs)
          )
        end // end of [T2Vfun]
      | T2Vlzy(_, _, _, ins) => 
        let
          val-T2Ienv(int, c3env0) = ins
        in
          if c3env0.0 = "_~_" then emit2_env_dec(out, funs)
          else (
            fprint!(out, "typedef struct{\n");
            emit_vars(c3env0.1);
            fprint!(out, "} env0_", c3env0.0, ";\n");
            fprint!(out, "typedef env0_", c3env0.0, " *env1_", 
              c3env0.0, ";\n");
            fprint!(out, "\n");
            emit2_env_dec(out, funs)
          )
        end // end of [T2Vfun]
      | _ => emit2_env_dec(out, funs)
      )
    )
  end // end of [emit2_env_dec]

(* ****** ****** *)
implement emit2_env_imp(out, tabs, outer_fnm, arg1, fun0) = 
  let
    fun loop_(fnm: string, arg0: string, c3env0: c3env, env_int: int) = (
      case c3env0.1 of 
      | mylist_nil() => ()
      | mylist_cons(v, lst) => (
        case- v.1 of 
        | T2Vnil() => (
          fprint!(out, tabs, "env", env_int, "->", v.0, 
            " = LAM_VAL_NIL();\n");
          loop_(fnm, arg0, @(c3env0.0, lst), env_int)
          )
        | T2Vint(int) => (
          fprint!(out, tabs, "env", env_int, "->", v.0, 
            " = LAM_VAL_INT(", int, ");\n");
          loop_(fnm, arg0, @(c3env0.0, lst), env_int)
          )
        | T2Vstr(str) => (
          fprint!(out, tabs, "env", env_int, "->", v.0, 
            " = LAM_VAL_STR(", str, ");\n");
          loop_(fnm, arg0, @(c3env0.0, lst), env_int)
          )
        | T2Vreg(int) => (
          fprint!(out, tabs, "env", env_int, "->", v.0, 
            " = reg", int, ";\n");
          loop_(fnm, arg0, @(c3env0.0, lst), env_int)
          )
        | T2Varg(_, var1) => 
          let 
            // val () = println!("outer_fnm: ", outer_fnm)
            // val () = println!("arg0: ", arg0)
            // val () = println!("arg1: ", arg1)
            // val () = println!("var1: ", var1) 
            // val () = println!("v.0: ", v.0)
          in (
            (if v.0 = arg0 then fprint!(out, tabs, "env", env_int, "->", 
              v.0, " = ((env1_", outer_fnm, ")env0)->", var1, ";\n")
            else 
            // if v.0 = arg1 the
            fprint!(out, tabs, "env", env_int, "->", v.0, " = arg0;\n"));
            loop_(fnm, arg0, @(c3env0.0, lst), env_int)
          ) 
          end
        | T2Vfun(fnm, _, _, ins) => (
          let 
            val-T2Ienv(env_int1, _) = ins 
            val bool = find_env(env_int1)
            // val () = 
          in (
            (if bool then ()
            else emit2_env_imp(out, tabs, c3env0.0, arg1, v.1));
            fprint!(out, tabs, "env", env_int, "->", fnm, 
              " = LAM_CFP(LAM_FUN_", fnm, ", env", env_int1, ");\n");
            loop_(fnm, arg0, @(c3env0.0, lst), env_int)
            )
          end
          )
        | T2Vlzy _ => println!("TODO")
        ) 
      ) // end of [loop_]
  in 
    case- fun0 of
    | T2Vfun(fnm, arg0, _, ins) => 
      let 
        val-T2Ienv(env_int, c3env0) = ins
      in (
        fprint!(out, "\n", tabs, "env1_", fnm, " env", env_int, " = ", 
          "mymalloc(sizeof(env0_", fnm, "));\n");
        loop_(fnm, arg0, c3env0, env_int)
        )
      end // end of [T2Vfun]
    | T2Vlzy(fnm, arg0, _, ins) => 
      let 
        val-T2Ienv(env_int, c3env0) = ins
      in (
        fprint!(out, "\n", tabs, "env1_", fnm, " env", env_int, " = ", 
          "mymalloc(sizeof(env0_", fnm, "));\n");
        loop_(fnm, arg0, c3env0, env_int)
        )
      end // end of [T2Vlzy]
  end
// end of [emit2_env_imp]

(* ****** ****** *)
implement handle_env(out, tabs, box1, c3env0): void = 
  case- box1 of
  | T2Vfun(f0, arg1, _, ins) =>
    let
      val-T2Ienv(int, _) = ins
      val bool = find_env(int)
    in 
      if bool then ()
        else emit2_env_imp(out, tabs, c3env0.0, arg1, box1)
    end // end of [T2Vfun]
  | T2Vlzy(f0, arg1, _, ins) =>
    let
      val-T2Ienv(int, _) = ins
      val bool = find_env(int)
    in 
      if bool then ()
      else emit2_env_imp(out, tabs, c3env0.0, arg1, box1)
    end // end of [T2Vlzy]
  | _ => ()

(* ****** ****** *)
implement emit2_reg_dec(out, tabs, bnds0) = 
  emit_regs(loop(bnds0, mylist_nil()))
  where {
    fun loop(bnds0: t2bndlst, regs: mylist(t2reg)): mylist(t2reg) = (
      case bnds0 of
      | mylist_nil() => regs
      | mylist_cons(bnd, bnds0) => (
        case bnd of 
        | T2BND(reg, ins) => (
            case ins of 
            | T2Iif0(_, bnds1, bnds2) => (
              mylist_append(
                loop(bnds2, mylist_nil()),
                mylist_append(
                  loop(bnds1, mylist_nil()),
                  loop(bnds0, mylist_cons(reg, regs))
                ))
              )
            | _ => loop(bnds0, mylist_cons(reg, regs))
          )
          | T2BOX _ => loop(bnds0, regs))
    ) // end of [loop]
    fun emit_regs(regs: mylist(t2reg)) = 
      let
        fun loop1(xs: mylist(t2reg), i0: int): void = (
          case+ xs of
          | mylist_nil() => ()
          | mylist_cons(x1, xs) => (
            if i0 > 0 then fprint!(out, ", "); 
            fprint!(out, "reg", x1); loop1(xs, i0+1)
            )
          ) // end of [loop1]
        val len = mylist_length(regs)
      in 
        if len > 0 then (
          fprint!(out, tabs, "lamval1 "); 
          loop1(regs, 0); fprint!(out, ";\n")
        )
        else ()
      end // end of [emit_regs]
  } // end of [emit2_reg_dec]

(* ****** ****** *)
implement emit2_cmp(out, tabs, cmp0, arg0, c3env0) = 
  let
    val T2CMP(bnds, box0) = cmp0
  in
    case+ box0 of 
    | T2Vfun _ => emit2_fun(out, tabs, box0)
    | T2Vlzy _ => emit2_lzy(out, tabs, box0)
    | _ => emit2_bnds(out, tabs, bnds, arg0, c3env0)
  end // end of [emit2_cmp]

(* ****** ****** *)
implement emit2_box(out, tabs, box, arg0, c3env0) = (
  case- box of 
  | T2Vnil _ => fprint!(out, "LAM_VAL_NIL()")
  | T2Vint(int) => fprint!(out, "LAM_VAL_INT(", int, ")")
  | T2Vbtf(btf) => fprint!(out, "LAM_VAL_BTF(", btf, ")")
  | T2Vstr(str) => (fprint!(out, "LAM_VAL_STR(\""); 
      emit2_str(out, str); fprint!(out, "\")"))
  | T2Vreg(reg0) => 
    let
      fun find_(lst: t3env) = 
        case lst of
        | mylist_nil() => false
        | mylist_cons(e, lst) => (
          case- e.1 of
          | T2Vreg(reg1) => 
            if reg1 = reg0 then (fprint!(out, "((env1_", 
              c3env0.0, ")env0)->", e.0); true)
            else find_(lst)
          | _ => find_(lst)
        )
    in
      if find_(c3env0.1) then ()
      else fprint!(out, "reg", reg0)
    end
  | T2Varg(int, arg) => (
    if arg0 = arg then fprint!(out, "arg", int)
    else fprint!(out, "((env1_", c3env0.0, ")env0)->", arg) // ???
    )
  | T2Vfun(fnm, _, _, ins) => 
    let
      val-T2Ienv(int, _) = ins
    in
      fprint!(out, "LAM_CFP(LAM_FUN_", fnm, 
        ", env", int, ")")
    end
  | T2Vfnm(fnm) => fprint!(out, "LAM_CFP(LAM_FUN_", fnm, ", NULL)")
  | T2Vlzy(fnm, _, _, ins) => // TODO
    let
      val-T2Ienv(int, _) = ins
    in
      fprint!(out, "LAM_CFP(LAM_LAZY_", fnm, 
        ", env", int, ")") 
    end
  | _ => (fprint!(stdout_ref, box); fprint!(out, "LAM_ERR()"))
) // end of [emit2_box]

(* ****** ****** *)
implement emit2_boxs(out, tabs, boxs, arg0, c3env0) = 
  loop(boxs, 0) 
  where {
    fun loop(xs: t2boxlst, i0: int): void = (
      case+ xs of
      | mylist_nil() => ()
      | mylist_cons(x1, xs) => (
        if i0 > 0 then fprint!(out, ", ");
        emit2_box(out, tabs, x1, arg0, c3env0); loop(xs, i0+1)
        )
      )
  } // end of [emit2_boxs]

(* ****** ****** *)
implement emit2_bnd(out, tabs, bnd0, arg0, c3env0) = 
  let
    val-T2BND(reg0, ins) = bnd0
  in
    case ins of 
    | T2Iopr(opr, boxs) => 
      let
        fun loop_(lst: t2boxlst) =
          case lst of
          | mylist_nil() => ()
          | mylist_cons(box, lst) => (
            handle_env(out, tabs, box, c3env0);
            loop_(lst)
            )
      in (
        loop_(boxs);
        fprint!(out, tabs, "reg", reg0, " = LAM_OPR_", 
          opr_to_string(opr), "(");
        emit2_boxs(out, tabs, boxs, arg0, c3env0); 
        fprint!(out, ");\n")
      ) 
      end // end of [T2Iopr(opr, boxs)]
    | T2Ifst(box) => (
      handle_env(out, tabs, box, c3env0);
      fprint!(out, tabs, "reg", reg0, " = LAM_FST(");
      emit2_box(out, tabs, box, arg0, c3env0);
      fprint!(out, ");\n")
      ) // end of [T2Ifst(box)]
    | T2Isnd(box) => (
      handle_env(out, tabs, box, c3env0);
      fprint!(out, tabs, "reg", reg0, " = LAM_SND(");
      emit2_box(out, tabs, box, arg0, c3env0);
      fprint!(out, ");\n")
      ) // end of [T2Isnd(box)]
    | T2Itup(box1, box2) => (
      handle_env(out, tabs, box1, c3env0);
      handle_env(out, tabs, box2, c3env0);
      fprint!(out, tabs, "reg", reg0, " = LAM_VAL_TUP(");
      emit2_box(out, tabs, box1, arg0, c3env0);
      fprint!(out, ", ");
      emit2_box(out, tabs, box2, arg0, c3env0);
      fprint!(out, ");\n")
      ) // end of [T2Itup(box1, box2)]
    | T2Iif0(box1, bnds1, bnds2) => (
      let
        val () = handle_env(out, tabs, box1, c3env0)
        val () = fprint!(out, "\n", tabs, "if (((lamval1_btf)")
        val () = emit2_box(out, tabs, box1, arg0, c3env0)
        val () = fprint!(out, ")->data){\n")
        val box11 = emit2_bnds_for_if0(out, tabs + "\t", bnds1, arg0, c3env0)
        val () = handle_env(out, tabs, box11, c3env0)
        val () = fprint!(out, tabs + "\t", "reg", reg0, " = ")
        val () = emit2_box(out, tabs + "\t", box11, arg0, c3env0)
        val () = fprint!(out, ";\n", tabs, "}\n", tabs, "else{\n");
        val box12 = emit2_bnds_for_if0(out, tabs + "\t", bnds2, arg0, c3env0)
        val () = handle_env(out, tabs, box12, c3env0)
        val () = fprint!(out, tabs + "\t", "reg", reg0, " = ")
        val () =  emit2_box(out, tabs + "\t", box12, arg0, c3env0)
        val () = fprint!(out, ";\n", tabs, "}\n")
      in 
        () 
      end
      ) // end of [T2Iif0(box1, bnds1, bnds2)]
    | T2Ical(box1, box2) => 
      let
        fun emit(box1: t2box): void = (
          case- box1 of 
          | T2Vfun(str, arg1, _, ins) => (
            if c3env0.0 = "_~_" then ( // top-level function
              fprint!(out, tabs, "reg", reg0, " = ");
              fprint!(out, "LAM_CAL(LAM_CFP(LAM_FUN_", str, ", NULL), ");
              emit2_box(out, tabs, box2, arg0, c3env0);
              fprint!(out, ");\n")
            )
            else if mylist_length(c3env0.1) = 0 then (
              fprint!(out, tabs, "reg", reg0, " = LAM_CAL(");
              emit2_box(out, tabs, box1, arg0, c3env0);
              fprint!(out, ", ");
              emit2_box(out, tabs, box2, arg0, c3env0);
              fprint!(out, ");\n")
            )
            else (
              fprint!(out, tabs, "reg", reg0, " = ");
              fprint!(out, "LAM_CAL(((env1_", c3env0.0, ")env0)->", str, ", ");
              emit2_box(out, tabs, box2, arg0, c3env0);
              fprint!(out, ");\n")
            )
            ) // end of [T2Vfun]
          | T2Vfnm(str) => ( // for recursive functions
            fprint!(out, tabs, "reg", reg0, " = ");
            fprint!(out, "LAM_CAL(LAM_CFP(LAM_FUN_", str, ", env0), ");
            emit2_box(out, tabs, box2, arg0, c3env0);
            fprint!(out, ");\n")
            ) // end of [T2Vfnm]
          | T2Vreg(reg) => 
            let
              fun find_(reg: int, lst: t3env) = 
                case lst of
                | mylist_nil() => false
                | mylist_cons(e, lst) => (
                  case- e.1 of
                  | T2Vreg(reg1) => 
                    if reg = reg1 then (fprint!(out, "((env1_", 
                      c3env0.0, ")env0)->", e.0); true)
                    else find_(reg, lst)
                  | _ => find_(reg, lst)
                )
            in (
              fprint!(out, tabs, "reg", reg0, " = LAM_CAL(");
              (if find_(reg, c3env0.1) then () 
                else fprint!(out, "reg", reg));
              fprint!(out, ", ");
              emit2_box(out, tabs, box2, arg0, c3env0);
              fprint!(out, ");\n")
              ) 
            end // end of [T2Vreg]
          | T2Varg(_, arg1) => ( // TODO???
            fprint!(out, tabs, "reg", reg0, " = LAM_CAL(");
            emit2_box(out, tabs, box1, arg0, c3env0); 
            fprint!(out, ", ");
            emit2_box(out, tabs, box2, arg0, c3env0);
            fprint!(out, ");\n")
            ) // end of [T2Varg]
        ) // end of [emit]
      in (
        handle_env(out, tabs, box1, c3env0);
        handle_env(out, tabs, box2, c3env0);
        emit(box1)
        )
      end // end of [T2Ical(box1, box2)]
    | T2Ienv(int, c3env) => ()
  end // end of [emit2_bnd]

(* ****** ****** *)
implement emit2_bnds(out, tabs, bnds0, arg0, c3env0) = (
  case bnds0 of 
  | mylist_nil() => ()
  | mylist_cons(bnd, bnds0) => (
    (case bnd of 
    | T2BND _=> emit2_bnd(out, tabs, bnd, arg0, c3env0)
    | _ => ()
    ); 
    emit2_bnds(out, tabs, bnds0, arg0, c3env0)
    )
) // end of [emit2_bnds]

(* ****** ****** *)
implement emit2_bnds_for_if0(out, tabs, bnds0, arg0, c3env0) = 
  loop(bnds0, T2Vnil())
  where {
    fun loop(bnds0: t2bndlst, box: t2box) = 
      case bnds0 of 
      | mylist_nil() => box
      | mylist_cons(bnd, bnds0) => (
        case bnd of 
        | T2BND(reg1, _) => (
            emit2_bnd(out, tabs, bnd, arg0, c3env0); 
            loop(bnds0, T2Vreg(reg1))
          )
        | T2BOX(box) => loop(bnds0, box)
      )
  } // end of [emit2_bnds_for_if0]

implement emit2_str(out, str) = 
  loop(string2mylist(str))
  where {
    fun loop(cs: mylist(char)) = 
      case cs of
      | mylist_nil() => ()
      | mylist_cons('\n', cs) => (fprint!(out, "\\n"); loop(cs))
      | mylist_cons('\t', cs) => (fprint!(out, "\\t"); loop(cs))
      | mylist_cons('\b', cs) => (fprint!(out, "\\b"); loop(cs))
      | mylist_cons(c, cs) => (fprint!(out, c); loop(cs))
  } // end of [emit2_str]

(* ****** ****** *)
local
  val mylist = ref<mylist(int)>(mylist_nil())
  fun find(int, lst: mylist(int)) =
    case- lst of
    | mylist_nil() => false
    | mylist_cons(i, lst) => 
      if i = int then true 
      else find(int, lst)
in
  implement find_env(int): bool = btf
  where {
    val btf = find(int, mylist[])
    val () = (
      if btf then ()
      else (mylist[] := mylist_cons(int, mylist[]))
    )
  }
  implement reset_env_list() = 
    (mylist[] := mylist_nil())
end // end of [local(find_env, reset_env_list)]

(* ****** ****** *)
implement emit2_fun(out, tabs, box0) = 
  let
    val () = reset_env_list()
    val-T2Vfun(fnm, arg0, cmp0, ins0) = box0
    val-T2Ienv(int, c3env0) = ins0
    val T2CMP(bnds, box1) = cmp0
    val-(outer_fnm, _) = c3env0
    val () = fprint!(out, "extern lamval1 LAM_FUN_", fnm, 
      "(lamval1 arg0, void* env0){\n");
    val () = emit2_reg_dec(out, "\t", bnds)
    // val () = println!("\nIMPLEMENT FUNCTION")
    // val () = println!(box0)
    val () = emit2_bnds(out, "\t", bnds, arg0, c3env0)
  in 
    case- box1 of
    | T2Vreg(reg1) => 
      fprint!(out, "\n\treturn reg", reg1, ";\n}\n")
    | T2Varg(_, var1) => ( // ???
      if var1 = arg0 then fprint!(out, "\n\treturn arg0;\n}\n")
      else fprint!(out, "\n\treturn ((env1_", 
        outer_fnm, ")env0)->", var1, ";\n}\n")
      )
    | T2Vfun(fnm1, arg1, cmp1, ins) => 
      let
        val-T2Ienv(int1, c3env1) = ins
        val bool = find_env(int1)
      in ( 
        (if bool then ()
        else emit2_env_imp(out, tabs, outer_fnm, arg0, box1)); 
        fprint!(out, "\treturn LAM_CFP(LAM_FUN_", fnm1, 
          ", env", int1, ");\n}\n")
        )
      end // end of [T2Vfun]
    | T2Vlzy(fnm1, arg0, cmp1, ins) => 
      let
        val-T2Ienv(int1, c3env1) = ins
        val bool = find_env(int1)
      in ( 
        (if bool then ()
        else emit2_env_imp(out, tabs, outer_fnm, arg0, box1)); 
        fprint!(out, "\treturn LAM_LAZY(LAM_CFP(LAM_LAZY_", fnm1, 
          ", env", int1, "), arg0);\n}\n")
        )
      end // end of [T2Vlzy]
    | _ => (
      fprint!(out, tabs, "return ");
      handle_env(out, tabs, box1, c3env0);
      emit2_box(out, tabs, box1, arg0, c3env0);
      fprint!(out, ";\n}\n")
      )
  end // end of [emit2_fun]

(* ****** ****** *)
implement emit2_lzy(out, tabs, box0) = 
  let
    val-T2Vlzy(int, arg0, cmp0, ins0) = box0
    val-T2Ienv(int, c3env0) = ins0
    val T2CMP(bnds, box1) = cmp0
    val-(outer_fnm, _) = c3env0
    val () = fprint!(out, "extern lamval1 LAM_LAZY_lzy", int, 
          "(lamval1 arg0, void* env0){\n")
    val () = emit2_reg_dec(out, "\t", bnds)
    val () = emit2_bnds(out, "\t", bnds, arg0, c3env0)
  in 
    case- box1 of
    | T2Vreg(reg1) => // must be a computation
      fprint!(out, "\n\treturn reg", reg1, ";\n}\n")
  end // end of [emit2_lzy]

(* ****** ****** *)
implement opr_to_string(opr) = ( 
  case opr of 
  | "~" => "neg"
  | "+" => "add"
  | "-" => "sub"
  | "*" => "mul"
  | "%" => "mod"
  | "<" => "ilt"
  | ">" => "igt"
  | "=" => "ieq"
  | "<=" => "ile"
  | ">=" => "ige"
  | "!=" => "neq"
  | "$eval" => "strm_eval"
  | "$lazy" => "strm_lazy"
  | _ => opr
) // end of [opr_to_string]

(* ****** ****** *)

(* end of [CS525-2022-Fall/Final_project_cemit4.dats] *)
