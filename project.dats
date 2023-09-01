%{^
extern void libxatsopt_dynloadall();
%} (* %{^ *)
val () = $extfcall(void, "libxatsopt_dynloadall")

(* ****** ****** *)
#include "share/atspre_staload.hats"
#staload UN = "prelude/SATS/unsafe.sats"
#staload "./../../../mylib/mylib.dats"
#include "./../../HATS/libxatsopt.hats"
#staload "./project.sats"
#staload "./project_t1ype.dats"
#staload "./project_t1erm.dats"
#dynload "./project_trans.dats"
#dynload "./project_t1ype.dats"
#dynload "./project_t1erm.dats"
#dynload "./project_t1val.dats"
#dynload "./project_t2cmp.dats"
#dynload "./project_interp1.dats"
#dynload "./project_tcheck2.dats"
#dynload "./project_atrans3.dats"
#dynload "./project_cemit4.dats"

(* ****** ****** *)
extern fun the_fixity_load(XATSHOME: string) : void = 
  "ext#libxatsopt_the_fixity_load"

(* ****** ****** *)
local
  static fun process_stdin(): void
  static fun process_fpath(fp0: fpath_t, output: string): void
  static fun process_given(given: string, output: string): void

  (* ****** ****** *)
  implement process_stdin() =
    process_fpath(fp0, "")
    where {
      val fp0 = $FP0.the_filpath_stdin
    }
  implement process_fpath(fp0, output) =
    let
      val stadyn = 1 // dynamic
    in
      if (stadyn >= 0 ) then {
        val d0cs =
          let
            val dpar = parse_from_filpath_toplevel(stadyn, fp0)
          in
            case+ dpar.parsed() of
            | Some(d0cs) => d0cs
            | None((*void*)) => list_nil()
          end : d0eclist // end-of-val
        val () = synread_d0eclist(d0cs)
        val d1cs = trans01_declist(d0cs)
        // val () = println!("process_fpath: d1cs = ", d1cs)
        val () = tread01_d1eclist(d1cs)
        val t1ds = trans1m_d1eclist(d1cs)
        val () = 
          if output = "" then (
            let
              val () = println!("process_fpath: t1ds = ", t1ds, "\n")
              val () = println!("Part 1: Interpreting")
              val _ = t1dclist_interp0(t1ds)
              val () = println!()
              val () = println!("Part 2: Type Checking & Inference")
              val tps = t1dclist_oftype0(t1ds)
              val () = print!("types = (\n")
              val () = show_types(stdout_ref, tps)
              val () = println!(")\n")
              val () = println!("Part 3: A-Normal Form")
              val env2 = t1dclist_atrans0(t1ds)
              val () = print!("env2 = (\n")
              val () = show_env2(stdout_ref, env2)
              val () = println!(")\n")
              val () = println!("Part 4: Emit C code\n")
              val () = t2env_emit0(stdout_ref, env2)
            in () end
          )
          else (
            let
              val out = fileref_open_exn(string_append(output, ".c"), file_mode_w)
              val () = fprint!(out, "/*\n")
              val () = fprint!(out, "Program:\n")
              val () = show_program(out, t1ds)
              val () = fprint!(out, "Part 1: Interpreting\n")
              val env1 = t1dclist_interp0(t1ds)
              val () = fprint!(out, "env1 = (\n")
              val () = show_env1(out, env1)
              val () = fprint!(out, ")\n")
              // val () = fprint!(out, "Part 2: Type Checking & Inference\n")
              // val tps = t1dclist_oftype0(t1ds)
              // val () = fprint!(out, "types = (\n")
              // val () = show_types(out, tps)
              // val () = fprint!(out, ")\n")
              val () = fprint!(out, "Part 3: A-Normal Form\n")
              val env2 = t1dclist_atrans0(t1ds)
              val () = fprint!(out, "env2 = (\n")
              val () = show_env2(out, env2)
              val () = fprint!(out, ")\n")
              val () = fprint!(out, "*/\n\n")
            in (
              fprint!(out, "// Part 4: Emit C code\n"); 
              t2env_emit0(out, env2); 
              fileref_close(out)
            )
            end
          )
      } (* end of [then] *)
      else {
        // ~(stadyn >= 0) // not for loading code
      } (* end of [else] *)    
    end // end of [process_fpath]
    where {
      fun show_program(out: FILEref, t1ds: t1dclist) = 
        case t1ds of 
        | mylist_nil() => ()
        | mylist_cons(elem, t1ds) => (
            fprint!(out, "\t", elem, ", \n"); 
            show_program(out, t1ds)
          )
      fun show_env1(out: FILEref, env1: t1env) = 
        case env1 of 
        | mylist_nil() => ()
        | mylist_cons(elem, env1) => (
            fprint!(out, "\t", elem.0, ": ", elem.1, ", \n"); 
            show_env1(out, env1)
          )
      fun show_types(out: FILEref, tps: t1ctx) = 
        case tps of 
        | mylist_nil() => ()
        | mylist_cons(elem, tps) => (
            fprint!(out, "\t", elem.0, ": ", elem.1, ", \n");
            show_types(out, tps)
        )
      fun show_env2(out: FILEref, env2: t2env) = 
        case env2 of 
        | mylist_nil() => ()
        | mylist_cons(elem, env2) => (
            fprint!(out, "\t", elem.0, ": ", elem.1, ", \n"); 
            show_env2(out, env2)
          )
      }

  (* ****** ****** *)
  implement process_given(arg0, output) = 
    let
      val fp0 =
        let
          val given = arg0
          val fname = arg0
        in//let
          fpath_make(given, fname)
        end (*let*)//end-of-val(fp0)
      // val () = println!("process_given: arg0 = ", arg0)
    in
      process_fpath(fp0, output)
    end // end of [process_given]
in(*in-of-local*)
  implement project_main0(argc, argv) =
    let
      val XATSHOME = "./../../Xanadu"
      // val XATSHOME = $GLO.the_XATSHOME_get()
      val ((*void*)) = the_fixity_load(XATSHOME)
    in
      if (argc = 1) then process_stdin() 
      else if (argc = 2) then process_given(argv[1], "")
      else process_given(argv[1], argv[2])
    end // project_main0
end // end of [local]

(* ****** ****** *)
implement main0(argc, argv) =
  if (argc >= 2) then project_main0(argc, argv)
  else prerrln!("Hello from CS525(Final)!")

(* ****** ****** *)

(* end of [CS525-2022-Fall/Final_project.dats] *)
