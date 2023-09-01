(* ****** ****** *)

fun
ints_from
(n0: int): lazy(strm(int)) =
$lazy
(strm_cons(n0, ints_from(n0+1)))

(* ****** ****** *)

// fun ints_print(xs: lazy(strm(int))): void = 
//   (auxmain(xs)(0)(99)) 
//   where {
// fun auxmain(xs: lazy(strm(int))) =
//   lam(i0) => lam(n0): void =>
//     if (i0 >= n0) then print(",...") 
//     else (
//     let
//       val xs = $eval(xs)
//     in
//       if strm_nilq(xs) then print( "." )
//       else
//         let
//           val x1 = strm_uncons1(xs)
//           val xs = strm_uncons2(xs)
//         in
//           (if i0 > 0 then print(","); print(x1); 
//           auxmain(xs)(i0+1)(n0))
//         end
//     end
//     )
// }

// This works
fun auxmain(input: (lazy(strm(int)), (int, int))) =
let
  val xs = fst(input)
  val i0 = fst(snd(input))
  val n0 = snd(snd(input))
in
  if (i0 >= n0) then print(",...") 
  else (
    let
      val xs = $eval(xs)
    in
      if strm_nilq(xs) then print( "." )
      else
        let
          val x1 = strm_uncons1(xs)
          val xs = strm_uncons2(xs)
        in
          (if i0 > 0 then print(","); print(x1); auxmain((xs, (i0+1, n0))))
        end
    end
  )
end
//
fun
ints_print
( xs
: lazy
  (strm(int))): void = (auxmain(xs, (0, 100)))

val _ =
(ints_print(ints_from(2)); print("\n"))

(* ****** ****** *)

(* end of [CS525-2022-Fall/Final_project_TEST_lazy.dats] *)
