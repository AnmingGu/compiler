// // TODO: HANDLE HIGHER ORDER CLOSURES
fun foo(x) =
let
    fun a(x1) = x1 + 1
    fun b(x1) = 1 + x + a(2)
    fun c(x1) = if x1 < 0 then b(2) else c(x1 - 1) + 1
    fun d(x1) = 
      let
        val A = 1
        val B = a(A)
        fun D(x1) = B + x1
      in
        A + B + D(x1)
      end
in
  x + a(1) + c(10) + c(~9) + d(a(1))
end

// // TODO: HANDLE ENV
// fun foo(x) =
//   let
//     val y = 5
//     fun bar(x1: int): int =
//       let
//         val x = y + x1
//         val _ = (print("(x = "); print(x); print("); "))
//         fun j(x2) = x2 + y
//         // change j to this and doesnt work
//         // fun j(x1) = x1 + x
//       in
//         x * y - j(1)
//       end
//   in
//     bar(x + 1)
//   end

// TODO: 
// fun foo(x) =
// let
//   val y = 5
//   fun bar(x1: int): int =
//   let
//     val x = y + x1
//     fun bazz(x2: int): int = x2 + x
//   in
//     x * y - bazz(x)
//   end
// in
//   bar(x + 1)
// end

// TODO: HANDLE ENV
// fun foo(x) =
// let
//   fun bar(x1: int): int =
//     let
//       val x2 = y + x1
//       fun bazz(x3: int): int = 
//         let
//           val j = 5
//         in
//           // x3 + fizz1_(2)
//           x3 + j
//         end
//     in
//       // x * y - x2
//       x * y - bazz(1)
//     end
// in
//   bar(x + 1)
// end


val _ =
(
print("foo(10) = "); print(foo(10)); print("\n")
)

(* ****** ****** *)

(* end of [CS525-2022-Fall/Midterm_project_TEST_fact2.dats] *)
