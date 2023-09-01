(*
val
omega =
lam x => x(x)
val
Omega = omega(omega)
*)
(* ****** ****** *)

val I = lam x => x
val K = lam x => lam y => x
val K' = lam x => lam y => y
val S = lam x => lam y => lam z => x(z)(y(z))
val I' = S(K)(I)(K(I)(S))

(* ****** ****** *)
val _ = (print(I(5)); print("\n"))
val _ = (print(K(6)); print("\n"))
val _ = (print(K'(6)(5)); print("\n"))
val _ = (
print("I'(5) = "); 
print(I'(5)); 
print("\n"))

(* end of [nators.dats] *)
