val K1 = 
fix f(x) => x + 1

val _ =
(
print("K1(3) = "); print(K1(3)); print("\n")
)

val K2 = lam(x) => x + 2

val _ =
(
print("K2(3) = "); print(K2(3)); print("\n")
)

val K3 = 
fix h(x) => x % 5

val _ =
(
print("K3(3) = "); print(K3(3)); print("\n")
)
