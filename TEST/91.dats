val ninety_one = 
fix M(n: int): int => 
if n > 100 then n - 10
else M(M(n + 11))

val _ =
(
print("ninety_one(3) = "); print(ninety_one(3)); print("\n")
)

val _ =
(
print("ninety_one(90) = "); print(M(90)); print("\n")
)

val _ =
(
print("ninety_one(105) = "); print(M(105)); print("\n")
)