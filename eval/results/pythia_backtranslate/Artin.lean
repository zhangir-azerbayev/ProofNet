import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 {α : Type u} [monoid α] {a b : α} :
	is_conj a b (a * b) :=
sorry

theorem exercise_2_8_6 (G : Type w) (H : Type x) [group G]
	[group H] :
	(group_topology.prod_center G H)⁻¹ = group_topology.prod_center H G :=
sorry

theorem exercise_3_2_7 (F : Type*) [field F] {E : Type*} [field E]
	[algebra F E] :
	function.injective ⇑(algebra_map F E) :=
sorry

theorem exercise_3_7_2 : Type u} [field F] {V : Type v} [add_comm_group V] [module F V]
	[finite_dimensional F V] :
	¬∃ (s : set V) (H : s.finite), is_compl s V :=
sorry

theorem exercise_6_4_2 (p q : ℕ)
	(h : lucas_lehmer.int.prime p) :
	↑(p ^ q) = ↑p ^ q :=
sorry

theorem exercise_6_4_12 (A : Type*) [comm_group A]
	[fintype A] :
	is_simple_group_of_prime_power_series A :=
sorry

theorem exercise_10_1_13 {R : Type u} {x : R} [ring R]
	(h : is_nilpotent x) :
	is_unit (1 + x) :=
sorry

theorem exercise_10_4_7a {α : Type u} [semiring α] {I J : ideal α}
	(h : I ⊔ J = ⊤) :
	I ∩ J = I ⊓ J :=
sorry

theorem exercise_10_6_7 (R : Type*)
	[comm_ring R] (abv : absolute_value R ℤ) {x : R} (hx : x ∈ ideal.span {0}) :
	∃ (n : ℤ), x ^ n = 0 :=
sorry

theorem exercise_11_2_13 {α : Type*} [comm_ring α] {a b : α} :
	a * b ∣ a → a ∣ b :=
sorry

theorem exercise_11_4_6a (x : ℕ) :
	irreducible (x ^ 2 + x + 1) :=
sorry

theorem exercise_11_4_6c {x : ℕ} (hx : x ≠ 0) :
	irreducible (polynomial.X ^ 3 - 9) :=
sorry

theorem exercise_11_13_3 :
	(∃ (n : ℕ), nat.arithmetic_function.is_primitive.many_prime n ∧ n % 4 = -1) :=
sorry

theorem exercise_13_6_10 (K : Type*) [field K] [fintype K]
	[decidable_eq K] [is_absolute_value has_mul.mul] :
	finset.univ.prod (λ (u : Kˣ), (↑u)⁻¹) = -1 :=
sorry