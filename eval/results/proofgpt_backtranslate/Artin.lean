import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 {α β : Type*} [has_mul α] [has_mul β] (a b : α) :
	is_conj (a * b) (b * a) :=
sorry

theorem exercise_2_8_6 {G : Type*} [group G] {N : Type*} [group N]
	(f : G →* N) :
	subgroup.center (G × N) = subgroup.center G × subgroup.center N :=
sorry

theorem exercise_3_2_7 {α β : Type*} [field α] [field β] (f : α →+* β) :
	function.injective ⇑f :=
sorry

theorem exercise_3_7_2 {F : Type u} {V : Type v}
	[field F] [add_comm_group V] [module F V] [infinite F] :
	¬∃ (s : finset V), s.nonempty ∧ ∀ (t : V), t ∈ s → t.finite :=
sorry

theorem exercise_6_4_2 {G : Type*} [group G]
	{p q : ℕ} [hp : fact (nat.prime p)] [hq : fact (nat.prime q)] :
	simple_group.prime_and_prime (p * q) ↔ simple_group.prime_and_prime p ∧ simple_group.prime_and_prime q :=
sorry

theorem exercise_6_4_12 (α : Type*)
	[group α] [fintype α] [is_simple_group α] :
	¬fintype.card α = 2 * 1 :=
sorry

theorem exercise_10_1_13 {R : Type u} {x : R} [semiring R] :
	is_nilpotent x → is_unit (1 + x) :=
sorry

theorem exercise_10_4_7a {α : Type u} [ring α] {I J : ideal α} (h : I ⊔ J = ⊤) :
	I * J = I ⊓ J :=
sorry

theorem exercise_10_6_7 (x : gaussian_int) :
	∃ (n : ℕ), x ≠ 0 ∧ ↑n ≠ 0 :=
sorry

theorem exercise_11_2_13 {a b : ℤ} :
	↑a ∣ ↑b → a ∣ b :=
sorry

theorem exercise_11_4_6a :
	irreducible (2 ^ (2 + 1)) :=
sorry

theorem exercise_11_4_6c (p : ℕ) [hp : fact (nat.prime p)]
	(x : ℤ) :
	irreducible (x ^ 3 - 9) :=
sorry

theorem exercise_11_13_3 (n : ℕ) :
	∃ (k : ℕ), nat.prime k ∧ k ≡ -1 [MOD 4] :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] [fintype K] :
	finset.univ.prod (λ (k : K), -1) = -1 :=
sorry