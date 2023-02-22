import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 {G : Type*} [group G] (a b : G) :
  conjugate (a * b) (b * a) :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H] :
  subgroup.center (G × H) = subgroup.center G × subgroup.center H :=
sorry

theorem exercise_3_2_7 {α β : Type*} [field α] [field β] (f : α →+* β) :
  function.injective ⇑f :=
sorry

theorem exercise_3_7_2 {k V : Type*} [field k]
  [add_comm_group V] [vector_space k V] (hk : k.card ≠ finset.card.fintype) :
  ¬ (∃ (s : set (submodule k V)), s.finite ∧ ∀ (t : submodule k V), t ∈ s → t ≠ ⊤ ∧ t ≠ ⊥ ∧ t ≠ ⊤) :=
sorry

theorem exercise_6_4_2 {α : Type u} [group α]
  [fintype α] {p q : ℕ} [hp : fact (nat.prime p)] [hq : fact (nat.prime q)]
  (h : fintype.card α = p * q) :
  ¬ is_simple_group α :=
sorry

theorem exercise_6_4_12 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 224) : ¬ simple_group G :=
sorry

theorem exercise_10_1_13 {R : Type*} [comm_ring R] {x : R} (hx : ∃ (n : ℕ), x ^ n = 0) :
  is_unit (1 + x) :=
sorry

theorem exercise_10_4_7a {R : Type u} [comm_semiring R]
  {I J : ideal R} (h : I ⊔ J = ⊤) :
  I * J = I ⊓ J :=
sorry

theorem exercise_10_6_7 {R : Type*} [comm_ring R]
  {K : Type*} [field K] [algebra R K] [is_fraction_ring R K]
  {I : fractional_ideal (non_zero_divisors R) K} [nontrivial R] (hI : I ≠ 0) :
  ∃ (x : R) (H : x ≠ 0), ⇑(algebra_map R K) x ∈ I :=
sorry

theorem exercise_11_2_13 {a b c : ℤ} (h : a * b ∣ c) :
	b ∣ c / a :=
sorry

theorem exercise_11_4_6a {R : Type u} [field R]
  {p : polynomial R} (hp1 : p.degree = 1) (hm : p.monic) :
  irreducible p :=
sorry

theorem exercise_11_4_6c {R : Type u} [comm_ring R] [is_domain R]
  (r : R) :
  irreducible (polynomial.X - ⇑polynomial.C r) :=
sorry

theorem exercise_11_13_3 :
  ∃ (p : ℕ), nat.prime p ∧ p % 4 = 3 :=
sorry

theorem exercise_13_6_10 {K : Type*} [fintype K] [field K]
  (hK : ∀ (x : K), x ≠ 0) :
  ∏ (x : K), x ≠ 0 :=
sorry