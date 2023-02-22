import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 {G : Type*} [group G] (a b : G) :
  a * b ≈ b * a :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H] :
  center (G * H) = center G * center H :=
sorry

theorem exercise_3_2_7 {K L : Type*} [field K] [field L]
  (f : K →+* L) : function.injective f :=
sorry

theorem exercise_3_7_2 {V : Type*} [field F] [add_comm_group V]
  [vector_space F V] (hF : nonempty F) :
  ¬ (∃ (s : finset (submodule F V)), s.card < ∞ ∧ ∀ (U : submodule F V), U ∈ s → U ≠ ⊤ ∧
  ∀ (U : submodule F V), U ≠ ⊤ → ∃ (U' : submodule F V), U' ∈ s ∧ U ≤ U') :=
sorry

theorem exercise_6_4_2 {G : Type*} [group G] [fintype G]
  {p q : ℕ} (hp : nat.prime p) (hq : nat.prime q) (h : fintype.card G = p * q) :
  ¬simple_group G :=
sorry

theorem exercise_6_4_12 {G : Type*} [group G] (hG : card G = 224) :
  ¬ simple_group G :=
sorry

theorem exercise_10_1_13 {R : Type*} [ring R] (x : R) (hx : ∃ n : ℕ, x ^ n = 0) :
  is_unit (1 + x) :=
sorry

theorem exercise_10_4_7a {R : Type*} [comm_ring R] 
  {I J : ideal R} (h : I + J = ⊤) : I * J = I ∩ J :=
sorry

theorem exercise_10_6_7 {R : Type*} [integral_domain R]
  [algebra ℤ R] (I : ideal R) (hI : I ≠ ⊥) :
  ∃ (z : ℤ), z ≠ 0 ∧ z ∈ I :=
sorry

theorem exercise_11_2_13 {a b : ℤ} (h : a ∣ₚ b) : a ∣ b :=
sorry

theorem exercise_11_4_6a {R : Type*} [comm_ring R]
  [fintype R] (h : fintype.card R = 2) (p : polynomial R) (hp : p.degree = 2) :
  irreducible p :=
sorry

theorem exercise_11_4_6c (x : ℤ/31) :
  irreducible (polynomial.C (x^3 - 9)) :=
sorry

theorem exercise_11_13_3 :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 4] :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] [fintype K] [decidable_eq K]
  (hK : ¬(∃ (x : K), x ≠ 0 ∧ x ≠ 1)) :
  ∏ (x : K), x ≠ 0 → x = -1 :=
sorry