import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_2_9 {G : Type*} [group G] {a b : G}
  (hab : a * b = b * a) : is_abelian_group (subgroup.generated G (a, b)) :=
sorry

theorem exercise_2_4_19 {G : Type*} [group G]
  (h : ∃ (x : G), x ≠ 1 ∧ x * x = 1 ∧ ∀ (y : G), y ≠ 1 → y * y ≠ 1) :
  ∃ (x : G), x ≠ 1 ∧ x * x = 1 ∧ ∀ (y : G), y * x = x * y :=
sorry

theorem exercise_2_11_3 {G : Type*} [group G]
  (hG : ∃ n : ℕ, nat.even n ∧ card G = n) :
  ∃ (x : G), x ≠ 1 ∧ x * x = 1 :=
sorry

theorem exercise_3_5_6 {K V : Type*} [field K] 
  [add_comm_group V] [vector_space K V] [decidable_eq V] 
  (h : ∃ (s : set V), countable s ∧ span K s = ⊤) :
  ∃ (s : set V), countable s ∧ is_basis K s :=
sorry

theorem exercise_6_1_14 {G : Type*} [group G] [fintype G]
  (Z : subgroup G) (hZ : is_cyclic (G / Z)) :
  abelian G :=
sorry

theorem exercise_6_4_3 {G : Type*} [group G] 
  [fintype G] {p q : ℕ} (hp : nat.prime p) (hq : nat.prime q) 
  (h : fintype.card G = p ^ 2 * q) : ¬simple_group G :=
sorry

theorem exercise_6_8_1 {G : Type*} [group G]
  (a b : G) (h : a * b = b * a) :
  subgroup.generated G (a, b) = subgroup.generated G (b * a * b^2, b * a * b^3) :=
sorry

theorem exercise_10_2_4 (x : polynomial ℤ) :
  (ideal.span ℤ {2}).inter (ideal.span ℤ {x}) = ideal.span ℤ {2 * x} :=
sorry

theorem exercise_10_4_6 {R : Type*} [comm_ring R] 
  {I J : ideal R} (h : I ∩ J ≠ ⊥) :
  ∃ (n : ℕ), ∀ (x : R), x ∈ I ∩ J → x ^ n ∈ I * J :=
sorry

theorem exercise_10_7_10 {R : Type*} [ring R] (M : ideal R)
  (hM : ∀ x : R, x ∉ M → is_unit x) :
  is_maximal M ∧ ∀ (N : ideal R), is_maximal N → N = M :=
sorry

theorem exercise_11_4_1b :
  irreducible (polynomial.X^3 + 6 * polynomial.X + 12) :=
sorry

theorem exercise_11_4_6b {p : ℕ} 
  (hp : nat.prime p) :
  irreducible (polynomial.C (p : ℤ) + 1) :=
sorry

theorem exercise_11_4_8 {p n : ℕ} [fact (nat.prime p)] :
  irreducible (polynomial.X ^ n - C p) :=
sorry

theorem exercise_13_4_10 {p : ℕ} (hp : nat.prime p) 
  (h : p = 2 ^ (nat.find_prime_pow p) + 1) :
  ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 :=
sorry