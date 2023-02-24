import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_3 (a b c : ℤ) :
	(a + b) % c + c = a % c + b % c :=
sorry

theorem exercise_1_1_5 {n : ℕ} (h : 1 < n) :
  ¬group (nat.mod_add_group n) :=
sorry

theorem exercise_1_1_16 {G : Type u} {x : G} [monoid G] :
  x ^ 2 = 1 ↔ order_of x = 1 ∨ order_of x = 2 :=
sorry

theorem exercise_1_1_18 {G : Type*} [group G] {x y : G} :
  x * y = y * x ↔ y⁻¹ * x * y = x ↔ x⁻¹ * y⁻¹ * x * y = 1 :=
sorry

theorem exercise_1_1_22a {G : Type u} [group G] (x g : G) :
	order_of (g⁻¹ * x * g) = order_of x :=
sorry

theorem exercise_1_1_25 {G : Type*} [group G]
  (h : ∀ (x : G), x ^ 2 = 1) : is_commutative G :=
sorry

theorem exercise_1_1_34 {G : Type*} [group G] {n : ℤ}
  (hn : n ≠ 0) {x : G} (h : x ^ n = 1) :
  ∃ (n : ℕ), 0 < n ∧ x ^ n = 1 :=
sorry

theorem exercise_1_6_4 :
	¬isomorphic (multiplicative (real.nonzero)) (multiplicative (complex.nonzero)) :=
sorry

theorem exercise_1_6_17 {G : Type*} [group G] :
  is_group_hom (λ (g : G), g⁻¹) ↔ abelian G :=
sorry

theorem exercise_2_1_5 {G : Type*} [group G]
  (H : subgroup G) [fintype ↥H] (h : fintype.card ↥H = fintype.card G - 1)
  (hG : fintype.card G > 2) : H = ⊥ :=
sorry

theorem exercise_2_4_4 {G : Type*} [group G]
  (H : subgroup G) :
  H.generated_by (H.erase 1) = H :=
sorry

theorem exercise_2_4_16b {n : ℕ} :
	is_maximal_subgroup (dihedral_group.rotation_subgroup n) :=
sorry

theorem exercise_3_1_3a  {G : Type*} [add_group G] {A : add_subgroup G} [hA : A.normal] :
  add_group (quotient_add_group.quotient_add_group_of A) :=
sorry

theorem exercise_3_1_22b {G : Type*} [group G] {ι : Type*} (s : ι → set G)
  (h : ∀ i, is_normal_subgroup (s i)) (hne : ∃ i, s i ≠ ∅) :
  is_normal_subgroup (⋂ i, s i) :=
sorry

theorem exercise_3_2_11 {G : Type*} [group G]
  (H K : subgroup G) (hHK : H ≤ K) :
  nat.card ↥H * nat.card ↥K = nat.card G :=
sorry

theorem exercise_3_2_21a {G : Type*} [add_group G]
  (H : add_subgroup G) [fintype ↥H] [fintype G]
  (h : fintype.card ↥H = fintype.card G) :
  H = ⊤ :=
sorry

theorem exercise_3_4_1 {G : Type*} [hG : group G] [hG' : abelian G]
  [hG'' : simple_group G] : is_cyclic G :=
sorry

theorem exercise_3_4_5a {G : Type*} [group G] (H : subgroup G)
  [h : is_solvable G] :
  is_solvable H :=
sorry

theorem exercise_3_4_11 {G : Type*}
  [group G] (H : subgroup G) [H.normal] [h : is_solvable G]
  (hH : H ≠ ⊥) :
  ∃ (A : subgroup G), A.abelian ∧ A.normal ∧ A ≠ ⊥ :=
sorry

theorem exercise_4_2_14  {G : Type*} [group G] [fintype G] (hG : ¬is_simple_group G)
  (hGc : nat.prime.coprime (fintype.card G) 2) :
  ∀ (k : ℕ), k ∣ fintype.card G → ∃ (H : subgroup G), fintype.card H = k :=
sorry

theorem exercise_4_3_26 {G : Type*} [group G]
  [fintype G] {A : Type*} [fintype A] (hG : transitive G A)
  (hA : fintype.card A > 1) :
  ∃ (σ : G), σ ≠ 1 :=
sorry

theorem exercise_4_4_6a {G : Type*} [group G] (H : subgroup G)
  [h : H.characteristic] :
  H.normal :=
sorry

theorem exercise_4_4_7 {G : Type*} [group G] {H : subgroup G}
  (hH : unique_of_order_of_subgroup H) :
  H.char :=
sorry

theorem exercise_4_5_1a {p : ℕ} {G : Type*} [group G] (P : sylow p G) {N : subgroup G}
  (h : ↑P ≤ N) :
  sylow p ↥N :=
sorry

theorem exercise_4_5_14 {G : Type u} [group G]
  (hG : fintype.card G = 312) :
  ∃ (p : ℕ) (h : nat.prime p) (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_16 {G : Type*} [group G] {p q r : ℕ}
  (h : fintype.card G = p * q * r) (hp : nat.prime p) (hq : nat.prime q)
  (hr : nat.prime r) (h1 : p < q) (h2 : q < r) :
  ∃ (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_18 {G : Type*} [group G]
  (hG : fintype.card G = 200) :
  ∃ (P : sylow 5 G), ↑P.normal :=
sorry

theorem exercise_4_5_20 {G : Type*} [group G] [fintype G]
  (h : fintype.card G = 1365) : ¬ simple_group G :=
sorry

theorem exercise_4_5_22 {G : Type*} [group G] [fintype G]
  (h : fintype.card G = 132) : ¬ simple_group G :=
sorry

theorem exercise_4_5_28 {G : Type*} [group G]
  (hG : fintype.card G = 105) (P : sylow 3 G) (hP : P.normal) :
  abelian G :=
sorry

theorem exercise_5_4_2 {G : Type*} [group G] {H : subgroup G} :
  H.normal ↔ comm_subgroup.commutator G H ≤ H :=
sorry

theorem exercise_7_1_11 {R : Type*} [integral_domain R] (x : R) :
	x ^ 2 = 1 ↔ x = 1 ∨ x = -1 :=
sorry

theorem exercise_7_1_15 {R : Type*} [ring R] (h : ∀ (a : R), a ^ 2 = a) :
	comm_ring R :=
sorry

theorem exercise_7_2_12 {G : Type*} [group G] {a : G} :
  a ∈ is_subgroup.center G ↔ ∀ (g : G), g * a = a * g :=
sorry

theorem exercise_7_3_37 {R : Type*} [comm_monoid_with_zero R]
  [no_zero_divisors R] {n : R} (h : is_prime_pow n) :
  ∃ (k : ℕ), n ^ k = 0 :=
sorry

theorem exercise_8_1_12 {k n : ℕ} (hkn : n.coprime k)
  (hk : 1 < k) :
  ∃ (m : ℕ), n * m % k = 1 :=
sorry

theorem exercise_8_3_4 {r s : ℤ} (h : ∃ (a b : ℚ), r = a ^ 2 + b ^ 2) :
	∃ (a b : ℤ), r = a ^ 2 + b ^ 2 :=
sorry

theorem exercise_8_3_6a {α : Type u} [ring α]
  [invertible 2] (h : α = ℤ[i] / (1 + i)) :
  field.card (α) = 2 :=
sorry

theorem exercise_9_1_6 (x y : ℚ) :
  ¬ (ideal.span {x, y} : ideal ℚ) = ⟨x, y⟩ :=
sorry

theorem exercise_9_3_2 {R : Type u} [comm_ring R] {f g : polynomial R}
  (hf : f.separable) (hg : g.separable) (h : is_coprime f g) :
  (f * g).separable :=
sorry

theorem exercise_9_4_2b (R : Type*) [ring R] :
  irreducible (polynomial.X ^ 6 + 30 * polynomial.X ^ 5 - 15 * polynomial.X ^ 3 + 6 * polynomial.X - 120) :=
sorry

theorem exercise_9_4_2d {p : ℕ} (hp : nat.prime p) (hodd : p ≠ 2) :
  irreducible (λ (x : ℤ), (x + 2) ^ p - 2 ^ p) :=
sorry

theorem exercise_9_4_11 {R : Type*} [comm_ring R]
  (x y : R) :
  irreducible (X ^ 2 + X ^ 2 - 1) :=
sorry