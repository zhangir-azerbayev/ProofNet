import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_3 (n : ℕ) (a b c : ℤ) :
  (a + b) % n + c % n = (a + c) % n + b % n :=
sorry

theorem exercise_1_1_5 {n : ℕ} (h : 1 < n) :
  ¬group (add_comm_group.quotient (nat.mod_add_sub_group n)) :=
sorry

theorem exercise_1_1_16 {G : Type*} [group G] (x : G)
  (hx : x ^ 2 = 1) : order x = 1 ∨ order x = 2 :=
sorry

theorem exercise_1_1_18 {G : Type*} [group G]
  (x y : G) : x * y = y * x ↔ y⁻¹ * x * y = x ↔ x⁻¹ * y⁻¹ * x * y = 1 :=
sorry

theorem exercise_1_1_22a (G : Type*) [group G] (x g : G) :
  order x = order (g⁻¹ * x * g) :=
sorry

theorem exercise_1_1_25 {G : Type*} [group G] (h : ∀ x : G, x ^ 2 = 1) :
  abelian G :=
sorry

theorem exercise_1_1_34 (G : Type*) [group G] (x : G)
  (hx_inf : ∀ n : ℕ, x ^ n ≠ 1) :
  ∀ m n : ℤ, m ≠ n → x ^ m ≠ x ^ n :=
sorry

theorem exercise_1_6_4 :
  ¬(multiplicative ℝ ≃* multiplicative ℂ) :=
sorry

theorem exercise_1_6_17 {G : Type*} [group G] :
  function.is_group_hom (λ g : G, g⁻¹) ↔ comm_group G :=
sorry

theorem exercise_2_1_5 {G : Type*} [group G] 
  [fintype G] (hG : fintype.card G > 2) :
  ¬ ∃ (H : subgroup G), fintype.card H = fintype.card G - 1 :=
sorry

theorem exercise_2_4_4 {G : Type*} [group G] {H : subgroup G} :
  H = subgroup.generated (H.erase 1) :=
sorry

theorem exercise_2_4_16b {n : ℕ} (hn : nat.prime n) :
  maximal (dihedral_group n).rotations :=
sorry

theorem exercise_3_1_3a {A B : Type*} [group A] [group B]
  [abelian A] (hB : is_subgroup B) : abelian (A / B) :=
sorry

theorem exercise_3_1_22b {G : Type*} [group G] 
  {I : Type*} (hI : nonempty I) (H : I → subgroup G) (hH : ∀ i, is_normal (H i)) :
  is_normal (⋂ i, H i) :=
sorry

theorem exercise_3_2_11 {G H K : Type*} [group G]
  [group H] [group K] (hH : H ≤ K) (hK : K ≤ G) :
  card (G ⧸ H) = card (G ⧸ K) * card (K ⧸ H) :=
sorry

theorem exercise_3_2_21a :
  ∀ (G : Type*) [group G] [fintype G] [decidable_eq G] (H : subgroup G) 
  (hH : H ≠ ⊥) (hH_fin : fintype.card H < fintype.card G),
  H = ⊤ :=
sorry

theorem exercise_3_4_1 {G : Type*} [group G] [fintype G]
  (hG : abelian_simple G) : is_cyclic G :=
sorry

theorem exercise_3_4_5a {G : Type*} [group G] [solvable G] 
  {H : subgroup G} : solvable H :=
sorry

theorem exercise_3_4_11 {G : Type*} 
  [group G] [fintype G] [decidable_eq G] (H : subgroup G) (hH : H ≠ ⊥) 
  (hH_normal : H.normal) (hG_solvable : solvable_group G) :
  ∃ (A : subgroup G), A ≠ ⊥ ∧ A.normal ∧ abelian_group A :=
sorry

theorem exercise_4_2_14 {G : Type*} 
  [group G] [fintype G] (hG : ¬ is_prime (card G)) 
  (h : ∀ (k : ℕ), k ∣ card G → ∃ (H : subgroup G), card H = k) :
  ¬ simple_group G :=
sorry

theorem exercise_4_3_26 {G : Type*} 
  [fintype G] [decidable_eq G] [fintype A] [decidable_eq A] 
  [perm_group G] (hG : transitive_perm_group G A) (hA : A.card > 1) :
  ∃ (σ : G), ∀ (a : A), σ a ≠ a :=
sorry

theorem exercise_4_4_6a {G : Type*} [group G] {p : ℕ} [hp : fact (nat.prime p)]
  (H : subgroup G) (hH : H.is_char p) :
  normal_subgroup H :=
sorry

theorem exercise_4_4_7 {G : Type*} [group G] {H : subgroup G}
  (hH : ∃ (n : ℕ), H.order = n) (hH' : ∀ (H' : subgroup G), H'.order = H.order → H' = H) :
  char_subgroup H :=
sorry

theorem exercise_4_5_1a {G : Type*} [group G] {p : ℕ}
  (hG : is_p_group p G) (H : subgroup G) (P : sylow p G) (hP : P ≤ H) :
  sylow p H :=
sorry

theorem exercise_4_5_14 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 312) :
  ∃ (p : ℕ) (hp : nat.prime p) (H : sylow p G), H.normal :=
sorry

theorem exercise_4_5_16 {G : Type*} [group G] 
  [fintype G] {p q r : ℕ} [hp : fact (nat.prime p)] [hq : fact (nat.prime q)] 
  [hr : fact (nat.prime r)] (h : p < q ∧ q < r) (hG : fintype.card G = p * q * r) :
  ∃ (P : sylow p G), P.normal :=
sorry

theorem exercise_4_5_18 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 200) :
  ∃ (P : sylow 5 G), P.normal :=
sorry

theorem exercise_4_5_20 {G : Type*} [group G] [fintype G] 
  (hG : fintype.card G = 1365) : ¬ simple_group G :=
sorry

theorem exercise_4_5_22 {G : Type*} [group G] [fintype G] 
  (hG : fintype.card G = 132) : ¬simple_group G :=
sorry

theorem exercise_4_5_28 {G : Type*} [group G] 
  (hG : card G = 105) (H : sylow 3 G) (hH : H.normal) :
  abelian G :=
sorry

theorem exercise_5_4_2 {G : Type*} [group G] {H : subgroup G} :
  H ≤ normalizer G H ↔ comm_group.comm_subgroup G H ≤ H :=
sorry

theorem exercise_7_1_11 {R : Type*} 
  [integral_domain R] (x : R) (hx : x ^ 2 = 1) : x = 1 ∨ x = -1 :=
sorry

theorem exercise_7_1_15 {R : Type*} [ring R] (h : ∀ (a : R), a ^ 2 = a) :
  comm_ring R :=
sorry

theorem exercise_7_2_12 {R : Type*} [comm_ring R] 
  {G : Type*} [fintype G] [group G] :
  (∑ g : G, g) ∈ center (group_ring R G) :=
sorry

theorem exercise_7_3_37 {R : Type*} [comm_ring R] (N : ideal R) 
  (hN : ∃ (n : ℕ), N ^ n = ⊥) :
  nilpotent N :=
sorry

theorem exercise_8_1_12 {N M M₁ d : ℕ}
  (hN : 0 < N) (hM : nat.coprime N M) (hM₁ : nat.coprime N M₁)
  (hM₁d : M₁ ≡ M ^ d [MOD N]) (hdd' : nat.coprime (nat.phi N) d) :
  ∃ (d' : ℕ), M ≡ M₁ ^ d' [MOD N] :=
sorry

theorem exercise_8_3_4 {n : ℤ} (h : ∃ (a b : ℚ), n = a ^ 2 + b ^ 2) :
  ∃ (a b : ℤ), n = a ^ 2 + b ^ 2 :=
sorry

theorem exercise_8_3_6a :
  is_field (ℤ[i] / (1 + i)) ∧ (ℤ[i] / (1 + i)).card = 2 :=
sorry

theorem exercise_9_1_6 (x y : polynomial ℚ) :
  ¬is_principal_ideal (ideal.span {x, y}) :=
sorry

theorem exercise_9_3_2 {α : Type*} 
  [integral_domain α] [decidable_eq α] {β : Type*} [integral_domain β] 
  [decidable_eq β] [algebra α β] {f g : polynomial β} (hfg : f * g = (f.map coe) * (g.map coe)) :
  ∀ (a : β) (b : α), a * b ∈ (int.algebra α) → a * b ∈ (int.algebra β) :=
sorry

theorem exercise_9_4_2b :
  irreducible (polynomial.C 120 * X^6 + polynomial.C 6 * X^5 - polynomial.C 15 * X^3 + polynomial.C 30 * X^4 + 1) :=
sorry

theorem exercise_9_4_2d (p : ℕ) (hp : nat.prime p) (hp_odd : p % 2 = 1) :
  irreducible (polynomial.C (p : ℤ) * X ^ p - polynomial.C (2 ^ p : ℤ)) :=
sorry

theorem exercise_9_4_11 
  (x y : polynomial ℚ) :
  irreducible (x^2 + y^2 - 1) :=
sorry