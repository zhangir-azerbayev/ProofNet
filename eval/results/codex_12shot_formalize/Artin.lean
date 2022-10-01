import algebra.group.basic
import group_theory.order_of_element
import data.real.basic
import data.complex.basic
import data.fintype.basic
import data.zmod.basic
import data.countable.basic
import data.set.countable
import data.polynomial.basic
import group_theory.subgroup.basic
import group_theory.quotient_group
import group_theory.index
import group_theory.specific_groups.cyclic
import group_theory.solvable
import group_theory.free_group
import group_theory.presented_group
import number_theory.zsqrtd.gaussian_int
import ring_theory.ideal.operations
import algebra.char_p.basic

open function fintype subgroup ideal polynomial submodule zsqrtd
open char_p
open_locale big_operators
noncomputable theory

theorem exercise_10_1_13 {R : Type*} [ring R] (x : R) (hx : ∃ n : ℕ, x ^ n = 0) :
  is_unit (1 + x) :=
sorry

theorem exercise_10_2_4 (x : polynomial ℤ) :
  (ideal.span ℤ {2}).inter (ideal.span ℤ {x}) = ideal.span ℤ {2 * x} :=
sorry

theorem exercise_10_4_6 {R : Type*} [comm_ring R] 
  {I J : ideal R} (hIJ : I ∩ J = ⊥) :
  ∀ (x : R), x ∈ I → x ∈ J → x ∈ I * J → nilpotent (x + I * J) :=
sorry

theorem exercise_10_4_7a {R : Type*} [comm_ring R] 
  {I J : ideal R} (h : I + J = ⊤) : I * J = I ∩ J :=
sorry

theorem exercise_10_5_16  {F : Type*} [field F] (f : polynomial F →+* polynomial F)
  (hf : f.is_ring_iso) (g : polynomial F →+* polynomial F)
  (hg : g.is_ring_iso) (h : characteristic F = 2) :
  f.is_ring_iso ∧ g.is_ring_iso :=
sorry

theorem exercise_10_6_7 {R : Type*} [integral_domain R]
  [algebra ℤ R] (I : ideal R) (hI : I ≠ ⊥) :
  ∃ (z : ℤ), z ≠ 0 ∧ z ∈ I :=
sorry

theorem exercise_10_7_10 {R : Type*} [ring R] (M : ideal R)
  (hM : ∀ x : R, x ∉ M → is_unit x) :
  is_maximal M ∧ ∀ (N : ideal R), is_maximal N → N = M :=
sorry

theorem exercise_10_7_6 {α : Type*} 
  [integral_domain α] [fintype α] [decidable_eq α] (p : polynomial α) 
  (hp : p.degree = 2) (hp_irred : irreducible p) :
  is_field (polynomial.quotient p) :=
sorry

theorem exercise_11_12_3 {p : ℕ} 
  (hp : nat.prime p) (h : ∃ (x : ℤ), x ^ 2 ≡ -5 [MOD p]) :
  ∃ (x y : ℤ), x ^ 2 + 5 * y ^ 2 = p ∨ 2 * x ^ 2 + 2 * x * y + 3 * y ^ 2 = p :=
sorry

theorem exercise_11_13_3 :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 4] :=
sorry

theorem exercise_11_2_13 {a b : ℤ} (h : a ∣ₚ b) : a ∣ b :=
sorry

theorem exercise_11_3_1 {F : Type*} [field F]
  (a b : F) (ha : a ≠ 0) (f : polynomial F) :
  irreducible f ↔ irreducible (f.map (λ x, a * x + b)) :=
sorry

theorem exercise_11_3_4 {R : Type*} [integral_domain R]
  [decidable_eq R] {f g : polynomial R} (hf : f ≠ 0) (hg : g ≠ 0)
  (h : polynomial.rel_prime f g) :
  ∃ (a : ℤ), a ∈ (ideal.span ℤ {f, g}) :=
sorry

theorem exercise_11_4_1b :
  irreducible (polynomial.X^3 + 6 * polynomial.X + 12) :=
sorry

theorem exercise_11_4_6a {R : Type*} [comm_ring R]
  [fintype R] (h : fintype.card R = 2) (p : polynomial R) (hp : p.degree = 2) :
  irreducible p :=
sorry

theorem exercise_11_4_6b {p : ℕ} 
  (hp : nat.prime p) :
  irreducible (polynomial.C (p : ℤ) + 1) :=
sorry

theorem exercise_11_4_6c (x : ℤ/31) :
  irreducible (polynomial.C (x^3 - 9)) :=
sorry

theorem exercise_11_4_8 {p n : ℕ} [fact (nat.prime p)] :
  irreducible (polynomial.X ^ n - C p) :=
sorry

theorem exercise_13_4_10 {p : ℕ} (hp : nat.prime p) 
  (h : p = 2 ^ (nat.find_prime_pow p) + 1) :
  ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] [fintype K] [decidable_eq K]
  (hK : ¬ is_zero_of_nonzero_fintype K) :
  ∏ (x : K) (hx : x ≠ 0), x = -1 :=
sorry

theorem exercise_2_11_3 {G : Type*} [group G]
  (hG : ∃ n : ℕ, nat.even n ∧ card G = n) :
  ∃ (x : G), x ≠ 1 ∧ x * x = 1 :=
sorry

theorem exercise_2_2_9 {G : Type*} [group G] {a b : G} 
  (hab : a * b = b * a) :
  is_abelian_group (subgroup.generated G (a, b)) :=
sorry

theorem exercise_2_3_1 {R : Type*} [ring R] 
  [add_comm_group R] [add_group R] [mul_comm_group R] [mul_group R] 
  (hR : ∀ (x : R), x ≠ 0 → x > 0 ∨ x < 0) :
  add_group.is_add_group_hom.is_add_group_isomorphism 
  (λ (x : R), x⁻¹) (λ (x : R), x⁻¹) :=
sorry

theorem exercise_2_3_2 {G : Type*} [group G] (a b : G) :
  a * b ≫ a⁻¹ = b :=
sorry

theorem exercise_2_4_19 {G : Type*} [group G]
  (h : ∃ (x : G), x ≠ 1 ∧ x * x = 1 ∧ ∀ (y : G), y ≠ 1 → y * y ≠ 1) :
  ∃ (x : G), x ≠ 1 ∧ x * x = 1 ∧ ∀ (y : G), y * x = x * y :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H] :
  center (G * H) = center G * center H :=
sorry

theorem exercise_3_2_7 {K L : Type*} [field K] [field L]
  (f : K →+* L) : function.injective f :=
sorry

theorem exercise_3_5_6 {K V : Type*} [field K] 
  [add_comm_group V] [vector_space K V] {s : set V} (hs : s.countable) 
  (h : ∀ (v : V), v ∈ span K s → ∃ (f : ℕ → K), v = ∑ i, f i • (s.to_finset).nth_le i.1 i.2) :
  ∃ (b : set V), b.countable ∧ b.finite_basis K :=
sorry

theorem exercise_3_7_2 {V : Type*} [field F] [add_comm_group V]
  [vector_space F V] (hF : nonempty F) :
  ¬ (∃ (s : finset (submodule F V)), s.card < ∞ ∧ ∀ (U : submodule F V), U ∈ s → U ≠ ⊤ ∧
  ∀ (U : submodule F V), U ≠ ⊤ → ∃ (U' : submodule F V), U' ∈ s ∧ U ≤ U') :=
sorry

theorem exercise_6_1_14 {G : Type*} [group G] 
  (Z : subgroup G) (hZ : Z ≤ center G) (hG : is_cyclic (G / Z)) :
  abelian G :=
sorry

theorem exercise_6_4_12 {G : Type*} [group G] (hG : card G = 224) :
  ¬ simple_group G :=
sorry

theorem exercise_6_4_2 {G : Type*} [group G] [fintype G]
  {p q : ℕ} (hp : nat.prime p) (hq : nat.prime q) (h : fintype.card G = p * q) :
  ¬ simple_group G :=
sorry

theorem exercise_6_4_3 {G : Type*} [group G] 
  [fintype G] {p q : ℕ} (hp : nat.prime p) (hq : nat.prime q) 
  (h : fintype.card G = p ^ 2 * q) : ¬simple_group G :=
sorry

theorem exercise_6_8_1  {G : Type*} [group G] (a b : G) :
  subgroup.generated G (a, b) = subgroup.generated G (b * a * b⁻¹, b * a * b⁻²) :=
sorry

theorem exercise_6_8_4 (x y z : Type*) [group x] [group y] [group z]
  (h : y * x * y * z⁻¹ * z⁻¹ = 1) : free_group x y z :=
sorry

theorem exercise_6_8_6 {G : Type*} 
  [group G] [fintype G] {N : subgroup G} (hN : N.normal) 
  (hG : is_cyclic G) (hGN : is_cyclic (G / N)) :
  ∃ (x y : G), G = ⟨x, y⟩ :=
sorry