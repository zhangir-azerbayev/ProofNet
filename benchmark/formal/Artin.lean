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

theorem exercise_2_2_9 {G : Type*} [group G] {a b : G}
  (h : a * b = b * a) :
  ∀ x y : closure {x | x = a ∨ x = b}, x*y = y*x :=
sorry

-- this isn't quite correct, mult group of positive reals?
theorem exercise_2_3_1 : multiplicative ℝ ≃ additive ℝ  :=
sorry

theorem exercise_2_3_2 {G : Type*} [group G] (a b : G) :
  ∃ g : G, b* a = g * a * b * g⁻¹ :=
sorry

theorem exercise_2_4_19 {G : Type*} [group G] {x : G}
  (hx : order_of x = 2) (hx1 : ∀ y, order_of y = 2 → y = x) :
  x ∈ center G :=
sorry

-- is this ok? Are they actually equal in Lean?
theorem exercise_2_8_6 {G H : Type*} [group G] [group H] :
  center (G × H) ≃* (center G) × (center H) :=
sorry

-- how to talk about ℝ+?
theorem exercise_2_10_11
  {G : Type*} [add_group G] {H : Type*} [add_group H] {f : G → H}
  (hf : is_add_group_hom f) {g : H → G} (hg : is_add_group_hom g) :
  is_add_group_hom (g ∘ f) :=
sorry

theorem exercise_2_11_3 {G : Type*} [group G] [fintype G]
  (hG : even (card G)) : ∃ x : G, order_of x = 2 :=
sorry

theorem exercise_3_2_7 {F : Type*} [field F] {G : Type*} [field G]
  (φ : F →+* G) : injective φ :=
sorry

theorem exercise_3_5_6 {K V : Type*} [field K] [add_comm_group V]
  [module K V] {S : set V} (hS : set.countable S)
  (hS1 : span K S = ⊤) {ι : Type*} (R : ι → V)
  (hR : linear_independent K R) : countable ι :=
sorry

theorem exercise_3_7_2 {K V : Type*} [field K] [add_comm_group V]
  [module K V] {ι : Type*} [fintype ι] (γ : ι → submodule K V) :
  (⋂ (i : ι), (γ i : set V)) ≠ ⊤ :=
sorry

theorem exercise_6_1_14 (G : Type*) [group G]
  (hG : is_cyclic $ G ⧸ (center G)) :
  center G = ⊤  :=
sorry

theorem exercise_6_4_2 {G : Type*} [group G] [fintype G] {p q : ℕ}
  (hp : prime p) (hq : prime q) (hG : card G = p*q) :
  is_simple_group G → false :=
sorry

theorem exercise_6_4_3 {G : Type*} [group G] [fintype G] {p q : ℕ}
  (hp : prime p) (hq : prime q) (hG : card G = p^2 *q) :
  is_simple_group G → false :=
sorry

theorem exercise_6_4_12 {G : Type*} [group G] [fintype G]
  (hG : card G = 224) :
  is_simple_group G → false :=
sorry

theorem exercise_6_8_1 {G : Type*} [group G]
  (a b : G) : closure ({a, b} : set G) = closure {b*a*b^2, b*a*b^3} :=
sorry

-- How to talk about free groups in the right way?
theorem exercise_6_8_4 {α : Type*} [group α] [free_group α] (x y z : α):
  closure ({x,y,z} : set α) :=
sorry

theorem exercise_6_8_6 {G : Type*} [group G] (N : subgroup G)
  [N.normal] (hG : is_cyclic G) (hGN : is_cyclic (G ⧸ N)) :
  ∃ (g h : G), closure ({g,h} : set G) = ⊤ :=
sorry

theorem exercise_10_1_13 {R : Type*} [ring R] {x : R}
  (hx : is_nilpotent x) : is_unit (1 + x) :=
sorry

theorem exercise_10_2_4 :
  span ({2} : set $ polynomial ℤ) ⊓ (span {X}) =
  span ({2 * X} : set $ polynomial ℤ) :=
sorry

theorem exercise_10_6_7 {I : ideal gaussian_int}
  (hI : I ≠ ⊥) : ∃ (z : I), z ≠ 0 ∧ (z : gaussian_int).im = 0 :=
sorry

-- this is a thorny one
theorem exercise_10_6_16 {R : Type*} [ring R] (x : polynomial R)
  (a : R) :
  ∃ n : ℕ, x = ∑ (i : fin n), :=
sorry

-- problem of passing typeclass
theorem exercise_10_3_24a :
  ∃ (R : Type*) [ring R] (I J: ideal R), :=
sorry

theorem exercise_10_4_6 {R : Type*} [comm_ring R]
  [no_zero_divisors R] (I J : ideal R) :
  ∀ x : I ⊓ J, is_nilpotent ((ideal.quotient.mk (I*J)) x) :=
sorry

theorem exercise_10_4_7a {R : Type*} [comm_ring R] [no_zero_divisors R]
  (I J : ideal R) (hIJ : I + J = ⊤) : I * J = I ⊓ J :=
sorry

theorem exercise_10_5_16 {F : Type*} [fintype F] [field F] :
  is_empty ((polynomial F) ⧸ ideal.span ({X^2} : set (polynomial F)) ≃
  (polynomial F) ⧸ ideal.span ({X^2 - 1} : set (polynomial F))) ↔
  ring_char F ≠ 2 :=
sorry

theorem exercise_10_7_6 {F : Type*} [fintype F] [field F]
  (hF : card F = 5) :
  field $ (polynomial F) ⧸ ideal.span ({X^2 + X + 1} : set (polynomial F)) :=
sorry

theorem exercise_10_7_10 {R : Type*} [ring R]
  (M : ideal R) (hM : ∀ (x : R), x ∉ M → is_unit x) :
  is_maximal M ∧ ∀ (N : ideal R), is_maximal N → N = M :=
sorry

theorem exercise_11_2_13 (a b : ℤ) :
  (of_int a : gaussian_int) ∣ of_int b → a ∣ b :=
sorry

theorem exercise_11_3_1 {F : Type*} [field F] (a b : F) (ha : a ≠ 0) (p : polynomial F) :
  irreducible p ↔ irreducible (∑ n in p.support, p.coeff n • (a • X + b • 1)^n : polynomial F) :=
sorry

theorem exercise_11_3_2 {F : Type*}
  [field F] (f g : polynomial F) (h : ∃ (h : polynomial F),
  is_factor_of h f ∧ is_factor_of h g) :
  ∃ (h : polynomial F), is_factor_of h f ∧ is_factor_of h g :=
sorry

theorem exercise_11_3_4 : irreducible (X^3 + 6*X + 12 : polynomial ℚ) :=
sorry

theorem exercise_11_4_1b {F : Type*} [field F] [fintype F] (hF : card F = 2) :
  irreducible (12 + 6 * X + X ^ 3 : polynomial F) :=
sorry

theorem exercise_11_4_6a {F : Type*} [field F] [fintype F] (hF : card F = 7) :
  irreducible (X ^ 2 + 1 : polynomial F) :=
sorry

theorem exercise_11_4_6b {F : Type*} [field F] [fintype F] (hF : card F = 31) :
  irreducible (X ^ 3 - 9 : polynomial F) :=
sorry

theorem exercise_11_4_6c : irreducible (X^3 - 9 : polynomial ℤ_31) :=
sorry

theorem exercise_11_4_8 {p : ℕ} (hp : prime p) (n : ℕ) :
  irreducible (X ^ n - p : polynomial ℚ) :=
sorry

-- this is thorny
theorem exercise_11_4_10 irreducible_of_degree_two_n_plus_one_of_nonzero_leading_coeff_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant :=
sorry

theorem exercise_11_9_4 {R : Type*}
  [comm_ring R] (p : R) (h : nat.prime (p.nat_abs)) (hp : p.nat_abs.prime_splits)
  (α : R) (hα : p ∣ α → p ∣ 1) :
  ideal.generated_by (p, α) = ideal.mul (ideal.generated_by p) (ideal.generated_by α) :=
sorry

theorem exercise_11_12_3 (p : ℕ) (hp : nat.prime p) {a : zmod p}
  (ha : a^2 = -5) :
  ∃ (x y : ℤ), x ^ 2 + 5 * y ^ 2 = p ∨ 2 * x ^ 2 + 2 * x * y + 3 * y ^ 2 = p :=
sorry

theorem exercise_11_13_3 (N : ℕ):
  ∃ p ≥ N, nat.prime p ∧ p + 1 ≡ 0 [MOD 4] :=
sorry

-- Why is this erroring?
theorem exercise_13_1_3 {R : Type*}
  --[comm_ring R] [is_domain R]
  [division_ring R]
  {F : subring R} [field F] [module R F] [finite_dimensional R F] :
  field R :=
sorry

-- Not really sure how to do this
theorem exercise_13_3_1 {F : Type*} [field F] (f : polynomial F)
  (hf : f.degree = 5)   :=
sorry

-- also not sure how to do this
theorem exercise_13_3_8 {F : Type*} [field F] {K : Type*}
  [field K] (hK : algebra.is_field_extension F K) (α : K) (β : K)
  (hα : algebra.is_integral F α) (hβ : algebra.is_integral F β)
  (hαβ : nat.coprime (algebra.degree F α) (algebra.degree F β)) :
  algebra.degree F K = algebra.degree F α * algebra.degree F β :=
sorry

theorem exercise_13_4_10
  {p : ℕ} {hp : nat.prime p} (h : ∃ r : ℕ, p = 2 ^ r + 1) :
  ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] [fintype Kˣ] :
  ∏ (x : Kˣ), x = -1 :=
sorry
