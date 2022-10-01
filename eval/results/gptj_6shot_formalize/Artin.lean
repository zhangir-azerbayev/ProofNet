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


theorem exercise_10_1_13 {R : Type*} [ring R] [ring_unit R]
  (h : nilpotent.element R) :
  (1 + h) · (1 + h) = 1 + h :=
sorry

theorem exercise_10_2_4 {R : Type*} [ring R]
  (h : R.is_prime (2)) (h : R.is_prime (x)) : R.is_prime (2 x) :=
sorry

theorem exercise_10_4_6 {R : Type*} [ring R] 
  [ideal I J : ideal R] (h : I ∩ J = 0) :
  residue (I ∩ J) = 0 :=
sorry

theorem exercise_10_4_7a {I J : ideal R} : I J = I ∩ J :=
sorry

theorem exercise_10_5_16 {F : Type*} [field F] :
  char2 F →* F.is_isomorphic_to_ring_of_square_root_of_minus_one
    ↔ F.is_isomorphic_to_ring_of_square_root_of_minus_one :=
sorry

theorem exercise_10_6_7 {R : Type*} [ring R] [nonzero_ideal R]
  (h : nonzero_ideal R) : ∃ (x : R), x ≠ 0 :=
sorry

theorem exercise_10_7_10 {R : Type*} [ring R] [ideal M]
  (h : M.is_ideal) (h : M.is_maximal) :
  M.is_maximal :=
sorry

theorem exercise_10_7_6 {F : Type*} [field F] :
  field F :=
sorry

theorem exercise_11_12_3 {p : ℕ} [group p]
  (h : p ≡ -5 [MOD p]) :
  ellipse_with_integer_point_on_one_ellipse p :=
sorry

theorem exercise_11_13_3 {p : ℕ} [prime p] :
  ∃ (q : ℕ), p ≡ -1 [MOD 4] ∧ q ≠ p :=
sorry

theorem exercise_11_2_13 (a b : ℤ) : a divides b → a divides b in ℤ :=
sorry

theorem exercise_11_3_1 {F : Type*} [field F] 
  [add_comm_group F] [module F F] {a b : F} (h : a ≠ 0) :
  irreducible (polynomial a b) ↔ irreducible (polynomial a b ⧸ a) :=
sorry

theorem exercise_11_3_4 (P Q : ℕ[x]) :
  prime_poly P = prime_poly Q ↔ (P.ideal ⊆ Q.ideal) :=
sorry

theorem exercise_11_4_1b {Q : Type*} [field Q] :
  irreducible (x^3 + 6x + 12) :=
sorry

theorem exercise_11_4_6a {F : Type*} [field F] : irreducible (F.add F.one) :=
sorry

theorem exercise_11_4_6b {F : Type*} [field F] :
  irreducible (F.add F.one) :=
sorry

theorem exercise_11_4_6c (f : ℤ → ℤ) (hf : irreducible f) :
  irreducible (f.quot (f.degree f)) :=
sorry

theorem exercise_11_4_8 (p : ℕ) (n : ℕ) :
  irreducible (x^n - p) :=
sorry

theorem exercise_13_4_10prime_form_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_2_power_:=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] :
  product (nonzero_elements K) = -1 :=
sorry

theorem exercise_2_11_3 {G : Type*} [group G] :
  order G = 2 → ∃ x : G, x ^ 2 = 1 :=
sorry

theorem exercise_2_2_9 {G : Type*} [group G] 
  [abelian_group G] (a b : G) (h : a b = b a) : abelian_group G :=
sorry

theorem exercise_2_3_1 {R : Type*} [group R] :
  is_additive_group R.additive_group :=
sorry

theorem exercise_2_3_2 {G : Type*} [group G] 
  (a : G) (b : G) (h : a = b.conjugate) : a b = b a :=
sorry

theorem exercise_2_4_19 {G : Type*} [group G] (h : ∃ x, x ^ 2 = 1) :
  ∃ (z : G), z ∈ center G :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H]
  [center G] [center H] (f : G × H → G) (hf : f.ker ≤ center G) :
  center (G × H) :=
sorry

theorem exercise_3_2_7 {K : Type*} [field K] [module K K]
  (h : hom K) : injective h :=
sorry

theorem exercise_3_5_6 {V : Type*} 
  [vector_space V] [add_comm_group V] [module V] [finite_dimensional V] 
  (h : finite_dimensional.finrank V + 1 < t.card) :
  ∃ (f : V → K), t.sum (λ (e : V), f e • e) = 0 ∧ t.sum (λ (e : V), f e) = 0 
  ∧ ∃ (x : V) (H : x ∈ t), f x ≠ 0 :=
sorry

theorem exercise_3_7_2  {V : Type*} [vector_space V] [field F]
  (h : finite_dimensional.dim V <> 0) :
  ∃ (W : finset V), ∀ (W1 : finset V), W ⊂ W1 → W = W1 :=
sorry

theorem exercise_6_1_14 {G : Type*} [group G] [group Z]
  (h : cyclic G) : abelian G :=
sorry

theorem exercise_6_4_12 {G : Type*} [group G] :
  no_simple_group G.order 224 :=
sorry

theorem exercise_6_4_2 {G : Type*} [group G] [group G]
  [p : nat.prime] [q : nat.prime] (h : p * q = 1) :
  no_simple_group G :=
sorry

theorem exercise_6_4_3 {G : Type*} [group G] [group G]
  (h : G.order = p ^ 2 q) :
  G.is_simple :=
sorry

theorem exercise_6_8_1 {G : Type*} [group G] 
  (h : ∀ (x : G), x ∈ G.order) (h2 : ∀ (x : G), x ∈ G.order) :
  subgroup.generated_by (λ x, x) (λ x, x) (λ x, x) :=
sorry

theorem exercise_6_8_4  {G : Type*} [group G] [group G] [group G]
  (hx : x ∈ G) (hy : y ∈ G) (hz : z ∈ G) (hxy : y x = 1) (hzy : z y = 1)
  (h : free_group G) : free_group G :=
sorry

theorem exercise_6_8_6 {G : Type*} [group G] 
  [cyclic G] [cyclic G / N] [cyclic G / N] [cyclic G] [cyclic G / N]
  (h : cyclic G) (hN : cyclic G / N) (hG : cyclic G) :
  cyclic G :=
sorry