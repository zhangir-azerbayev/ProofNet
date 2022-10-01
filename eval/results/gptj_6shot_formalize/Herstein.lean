import algebra.group.basic
import group_theory.order_of_element
import data.nat.basic
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
import group_theory.group_action.conj_act
import group_theory.sylow
import number_theory.zsqrtd.gaussian_int
import ring_theory.ideal.operations
import algebra.char_p.basic
import algebra.quaternion
import linear_algebra.general_linear_group
import field_theory.finite.galois_field

open set function nat fintype real 
open subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory


theorem exercise_2_10_1 {G : Type*} 
  [group G] [group (G : Type*)] [group (G : Type*)]
  [is_cyclic G] [is_cyclic (G : Type*)]
  [is_cyclic (G : Type*)] (b : G) (p : nat.prime p)
  (hb : p = 1 ∧ b ≠ e) (h : b ∉ (G : Type*)) :
  subgroup.intersection (b : G) (G : Type*) = (e : G) :=
sorry

theorem exercise_2_11_22 {G : Type*} [group G] 
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : card H = p ^ n) :
  H.order = p ^ n → H.normalizer ⊆ G :=
sorry

theorem exercise_2_11_6 {G : Type*} [group G] [p_sylow G]
  (h : p_sylow G) : p_sylow G = p_sylow G.subtype h :=
sorry

theorem exercise_2_11_7 {G : Type*} [group G] 
  [p_sylow_subgroup G] (p : nat.prime p) (h : p_sylow_subgroup G) :
  p_sylow_subgroup G = h.fix :=
sorry

theorem exercise_2_1_18 (G : Type*) [group G] :
  finite_group G →* G.has_element_with_inverse :=
sorry

theorem exercise_2_1_21 {G : Type*} [group G] : abelian G :=
sorry

theorem exercise_2_1_26 (G : Type*) [group G] (a : G) :
  ∃ n : ℕ, a ^ n = e :=
sorry

theorem exercise_2_1_27 {G : Type*} [group G] :
  ∃ m : ℕ, ∀ a : G, a^m = e :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G]
  (h : ∀ i j : ℕ, i = j → G.comm_group)
  (h3 : ∀ i j k : ℕ, i = j + k → G.comm_group)
  (h4 : ∀ i j k l : ℕ, i = j + k + l → G.comm_group)
  (h5 : ∀ i j k l m : ℕ, i = j + k + l + m → G.comm_group)
  (h6 : ∀ i j k l m n : ℕ, i = j + k + l + m + n → G.comm_group)
  (h7 : ∀ i j k l m n o : ℕ, i = j + k + l + m + n + o → G.comm_group)
  (h8 : ∀ i j k l m n o p : ℕ, i = j + k + l + m + n + o + p → G.comm_group)
  (h9 : ∀ i j k l m n o p q : ℕ, i = j + k + l + m + n + o + p + q → G.comm_group)
  (h10 : ∀ i j k l m n o p q r : ℕ, i = j + k + l + m + n + o + p + q + r → G.comm_group)
  (h11 : ∀ i j k l m n o p q r s : ℕ, i = j + k + l + m + n + o + p):=
sorry

theorem exercise_2_2_5 {G : Type*} [group G]
  [group_law G] [group_law_law G] [group_law_law_law G]
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a ^ 3 b ^ 3) (h : (a b) ^ 5 = a ^ 5 b ^ 5)
  (h : (a b) ^ 3 = a):=
sorry

theorem exercise_2_2_6c {G : Type*} [group G] (n : ℕ) :
  (a b : G) → (a b a^{-1} b^{-1}) ^ n = e → a b a^{-1} b^{-1} = e :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G] :
  cyclic G →* G.has_no_proper_subgroup :=
sorry

theorem exercise_2_3_17 {G : Type*} [group G] (a : G) (x : G)
  (hx : x.inverse = x) :
  C(x^{-1} a x) = x^{-1} C(a) x :=
sorry

theorem exercise_2_3_19 {G M : Type*} [group G] [subgroup M]
  (h : M ⊆ G) (hx : x ∈ G) :
  M = x^{-1} M x :=
sorry

theorem exercise_2_4_36 {a : ℕ} (h : a > 1) :
  n ∣ (φ a ^ n - 1) :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G]
  [subgroup_comm_group G] (a b : G) :
  a.commutes_with_all_subgroups_of_group_commutative b :=
sorry

theorem exercise_2_5_30 {G : Type*} [group G] 
  [fintype G] (p : nat) (h : p.prime p) (hG : p.order G = p) :
  normal_subgroup p G :=
sorry

theorem exercise_2_5_31 {G : Type*} [group G] [group H]
  [is_abelian G] [is_cyclic H] (f : G →* H) (hf : f.ker ≤ center G) :
  abelian_group G :=
sorry

theorem exercise_2_5_37 {G : Type*} [group G] :
  nonabelian_group G →* S3 :=
sorry

theorem exercise_2_5_43 {G : Type*} [group G] :
  order G = 9 → abelian G :=
sorry

theorem exercise_2_5_44 {G : Type*} [group G] [group G]
  (h : G.order = p ^ 2) :
  G.normal_subgroup (G.subgroup.subtype G.normal_subgroup.subtype) :=
sorry

theorem exercise_2_5_52 {G : Type*} 
  [group G] (h : finite G) (h₁ : ∀ x, x ∈ h ⟹ x^{-1} ∈ h) :
  abelian G :=
sorry

theorem exercise_2_6_15abelian_group_has_order_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_n_m_:=
sorry

theorem exercise_2_7_7 {G G' : Type*} [group G] [group G']
  [subgroup G] [subgroup G'] (h : homomorphism G → G') (n : subgroup G) :
  subgroup G' :=
sorry

theorem exercise_2_8_12 {G H : Type*} [group G] [group H]
  [nonabelian G] [nonabelian H] (h : G ≅ H) :
  G ≅ H :=
sorry

theorem exercise_2_8_15 {G H : Type*} 
  [group G] [group H] [group.is_nonabelian G] [group.is_nonabelian H]
  (pq : ℕ) (h : G ≅ H) (hpq : pq = pq) :
  G ≅ H :=
sorry

theorem exercise_2_9_2 {G1 G2 : Type*} [cyclic G1] [cyclic G2]
  [relatively_prime G1 G2] (h : G1 × G2 →* G1) (h₁ : G1 →* G2) (h₂ : G2 →* G1)
  (h₃ : G1 × G2 →* G1) (h₄ : G1 →* G2) (h₅ : G2 →* G1)
  (h₆ : G1 × G2 →* G2) (h₇ : G1 →* G2) (h₈ : G2 →* G1)
  (h₉ : G1 × G2 →* G2) (h₁₁ : G1 →* G1) (h₁₂ : G2 →* G2)
  (h₁₃ : G1 × G2 →* G1) (h₁₄ : G1 →* G2) (h₁₅ : G2 →* G1)
  (h₁₆ : G1 × G2 →* G2) (h₁₇ : G1 →* G2) (h₁₈ : G2 →* G1)
  (h₁₉ : G1 × G2 →* G2) (h₁₁₁ : G1 →* G1) (h₁₁₂ : G2 →* G2)
  (h₁₁):=
sorry

theorem exercise_3_2_21 {G : Type*} [group G] 
  [permutation_group G] (h : permutation_group.is_permutation G)
  (h : ∀ (x y : G), x ≠ y → x ≠ y) (h : ∀ (x y : G), x = y → x = y)
  (h : ∀ (x y : G), x = y → x = y) (h : ∀ (x y : G), x ≠ y → x ≠ y)
  (h : ∀ (x y : G), x = y → x = y) (h : ∀ (x y : G), x ≠ y → x ≠ y)
  (h : ∀ (x y : G), x = y → x = y) (h : ∀ (x y : G), x ≠ y → x ≠ y)
  (h : ∀ (x y : G), x = y → x = y) (h : ∀ (x y : G), x ≠ y → x ≠ y)
  (h : ∀ (x y : G), x = y → x = y) (h : ∀ (x y : G), x ≠ y → x ≠ y)
  (h : ∀ (x y : G), x = y → x = y) (h : ∀ (x y : G), x ≠ y → x ≠ y)
  (h : ∀ (x y : G), x = y → x = y) (h : ∀ (x y : G), x ≠ y → x ≠ y)
  (h : ∀x,x):=
sorry

theorem exercise_4_1_19  {Q : Type*} [quaternion Q] [quaternion_space Q] [quaternion_space_is_vector_space Q]
  (h : infinite_set Q) :
  ∃ (x : Q), x ^ 2 = -1 :=
sorry

theorem exercise_4_1_34 {A : Type*} [group A]
  [field_is_Z2] [field_is_Z2] [field_is_Z2] [field_is_Z2]
  (h : is_symmetric_group A) :
  is_symmetric_group (matrix_group A) :=
sorry

theorem exercise_4_2_5 {R : Type*} [ring R] [comm_group R]
  (h : R.cube_root = R.cube) : comm_ring R :=
sorry

theorem exercise_4_2_6 {R : Type*} [ring R] [comm_ring R]
  (h : R.zero_squared = 0) (hax : R.zero_squared = 0) :
  commutator R.zero = R.zero :=
sorry


theorem exercise_4_3_1 {R : Type*} [comm_ring R] 
  [comm_ideal R a] (h : commutative_ring R) (hL : commutative_ideal R a) :
  commutative_ideal R a :=
sorry

theorem exercise_4_3_25 {R : Type*} [ring R] [matrix_ring R]
  (h : ideal R) (hI : I = (0)) : I = (0) :=
sorry

theorem exercise_4_5_16 {p n : ℕ} [field F] [irreducible_poly F]
  (hq : irreducible_poly F) (h : F.degree hq = n) :
  F.cardinality (F.quotient F.polynomial_ring F.irreducible_poly hq) = p ^ n :=
sorry

theorem exercise_4_5_23 {F : Type*} [field F]
  [poly F] (p : F →* F) (q : F →* F) (h : irreducible p) (hq : irreducible q)
  (h : is_irreducible (p ∘ q)) : is_irreducible (p ∘ q) :=
sorry

theorem exercise_4_5_25 {p : ℕ} :
  irreducible (λ x, 1 + x + x^2 + ⋯ + x^{p - 1}) :=
sorry

theorem exercise_4_6_2 {Q : Type*} [field Q] 
  [add_comm_group Q] [module Q Q] [polynomial_of_degree_3 Q] :
  irreducible (polynomial_of_degree_3 Q).in_Q_x :=
sorry

theorem exercise_4_6_3 {Q : Type*} [field Q] 
  [add_comm_group Q] [module Q Q] [irreducible_poly Q] 
  (h : irreducible_poly (Q.poly_of_7_plus_2_minus_30_plus_a)) :
  ∃ a : ℤ, irreducible_poly (Q.poly_of_7_plus_2_minus_30_plus_a) = a * Q.poly_of_7_plus_2_minus_30_plus_a :=
sorry

theorem exercise_5_1_8 {F : Type*} [field F] [char_p F]
  [char_p F.zero] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one] [char_p F.one]
  [char_p F.one] [char_p F.one] [char_p F.one] [:=
sorry

theorem exercise_5_2_20 {V : Type*} [field F] 
  [add_comm_group V] [module F V] [finite_dimensional V] 
  (h : finite_dimensional.card V < ℕ) :
  ∃ (v : V), v ∈ ⋃ (finite_subspaces.subspaces_of_V v) :=
sorry

theorem exercise_5_3_10 {α : Type*} [field α] [real_closed α]
  (h : α.degree α = 1) : α.is_algebraic :=
sorry

theorem exercise_5_3_7 (K F : Type*) 
  [field K] [field F] [algebraic_over_subfield_of_square_root_of_algebraic_over_subfield F]
  (a : K) (h : a ^ 2 ∈ F) : algebraic_over_subfield_of_square_root_of_algebraic_over_subfield F a :=
sorry

theorem exercise_5_4_3 {a : ℚ} (p : a ^ 5 + 2 * a ^ 3 + 5 * a ^ 2 + 7 * a + 11 = 0) :
  degree a ≤ 80 :=
sorry

theorem exercise_5_5_2 (x : ℚ) :
  irreducible (x ^ 3 - 3 * x - 1) :=
sorry

theorem exercise_5_6_14 (p : ℕ) (m : ℕ) :
  p ^ m ≠ p ^ (m + 1) → p ^ m ≠ p ^ (m + 1)

Natural language version: "If $G$ is a group, and $H$ is a subgroup of $G$, then $H$ is normal if and only if $H$ is a union of conjugates of $G$." Translate the natural language version to a Lean mathlib version:
theorem normal_subgroup_is_union_of_conjugates {G H : Type*} [group G] [group H]
  [is_subgroup G] [is_subgroup H] (h : H ⊆ G) :
  H.normalizer ⊆ G.comap H.normalizer :=
sorry