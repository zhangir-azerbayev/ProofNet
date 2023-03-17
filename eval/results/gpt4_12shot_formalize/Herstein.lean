import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 exists_inv_eq_self_of_even_card {G : Type*} [group G] [fintype G]
  (hG : 2 ∣ card G) : ∃ (a : G), a ≠ 1 ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 exists_pow_eq_one_of_finite_group {G : Type*} [group G] [fintype G]
  (a : G) : ∃ n : ℕ, 0 < n ∧ a ^ n = 1 :=
sorry

theorem exercise_2_2_3 abelian_group_of_consecutive_powers {G : Type*} [group G]
  (h : ∀ (a b : G) (i : ℤ), i ∈ [-1, 0, 1] → (a * b) ^ i = a ^ i * b ^ i) :
  comm_group G :=
sorry

theorem exercise_2_2_6c commutator_pow_n_n_minus_one_eq_one {G : Type*} [group G] (n : ℕ) (hn : n > 1)
  (h : ∀ a b : G, (a * b) ^ n = a ^ n * b ^ n) :
  ∀ a b : G, (a * b * a⁻¹ * b⁻¹) ^ (n * (n - 1)) = 1 :=
sorry

theorem exercise_2_3_16 cyclic_of_no_proper_subgroups {G : Type*} [group G] [fintype G]
  (h : ∀ (H : subgroup G), H = ⊥ ∨ H = ⊤) :
  ∃ (p : ℕ) (hp : fact (nat.prime p)), card G = p :=
sorry

theorem exercise_2_5_23 exists_j_pow_eq_mul {G : Type*} [group G] (hG : ∀ H : subgroup G, is_normal_subgroup H)
  (a b : G) : ∃ j, b * a = a ^ j * b :=
sorry

theorem exercise_2_5_31 characteristic_subgroup_of_abelian_group_of_order_pn {G : Type*} [group G]
  [fintype G] {p n m : ℕ} (hp : nat.prime p) (hG : fintype.card G = p ^ n * m)
  (hpm : p ∣ m → false) (H : subgroup G) (hH : fintype.card H = p ^ n) :
  H.is_characteristic_subgroup G :=
sorry

theorem exercise_2_5_43 group_of_order_nine_is_abelian (G : Type*) [group G] [fintype G]
  (hG : fintype.card G = 9) : is_abelian G :=
sorry

theorem exercise_2_5_52 involution_automorphism_implies_abelian {G : Type*} [fintype G] [group G]
  (ϕ : G ≃* G) (hϕ : (card {x : G | ϕ x = x⁻¹}) > (3 * card G) / 4) :
  ∀ y : G, ϕ y = y⁻¹ :=
sorry

theorem exercise_2_7_7 normal_of_map_normal {G G' : Type*} [group G] [group G']
  (ϕ : G →* G') (N : subgroup G) (hN : N.normal) :
  (ϕ.range.subgroup_of ϕ N).normal :=
sorry

theorem exercise_2_8_15 isomorphic_nonabelian_groups_of_order_pq {p q : ℕ} [fact (nat.prime p)] [fact (nat.prime q)]
  (hpq : p > q) (hq_div_p_sub1 : q ∣ p - 1) (G H : Type*) [group G] [group H]
  (hG : fintype.card G = p * q) (hH : fintype.card H = p * q)
  (hG_nonabelian : ¬is_abelian G) (hH_nonabelian : ¬is_abelian H) :
  nonempty (G ≃* H) :=
sorry

theorem exercise_2_10_1 prime_order_element_not_in_normal_subgroup_inter {G : Type*} [group G]
  {A : subgroup G} (hA : A.normal) {b : G} {p : ℕ} (hp : nat.prime p)
  (hb : order_of b = p) (hbA : b ∉ A) : A ⊓ (subgroup.generate {b}) = ⊥ :=
sorry

theorem exercise_2_11_7 sylow_fixed_by_aut {G : Type*} [group G] {p : ℕ} [fact (nat.prime p)]
  (P : sylow p G) (hP : P.1 ≤ center G) (ϕ : G ≃* G) :
  ϕ.to_monoid_hom.range.subgroup_of P.1 = P.1 :=
sorry

theorem exercise_3_2_21 disjoint_permutations_eq_id {α : Type*} [decidable_eq α] (σ τ : equiv.perm α)
  (h_disjoint : disjoint (equiv.perm.support σ) (equiv.perm.support τ))
  (h_comp : σ * τ = equiv.refl α) : σ = equiv.refl α ∧ τ = equiv.refl α :=
sorry

theorem exercise_4_1_34 matrix_group_isomorphic_to_symmetric_group {R : Type*} [comm_ring R]
  (F : Type*) [field F] [algebra R F] [char_zero F] :
  matrix.special_linear_group (fin 2) R ≃* symmetric_group (fin 3) :=
sorry

theorem exercise_4_2_6 commutes_with_a_of_a_square_zero {R : Type*} [ring R] (a x : R)
  (ha : a^2 = 0) : (a * x + x * a) * a = a * (a * x + x * a) :=
sorry

theorem exercise_4_3_1 L_is_ideal {R : Type*} [comm_ring R] (a : R) :
  is_ideal R {x : R | x * a = 0} :=
sorry

theorem exercise_4_4_9 quadratic_residues_and_nonresidues_count {p : ℕ} [fact p.prime] :
  (finset.card (finset.filter (quadratic_residue p) (finset.range (p - 1))) = (p - 1) / 2)
  ∧ (finset.card (finset.filter (λ x, ¬quadratic_residue p x) (finset.range (p - 1))) = (p - 1) / 2) :=
sorry

theorem exercise_4_5_23 data.polynomial
import field_theory.finite
import field_theory.algebraic_closure
import ring_theory.polynomial.basic

open_locale classical

lemma irreducible_p_q (F : Type*) [field F] [fintype F] (hp : fact (fintype.card F = 7)) :
  let p :=
sorry

theorem exercise_4_6_2 cubic_irreducible {R : Type*} [field R] (f : polynomial R) :
  f = polynomial.C (f.coeff 0) + polynomial.C (f.coeff 1) * polynomial.X
  + polynomial.C (f.coeff 3) * polynomial.X ^ 3 →
  ¬is_unit f →
  irreducible f :=
sorry

theorem exercise_5_1_8 frobenius_power {F : Type*} [field F] {p n : ℕ} [fact p.prime]
  [char_p F p] (a b : F) (m : ℕ) (h : m = p^n) :
  (a + b)^m = a^m + b^m :=
sorry

theorem exercise_5_3_7 algebraic_of_square_algebraic {F K : Type*} [field F] [field K]
  [algebra F K] (a : K) (h : is_algebraic F (a ^ 2)) :
  is_algebraic F a :=
sorry

theorem exercise_5_4_3 algebraic_degree_bound {a : ℂ}
  (h : a ^ 5 + real.sqrt 2 * a ^ 3 + real.sqrt 5 * a ^ 2 + real.sqrt 7 * a + real.sqrt 11 = 0) :
  algebraic a ∧ algebraic.degree ℚ a ≤ 80 :=
sorry

theorem exercise_5_6_14 distinct_roots_of_x_pow_m_sub_x {F : Type*} [field F] {p m : ℕ}
  (hp : fact (nat.prime p)) (hf : char_p F p) (n : ℕ) (hn : m = p ^ n) :
  (polynomial.x ^ m - polynomial.x).roots.to_finset.card = m :=
sorry