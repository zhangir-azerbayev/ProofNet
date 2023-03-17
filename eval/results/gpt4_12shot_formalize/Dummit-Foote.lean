import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_2a not_commutative_star_op : ¬commutative (λ a b : ℤ, a - b) :=
sorry

theorem exercise_1_1_4 mul_assoc_mod_n (n : ℕ) (a b c : ℕ) :
  (((a % n) * (b % n)) % n * (c % n)) % n = ((a % n) * ((b % n) * (c % n)) % n) % n :=
sorry

theorem exercise_1_1_15 inv_prod {G : Type*} [group G] (n : ℕ) (a : ℕ → G) :
  (finset.prod (finset.range n) a)⁻¹ = finset.prod (finset.range n) (λ i, (a i)⁻¹) :=
sorry

theorem exercise_1_1_17 inv_eq_pow_pred_of_order_eq {G : Type*} [group G] (x : G) (n : ℕ)
  (hx : order_of x = n) : x⁻¹ = x^(n - 1) :=
sorry

theorem exercise_1_1_20 order_of_inv_eq_order_of {G : Type*} [group G] (x : G) :
  order_of x⁻¹ = order_of x :=
sorry

theorem exercise_1_1_22b abs_mul_eq_abs_mul_reverse {G : Type*} [group G] [fintype G]
  (a b : G) : card (a * b • subgroup.center G) = card (b * a • subgroup.center G) :=
sorry

theorem exercise_1_1_29 prod_abelian_iff {A B : Type*} [group A] [group B] :
  is_abelian (A × B) ↔ is_abelian A ∧ is_abelian B :=
sorry

theorem exercise_1_3_8 infinite_symmetric_group (ℕ : Type*) [infinite ℕ] :
  infinite (perm ℕ) :=
sorry

theorem exercise_1_6_11 prod_comm_equiv {A B : Type*} [group A] [group B] :
  (A × B) ≃* (B × A) :=
sorry

theorem exercise_1_6_23 abelian_of_aut_order_two {G : Type*} [group G] [fintype G]
  (σ : G ≃* G) (hσ : ∀ g : G, σ g = g ↔ g = 1) (hσ2 : σ * σ = 1) :
  is_abelian G :=
sorry

theorem exercise_2_1_13 subgroup_of_rationals_with_inv_iff {H : add_subgroup ℚ} :
  (∀ x ∈ H, x ≠ 0 → x⁻¹ ∈ H) → H = ⊥ ∨ H = ⊤ :=
sorry

theorem exercise_2_4_16a exists_maximal_subgroup_of_proper_subgroup {G : Type*} [group G] [fintype G]
  (H : subgroup G) (hH : H ≠ ⊤) :
  ∃ (M : subgroup G), M.is_maximal ∧ H ≤ M :=
sorry

theorem exercise_2_4_16c maximal_subgroup_iff_prime_pow_generator {G : Type*} [group G]
  {x : G} {n : ℕ} (hn : n ≥ 1) (hx : order_of x = n) (H : subgroup G)
  (hH : H ≤ subgroup.gpowers x) :
  H.is_maximal ↔ ∃ (p : ℕ) (hp : nat.prime p), H = subgroup.gpowers (x ^ p) ∧ p ∣ n :=
sorry

theorem exercise_3_1_22a normal_intersection_of_normal_subgroups {G : Type*} [group G]
  (H K : subgroup G) (hH : H.normal) (hK : K.normal) :
  (H ⊓ K).normal :=
sorry

theorem exercise_3_2_8 coprime_orders_subgroups_intersect_trivial {G : Type*} [group G]
  {H K : subgroup G} (hH : fintype.card H) (hK : fintype.card K)
  (h_coprime : nat.coprime (fintype.card H) (fintype.card K)) :
  H ⊓ K = ⊥ :=
sorry

theorem exercise_3_2_16 fermat_little_theorem (p : ℕ) [fact p.prime] (a : ℕ) :
  a ^ p ≡ a [MOD p] :=
sorry

theorem exercise_3_3_3 prime_index_normal_subgroup_or_join {G H K : Type*} [group G] [group H] [group K]
  (hG : fintype G) (hH : fintype H) (hK : fintype K)
  (hH_normal : normal_subgroup H G) (hp : fact (nat.prime (card (G ⧸ H))))
  (hK_leq_G : K ≤ G) :
  K ≤ H ∨ (G = H * K ∧ card (K ⧸ (K ⊓ H)) = hp.1) :=
sorry

theorem exercise_3_4_4 exists_subgroup_of_order_n_of_finite_abelian_group
  {G : Type*} [group G] [fintype G] (hG : is_abelian G) (n : ℕ)
  (hn : n ∣ fintype.card G) : ∃ (H : subgroup G), fintype.card H = n :=
sorry

theorem exercise_3_4_5b solvable_of_quotient {G : Type*} [group G] (H : subgroup G)
  (hG : is_solvable G) : is_solvable (G ⧸ H) :=
sorry

theorem exercise_4_2_8 exists_normal_subgroup_of_finite_index {G : Type*} [group G]
  {H : subgroup G} (hH : H.index < cardinal.omega) :
  ∃ (K : subgroup G), K ≤ H ∧ K.normal ∧ cardinal.mk (quotient_group.quotient K) ≤ hH.fact :=
sorry

theorem exercise_4_2_9a normal_of_prime_pow_order_of_index {G : Type*} [group G] [fintype G]
  {p α : ℕ} (hp : fact (nat.prime p)) (hG : card G = p ^ α)
  (H : subgroup G) (hH : card (G ⧸ H) = p) :
  H.normal :=
sorry

theorem exercise_4_4_2 abelian_group_of_order_pq_is_cyclic {G : Type*} [group G] [fintype G]
  (hpq : fact (fintype.card G = p * q)) (hab : comm_group G) :
  is_cyclic G :=
sorry

theorem exercise_4_4_6b exists_normal_not_characteristic {G : Type*} [group G] [fintype G] :
  ∃ (N : subgroup G), N.normal ∧ ¬N.characteristic :=
sorry

theorem exercise_4_4_8a char_of_normal_is_normal {G H K : Type*} [group G] [group H] [group K]
  (hHK : H ≤ K) (hKH : is_char_subgroup H K) (hKG : is_normal_subgroup K G) :
  is_normal_subgroup H G :=
sorry

theorem exercise_4_5_13 exists_normal_sylow_subgroup_of_order_56 (G : Type*) [group G] [fintype G]
  (hG : fintype.card G = 56) : ∃ (p : ℕ) (H : sylow p G), nat.prime p ∧ H.normal :=
sorry

theorem exercise_4_5_15 normal_sylow_subgroup_of_order_351 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 351) : ∃ p [fact (nat.prime p)], ∃ (P : sylow p G), is_normal P :=
sorry

theorem exercise_4_5_17 normal_sylow_5_and_7_of_card_105 {G : Type*} [group G] [fintype G]
  (hG : card G = 105) :
  (∃ (P5 : sylow 5 G), is_normal P5) ∧ (∃ (P7 : sylow 7 G), is_normal P7) :=
sorry

theorem exercise_4_5_19 not_simple_of_card_6545 (G : Type*) [group G] [fintype G] (hG : card G = 6545) :
  ¬ is_simple_group G :=
sorry

theorem exercise_4_5_21 not_simple_of_order_2907 (G : Type*) [group G] [fintype G] (hG : fintype.card G = 2907) :
  ¬ is_simple_group G :=
sorry

theorem exercise_4_5_23 not_simple_of_order_462 (G : Type*) [group G] [fintype G] (hG : card G = 462) :
  ¬ is_simple_group G :=
sorry

theorem exercise_4_5_33 unique_sylow_p_subgroup_of_intersection {G : Type*} [group G] [fintype G]
  {p : ℕ} [fact (nat.prime p)] {P : sylow p G} (hP : is_normal P)
  {H : subgroup G} :
  ∃! (Q : sylow p H), P.to_subgroup ⊓ H = Q.to_subgroup :=
sorry

theorem exercise_7_1_2 neg_unit_of_unit {R : Type*} [ring R] (u : R) (hu : is_unit u) :
  is_unit (-u) :=
sorry

theorem exercise_7_1_12 subring_of_field_is_integral_domain {K : Type*} [field K]
  (R : Type*) [ring R] [is_subring R K] : integral_domain R :=
sorry

theorem exercise_7_2_2 zero_divisor_iff_exists_nonzero_mul_zero {R : Type*} [comm_ring R]
  (p : polynomial R) :
  is_zero_divisor p ↔ ∃ (b : R), b ≠ 0 ∧ b • p = 0 :=
sorry

theorem exercise_7_3_16 center_image_subset_center {R S : Type*} [comm_ring R] [comm_ring S]
  (ϕ : R →+* S) (hϕ : function.surjective ϕ) :
  ϕ '' (center R) ⊆ center S :=
sorry

theorem exercise_7_4_27 unit_of_nilpotent {R : Type*} [comm_ring R] (a : R)
  (ha : is_nilpotent a) (b : R) :
  is_unit (1 - a * b) :=
sorry

theorem exercise_8_2_4 is_principal_ideal_domain_of_gcd_and_chain_condition
  (R : Type*) [integral_domain R] [nontrivial R]
  (h_gcd : ∀ (a b : R), a ≠ 0 → b ≠ 0 →
    ∃ (d : R) (r s : R), d = gcd a b ∧ d = a * r + b * s)
  (h_chain : ∀ (a : ℕ → R), (∀ i, a (i + 1) ∣ a i) →
    ∃ (N : ℕ), ∀ (n : ℕ), n ≥ N → ∃ (u : R), is_unit u ∧ a n = u * a N) :
  is_principal_ideal_domain R :=
sorry

theorem exercise_8_3_5a ring_theory.algebraic
import ring_theory.int.basic
import data.int.sqrt

theorem irreducible_in_Z_sqrt_n (n : ℕ) (hn : squarefree n) (hn_gt_3 : n > 3) :
  irreducible (2 : ℤ[sqrt (-n)]) ∧
  irreducible (sqrt (-n) : ℤ[sqrt (-n)]) ∧
  irreducible (1 + sqrt (-n) : ℤ[sqrt (-n)]) :=
sorry

theorem exercise_8_3_6b quotient_ring_zi_by_prime_mod_four_eq_field_with_q_squared_elements
  (q : ℕ) [fact (nat.prime q)] (hq : q % 4 = 3) :
  fintype.card (units (zmod q)[X] / (X ^ 2 + 1)) = q ^ 2 :=
sorry

theorem exercise_9_1_10 infinite_minimal_prime_ideals_in_quotient_ring
  (R : Type*) [comm_ring R] (x : ℕ → R) :
  infinite {P : ideal (R ⧸ ideal.span (set.range (λ n, x (2 * n) * x (2 * n + 1)))) | P.is_prime ∧ P.is_minimal} :=
sorry

theorem exercise_9_4_2a irreducible_x_pow_four_minus_four_x_pow_three_plus_six :
  irreducible (polynomial.X ^ 4 - 4 * polynomial.X ^ 3 + 6 : polynomial ℤ) :=
sorry

theorem exercise_9_4_2c data.polynomial

theorem irreducible_x_pow_four {R : Type*} [comm_ring R] :
  irreducible (polynomial.X ^ 4 + 4 * polynomial.X ^ 3 + 6 * polynomial.X ^ 2 + 2 * polynomial.X + 1 : polynomial R) :=
sorry

theorem exercise_9_4_9 irreducible_x_pow_2_minus_sqrt_2 {R : Type*} [integral_domain R]
  [algebra ℚ[R] ℝ] (hR : ∀ (x : ℝ), algebra_map ℚ[R] ℝ (real.sqrt 2 * x) = x * real.sqrt 2)
  (hR' : ∀ (x : ℝ), algebra_map ℚ[R] ℝ (x * real.sqrt 2) = x * real.sqrt 2) :
  irreducible (X ^ 2 - C (real.sqrt 2)) :=
sorry

theorem exercise_11_1_13 real_vector_space_iso_to_real_power (n : ℕ) :
  nonempty (module_equiv ℚ (euclidean_space ℝ (fin n)) ℝ) :=
sorry