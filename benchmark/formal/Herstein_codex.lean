theorem exercise_2.1.18
 exists_inv_eq_self_of_even_order {G : Type*} [group G] 
    (hG : fintype G) (hG2 : 2 ∣ fintype.card G) :
    ∃ (a : G), a ≠ 1 ∧ a = a⁻¹ 

theorem exercise_2.1.21
 order_5_is_abelian (G : Type*) [group G] (hG : fintype.card G = 5) :
    abelian_group G 

theorem exercise_2.1.26
 exists_pow_eq_one_of_finite {G : Type*} [group G] (a : G) 
    (hG : fintype G) : ∃ (n : ℕ), a ^ n = 1 

theorem exercise_2.1.27
 exists_m_pow_eq_one_of_finite {G : Type*} [group G] 
    (hG : fintype G) : ∃ (m : ℕ), ∀ (a : G), a ^ m = 1 

theorem exercise_2.2.3
 is_abelian_of_consecutive_powers_eq {G : Type*} [group G] 
    (a b : G) (i j k : ℕ) (h : (a * b) ^ i = a ^ i * b ^ i) 
    (h' : (a * b) ^ j = a ^ j * b ^ j) (h'' : (a * b) ^ k = a ^ k * b ^ k) :
    is_abelian G 

theorem exercise_2.2.5
 abelian_of_pow_eq_pow_of_pow_eq_pow {G : Type*} [group G] 
    (h : ∀ (a b : G), (a * b) ^ 3 = a ^ 3 * b ^ 3 ∧ (a * b) ^ 5 = a ^ 5 * b ^ 5) :
    abelian G 

theorem exercise_2.2.6c
 comm_group.comm_power_eq_one_of_comm_power_eq_one_of_comm_power_eq_one 
    {G : Type*} [comm_group G] {n : ℕ} (hn : n > 1) (h : ∀ (a b : G), (a * b) ^ n = a ^ n * b ^ n) :
    ∀ (a b : G), (a * b * a⁻¹ * b⁻¹) ^ (n * (n - 1)) = 1 

theorem exercise_2.3.17
 conjugate_comm_of_comm {G : Type*} [group G] {a x : G} 
    (h : a ∈ comm_group.comm_of G) :
    conjugate x⁻¹ a x = x⁻¹ * conjugate a x 

theorem exercise_2.3.19
 conjugate_subset_iff_subgroup {G : Type*} [group G] {M : subgroup G} 
    (hM : ∀ (x : G), x⁻¹ * M * x ⊆ M) :
    ∀ (x : G), x⁻¹ * M * x = M 

theorem exercise_2.3.16
 cyclic_of_order_prime_of_no_proper_subgroups {G : Type*} [group G] 
    (hG : ¬ ∃ (H : subgroup G), H ≠ ⊥ ∧ H ≠ ⊤) :
    ∃ (p : ℕ), nat.prime p ∧ G ≅ (multiplicative (fin p)) 

theorem exercise_2.3.21
 is_subgroup_of_mul_subgroup {G : Type*} [group G] 
    {A B : subgroup G} (h : ∀ b : G, b ∈ B → ∀ a : G, a ∈ A → b⁻¹ * a * b ∈ A) :
    is_subgroup (A * B) 

theorem exercise_2.3.22
 mul_subgroup_of_rel_prime_order {G : Type*} [group G] 
    (A B : subgroup G) (hA : fintype.card A.carrier = nat.prime_factors A.card) 
    (hB : fintype.card B.carrier = nat.prime_factors B.card) 
    (hAB : nat.relprime A.card B.card) :
    is_subgroup (A.carrier * B.carrier) 

theorem exercise_2.3.28
 conjugate_subset_conjugate_mul {G : Type*} [group G] 
    {M N : subgroup G} (hM : ∀ (x : G), x⁻¹ * M * x ⊆ M) 
    (hN : ∀ (x : G), x⁻¹ * N * x ⊆ N) :
    ∀ (x : G), x⁻¹ * (M * N) * x ⊆ M * N 

theorem exercise_2.3.29
 eq_of_subset_of_conj {G : Type*} [group G] {M : subgroup G} 
    (hM : ∀ (x : G), x⁻¹ * M * x ⊆ M) :
    ∀ (x : G), x⁻¹ * M * x = M 

theorem exercise_2.4.8
 eq_of_right_coset_eq_left_coset {G : Type*} [group G] 
    (H : subgroup G) (h : ∀ (a : G), H * a = a * H) :
    ∀ (a : G), a * H * a⁻¹ = H 

theorem exercise_2.4.26
 exists_bijective_of_right_cosets_to_left_cosets {G : Type*} 
    [group G] (H : subgroup G) :
    ∃ (f : set G → set G), function.bijective f ∧ 
    ∀ (x : set G), x ∈ set.range f ↔ x ∈ set.range (λ (x : set G), x⁻¹) 

theorem exercise_2.4.32
 order_dvd_order_of_subgroup {G : Type*} [group G] [fintype G]
    (H : subgroup G) (a : G) :
    ∃ (m : ℕ), a ^ m ∈ H ∧ nat.dvd (order a) m 

theorem exercise_2.4.36
 euler_phi_divides_phi_pow_n (a n : ℕ) (h : a > 1) :
    n ∣ nat.euler_phi (a ^ n - 1) 

theorem exercise_2.5.6
 is_subgroup_image {G : Type*} [group G] {G' : Type*} [group G']
    (f : G → G') (hf : is_group_hom f) : is_subgroup (set.range f) 

theorem exercise_2.5.23
 exists_j_of_all_subgroups_normal {G : Type*} [group G] 
    (hG : ∀ (H : subgroup G), H ≤ normalizer G H) (a b : G) :
    ∃ (j : ℕ), b * a = a ^ j * b 

theorem exercise_2.5.30
 char_of_order_p_dvd_order_of_group {G : Type*} [group G] 
    (H : subgroup G) (hH : H.order = p) (hHn : H.normal) 
    (hG : G.order = p * m) (hp : p ∣ m) :
    H.is_char 

theorem exercise_2.5.31
 characteristic_of_p_power_subgroup {G : Type*} [group G] 
    (p : ℕ) (h : nat.prime p) (n : ℕ) (m : ℕ) (hG : G.card = p ^ n * m) 
    (H : subgroup G) (hH : H.card = p ^ n) :
    characteristic G H 

theorem exercise_2.5.37
 nonabelian_group_of_order_6_is_S3 (G : Type*) [group G] 
    (hG : G.card = 6) (hG' : ¬ abelian G) :
    G ≅ symmetric_group 3 

theorem exercise_2.5.43
 group_of_order_9_is_abelian (G : Type*) [group G] (hG : order G = 9) :
    abelian G 

theorem exercise_2.5.44
 exists_normal_subgroup_of_order_p_of_order_p_pow_two {G : Type*} 
    [group G] (p : ℕ) (hp : nat.prime p) (hG : (p ^ 2 : ℕ) ∣ fintype.card G) :
    ∃ (N : subgroup G), N.order = p ∧ N.normal 

theorem exercise_2.5.52
 is_abelian_of_inverse_automorphism {G : Type*} [group G] 
    (φ : G → G) (hφ : is_automorphism φ) (h : ∀ x : G, φ x = x⁻¹) : 
    is_abelian G 

theorem exercise_2.6.15
 exists_order_mul_of_rel_prime {G : Type*} [group G] 
    (m n : ℕ) (hm : ∃ (g : G), g.order = m) (hn : ∃ (g : G), g.order = n) 
    (hmn : nat.relprime m n) :
    ∃ (g : G), g.order = m * n 

theorem exercise_2.7.3
 quotient_group_of_nonzero_real_numbers_is_positive_real_numbers :
    quotient_group (nonzero ℝ) (group_of_order_two) ≅ multiplicative ℝ 

theorem exercise_2.7.7
 normal_of_hom_image {G : Type*} [group G] {G' : Type*} [group G']
    (φ : G →* G') (hφ : function.surjective φ) (N : set G) 
    (hN : is_normal_subgroup N) : is_normal_subgroup (φ '' N) 

theorem exercise_2.8.7
 card_mul_card_eq_card_mul_card_of_coprime {G : Type*} [group G] 
    {A B : set G} (hA : is_subgroup A) (hB : is_subgroup B) 
    (hA_finite : fintype.card A < ⊤) (hB_finite : fintype.card B < ⊤) 
    (hAB_coprime : nat.coprime (fintype.card A) (fintype.card B)) :
    fintype.card (A * B) = fintype.

theorem exercise_2.8.12
 nonabelian_group_of_order_21_is_isomorphic (G H : Type*) 
    [group G] [group H] (hG : G.card = 21) (hH : H.card = 21) 
    (hG_nonabelian : ¬ abelian G) (hH_nonabelian : ¬ abelian H) :
    G ≃ H 

theorem exercise_2.8.15
 nonabelian_groups_of_order_pq_are_isomorphic (p q : ℕ) 
    (hp : nat.prime p) (hq : nat.prime q) (h : q ∣ p - 1) :
    ∀ (G H : Type*) [group G] [group H] [fintype G] [fintype H] 
    (hG : G.card = p * q) (hH : H.card = p * q) (hGn : ¬ abelian G) 
    (hHn : ¬ abelian H), G ≃ H 

theorem exercise_2.9.2
 is_cyclic_prod_iff_gcd_one {G₁ G₂ : Type*} [group G₁] [group G₂]
    [is_cyclic G₁] [is_cyclic G₂] :
    is_cyclic (G₁ × G₂) ↔ nat.gcd (order G₁) (order G₂) = 1 

theorem exercise_2.10.1
 normal_subgroup.prime_order_not_mem_inter_eq_one {G : Type*} 
    [group G] (A : set G) (hA : is_normal_subgroup A) (b : G) 
    (hb : nat.prime (order b)) (hbA : b ∉ A) :
    A ∩ (b) = {1} 

theorem exercise_2.11.6
 sylow_normal_unique {p : ℕ} {G : Type*} [group G] {P : sylow p G} 
    (hP : P.normal) :
    ∀ (Q : sylow p G), P = Q 

theorem exercise_2.11.7
 sylow_p_subgroup_is_fixed_by_automorphism {p : ℕ} {G : Type*} 
    [group G] {P : subgroup G} (hP : is_p_group p P) (hP_sylow : is_sylow p G P) 
    (φ : G →+* G) : φ P = P 

theorem exercise_2.11.22
 normal_of_order_p_pow_sub_one {p : ℕ} {n : ℕ} {G : Type*} [group G]
    (hG : order_of G = p ^ n) (H : subgroup G) (hH : order_of H = p ^ (n - 1)) :
    is_normal H G 

theorem exercise_3.2.21
 perm.mul_eq_one_of_disjoint_support {α : Type*} [decidable_eq α]
  (σ τ : perm α) (hστ : disjoint_support σ τ) (hστ_eq_one : σ * τ = 1) :
  σ = 1 ∧ τ = 1 

theorem exercise_3.2.23
 exists_conjugate_of_perm_with_same_cycle_type {α : Type*} 
    [fintype α] [decidable_eq α] (σ τ : perm α) 
    (hσ : σ.is_cycle_decomposition) (hτ : τ.is_cycle_decomposition) 
    (hστ : σ.cycle_type = τ.cycle_type) :
    ∃ (β : perm α), τ = β * σ * β⁻¹ 

theorem exercise_3.3.2
 is_odd_of_is_k_cycle_of_even_k {k : ℕ} (hk : k % 2 = 0) 
    (σ : perm k) (hσ : σ.is_k_cycle) : σ.is_odd 

theorem exercise_3.3.9
 exists_three_cycle_of_normal_subgroup_of_an (n : ℕ) (h : n ≥ 5) 
    (N : subgroup (perm.perm_group n)) (hN : N ≠ ⊥) (hN_normal : N.normal) :
    ∃ (p : perm n), p.is_three_cycle 

theorem exercise_4.1.19
 exists_infinite_solutions_of_x_squared_eq_neg_one_in_quaternions :
    ∃ (x : quaternion ℂ), x ^ 2 = -1 

theorem exercise_4.1.28
 det_ne_zero_is_group {R : Type*} [ring R] [decidable_eq R] 
    [fintype R] [decidable_eq (matrix R 2 2)] :
    group (matrix R 2 2) 

theorem exercise_4.1.29
 det_eq_zero_iff_zero_divisor {R : Type*} [comm_ring R] 
    (x : R) :
    det x = 0 ↔ x ≠ 0 ∧ x ∈ zero_divisors R 

theorem exercise_4.1.34
 is_isomorphic_to_symmetric_group_3 (T : Type*) [group T] 
    (hT : ∀ (A : T), det A ≠ 0) :
    is_isomorphic T (perm.group 3) 

theorem exercise_4.2.5
 comm_ring_of_cube_eq_id {R : Type*} [ring R] 
    (h : ∀ x : R, x ^ 3 = x) : comm_ring R 

theorem exercise_4.2.6
 comm_of_square_eq_zero {R : Type*} [ring R] (a x : R) 
    (h : a ^ 2 = 0) : a * (x + x * a) = (x + x * a) * a 

theorem exercise_4.2.9
 odd_prime_divides_sum_of_reciprocals_of_odd_nats (p : ℕ) 
    (hp : nat.prime p) (hp1 : p % 2 = 1) :
    ∀ (a b : ℕ), a / b = ∑ i in (finset.range (p - 1) : finset ℕ), 1 / (i + 1) → p ∣ a 

theorem exercise_4.3.1
 ideal.left_mul_eq_zero {R : Type*} [comm_ring R] (a : R) :
    ideal.left_mul a = {x | x * a = 0} 

theorem exercise_4.3.4
 ideal.add_is_ideal {R : Type*} [comm_ring R] (I J : ideal R) :
    ideal.is_ideal (I + J) 

theorem exercise_4.3.25
 ideal_of_matrix_ring_is_zero_or_ring {R : Type*} [ring R] 
    (M : Type*) [add_comm_group M] [module R M] [fintype M] [decidable_eq M] 
    (I : ideal R) :
    I = ⊥ ∨ I = ⊤ 

theorem exercise_4.4.9
 quadratic_residues_and_nonresidues_mod_p (p : ℕ) (hp : nat.prime p) :
    (p - 1) / 2 = card {x : fin (p - 1) | x.val.quadratic_residue p hp} 

theorem exercise_4.5.12
 exists_rel_prime_of_rel_prime {F K : Type*} [field F] [field K]
    [algebra F K] (f g : polynomial F) (hf : f.is_unit) (hg : g.is_unit) :
    ∃ (h : polynomial K), h.is_unit ∧ h.gcd f g = 1 

theorem exercise_4.5.16
 exists_eq_card_of_irreducible {p : ℕ} (hp : nat.prime p) 
    (n : ℕ) (q : polynomial ℤ_p) (hq : irreducible q) :
    ∃ (F : Type*) [field F], cardinal.mk F = p ^ n 

theorem exercise_4.5.23
 is_irreducible_of_degree_three_and_not_root_of_unity 
    {F : Type*} [field F] (p : polynomial F) (h : p.degree = 3) 
    (h2 : ∀ (a : F), p.eval a = 0 → is_root_of_unity a) : 
    is_irreducible p 

theorem exercise_4.5.25
 irreducible_q_polynomial (p : ℕ) (hp : nat.prime p) :
    irreducible (polynomial.q_polynomial p) 

theorem exercise_4.6.2
 irreducible_polynomial_of_degree_3 (x : polynomial ℚ) :
  irreducible (x^3 + 3*x + 2) 

theorem exercise_4.6.3
 exists_infinite_a_irreducible_polynomial (a : ℕ) :
    ∃ (a : ℕ), irreducible (polynomial.C a * polynomial.X ^ 7 + polynomial.C 15 * polynomial.X ^ 2 - polynomial.C 30 * polynomial.X + polynomial.C a) 

theorem exercise_5.1.8
 char_p_field.pow_add_pow {F : Type*} [field F] (hF : char_p F) 
    (a b : F) (m : ℕ) (hmn : m = (char_p.pos hF).pow n) (hn : 0 < n) :
    (a + b) ^ m = a ^ m + b ^ m 

theorem exercise_5.2.20
 not_finite_union_of_proper_subspaces {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] (hV : ∀ (n : ℕ), ∃ (W : submodule ℂ V), 
    (finite.card W = n) ∧ (W ≠ ⊤)) : false 

theorem exercise_5.3.7
 is_algebraic_of_is_algebraic_pow {K : Type*} [field K] {F : Type*} 
    [field F] (a : K) (h : is_algebraic K (a ^ 2) F) : is_algebraic K a F 

theorem exercise_5.3.10
 cos_one_degree_is_algebraic : algebraic ℚ (cos (1 : ℝ)) 

theorem exercise_5.4.3
 exists_polynomial_of_degree_le_80_of_root_of_polynomial_of_degree_5 
    (a : ℂ) (h : polynomial.eval ℂ (polynomial.X ^ 5 + √2 * polynomial.X ^ 3 + 
    √5 * polynomial.X ^ 2 + √7 * polynomial.X + √11) a = 0) :
    ∃ (p : polynomial ℂ), degree p ≤ 80 ∧ polynomial.eval ℂ p a = 0 

theorem exercise_5.5.2
 irreducible_polynomial_x3_3x_1 : irreducible (polynomial.C (-1) * X^3 + polynomial.C 3 * X + 1) 

theorem exercise_5.6.3
 exists_splitting_field_of_degree_4 {α : Type*} [integral_domain α] 
    [field α] (p : polynomial α) (hp : p.degree = 4) :
    ∃ (K : Type*) [field K] [algebra α K] (h : degree K = 4), 
    is_splitting_field K p 

theorem exercise_5.6.14
 distinct_roots_of_x_pow_m_sub_x {F : Type*} [field F] 
    (p : ℕ) (hp : p ≠ 0) (n : ℕ) :
    (∀ (x : F), x ^ p ^ n - x = 0) → 
    ∀ (x y : F), x ≠ y → x ^ p ^ n ≠ y ^ p ^ n 