theorem exercise_1.1.2a
 not_commutative_star (a b : ℤ) : a ∗ b ≠ b ∗ a 

theorem exercise_1.1.3
 add_assoc (n : ℕ) : 
    ∀ (a b c : ℕ), (a + b) % n + c % n = (a + c) % n + b % n 

theorem exercise_1.1.4
 mul_assoc (n : ℕ) : 
    ∀ (a b c : ℕ), (a * b) * c ≡ a * (b * c) [MOD n] 

theorem exercise_1.1.5
 not_group_of_residue_classes (n : ℕ) (hn : 1 < n) : 
    ¬ group (residue_ring n) 

theorem exercise_1.1.15
 inv_prod_eq_prod_inv {G : Type*} [group G] (a : G) (b : G) :
    (a * b)⁻¹ = b⁻¹ * a⁻¹ 

theorem exercise_1.1.16
 order_eq_one_or_two_of_pow_two_eq_one {G : Type*} [group G] 
    (x : G) (hx : x ^ 2 = 1) :
    order x = 1 ∨ order x = 2 

theorem exercise_1.1.17
 inv_eq_of_order_eq_of_pos {G : Type*} [group G] {x : G} 
    (hx : x ≠ 1) (hxo : x.order = n) (hn : n > 0) :
    x⁻¹ = x ^ (n - 1) 

theorem exercise_1.1.18
 mul_comm_iff_mul_inv_eq_of_mul_eq_one {G : Type*} [group G] 
    {x y : G} : x * y = y * x ↔ y⁻¹ * x * y = x ∧ x⁻¹ * y⁻¹ * x * y = 1 

theorem exercise_1.1.20
 order_eq_inv_of_order_eq {G : Type*} [group G] {x : G} 
    (hx : x ≠ 1) (h : order x = order x⁻¹) : x = x⁻¹ 

theorem exercise_1.1.22a
 order_eq_order_of_conjugate {G : Type*} [group G] (x g : G) :
    order x = order (g⁻¹ * x * g) 

theorem exercise_1.1.22b
 mul_comm (a b : G) : a * b = b * a 

theorem exercise_1.1.25
 is_abelian_of_pow_two_eq_one {G : Type*} [group G] 
    (h : ∀ x : G, x ^ 2 = 1) : is_abelian G 

theorem exercise_1.1.29
 prod.comm_group_iff_comm_group {A B : Type*} [group A] [group B] :
  comm_group (A × B) ↔ comm_group A ∧ comm_group B 

theorem exercise_1.1.34
 ne_of_infinite_order_of_pow {G : Type*} [group G] {x : G} 
    (hx : x ≠ 1) (hxo : x.order = ⊤) : ∀ (n : ℤ), x ^ n ≠ 1 

theorem exercise_1.3.8
 infinite_symmetric_group (ω : Type*) [fintype ω] : 
    infinite (perm ω) 

theorem exercise_1.6.4
 not_isomorphic_real_complex : 
    ¬ (multiplicative ℝ ≃* multiplicative ℂ) 

theorem exercise_1.6.11
 is_group_hom.prod_comm {A B : Type*} [group A] [group B] 
    (f : A → B) (g : B → A) (hf : is_group_hom f) (hg : is_group_hom g) :
    is_group_hom (f.prod g) 

theorem exercise_1.6.17
 is_group_hom.inv_iff_comm {G : Type*} [group G] (f : G → G) 
    (hf : is_group_hom f) :
    f = function.inv G ↔ ∀ (x y : G), x * y = y * x 

theorem exercise_1.6.23
 is_abelian_of_sigma_squared_id {G : Type*} [group G] 
    (hG : fintype G) (sigma : G → G) (hsigma : is_automorphism sigma) 
    (h1 : ∀ g : G, sigma g = g ↔ g = 1) (hsigma2 : sigma ∘ sigma = id) :
    is_abelian G 

theorem exercise_1.7.5
 kernel_of_action_eq_kernel_of_perm_rep {G : Type*} [group G] 
    {A : Type*} [fintype A] (α : G →* (perm A)) :
    ker α = ker (perm_rep α) 

theorem exercise_1.7.6
 faithful_iff_kernel_eq_singleton {G : Type*} [group G] {A : Type*} 
    (h : group_action G A) :
    faithful h ↔ h.kernel = {1} 

theorem exercise_2.1.5
 not_exists_subgroup_of_size_n_minus_1 {G : Type*} [group G] 
    (hG : 2 < fintype.card G) :
    ¬ ∃ (H : subgroup G), fintype.card H = fintype.card G - 1 

theorem exercise_2.1.13
 eq_zero_or_eq_univ_of_inv_mem {H : Type*} [add_group H] 
    [decidable_eq H] (hH : ∀ (x : H), x ≠ 0 → 1 / x ∈ H) :
    H = (0 : H) ∨ H = univ 

theorem exercise_2.4.4
 subgroup.generated_by_subset_singleton_diff {G : Type*} [group G] 
    (H : subgroup G) :
    H = subgroup.generated (H.carrier \ {1}) 

theorem exercise_2.4.13
 is_subgroup.generate_of_prime_inverse {q : ℚ} (hq : 0 < q) 
    (hqp : q.prime) :
    is_subgroup.generate {q : ℚ | 0 < q} (λ p hp, 1 / p) 

theorem exercise_2.4.16a
 exists_maximal_subgroup_of_finite_group {G : Type*} [group G] 
    [fintype G] {H : subgroup G} (hH : H ≠ ⊥) :
    ∃ (M : subgroup G), is_maximal_subgroup M ∧ H ≤ M 

theorem exercise_2.4.16b
 dihedral_rotation_subgroup_is_maximal (n : ℕ) : 
    maximal_subgroup (dihedral_group n).rotation_subgroup 

theorem exercise_2.4.16c
 maximal_iff_prime_power {G : Type*} [group G] (x : G) (n : ℕ) 
    (hx : x.order = n) (H : subgroup G) (hH : H.is_maximal) :
    ∃ (p : ℕ), nat.prime p ∧ p ∣ n ∧ H = ⟨x ^ p⟩ 

theorem exercise_3.1.3a
 quotient_group.is_abelian {A : Type*} [group A] {B : set A} 
    (hB : is_subgroup B) :
    abelian_group (quotient_group.quotient B) 

theorem exercise_3.1.22a
 normal_inter (G : Type*) [group G] (H K : set G) 
    (hH : is_normal G H) (hK : is_normal G K) :
    is_normal G (H ∩ K) 

theorem exercise_3.1.22b
 normal_inter_of_normal_subgroups {G : Type*} [group G] 
    (N : set (set G)) (hN : ∀ (n : set G), n ∈ N → is_normal_subgroup n) :
    is_normal_subgroup (set.Inter N) 

theorem exercise_3.2.8
 coprime_order_intersection_eq_one {G : Type*} [group G] 
    {H K : subgroup G} (hH : fintype H) (hK : fintype K) 
    (h : nat.coprime (fintype.card H) (fintype.card K)) : 
    H ∩ K = ⊥ 

theorem exercise_3.2.11
 index_mul_index_of_subgroup {G : Type*} [group G] {H K : subgroup G} 
    (hH : H ≤ K) (hK : K ≤ G) :
    index G H * index G K = index G K * index K H 

theorem exercise_3.2.16
 fermat_little_theorem (p : ℕ) (hp : nat.prime p) (a : ℕ) :
  nat.coprime a p → a ^ p ≡ a [MOD p] 

theorem exercise_3.2.21a
 no_proper_subgroups_of_finite_index (G : Type*) [group G] 
    [fintype G] [decidable_eq G] (H : subgroup G) (hH : H ≠ ⊥) :
    H = ⊤ 

theorem exercise_3.3.3
 prime_index_of_normal_subgroup_of_prime_index {G : Type*} [group G] 
    {H : subgroup G} (hH : H.normal) (p : ℕ) (hp : nat.prime p) 
    (hG : p ∣ G.card) (K : subgroup G) (hK : K ≤ G) :
    K ≤ H ∨ (G = H * K ∧ p ∣ K.card / K.inter H.card) 

theorem exercise_3.4.1
 abelian_simple_iff_cyclic_prime (G : Type*) [group G] 
    (hG : abelian_group G) (hG' : simple_group G) :
    ∃ (p : ℕ), nat.prime p ∧ G ≅ (multiplicative (fin p)) 

theorem exercise_3.4.4
 exists_subgroup_of_order_dvd_of_finite_abelian {G : Type*} 
    [group G] [fintype G] [decidable_eq G] (n : ℕ) (h : n ∣ fintype.card G) :
    ∃ (H : subgroup G), fintype.card H = n 

theorem exercise_3.4.5a
 solvable_of_solvable_subgroup {G : Type*} [group G] 
    (H : subgroup G) [solvable H] : solvable G 

theorem exercise_3.4.5b
 solvable_quotient_group {G : Type*} [group G] [solvable_group G] 
    (N : set G) [normal_subgroup N] : solvable_group (quotient_group.quotient N) 

theorem exercise_3.4.11
 exists_abelian_normal_subgroup_of_solvable {G : Type*} [group G] 
    (H : subgroup G) (hH : H ≠ ⊥) (hH_normal : H ≤ normalizer G H) 
    (hG_solvable : is_solvable G) :
    ∃ (A : subgroup G), A ≠ ⊥ ∧ A ≤ H ∧ A ≤ normalizer G A ∧ is_abelian A 

theorem exercise_4.2.8
 exists_normal_subgroup_of_finite_index {G : Type*} [group G] 
    {H : subgroup G} (hH : fintype (quotient_group.quotient H)) :
    ∃ (K : subgroup G), K ≤ H ∧ K.normal ∧ fintype (quotient_group.quotient K) ∧ 
    fintype.card (quotient_group.quotient K) ≤ fintype.card (quotient_group.quotient H) 

theorem exercise_4.2.9a
 normal_of_index_p_of_order_p_pow_alpha {p : ℕ} (hp : nat.prime p) 
    {G : Type*} [group G] (hG : order G = p ^ nat.succ (nat.find hp)) 
    {H : subgroup G} (hH : index G H = p) :
    is_normal H G 

theorem exercise_4.2.14
 not_simple_of_composite_order_of_subgroup_of_each_divisor 
    {G : Type*} [fintype G] [group G] (hG : ¬ is_simple G) 
    (hG_order : nat.prime (fintype.card G) → false) :
    ∀ (k : ℕ), k ∣ fintype.card G → ∃ (H : subgroup G), fintype.card H = k 

theorem exercise_4.3.5
 card_conj_classes_le_index_of_center {G : Type*} [group G] 
    (hG : ∀ (g : G), g ∈ center G) :
    ∀ (g : G), card (conj_classes g) ≤ index_of_center G 

theorem exercise_4.3.26
 exists_ne_of_transitive_permutation_group {G : Type*} [group G] 
    {A : Type*} [fintype A] (hG : transitive_permutation_group G A) 
    (hA : fintype.card A > 1) :
    ∃ (σ : G), ∀ (a : A), σ a ≠ a 

theorem exercise_4.3.27
 abelian_of_commuting_conjugacy_classes {G : Type*} [group G] 
    (hG : fintype G) (g : finset G) (hg : g.card = fintype.card G) 
    (hg_comm : ∀ (x y : G), x ∈ g → y ∈ g → x * y = y * x) :
    abelian G 

theorem exercise_4.4.2
 cyclic_of_order_pq {G : Type*} [group G] (hG : abelian G) 
    (hpq : ∃ (p q : ℕ), nat.prime p ∧ nat.prime q ∧ p ≠ q ∧ card G = p * q) :
    cyclic G 

theorem exercise_4.4.6a
 char_normal (G : Type*) [group G] (p : ℕ) (h : p.prime) :
  is_normal (char_subgroup p h) 

theorem exercise_4.4.6b
 exists_normal_not_char (G : Type*) [group G] :
    ∃ (N : subgroup G), normal N ∧ ¬ is_char_subgroup N 

theorem exercise_4.4.7
 unique_of_order_is_char (G : Type*) [group G] (H : subgroup G) 
    (hH : ∃ (n : ℕ), ∃! (K : subgroup G), order K = n) :
    is_char_subgroup H 

theorem exercise_4.4.8a
 normal_of_char_subgroup_of_normal_subgroup {G : Type*} [group G] 
    {H K : subgroup G} (hH : H ≤ K) (hK : K ≤ G) (hK_normal : normal K G) 
    (hH_char : char_subgroup H K) : normal H G 

theorem exercise_4.5.1a
 sylow_subgroup_of_subgroup {p : ℕ} {G : Type*} [group G] 
    {P : subgroup G} (hP : is_p_group p P) (H : subgroup G) 
    (hH : P ≤ H) : is_p_group p H 

theorem exercise_4.5.13
 exists_normal_sylow_of_order_56 {G : Type*} [group G] 
    (hG : fintype.card G = 56) :
    ∃ (p : ℕ) (P : sylow p G), P.normal 

theorem exercise_4.5.14
 exists_normal_sylow_of_order_312 {G : Type*} [group G] 
    (hG : order G = 312) :
    ∃ (p : ℕ) (P : sylow p G), P.normal 

theorem exercise_4.5.15
 exists_normal_sylow_of_order_351 {G : Type*} [group G] 
    (hG : order G = 351) :
    ∃ (p : ℕ) (P : sylow p G), P.normal 

theorem exercise_4.5.16
 exists_normal_sylow_of_order_eq_mul_primes {G : Type*} [group G] 
    (p q r : ℕ) (hp : nat.prime p) (hq : nat.prime q) (hr : nat.prime r) 
    (h : p < q) (h' : q < r) (hG : (card G : ℚ) = p * q * r) :
    ∃ (P : sylow p G), P.is_normal 

theorem exercise_4.5.17
 exists_normal_sylow_of_order_105 {G : Type*} [group G] 
    (hG : fintype.card G = 105) :
    ∃ (P : sylow 5 G) (Q : sylow 7 G), P.normal ∧ Q.normal 

theorem exercise_4.5.18
 exists_normal_sylow_5_subgroup {G : Type*} [group G] 
    (hG : order G = 200) :
    ∃ (P : sylow 5 G), P.normal 

theorem exercise_4.5.19
 not_simple_of_card_eq_6545 {G : Type*} [group G] 
    (hG : card G = 6545) : ¬ simple_group G 

theorem exercise_4.5.20
 not_simple_of_card_eq_1365 {G : Type*} [group G] 
    (hG : card G = 1365) : ¬ simple_group G 

theorem exercise_4.5.21
 not_simple_of_card_eq_2907 {G : Type*} [fintype G] [group G] 
    (hG : fintype.card G = 2907) : ¬ simple_group G 

theorem exercise_4.5.22
 not_simple_of_order_eq_132 {G : Type*} [group G] 
    (hG : group.order G = 132) : ¬ simple_group G 

theorem exercise_4.5.23
 not_simple_of_order_462 {G : Type*} [group G] (hG : |G| = 462) :
    ¬ simple_group G 

theorem exercise_4.5.28
 abelian_of_sylow_normal {G : Type*} [group G] (hG : card G = 105) 
    (hS : ∀ (S : sylow 3 G), is_normal S) :
    abelian G 

theorem exercise_4.5.33
 sylow_inter_subgroup {p : ℕ} {G : Type*} [group G] {P : sylow p G} 
    (hP : P.is_normal) {H : subgroup G} :
    sylow p H = P ∩ H 

theorem exercise_5.4.2
 normal_iff_comm_subgroup_le_self {G : Type*} [group G] (H : subgroup G) :
  H ≤ H.normalizer ↔ H.comm_subgroup ≤ H 

theorem exercise_7.1.2
 neg_unit_of_unit {R : Type*} [comm_ring R] {u : R} (hu : is_unit u) :
    is_unit (-u) 

theorem exercise_7.1.11
 integral_domain.eq_one_or_neg_one_of_pow_two_eq_one {R : Type*} 
    [integral_domain R] {x : R} (hx : x ^ 2 = 1) : x = 1 ∨ x = -1 

theorem exercise_7.1.12
 subring_of_field_is_integral_domain {R : Type*} [field R] 
    (S : subring R) (hS : 1 ∈ S) : integral_domain S 

theorem exercise_7.1.15
 boolean_ring.comm (R : Type*) [ring R] (hR : boolean_ring R) : 
    commutative R 

theorem exercise_7.2.2
 is_zero_divisor_iff_exists_nonzero_mul_eq_zero {R : Type*} 
    [comm_ring R] (p : polynomial R) :
    p.is_zero_divisor ↔ ∃ (b : R), b ≠ 0 ∧ b * p = 0 

theorem exercise_7.2.4
 is_integral_domain_of_is_integral_domain {R : Type*} 
    [integral_domain R] : integral_domain (power_series R) 

theorem exercise_7.2.12
 center_sum_of_elements {R : Type*} [comm_ring R] {G : Type*} 
    [group G] (g : finset G) :
    ∀ (h : g.card = n), (∑ x in g, x) ∈ center (group_ring R G) 

theorem exercise_7.3.16
 center_image_subset_center {R : Type*} [comm_ring R] 
    {S : Type*} [comm_ring S] (f : R → S) (hf : function.surjective f) :
    f '' (center R) ⊆ center S 

theorem exercise_7.3.28
 integral_domain.char_eq_zero_or_prime_of_prime_ideal_eq_zero 
    {R : Type*} [integral_domain R] (p : ℕ) (hp : nat.prime p) 
    (h : ideal.span {p} = ⊥) :
    integral_domain.char R = 0 ∨ integral_domain.char R = p 

theorem exercise_7.3.37
 is_nilpotent_of_pow_eq_zero {R : Type*} [comm_ring R] 
    (N : ideal R) (hN : ∃ (n : ℕ), N ^ n = ⊥) :
    is_nilpotent N 

theorem exercise_7.4.27
 is_unit_of_nilpotent_of_ne_zero {R : Type*} [comm_ring R] 
    (a : R) (h1 : a ≠ 0) (h2 : a^2 = 0) : ∀ b : R, is_unit (1 - a * b) 

theorem exercise_8.1.12
 eq_mod_of_rel_prime_of_rel_prime_of_eq_mod_of_rel_prime 
    (N M M₁ d : ℕ) (hN : 0 < N) (hM : nat.coprime N M) 
    (hM₁ : nat.coprime N M₁) (hM₁d : M₁ ≡ M^d [MOD N]) 
    (hdd' : nat.coprime (nat.euler_phi N) d) :
    ∃ (d' : ℕ), M ≡ M₁^d'

theorem exercise_8.2.4
 is_PID_of_gcd_exists_and_divides_of_infinite_chain {R : Type*} 
    [integral_domain R] (hgcd : ∀ (a b : R), a ≠ 0 ∧ b ≠ 0 → ∃ (r s : R), gcd a b = r * a + s * b) 
    (hdiv : ∀ (a : ℕ → R), (∀ (i : ℕ), a (i + 1) ∣ a i) → ∃ (N : ℕ), ∀ (n : ℕ), n ≥ N → a n =

theorem exercise_8.3.4
 exists_int_sq_sum_of_rat_sq_sum {n : ℕ} (h : ∃ (a b : ℚ), n = a ^ 2 + b ^ 2) :
    ∃ (a b : ℤ), n = a ^ 2 + b ^ 2 

theorem exercise_8.3.5a
 irreducible_of_sqrt_minus_n {n : ℕ} (hn : n > 3) (h : nat.prime n) :
    irreducible (sqrt_minus_n n) 

theorem exercise_8.3.6a
 quotient_ring_is_field_of_order_2 : 
    is_field (quotient_ring.quotient (ideal.span {1 + I})) 

theorem exercise_8.3.6b
 quotient_ring_is_field {q : ℤ} (hq : nat.prime q) (hq3 : q ≡ 3 [MOD 4]) :
    is_field (quotient_ring ℤ (ideal.span {q})) 

theorem exercise_9.1.6
 not_principal_ideal_of_xy (x y : polynomial ℚ) : 
    ¬ is_principal_ideal (ideal.span {x, y}) 

theorem exercise_9.1.10
 minimal_primes_of_infinitely_many_variables (n : ℕ) :
  ∃ (p : polynomial ℤ), p.is_prime ∧ p.is_minimal_prime 

theorem exercise_9.3.2
 int.coe_mul_coe_of_int_mul_int {α : Type*} [integral_domain α] 
    [decidable_eq α] (f g : polynomial α) (h : f.map (int.cast_ring_hom α) * 
    g.map (int.cast_ring_hom α) ∈ (polynomial ℤ).map (int.cast_ring_hom α)) :
    ∀ (a b : α), a ∈ f.coeffs → b ∈ g.coeffs → int.cast (a * b) ∈ (f.map (int.cast

theorem exercise_9.4.2a
 irreducible_polynomial_of_degree_4 (x : polynomial ℤ) :
  irreducible (x^4 - 4*x^3 + 6) 

theorem exercise_9.4.2b
 irreducible_polynomial_of_degree_6 :
  irreducible (polynomial.C 120 * X ^ 6 + polynomial.C (-6) * X ^ 5 + polynomial.C 30 * X ^ 4 + polynomial.C (-15) * X ^ 3 + X ^ 2) 

theorem exercise_9.4.2c
 irreducible_polynomial_of_degree_4 : irreducible (X^4 + 4*X^3 + 6*X^2 + 2*X + 1) 

theorem exercise_9.4.2d
 irreducible_of_odd_prime {p : ℕ} (hp : nat.prime p) (hp_odd : p % 2 = 1) :
  irreducible (polynomial.C (p : ℤ) * X ^ (p - 1) + polynomial.C 2 ^ (p - 1)) 

theorem exercise_9.4.9
 irreducible_of_polynomial_of_degree_two_over_ufd {α : Type*} 
    [integral_domain α] [unique_factorization_domain α] 
    (x : α) (hx : x ^ 2 - 2 = 0) :
    irreducible (polynomial.X ^ 2 - polynomial.C x) 

theorem exercise_9.4.11
 irreducible_polynomial_of_two_variables (x y : polynomial ℚ) :
    irreducible (x ^ 2 + y ^ 2 - 1) 

theorem exercise_11.1.13
 Q_vector_space_equiv_R_of_n_pos {n : ℕ} (hn : 0 < n) :
    (ℝ^n : Type*) ≃ₗ[ℚ] ℝ 

theorem exercise_11.3.3bi
 ann_add_eq_inter_ann (V : Type*) [add_comm_group V] 
    [vector_space ℂ V] [finite_dimensional ℂ V] (W1 W2 : submodule ℂ V) :
    ann_submodule ℂ W1 + ann_submodule ℂ W2 = 
    ann_submodule ℂ W1 ∩ ann_submodule ℂ W2 

theorem exercise_11.3.3bii
 ann_add_eq_ann_inter (V : Type*) [add_comm_group V] 
    [vector_space ℂ V] [finite_dimensional ℂ V] (W1 W2 : submodule ℂ V) :
    (ann W1 : submodule ℂ V) + (ann W2 : submodule ℂ V) = ann (W1 ⊓ W2) 

theorem exercise_11.3.3c
 eq_iff_ann_eq {V : Type*} [finite_dimensional ℝ V] 
    (W1 W2 : submodule ℝ V) :
    W1 = W2 ↔ W1.ann = W2.ann 

theorem exercise_11.3d
 ann_eq_ann_span {V : Type*} [add_comm_group V] [vector_space ℂ V]
    (S : set (dual_vector_space.dual V)) :
    (ann S : set V) = ann (submodule.span ℂ S) 

theorem exercise_11.3f
 dim_ann_eq_dim_sub {V : Type*} [finite_dimensional ℝ V] 
    (W : submodule ℝ V) :
    dim (ann W) = dim V - dim W 