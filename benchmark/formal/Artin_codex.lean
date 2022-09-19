theorem exercise_2.2.9
 is_abelian_of_comm {G : Type*} [group G] {a b : G} 
    (h : a * b = b * a) : is_abelian (subgroup.generated G (a, b)) 

theorem exercise_2.3.1
 is_add_group_hom.is_mul_group_hom_of_pos {R : Type*} 
    [ring R] (f : R →+* R) (hf : ∀ x : R, 0 < f x) :
    is_mul_group_hom f 

theorem exercise_2.3.2
 conjugate_mul_comm {G : Type*} [group G] (a b : G) :
    a * b ≈ b * a 

theorem exercise_2.4.19
 center_of_group_contains_unique_order_2_element {G : Type*} 
    [group G] (hG : ∃ (x : G), x ≠ 1 ∧ order x = 2) :
    ∃ (x : G), x ≠ 1 ∧ order x = 2 ∧ x ∈ center G 

theorem exercise_2.8.6
 center_mul_eq_mul_center {G H : Type*} [group G] [group H] :
    center (G * H) = center G * center H 

theorem exercise_2.10.11
 is_add_group_hom.is_add_group_hom_of_is_add_group_hom_comp 
    {G : Type*} [add_group G] {H : Type*} [add_group H] {K : Type*} 
    [add_group K] (f : G → H) (g : H → K) (hf : is_add_group_hom f) 
    (hg : is_add_group_hom g) :
    is_add_group_hom (g ∘ f) 

theorem exercise_2.11.3
 exists_two_order_of_even_order {G : Type*} [group G] 
    (hG : even (card G)) : ∃ (g : G), g ≠ 1 ∧ g^2 = 1 

theorem exercise_3.2.7
 hom_of_fields_is_injective {F : Type*} [field F] {G : Type*} [field G]
    {f : F → G} (hf : is_ring_hom f) : injective f 

theorem exercise_3.5.6
 exists_countable_basis_of_countable_span {V : Type*} [add_comm_group V] [vector_space ℂ V]
    (hV : ∃ (s : set V), countable s ∧ span ℂ s = ⊤) :
    ∃ (s : set V), countable s ∧ is_basis ℂ s 

theorem exercise_3.7.2
 not_finite_union_of_proper_subspaces {V : Type*} [vector_space ℂ V] 
    (hV : ∀ (S : set V), finite S → ∃ (v : V), v ∉ ⋃₀ S) :
    ∀ (S : set V), finite S → ∃ (v : V), v ∉ ⋃₀ S 

theorem exercise_6.1.14
 cyclic_quotient_of_center_implies_abelian (G : Type*) [group G] 
    (Z : set G) (hZ : is_subgroup Z) (hZc : is_center G Z) 
    (hGZ : cyclic (quotient_group.quotient Z)) :
    abelian G 

theorem exercise_6.4.2
 not_simple_of_prime_mul_prime {p q : ℕ} (hp : nat.prime p) 
    (hq : nat.prime q) (h : p * q ≠ 1) :
    ¬ simple_group (multiplicative (fin (p * q))) 

theorem exercise_6.4.3
 not_simple_of_order_p_pow_2_mul_q {p q : ℕ} (hp : nat.prime p) 
    (hq : nat.prime q) (h : p ^ 2 * q ≠ 1) :
    ¬ simple_group (p ^ 2 * q) 

theorem exercise_6.4.12
 not_simple_of_order_224 (G : Type*) [group G] (hG : card G = 224) :
    ¬ simple_group G 

theorem exercise_6.8.1
 subgroup.generated_eq_of_mem_conj {G : Type*} [group G] 
    (a b : G) :
    subgroup.generated G (a :: b :: []) = subgroup.generated G (b * a * b^2 :: b * a * b^3 :: []) 

theorem exercise_6.8.4
 free_group_of_three_generators_with_one_relation : 
    free_group 3 

theorem exercise_6.8.6
 exists_two_generators_of_cyclic_quotient {G : Type*} [group G] 
    (N : subgroup G) (hN : N.normal) (hG : is_cyclic G) 
    (hGN : is_cyclic (G / N)) :
    ∃ (g h : G), G = ⟨g, h⟩ 

theorem exercise_10.1.13
 is_unit_of_nilpotent {R : Type*} [ring R] (x : R) (hx : ∃ n : ℕ, x ^ n = 0) :
    is_unit (1 + x) 

theorem exercise_10.2.4
 int.ideal.inter_eq_mul_ideal (x : polynomial ℤ) : 
    (ideal.span {2}).inter (ideal.span {x}) = ideal.span {2 * x} 

theorem exercise_10.6.7
 exists_nonzero_int_of_nonzero_ideal {I : ideal ℤ[i]} 
    (hI : I ≠ ⊥) : ∃ (z : ℤ), z ≠ 0 ∧ z ∈ I 

theorem exercise_10.6.16
 polynomial.expand_at (f : polynomial ℤ) (a : ℤ) :
  ∃ (g : polynomial ℤ), f = g.eval₂ (λ x y, x - y) a 

theorem exercise_10.3.24a
 not_ideal_union {R : Type*} [comm_ring R] (I J : ideal R) :
    ¬ (I.is_ideal ∧ J.is_ideal) → ¬ (ideal.span (I.carrier ∪ J.carrier) = I.span ∪ J.span) 

theorem exercise_10.4.6
 is_nilpotent_of_mem_mul_ideal {R : Type*} [comm_ring R] 
    (I J : ideal R) (x : R) (hx : x ∈ I ∩ J) :
  is_nilpotent (x : R / I * J) 

theorem exercise_10.4.7a
 ideal.mul_eq_inter_of_sum_eq_univ {R : Type*} [comm_ring R] 
    (I J : ideal R) (hIJ : I + J = ⊤) : I * J = I ∩ J 

theorem exercise_10.5.16
 is_ring_hom.eq_iff_eq_on_prime_powers {R : Type*} 
    [comm_monoid_with_zero R] (f : nat.arithmetic_function R) 
    (hf : f.is_multiplicative) (g : nat.arithmetic_function R) 
    (hg : g.is_multiplicative) :
    f = g ↔ ∀ (p i : ℕ), nat.prime p → f (p ^ i) = g (p ^ i) 

theorem exercise_10.7.6
 is_field_of_finite_field_of_polynomial_quotient {α : Type*} 
    [comm_ring α] [fintype α] [decidable_eq α] (p : polynomial α) 
    (hp : irreducible p) :
    is_field (polynomial.quotient p) 

theorem exercise_10.7.10
 maximal_ideal_iff_units_complement {R : Type*} [comm_ring R] 
    (M : ideal R) :
    maximal_ideal M ↔ ∀ (x : R), x ∉ M → is_unit x 

theorem exercise_11.2.13
 int.dvd_of_dvd_gauss_int {a b : ℤ} (h : a ∣ᵤ b) : a ∣ b 

theorem exercise_11.3.1
 irreducible_iff_irreducible_of_linear_substitution {α : Type*} 
    [field α] {a b : α} (h : a ≠ 0) (f : polynomial α) :
    irreducible f ↔ irreducible (f.linear_substitution a b) 

theorem exercise_11.3.2
 exists_common_factor_of_common_factor_in_field_extension {F : Type*} 
    [field F] (f g : polynomial F) (h : ∃ (h : polynomial F), 
    is_factor_of h f ∧ is_factor_of h g) :
    ∃ (h : polynomial F), is_factor_of h f ∧ is_factor_of h g 

theorem exercise_11.3.4
 is_rel_prime_iff_contains_int {R : Type*} [integral_domain R] 
    [decidable_eq R] (f g : polynomial R) :
    is_rel_prime f g ↔ ∃ (a : R), a ∈ ideal.span {f, g} 

theorem exercise_11.4.1b
 irreducible_polynomial_x3_6x_12 : irreducible (polynomial.C 12 + 6 * X + X ^ 3) 

theorem exercise_11.4.6a
 irreducible_polynomial_x2_x_1 (p : ℕ) (hp : nat.prime p) :
  irreducible (polynomial.X^2 + polynomial.X + 1) 

theorem exercise_11.4.6b
 irreducible_x_squared_plus_1 (x : ℤ/7) : irreducible (x^2 + 1) 

theorem exercise_11.4.6c
 irreducible_x_3_sub_9 (p : ℕ) (hp : nat.prime p) (h : p > 3) :
  irreducible (polynomial.X^3 - 9) (finite_field.of_nat p) 

theorem exercise_11.4.8
 irreducible_of_prime_power {p : ℕ} (hp : nat.prime p) (n : ℕ) :
    irreducible (polynomial.X ^ n - C p) 

theorem exercise_11.4.10
 irreducible_of_degree_two_n_plus_one_of_nonzero_leading_coeff_of_zero_coeffs_of_nonzero_constant_coeff_of_nonzero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_nonzero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_nonzero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_nonzero_coeffs_of_zero_coeffs_of_non

theorem exercise_11.9.4
 ideal.eq_span_singleton_of_prime_splits_of_not_dvd_of_mem {R : Type*} 
    [comm_ring R] (p : R) (h1 : nat.prime (p.nat_abs)) (h2 : p.nat_abs.prime_splits R) 
    (h3 : ¬ p ∣ p) (h4 : p ∈ p.prime_ideal) :
    p.prime_ideal = ideal.span R {p, p.prime_ideal.gens.1} 

theorem exercise_11.12.3
 exists_int_point_on_ellipse_of_sol_of_x_sq_equiv_neg_5_mod_p 
    (p : ℕ) (hp : nat.prime p) (h : ∃ (x : ℕ), x ^ 2 ≡ -5 [MOD p]) :
    ∃ (x y : ℕ), x ^ 2 + 5 * y ^ 2 = p ∨ 2 * x ^ 2 + 2 * x * y + 3 * y ^ 2 = p 

theorem exercise_11.13.3
 exists_infinitely_many_primes_congr_neg_one_mod_four :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 4] 

theorem exercise_13.1.3
 is_field_of_finite_dimensional_over_field {R : Type*} 
    [integral_domain R] [field F] (hF : subring F R) 
    (hR : finite_dimensional F R) : is_field R 

theorem exercise_13.3.1
 exists_eq_of_degree_eq_of_generates {F : Type*} [field F] 
    {α : F} (hα : is_integral α) (hα2 : is_integral (α ^ 2)) 
    (hα5 : degree F α = 5) (hα2_5 : degree F (α ^ 2) = 5) :
    ∃ (β : F), is_integral β ∧ degree F β = 5 ∧ α = β ^ 2 

theorem exercise_13.3.8
 degree_mul_degree_of_rel_prime {F : Type*} [field F] {K : Type*} 
    [field K] (hK : algebra.is_field_extension F K) (α : K) (β : K) 
    (hα : algebra.is_integral F α) (hβ : algebra.is_integral F β) 
    (hαβ : nat.coprime (algebra.degree F α) (algebra.degree F β)) :
    algebra.degree F K = algebra.degree F α * algebra.degree F β 

theorem exercise_13.4.10
 exists_two_pow_two_pow_k_add_one_of_two_pow_r_add_one_prime 
    (p : ℕ) (hp : nat.prime p) (h : p = 2 ^ nat.find_prime_pow p + 1) :
    ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 

theorem exercise_13.6.10
 prod_nonzero_eq_neg_one {K : Type*} [field K] [fintype K] 
    (hK : ¬ (∃ (x : K), x ≠ 0)) :
    ∏ (x : K), x ≠ 0 → x = -1 