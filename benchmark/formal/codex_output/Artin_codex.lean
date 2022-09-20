theorem exercise_2_2_9 {G : Type*} [group G] {a b : G} 
    (h : a * b = b * a) : is_abelian (subgroup.generated G (a, b)) :=
sorry

theorem exercise_2_3_1 {R : Type*} 
    [ring R] (f : R →+* R) (hf : ∀ x : R, 0 < f x) :
    is_mul_group_hom f :=
sorry

theorem exercise_2_3_2 {G : Type*} [group G] (a b : G) :
    a * b ≈ b * a :=
sorry

theorem exercise_2_4_19 {G : Type*} 
    [group G] (hG : ∃ (x : G), x ≠ 1 ∧ order x = 2) :
    ∃ (x : G), x ≠ 1 ∧ order x = 2 ∧ x ∈ center G :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H] :
    center (G * H) = center G * center H :=
sorry

theorem exercise_2_10_11 
    {G : Type*} [add_group G] {H : Type*} [add_group H] {f : G → H} 
    (hf : is_add_group_hom f) {g : H → G} (hg : is_add_group_hom g) :
    is_add_group_hom (g ∘ f) :=
sorry

theorem exercise_2_11_3 {G : Type*} [group G] 
    (hG : even (card G)) : ∃ (g : G), g ≠ 1 ∧ g^2 = 1 :=
sorry

theorem exercise_3_2_7 {F : Type*} [field F] {G : Type*} [field G]
    (f : F → G) (hf : is_ring_hom f) : injective f :=
sorry

theorem exercise_3_5_6 {V : Type*} [add_comm_group V] [vector_space ℂ V]
    (hV : ∃ (s : set V), countable s ∧ span ℂ s = ⊤) :
    ∃ (s : set V), countable s ∧ is_basis ℂ s :=
sorry

theorem exercise_3_7_2 {V : Type*} [vector_space ℂ V] 
    (hV : ∀ (S : set V), finite S → ∃ (v : V), v ∉ ⋃₀ S) :
    ∀ (S : set V), finite S → ∃ (v : V), v ∉ ⋃₀ S :=
sorry

theorem exercise_6_1_14 (G : Type*) [group G] 
    (Z : set G) (hZ : is_subgroup Z) (hZc : is_center G Z) 
    (hGZ : cyclic (quotient_group.quotient Z)) :
    abelian G :=
sorry

theorem exercise_6_4_2 {p q : ℕ} (hp : nat.prime p) 
    (hq : nat.prime q) (h : p * q ≠ 1) :
    ¬ simple_group (multiplicative (fin (p * q))) :=
sorry

theorem exercise_6_4_3 {p q : ℕ} (hp : nat.prime p) 
    (hq : nat.prime q) (h : p ^ 2 * q ≠ 1) :
    ¬ simple_group (p ^ 2 * q) :=
sorry

theorem exercise_6_4_12 (G : Type*) [group G] (hG : card G = 224) :
  ¬ simple_group G :=
sorry

theorem exercise_6_8_1 {G : Type*} [group G] 
    (a b : G) :
    subgroup.generated G (a :: b :: []) = subgroup.generated G (b * a * b^2 :: b * a * b^3 :: []) :=
sorry

theorem exercise_6_8_4 : 
    free_group 3 :=
sorry

theorem exercise_6_8_6 {G : Type*} [group G] 
    (N : subgroup G) (hN : N.normal) (hG : is_cyclic G) 
    (hGN : is_cyclic (G / N)) :
    ∃ (g h : G), G = ⟨g, h⟩ :=
sorry

theorem exercise_10_1_13 {R : Type*} [ring R] (x : R) (hx : ∃ n : ℕ, x ^ n = 0) :
    is_unit (1 + x) :=
sorry

theorem exercise_10_2_4 (x : polynomial ℤ) : 
    (ideal.span {2}).inter (ideal.span {x}) = ideal.span {2 * x} :=
sorry

theorem exercise_10_6_7 {I : ideal ℤ[i]} 
    (hI : I ≠ ⊥) : ∃ (z : ℤ), z ≠ 0 ∧ z ∈ I :=
sorry

theorem exercise_10_6_16 (f : polynomial ℤ) (a : ℤ) :
  ∃ (g : polynomial ℤ), f = g.eval₂ (λ x y, x - y) a :=
sorry

theorem exercise_10_3_24a {R : Type*} [comm_ring R] (I J : ideal R) :
    ¬ (I.is_ideal ∧ J.is_ideal) → ¬ (I.is_ideal ∧ J.is_ideal ∧ (I ∪ J).is_ideal) :=
sorry

theorem exercise_10_4_6 {R : Type*} [comm_ring R] 
    (I J : ideal R) (x : R) (hx : x ∈ I ∩ J) :
  is_nilpotent (x + I J) :=
sorry

theorem exercise_10_4_7a {R : Type*} [comm_ring R] 
    (I J : ideal R) (hIJ : I + J = ⊤) : I * J = I ∩ J :=
sorry

theorem exercise_10_5_16 {R : Type*} 
    [comm_monoid_with_zero R] (f : nat.arithmetic_function R) 
    (hf : f.is_multiplicative) (g : nat.arithmetic_function R) 
    (hg : g.is_multiplicative) :
    f = g ↔ ∀ (p i : ℕ), nat.prime p → f (p ^ i) = g (p ^ i) :=
sorry

theorem exercise_10_7_6 {α : Type*} 
    [comm_ring α] [fintype α] [decidable_eq α] (p : polynomial α) 
    (hp : irreducible p) :
    is_field (polynomial.quotient p) :=
sorry

theorem exercise_10_7_10 {R : Type*} [ring R] 
    (M : ideal R) (hM : ∀ (x : R), x ∉ M → is_unit x) :
    is_maximal M ∧ ∀ (N : ideal R), is_maximal N → N = M :=
sorry

theorem exercise_11_2_13 {a b : ℤ} (h : a ∣ᵤ b) : a ∣ b :=
sorry

theorem exercise_11_3_1 {α : Type*} 
    [field α] {a b : α} (h : a ≠ 0) (f : polynomial α) :
    irreducible f ↔ irreducible (f.linear_substitution a b) :=
sorry

theorem exercise_11_3_2 {F : Type*} 
    [field F] (f g : polynomial F) (h : ∃ (h : polynomial F), 
    is_factor_of h f ∧ is_factor_of h g) :
    ∃ (h : polynomial F), is_factor_of h f ∧ is_factor_of h g :=
sorry

theorem exercise_11_3_4 {R : Type*} [integral_domain R] 
    [decidable_eq R] (f g : polynomial R) :
    is_rel_prime f g ↔ ∃ (a : R), a ∈ ideal.span {f, g} :=
sorry

theorem exercise_11_4_1b : irreducible (polynomial.C 12 + 6 * X + X ^ 3) :=
sorry

theorem exercise_11_4_6a : irreducible (polynomial.C (2 : ℤ)) (X^2 + X + 1) :=
sorry

theorem exercise_11_4_6b (x : ℤ/7) : irreducible (x^2 + 1) :=
sorry

theorem exercise_11_4_6c : irreducible (X^3 - 9 : polynomial ℤ_31) :=
sorry

theorem exercise_11_4_8 {p : ℕ} (hp : nat.prime p) (n : ℕ) :
    irreducible (polynomial.X ^ n - C p) :=
sorry

theorem exercise_11_4_10 irreducible_of_degree_two_n_plus_one_of_nonzero_leading_coeff_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant_coeff_of_zero_coeffs_of_zero_coeffs_of_nonzero_constant :=
sorry

theorem exercise_11_9_4 {R : Type*} 
    [comm_ring R] (p : R) (h : nat.prime (p.nat_abs)) (hp : p.nat_abs.prime_splits) 
    (α : R) (hα : p ∣ α → p ∣ 1) :
    ideal.generated_by (p, α) = ideal.mul (ideal.generated_by p) (ideal.generated_by α) :=
sorry

theorem exercise_11_12_3 
    (p : ℕ) (hp : nat.prime p) (h : ∃ (x : ℤ), x ^ 2 ≡ -5 [MOD p]) :
    ∃ (x y : ℤ), x ^ 2 + 5 * y ^ 2 = p ∨ 2 * x ^ 2 + 2 * x * y + 3 * y ^ 2 = p :=
sorry

theorem exercise_11_13_3 :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 4] :=
sorry

theorem exercise_13_1_3 {R : Type*} 
    [integral_domain R] [field F] (hF : subring F R) 
    (hR : finite_dimensional F R) : is_field R :=
sorry

theorem exercise_13_3_1 {F : Type*} [field F] 
    {α : F} (hα : is_integral α) (hα2 : is_integral (α ^ 2)) 
    (hα5 : degree F α = 5) (hα2_5 : degree F (α ^ 2) = 5) :
    ∃ (β : F), is_integral β ∧ degree F β = 5 ∧ α = β ^ 2 :=
sorry

theorem exercise_13_3_8 {F : Type*} [field F] {K : Type*} 
    [field K] (hK : algebra.is_field_extension F K) (α : K) (β : K) 
    (hα : algebra.is_integral F α) (hβ : algebra.is_integral F β) 
    (hαβ : nat.coprime (algebra.degree F α) (algebra.degree F β)) :
    algebra.degree F K = algebra.degree F α * algebra.degree F β :=
sorry

theorem exercise_13_4_10 
    (p : ℕ) (hp : nat.prime p) (h : p = 2 ^ p.nat_abs + 1) :
    ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] [fintype K] 
    (hK : ¬ (∃ (x : K), x ≠ 0 ∧ x ≠ 1)) : 
    ∏ (x : K), x ≠ 0 → x = -1 :=
sorry