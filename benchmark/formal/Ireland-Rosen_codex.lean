theorem exercise_1.27
 eight_dvd_pow_two_sub_one_of_odd (n : ℕ) (h : nat.odd n) :
    8 ∣ n ^ 2 - 1 

theorem exercise_1.30
 not_int_sum_inv_nat_lt_n (n : ℕ) : 
    ¬ (∃ (k : ℤ), ∀ (m : ℕ), m < n → (1 : ℚ) / m = k) 

theorem exercise_1.31
 exists_int_of_dvd_int_of_dvd_int {a b : ℤ} (h : a ∣ b) (h' : a ∣ b) :
  ∃ (c : ℤ), a * c = b 

theorem exercise_2.4
 gcd_of_pow_two_add_one_eq_one_or_two (a : ℕ) (h : a ≠ 0) (n m : ℕ) 
    (hnm : n > m) :
    nat.gcd (a ^ (2 ^ n) + 1) (a ^ (2 ^ m) + 1) = 1 ∨ nat.gcd (a ^ (2 ^ n) + 1) (a ^ (2 ^ m) + 1) = 2 

theorem exercise_2.21
 sum_mu_log_eq_wedge (n : ℕ) :
    ∑ d in divisors n, mu (n / d) * log d = log_wedge n 

theorem exercise_2.27a
 sum_of_inverse_of_square_free_diverges : 
    ∀ (n : ℕ), ∃ (m : ℕ), ∑ i in (square_free_nats m), 1 / i > n 

theorem exercise_3.1
 exists_infinitely_many_primes_congr_neg_one_mod_six :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 6] 

theorem exercise_3.4
 no_solution_of_3x2_plus_2_eq_y2 :
  ∀ (x y : ℤ), 3 * x ^ 2 + 2 ≠ y ^ 2 

theorem exercise_3.5
 no_solution_in_integers (x y : ℤ) : 7 * x ^ 3 + 2 ≠ y ^ 3 

theorem exercise_3.10
 not_prime_factorial_equiv_zero_mod_n (n : ℕ) (hn : ¬ nat.prime n) :
    (n - 1)! ≡ 0 [MOD n] 

theorem exercise_3.14
 fermat_little_theorem_odd_prime_powers {p q : ℕ} (hp : nat.prime p) 
    (hq : nat.prime q) (hpq : p ≠ q) (hp1 : (q - 1) % (p - 1) = 0) 
    (hcop : nat.coprime n (p * q)) :
    nat.pow n (q - 1) ≡ 1 [MOD p * q] 

theorem exercise_3.18
 chinese_remainder_theorem_nat {n : ℕ} (n_pos : 0 < n) 
    (f : ℕ → ℤ) (hf : ∀ x, f x ≡ 0 [ZMOD n]) :
    ∃ x, f x = 0 

theorem exercise_3.20
 pow_two_mod_two_pow_eq_one_of_pos {b : ℕ} (hb : b > 0) :
    ∃ (x : ℕ), x ^ 2 ≡ 1 [MOD 2 ^ b] 

theorem exercise_4.4
 is_primitive_root_iff_neg_is_primitive_root {p : ℕ} 
    (hp : nat.prime p) (h4t1 : p % 4 = 1) (a : ℕ) (ha : nat.coprime a p) :
    nat.is_primitive_root a p ↔ nat.is_primitive_root (-a) p 

theorem exercise_4.5
 primitive_root_iff_neg_has_order_two {p : ℕ} (hp : nat.prime p) 
    (h4t3 : p % 4 = 3) (a : ℕ) (ha : nat.coprime a p) :
    nat.primitive_root p a ↔ nat.order p (-a) = (p - 1) / 2 

theorem exercise_4.6
 fermat_prime_primitive_root_3 (n : ℕ) (h : 2 ^ n + 1 > 2) :
  ∃ (p : ℕ), nat.prime p ∧ p = 2 ^ n + 1 ∧ nat.is_primitive_root 3 p 

theorem exercise_4.8
 is_primitive_root_iff_not_congr_pow_prime_divisor {p : ℕ} 
    (hp : nat.prime p) (hodd : p % 2 = 1) (a : ℕ) (ha : nat.coprime a p) :
    nat.is_primitive_root a p ↔ ∀ (q : ℕ), nat.prime q ∧ q ∣ p - 1 → 
    ¬ (a ^ ((p - 1) / q) ≡ 1 [MOD p]) 

theorem exercise_4.9
 prod_primitive_roots_eq_neg_one_pow_phi_sub_one {p : ℕ} 
    (hp : nat.prime p) :
    (∏ (x : ℕ), x ∈ primitive_roots p → x) ≡ (-1 : ℤ) ^ (nat.phi (p - 1)) [MOD p] 

theorem exercise_4.10
 sum_primitive_roots_eq_mu_p_sub_one_mod_p (p : ℕ) (hp : nat.prime p) :
  (∑ x in primitive_roots p, x) ≡ nat.mu (p - 1) [MOD p] 

theorem exercise_4.11
 sum_of_powers_congr_zero_or_neg_one (p k : ℕ) (hp : nat.prime p) 
    (hk : p - 1 ∣ k) :
    (∑ i in finset.range (p - 1), i ^ k) ≡ -1 [MOD p] 

theorem exercise_4.22
 order_mul_eq_order_mul_order {p : ℕ} (hp : nat.prime p) (a : ℕ) 
    (ha : nat.coprime a p) (h3 : nat.order p a = 3) :
    nat.order p (1 + a) = 6 

theorem exercise_4.24
 eq_card_of_eq_card_of_prime_powers {p : ℕ} (hp : nat.prime p) 
    (a b c : ℕ) (m n : ℕ) :
    (a * m + b * n) % p = c % p →
    (a * nat.gcd m (p - 1) + b * nat.gcd n (p - 1)) % p = c % p 

theorem exercise_5.2
 quadratic_residue_count {p : ℕ} (hp : nat.prime p) (a : ℕ) 
    (ha : a % p ≠ 0) :
    (quadratic_residue_count_mod p a : ℕ) = 1 + (a / p) 

theorem exercise_5.3
 quadratic_residue_number_of_solutions_of_quadratic_congruence 
    (p a b c : ℕ) (hp : nat.prime p) (hpa : p ∤ a) :
    nat.quadratic_residue (b ^ 2 - 4 * a * c) p = 
    1 + nat.quadratic_residue (b ^ 2 - 4 * a * c) p 

theorem exercise_5.4
 sum_of_fractions_eq_zero {p : ℕ} (hp : nat.prime p) :
    ∑ a in finset.range (p - 1), a / p = 0 

theorem exercise_5.5
 sum_eq_zero_of_coprime {p : ℕ} (hp : nat.prime p) {a b x : ℕ} 
    (hx : x % p = 0) (ha : nat.coprime a p) :
    (∑ i in finset.range p, (a * i + b) / p) = 0 

theorem exercise_5.6
 sum_of_solutions_to_quadratic_residue (p : ℕ) (a : ℕ) 
    (hp : nat.prime p) :
    ∑ y in finset.range p, 1 + nat.legendre (y ^ 2 + a) p = 
    ∑ y in finset.range p, 1 + nat.legendre (y ^ 2 + a) p 

theorem exercise_5.7
 quadratic_residue.card_solutions_of_coprime_a {p : ℕ} (hp : nat.prime p) 
    (a : ℕ) (ha : a % p ≠ 0) :
    (quadratic_residue.solutions_of_coprime_a p a).card = p - 1 

theorem exercise_5.13
 prime_divisor_of_x4_x2_1_is_cong_1_mod_12 (x : ℕ) :
    ∀ (p : ℕ), nat.prime p → p ∣ x ^ 4 - x ^ 2 + 1 → p ≡ 1 [MOD 12] 

theorem exercise_5.27
 quadratic_residue_iff_square_eq_one_mod_p {p : ℕ} (hp : nat.prime p) 
    (a b : ℕ) (h : b ≡ a * nat.legendre p a) :
    nat.legendre p a = 1 ↔ nat.legendre p (a * a) = 1 

theorem exercise_5.28
 exists_sol_of_p_equiv_1_mod_4 {p : ℕ} (hp : nat.prime p) 
    (h1 : p % 4 = 1) :
    ∃ (x : ℕ), x ^ 4 ≡ 2 [MOD p] 

theorem exercise_5.37
 eq_of_dvd_of_dvd_of_dvd_of_dvd_of_neg {a p q : ℕ} 
    (h1 : p ≡ q [MOD 4 * a]) (h2 : p ∣ a) (h3 : q ∣ a) (h4 : a < 0) :
    a / p = a / q 

theorem exercise_6.18
 exists_algebraic_of_degree_gt {n : ℕ} (hn : 0 < n) :
    ∃ (x : ℚ), algebraic ℚ x ∧ degree x > n 

theorem exercise_7.6
 not_square_of_not_square_of_degree_three {F : Type*} [field F] 
    {K : Type*} [field K] (hK : finite_dimensional ℚ F K) 
    (hKF : degree ℚ F K = 3) (hα : ¬ is_square F α) : ¬ is_square K α 

theorem exercise_7.24
 exists_polynomial_of_additive {p : ℕ} (h : p.prime) 
    (f : polynomial ℤ) (hf : f.is_additive) :
    ∃ (g : polynomial ℤ), f = g 

theorem exercise_12.12
 sin_pi_12_is_algebraic : algebraic ℂ (sin (π / 12)) 

theorem exercise_12.19
 finite_integral_domain_is_field (R : Type*) [integral_domain R] 
    [fintype R] : field R 

theorem exercise_12.22
 exists_extensions_of_isomorphism_of_algebraic_number_fields 
    (F E : Type*) [field F] [field E] [algebra F E] [algebra ℂ E] 
    (f : F ≃ₐ[F] ℂ) :
    ∃ (g : E ≃ₐ[E] ℂ), ∀ (h : E ≃ₐ[E] ℂ), h ∘ f = g → h = g 

theorem exercise_12.30
 frobenius_automorphism_of_sqrt_p (p q : ℕ) (hp : nat.prime p) 
    (hq : nat.prime q) (hqp : q ≠ p) :
    ∃ (f : units (ℤ[√p])), f = (p / q) * √p 

theorem exercise_18.1
 no_integral_solution_165x2_21y2_19 : 
    ∀ (x y : ℤ), 165 * x ^ 2 - 21 * y ^ 2 = 19 → false 

theorem exercise_18.4
 exists_sum_of_two_cubes_in_two_ways (n : ℕ) :
    ∃ (a b c d : ℕ), a ^ 3 + b ^ 3 = c ^ 3 + d ^ 3 ∧ a ≠ b ∧ c ≠ d ∧ a ^ 3 + b ^ 3 = n 

theorem exercise_18.32
 coprime_of_square_free_of_y_squared_eq_x_cubed_sub_d 
    (d : ℕ) (h1 : d % 4 = 1 ∨ d % 4 = 2) (h2 : nat.prime_factors d = []) 
    (x y : ℕ) (h3 : y ^ 2 = x ^ 3 - d) :
    nat.coprime x (2 * d) 