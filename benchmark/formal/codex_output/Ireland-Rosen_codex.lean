theorem exercise_1_27 (n : ℕ) (h : nat.odd n) : 
    8 ∣ n ^ 2 - 1 :=
sorry

theorem exercise_1_30 (n : ℕ) : 
    ¬ (∃ (k : ℤ), ∀ (m : ℕ), m < n → (1 : ℚ) / m = k) :=
sorry

theorem exercise_1_31 {a b : ℤ} (h : a ∣ b) (h' : a ∣ b) :
  ∃ (c : ℤ), a * c = b :=
sorry

theorem exercise_2_4 (a : ℕ) (h : a ≠ 0) (n m : ℕ) 
    (hnm : n > m) :
    nat.gcd (a ^ (2 ^ n) + 1) (a ^ (2 ^ m) + 1) = 1 ∨ nat.gcd (a ^ (2 ^ n) + 1) (a ^ (2 ^ m) + 1) = 2 :=
sorry

theorem exercise_2_21 (n : ℕ) :
    ∑ d in divisors n, mu (n / d) * log d = log_wedge n :=
sorry

theorem exercise_2_27a : 
    ∀ (n : ℕ), ∃ (m : ℕ), ∑ i in (square_free_nats m), 1 / i > n :=
sorry

theorem exercise_3_1 :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 6] :=
sorry

theorem exercise_3_4 :
  ∀ (x y : ℤ), 3 * x ^ 2 + 2 ≠ y ^ 2 :=
sorry

theorem exercise_3_5 (x y : ℤ) : 7 * x ^ 3 + 2 ≠ y ^ 3 :=
sorry

theorem exercise_3_10 (n : ℕ) (hn : ¬ nat.prime n) :
    (n - 1)! % n = 0 :=
sorry

theorem exercise_3_14 {p q : ℕ} (hp : nat.prime p) 
    (hq : nat.prime q) (hpq : p ≠ q) (hp1 : (q - 1) % (p - 1) = 0) 
    (hcop : nat.coprime n (p * q)) :
    nat.pow n (q - 1) ≡ 1 [MOD p * q] :=
sorry

theorem exercise_3_18 {n : ℕ} (n_pos : 0 < n) 
    (f : ℕ → ℤ) (hf : ∀ x, f x ≡ 0 [ZMOD n]) :
    ∃ x, f x = 0 :=
sorry

theorem exercise_3_20 {b : ℕ} (hb : b ≥ 3) :
    ∃ (x : ℤ), x ^ 2 ≡ 1 [ZMOD (2 ^ b)] ∨ x ^ 2 ≡ 2 [ZMOD (2 ^ b)] ∨ x ^ 2 ≡ 4 [ZMOD (2 ^ b)] :=
sorry

theorem exercise_4_4 {p : ℕ} 
    (hp : nat.prime p) (h4t1 : p % 4 = 1) (a : ℕ) (ha : nat.coprime a p) :
    nat.is_primitive_root a p ↔ nat.is_primitive_root (-a) p :=
sorry

theorem exercise_4_5 {p : ℕ} (hp : nat.prime p) 
    (h4t3 : p % 4 = 3) (a : ℕ) (ha : nat.coprime a p) :
    nat.primitive_root p a ↔ nat.order p (-a) = (p - 1) / 2 :=
sorry

theorem exercise_4_6 (n : ℕ) (h : nat.prime (2^n + 1)) :
    nat.primitive_root 3 (2^n + 1) :=
sorry

theorem exercise_4_8 {p : ℕ} 
    (hp : nat.prime p) (hodd : p % 2 = 1) (a : ℕ) (ha : nat.coprime a p) :
    nat.is_primitive_root a p ↔ ∀ (q : ℕ), nat.prime q ∧ q ∣ p - 1 → 
    ¬ (a ^ ((p - 1) / q) ≡ 1 [MOD p]) :=
sorry

theorem exercise_4_9 {p : ℕ} 
    (hp : nat.prime p) :
    (∏ (x : ℕ), x ∈ primitive_roots p → x) ≡ (-1 : ℤ) ^ (nat.phi (p - 1)) [MOD p] :=
sorry

theorem exercise_4_10 (p : ℕ) (hp : nat.prime p) :
  (∑ x in primitive_roots p, x) ≡ μ (p - 1) [MOD p] :=
sorry

theorem exercise_4_11 (p k : ℕ) (hp : nat.prime p) 
    (hk : p - 1 ∣ k) :
    (∑ i in finset.range (p - 1), i ^ k) ≡ -1 [MOD p] :=
sorry

theorem exercise_4_22 {p : ℕ} (hp : nat.prime p) (a : ℕ) 
    (ha : nat.coprime a p) (h3 : nat.order p a = 3) :
    nat.order p (1 + a) = 6 :=
sorry

theorem exercise_4_24 (a b c : ℕ) (m n : ℕ) (p : ℕ) (hp : nat.prime p) :
    number_of_solutions_congruence_equation_mod_p a b c m n p hp = 
    number_of_solutions_congruence_equation_mod_p a b c (nat.gcd m (p - 1)) (nat.gcd n (p - 1)) p hp :=
sorry

theorem exercise_5_2 {p : ℕ} (hp : nat.prime p) (a : ℕ) 
    (ha : a % p ≠ 0) :
    (quadratic_residue_count_mod p a : ℕ) = 1 + (a / p) :=
sorry

theorem exercise_5_3 
    (p a b c : ℕ) (hp : nat.prime p) (hpa : p ∤ a) :
    nat.quadratic_residue (b ^ 2 - 4 * a * c) p = 
    1 + nat.quadratic_residue (b ^ 2 - 4 * a * c) p :=
sorry

theorem exercise_5_4 {p : ℕ} (hp : nat.prime p) :
    ∑ a in finset.range (p - 1), a / p = 0 :=
sorry

theorem exercise_5_5 {p : ℕ} (hp : nat.prime p) {a b x : ℕ} 
    (hx : x % p = 0) (ha : nat.coprime a p) :
    (∑ i in finset.range p, (a * i + b) / p) = 0 :=
sorry

theorem exercise_5_6 (p : ℕ) 
    (hp : nat.prime p) (a : ℕ) (ha : a % p ≠ 0) :
    ∑ y in finset.range p, legendre_symbol (a % p) p = 
    ∑ y in finset.range p, jacobi_symbol (a % p) p :=
sorry

theorem exercise_5_7 {p : ℕ} (hp : nat.prime p) 
    (a : ℕ) (ha : a % p ≠ 0) :
    (quadratic_residue.solutions_of_coprime_a p a).card = p - 1 :=
sorry

theorem exercise_5_13 (x : ℕ) :
    ∀ (p : ℕ), nat.prime p → p ∣ x ^ 4 - x ^ 2 + 1 → p ≡ 1 [MOD 12] :=
sorry

theorem exercise_5_27 {p : ℕ} (hp : nat.prime p) 
    (a b : ℕ) (h : b ≡ a * nat.legendre p a) :
    nat.legendre p a = 1 ↔ nat.legendre p (a * a) = 1 :=
sorry

theorem exercise_5_28 {p : ℕ} (hp : nat.prime p) 
    (h1 : p % 4 = 1) :
    ∃ (x : ℕ), x ^ 4 ≡ 2 [MOD p] :=
sorry

theorem exercise_5_37 {a p q : ℕ} 
    (h1 : p ≡ q [MOD 4 * a]) (h2 : p ∣ a) (h3 : q ∣ a) (h4 : a < 0) : 
    a / p = a / q :=
sorry

theorem exercise_6_18 {n : ℕ} (hn : 0 < n) :
    ∃ (x : ℚ), algebraic ℚ x ∧ degree x > n :=
sorry

theorem exercise_7_6 {F : Type*} [field F] 
    {K : Type*} [field K] (hK : finite_dimensional ℚ F K) 
    (hKF : degree ℚ F K = 3) (hα : ¬ is_square F α) : ¬ is_square K α :=
sorry

theorem exercise_7_24 {p : ℕ} (h : p.prime) 
    (f : polynomial ℤ) (hf : f.is_additive) :
    ∃ (g : polynomial ℤ), f = g :=
sorry

theorem exercise_12_12 : algebraic ℂ (sin (π / 12)) :=
sorry

theorem exercise_12_19 (R : Type*) [integral_domain R] 
    [fintype R] : field R :=
sorry

theorem exercise_12_22 
    (F E : Type*) [field F] [field E] [algebra F E] [algebra ℂ E] 
    (f : F ≃ₐ[F] ℂ) :
    ∃ (g : E ≃ₐ[E] ℂ), ∀ (h : E ≃ₐ[E] ℂ), h ∘ f = g → h = g :=
sorry

theorem exercise_12_30 (p q : ℕ) (hp : nat.prime p) 
    (hq : nat.prime q) (hqp : q ≠ p) :
    ∃ (f : units (ℤ[√p])), f = (p / q) * √p :=
sorry

theorem exercise_18_1 : 
    ∀ (x y : ℤ), 165 * x ^ 2 - 21 * y ^ 2 = 19 → false :=
sorry

theorem exercise_18_4 (n : ℕ) :
    ∃ (a b c d : ℕ), a ^ 3 + b ^ 3 = c ^ 3 + d ^ 3 ∧ a ≠ b ∧ c ≠ d ∧ a ^ 3 + b ^ 3 = n :=
sorry

theorem exercise_18_32 
    (d : ℕ) (h1 : d % 4 = 1 ∨ d % 4 = 2) (h2 : nat.prime_factors d = []) 
    (x y : ℕ) (h3 : y ^ 2 = x ^ 3 - d) :
    nat.coprime x (2 * d) :=
sorry