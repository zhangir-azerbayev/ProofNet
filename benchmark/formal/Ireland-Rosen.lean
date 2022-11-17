import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory

theorem exercise_1_27 {n : ℕ} (hn : odd n) : 8 ∣ (n^2 - 1) :=
sorry 

theorem exercise_1_30 {n : ℕ} : 
  ¬ ∃ a : ℤ, ∑ (i : fin n), (1 : ℚ) / (n+2) = a :=
sorry 

theorem exercise_1_31  : (⟨1, 1⟩ : gaussian_int) ^ 2 ∣ 2 := 
sorry 

theorem exercise_2_4 {a : ℤ} (ha : a ≠ 0) 
  (f_a := λ n m : ℕ, int.gcd (a^(2^n) + 1) (a^(2^m)+1)) {n m : ℕ} 
  (hnm : n > m) : 
  (odd a → f_a n m = 1) ∧ (even a → f_a n m = 2) :=
sorry 

theorem exercise_2_21 {l : ℕ → ℝ} 
  (hl : ∀ p n : ℕ, p.prime → l (p^n) = log p )
  (hl1 : ∀ m : ℕ, ¬ is_prime_pow m → l m = 0) :
  l = λ n, ∑ d : divisors n, moebius (n/d) * log d  := 
sorry 

theorem exercise_2_27a : 
  ¬ summable (λ i : {p : ℤ // squarefree p}, (1 : ℚ) / i) :=
sorry 

theorem exercise_3_1 : infinite {p : primes // p ≡ -1 [ZMOD 6]} :=
sorry 

theorem exercise_3_4 : ¬ ∃ x y : ℤ, 3*x^2 + 2 = y^2 :=
sorry 

theorem exercise_3_5 : ¬ ∃ x y : ℤ, 7*x^3 + 2 = y^3 :=
sorry 

theorem exercise_3_10 {n : ℕ} (hn0 : ¬ n.prime) (hn1 : n ≠ 4) : 
  factorial (n-1) ≡ 0 [MOD n] :=
sorry 

theorem exercise_3_14 {p q n : ℕ} (hp0 : p.prime ∧ p > 2) 
  (hq0 : q.prime ∧ q > 2) (hpq0 : p ≠ q) (hpq1 : p - 1 ∣ q - 1)
  (hn : n.gcd (p*q) = 1) : 
  n^(q-1) ≡ 1 [MOD p*q] :=
sorry 

theorem exercise_4_4 {p t: ℕ} (hp0 : p.prime) (hp1 : p = 4*t + 1) 
  (a : zmod p): 
  is_primitive_root a p ↔ is_primitive_root (-a) p :=
sorry 

theorem exercise_5_13 {p x: ℤ} (hp : prime p) 
  (hpx : p ∣ (x^4 - x^2 + 1)) : p ≡ 1 [ZMOD 12] :=
sorry 

theorem exercise_5_28 {p : ℕ} (hp : p.prime) (hp1 : p ≡ 1 [MOD 4]): 
  ∃ x, x^4 ≡ 2 [MOD p] ↔ ∃ A B, p = A^2 + 64*B^2 :=
sorry 

theorem exercise_5_37 {p q : ℕ} [fact(p.prime)] [fact(q.prime)] {a : ℤ}
  (ha : a < 0) (h0 : p ≡ q [ZMOD 4*a]) (h1 : ¬ ((p : ℤ) ∣ a)) :
  legendre_sym p a = legendre_sym q a :=
sorry 

theorem exercise_18_1 : ¬ ∃ x y : ℤ, 165*x^2 - 21*y^2 = 19 := 
sorry 

theorem exercise_12_12 : is_algebraic ℚ (sin (real.pi/12)) :=
sorry 

theorem exercise_18_4 {n : ℕ} (hn : ∃ x y z w : ℤ, 
  x^3 + y^3 = n ∧ z^3 + w^3 = n ∧ x ≠ z ∧ x ≠ w ∧ y ≠ z ∧ y ≠ w) : 
  n ≥ 1729 :=
sorry 




