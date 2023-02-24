import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_27 (n : ℕ) (h : n % 2 = 1) :
  8 ∣ n ^ 2 - 1 :=
sorry

theorem exercise_1_31 {R : Type*} [comm_ring R] 
  [decidable_eq R] (i : R) (hi : i^2 + 1 = 0) : 2 ∣ i^2 + 1 :=
sorry

theorem exercise_2_21 {n : ℕ} (hn : n ≠ 0) :
  ∑ d in divisors n, nat.mu (n / d) * log d = log_pow n :=
sorry

theorem exercise_3_1 :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 6] :=
sorry

theorem exercise_3_5 :
  ∀ (x y : ℤ), 7 * x ^ 3 + 2 ≠ y ^ 3 :=
sorry

theorem exercise_3_14 {p q n : ℕ}
  (hp : nat.prime p) (hq : nat.prime q) (hpq : p ≠ q) (hpqc : nat.coprime n (p * q))
  (h : p - 1 ∣ q - 1) :
  n ^ (q - 1) ≡ 1 [MOD p * q] :=
sorry

theorem exercise_4_5 {p : ℕ} (hp : nat.prime p)
  (h : p % 4 = 3) (a : ℤ) (ha : nat.coprime a p) :
  nat.is_primitive_root a p ↔ nat.order (-a) p = (p - 1) / 2 :=
sorry

theorem exercise_4_8  {p : ℕ} (hp : nat.prime p) (h : p % 2 = 1) {a : ℕ} (ha : nat.coprime a p)
  (h1 : ∀ (q : ℕ), nat.prime q → q ∣ p - 1 → a ^ ((p - 1) / q) % p ≠ 1) :
  is_primitive_root a p :=
sorry

theorem exercise_5_13 (x : ℤ) 
  (hx : x ^ 4 - x ^ 2 + 1 ≠ 0) :
  ∀ (p : ℕ), nat.prime p → p ∣ x ^ 4 - x ^ 2 + 1 → p ≡ 1 [MOD 12] :=
sorry

theorem exercise_5_37 {a p q : ℤ} (ha : a < 0) 
  (h : p ≡ q [MOD 4 * a]) (hp : ¬p ∣ a) :
  a / p = a / q :=
sorry

theorem exercise_18_4 (n : ℕ) :
  ∃ (a b c d : ℕ), a ^ 3 + b ^ 3 = c ^ 3 + d ^ 3 ∧ a ≠ b ∧ c ≠ d ∧ a ^ 3 + b ^ 3 = n :=
sorry