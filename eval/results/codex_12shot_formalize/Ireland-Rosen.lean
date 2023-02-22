import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_30 {n : ℕ} (h : ∑ i in finset.range n, 1 / (i + 1) < 1) :
  ¬ (∑ i in finset.range n, 1 / (i + 1)) ∈ ℤ :=
sorry

theorem exercise_2_4 (a : ℤ) (h : a ≠ 0) (m n : ℕ) (hmn : m < n) :
  gcd (a ^ (2 ^ n) + 1) (a ^ (2 ^ m) + 1) = 1 ∨ gcd (a ^ (2 ^ n) + 1) (a ^ (2 ^ m) + 1) = 2 :=
sorry

theorem exercise_2_27a :
  ∀ (n : ℕ), ∃ (m : ℕ), ∑ i in finset.range m, 1 / (i : ℕ) > n :=
sorry

theorem exercise_3_4 :
  ∀ (x y : ℤ), 3 * x ^ 2 + 2 ≠ y ^ 2 :=
sorry

theorem exercise_3_10 {n : ℕ} (h : ¬ nat.prime n) :
  (n - 1)! % n = 0 :=
sorry

theorem exercise_4_4 {p : ℕ} (hp : nat.prime p)
  (h : p % 4 = 1) (a : ℕ) (ha : nat.coprime a p) :
  nat.is_primitive_root a p ↔ nat.is_primitive_root (-a) p :=
sorry

theorem exercise_4_6 (p : ℕ) (hp : nat.prime p) 
  (h : p = 2 ^ nat.find_prime p + 1) :
  nat.primitive_root 3 p :=
sorry

theorem exercise_4_11 {p k : ℕ} (hp : nat.prime p) 
  (hk : ¬p - 1 ∣ k) :
  (∑ i in finset.range (p - 1), i ^ k) % p = 0 :=
sorry

theorem exercise_5_28  (p : ℕ) (hp : nat.prime p) (h : p % 4 = 1) :
  ∃ (x : ℕ), x ^ 4 ≡ 2 [MOD p] ↔
  ∃ (A B : ℕ), p = A ^ 2 + 64 * B ^ 2 :=
sorry

theorem exercise_12_12 : algebraic ℂ (sin (π / 12)) :=
sorry