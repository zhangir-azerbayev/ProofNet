import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_30 {n : ℕ} (h : 1 < n) :
  ¬ (int.of_nat (finset.range n).sum (λ (i : ℕ), 1 / (↑i + 1))).is_int :=
sorry

theorem exercise_2_4 {a : ℕ} (ha : a ≠ 0) {m n : ℕ}
  (hmn : m < n) :
  (a ^ (2 ^ n) + 1).gcd (a ^ (2 ^ m) + 1) = 1 ∨
  (a ^ (2 ^ n) + 1).gcd (a ^ (2 ^ m) + 1) = 2 :=
sorry

theorem exercise_2_27a :
  ∀ (n : ℕ), ∃ (m : ℕ), ∀ (k : ℕ), m ≤ k → (∃ (p : ℕ), p.prime ∧ p ∣ k) → 1 / k < 1 / n :=
sorry

theorem exercise_3_4 :
  ∀ (x y : ℤ), 3 * x ^ 2 + 2 ≠ y ^ 2 :=
sorry

theorem exercise_3_10 {n : ℕ} (h : ¬nat.prime n)
  (h4 : n ≠ 4) :
  (n - 1).factorial % n = 0 :=
sorry

theorem exercise_4_4 {R : Type*} [comm_ring R] (p : ℕ)
  [nontrivial R] [h : char_p R p] (hp : p ≠ 2) (a : R)
  (hpa : is_primitive_root a p) :
  is_primitive_root (-a) p ↔ is_primitive_root a p :=
sorry

theorem exercise_4_6 (p : ℕ) (hp : fermat_prime p) :
  primitive_root 3 p :=
sorry

theorem exercise_4_11 {p k : ℕ} (hp : nat.prime p)
  (hk : ¬p - 1 ∣ k) :
  (finset.range (p - 1)).sum (λ (x : ℕ), x ^ k) ≡ 0 [MOD p] :=
sorry

theorem exercise_5_28 {p : ℕ} [fact (nat.prime p)]
  (hp : p % 4 = 1) :
  is_square 2 ↔ ∃ (a b : ℕ), p = a ^ 2 + 64 * b ^ 2 :=
sorry

theorem exercise_12_12 :
	real.sin (real.pi / 12) = real.sqrt (3 + 2 * real.sqrt 2) / 2 :=
sorry