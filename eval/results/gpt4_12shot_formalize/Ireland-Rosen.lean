import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_30 {n : ℕ} (hn : n ≥ 2) :
  ¬(∃ k : ℕ, ∑ i in finset.range n.succ, 1 / (i + 1 : ℝ) = k) :=
sorry

theorem exercise_2_4 {a m n : ℕ} (ha : a ≠ 0) (hn : n > m) :
  nat.gcd (a ^ (2 ^ n) + 1) (a ^ (2 ^ m) + 1) = if a % 2 = 0 then 2 else 1 :=
sorry

theorem exercise_2_27a : ¬summable (λ n, if square_free n then 1 / n else 0) :=
sorry

theorem exercise_3_4 :
  ¬∃ (x y : ℤ), 3 * x^2 + 2 = y^2 :=
sorry

theorem exercise_3_10 {n : ℕ} (hn : n ≠ 1) (hnp : ¬nat.prime n) :
  (n ≠ 4) → (n - 1)! % n = 0 :=
sorry

theorem exercise_4_4
  {p t : ℕ} (hp : nat.prime p) (hpt : p = 4 * t + 1) (a : ℕ) :
  is_primitive_root a p ↔ is_primitive_root (-a % p) p :=
sorry

theorem exercise_4_6 {n p : ℕ}
  (hp : p = 2 ^ n + 1) (hfp : nat.prime p) :
  is_primitive_root 3 p :=
sorry

theorem exercise_4_11 {p k : ℕ} (hp : nat.prime p) (hk : k % (p - 1) ≠ 0) :
  (∑ i in finset.range p, i ^ k) % p = if k % (p - 1) = 0 then p - 1 else 0 :=
sorry

theorem exercise_5_28 {p : ℕ} (hp : nat.prime p) (hp1 : p ≡ 1 [MOD 4]) :
  (∃ x : ℤ, x ^ 4 ≡ 2 [ZMOD p]) ↔ (∃ A B : ℤ, p = A ^ 2 + 64 * B ^ 2) :=
sorry

theorem exercise_12_12 : is_algebraic ℚ (real.sin (real.pi / 12)) :=
sorry