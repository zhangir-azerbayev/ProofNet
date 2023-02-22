import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_30 :=
sorry

theorem exercise_2_4 {a : ℕ} (n m : ℕ) :
  odd a = 1 ↔ a ^ 2 ^ n = 1 ∧ even a = 1 ↔ a ^ 2 ^ m = 1 :=
sorry

theorem exercise_2_27a {n : ℕ} (h : ∃ m : ℕ, m^2 ≠ 1) :
  ∃ (f : ℕ → ℕ), ∑ n.1 / n = ∑ f n.1 :=
sorry

theorem exercise_3_4 :
  ∀ (x : ℤ), 3 x ^ 2 + 2 = y ^ 2 → x = 0 ∨ x = 1 :=
sorry

theorem exercise_3_10 [nat.prime n] :
  (n-1)! ≡ 0 n (n = 4) :=
sorry

theorem exercise_4_4 {p : ℕ} [p : ℕ]
  (a : p = 4 t + 1) :
  primitive_root_mod p ↔ -a is primitive_root_mod p :=
sorry

theorem exercise_4_6 {p : ℕ} [prime p] (n : ℕ) : 3 ∣ (p - 1) ^ n :=
sorry

theorem exercise_4_11 {p : ℕ} [p_mod_p] [p_mod_p]
  (k : ℕ) (h : k ≡ 0 (p-1)) :
  1^k + 2^k + \cdots + (p-1)^k ≡ 0 (p) :=
sorry

theorem exercise_5_28 :=
sorry

theorem exercise_12_12 :
  ∃ (a : ℚ), √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1 + √(1:=
sorry