import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_a3 (f : ℝ → ℝ) (hf : continuous f)
  (hf''' : ∀ (x : ℝ), continuous (deriv^[3] f x)) :
  ∃ (a : ℝ), deriv^[3] f a = 0 :=
sorry

theorem exercise_2000_a2 (n : ℕ) :
	∃ (a b c d : ℕ), a ^ 2 + b ^ 2 = n ∧ c ^ 2 + d ^ 2 = n + 1 ∧
		(a + 1) ^ 2 + (b + 1) ^ 2 = n + 2 :=
sorry

theorem exercise_2010_a4 {n : ℕ} (hn : 0 < n) :
	¬nat.prime (10 ^ 10 ^ 10 ^ n + 10 ^ 10 ^ n + 10 ^ n - 1) :=
sorry

theorem exercise_2017_b3 {R : Type u} [semiring R] (f : power_series R)
  (i : ℤ) :
  ↑f.coeff i = ite (i < 0) 0 (⇑(power_series.coeff R i.nat_abs) f) :=
sorry

theorem exercise_2018_b2 {n : ℕ} (h : 0 < n) :
  ∀ (z : ℂ), abs z ≤ 1 → (polynomial.X ^ n - 1).eval z ≠ 0 :=
sorry

theorem exercise_2020_b5 {z : ℂ} (hz : z ≠ 1)
  (h : abs z = 1) : z ≠ 0 :=
sorry