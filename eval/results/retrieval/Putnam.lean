import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_b6 {a b c : ℤ} :
  ∃ (n : ℕ), ¬(∃ (x : ℤ), (↑n ^ 3 + a * ↑n ^ 2 + b * ↑n + c) ^ 2 = x ^ 2) :=
sorry

theorem exercise_1999_b4 {f : ℝ → ℝ} (hf : continuous f)
  (hf' : differentiable ℝ f) (hf'' : differentiable ℝ (deriv f))
  (hf''' : differentiable ℝ (deriv^[2] f))
  (hf_pos : ∀ (x : ℝ), 0 < f x) (hf'_pos : ∀ (x : ℝ), 0 < deriv f x)
  (hf''_pos : ∀ (x : ℝ), 0 < deriv^[2] f x)
  (hf'''_pos : ∀ (x : ℝ), 0 < deriv^[3] f x)
  (hf'''_le : ∀ (x : ℝ), deriv^[3] f x ≤ f x) :
  ∀ (x : ℝ), deriv f x < 2 * f x :=
sorry

theorem exercise_2001_a5 {a n : ℕ} :
  a ^ (n + 1) - (a + 1) ^ n = 2001 ↔ (∃ (a n : ℕ), a ^ (n + 1) - (a + 1) ^ n = 2001 ∧ ∀ (a' n' : ℕ), a ^ (n + 1) - (a + 1) ^ n = 2001 → a = a' ∧ n = n') :=
sorry

theorem exercise_2014_a5 {α : Sort u} {p : α → Prop}
  (h : ∃! (x : α), p x) :
  ∃ (x : α), p x :=
sorry

theorem exercise_2018_a5 {f : ℝ → ℝ} (hf : ∀ x, f x ≥ 0)
  (hf0 : f 0 = 0) (hf1 : f 1 = 1) (hf_inf_diff : ∀ n, ∃ f', cont_diff_at ℝ n f') :
  ∃ (n : ℕ) (x : ℝ), deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (deriv (:=
sorry

theorem exercise_2018_b4 {a : ℝ} (h : a ≠ 0) (h₀ : a ≠ 1)
  (h₁ : a ≠ -1) (h₂ : a ≠ 2) (h₃ : a ≠ -2) (h₄ : a ≠ 3) (h₅ : a ≠ -3)
  (h₆ : a ≠ 4) (h₇ : a ≠ -4) (h₈ : a ≠ 5) (h₉ : a ≠ -5) (h₁₀ : a ≠ 6)
  (h₁₁ : a ≠ -6) (h₁₂ : a ≠ 7) (h₁₃ : a ≠ -7) (h₁₄ : a ≠ 8) (h₁₅ : a ≠ -8)
  (h₁₆ : a ≠ 9) (h₁₇ : a ≠ -9) (h₁₈ : a ≠ 10) (h₁₉ : a ≠ -10) (h₂₀ : a ≠ 11)
  (h₂₁ : a ≠ -11) (h₂₂ : a ≠ 12) (h₂₃ : a ≠ -12) (h₂₄ : a ≠ 13) (h₂₅ : a ≠ -13)
  (h₂₆ : a ≠ 14) (h:=
sorry