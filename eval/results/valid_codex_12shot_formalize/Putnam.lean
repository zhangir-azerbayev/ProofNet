import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_a3 {f : ℝ → ℝ} (hf : continuous (f''')) :
  ∃ (a : ℝ), f''' a = 0 :=
sorry

theorem exercise_2000_a2 (n : ℕ) :
  ∃ (m : ℕ), ∀ (k : ℕ), m ≤ k → ∃ (a b c d : ℕ), a ^ 2 + b ^ 2 = k ∧ c ^ 2 + d ^ 2 = k + 1 ∧
  a ^ 2 + c ^ 2 = k + 2 :=
sorry

theorem exercise_2010_a4 (n : ℕ) (hn : 0 < n) :
  ¬ prime (10 ^ 10 ^ 10 ^ n + 10 ^ 10 ^ n + 10 ^ n - 1) :=
sorry

theorem exercise_2017_b3 {α : Type*} 
  [discrete_field α] [decidable_eq α] {f : ℕ → ℕ} (hf : ∀ i, f i = 0 ∨ f i = 1) 
  (hf₁ : f 0 = 1) (hf₂ : f 1 = 1) (hf₃ : f 2 = 0) (hf₄ : f 3 = 1) 
  (hf₅ : f 4 = 0) (hf₆ : f 5 = 0) (hf₇ : f 6 = 0) (hf₈ : f 7 = 0) 
  (hf₉ : f 8 = 0) (hf₁₀ : f 9 = 0) (hf₁₁ : f 10 = 0) (hf₁₂ : f 11 = 0) 
  (hf₁₃ : f 12 = 0) (hf₁₄ : f 13 = 0) (hf₁₅ : f 14 = 0) (hf₁₆ : f 15 = 0) 
  (hf₁₇ : f 16 = 0) (hf₁₈ : f 17 = 0) (hf₁₉ : f 20 = 0) (hf₂₀ : f 21 = 0) 
  (hf₂₁ : f 22 = 0) (hf₂₂ : f 23 = 0) (hf₂:=
sorry

theorem exercise_2018_b2 (n : ℕ) (h : 0 < n) :
  ∀ z : ℂ, abs z ≤ 1 → polynomial.eval (polynomial.X ^ n + (n - 1) * polynomial.X ^ (n - 1) +
  (n - 2) * polynomial.X ^ (n - 2) + (n - 3) * polynomial.X ^ (n - 3) +
  (n - 4) * polynomial.X ^ (n - 4) + (n - 5) * polynomial.X ^ (n - 5) +
  (n - 6) * polynomial.X ^ (n - 6) + (n - 7) * polynomial.X ^ (n - 7) +
  (n - 8) * polynomial.X ^ (n - 8) + (n - 9) * polynomial.X ^ (n - 9) +
  (n - 10) * polynomial.X ^ (n - 10) + (n - 11) * polynomial.X ^ (n - 11) +
  (n - 12) * polynomial.X ^ (n - 12) + (n - 13) * polynomial.X ^ (n - 13) +
  (n - 14) * polynomial.X ^ (n - 14) + (n - 15) * polynomial.X ^ (n - 15) +
  (n - 16) * polynomial.X ^ (n - 16) + (n - 17) * polynomial.X ^ (n - 17) +
  (n - 18) * polynomial.X ^ (n -:=
sorry

theorem exercise_2020_b5 
  (z₁ z₂ z₃ z₄ : ℂ) (h₁ : abs z₁ = 1) (h₂ : abs z₂ = 1) (h₃ : abs z₃ = 1) 
  (h₄ : abs z₄ = 1) (h₅ : z₁ ≠ 1) (h₆ : z₂ ≠ 1) (h₇ : z₃ ≠ 1) (h₈ : z₄ ≠ 1) :
  3 - z₁ - z₂ - z₃ - z₄ + z₁ * z₂ * z₃ * z₄ ≠ 0 :=
sorry