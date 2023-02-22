import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_b6 (a b c : ℤ) :
  ∃ (n : ℕ), ¬ (∃ (z : ℤ), z ^ 3 + a * z ^ 2 + b * z + c = n ^ 2) :=
sorry

theorem exercise_1999_b4  {f : ℝ → ℝ} (hf : differentiable ℝ f) (hf' : differentiable ℝ f')
  (hf'' : differentiable ℝ f'') (hf''' : differentiable ℝ f''')
  (hf_pos : ∀ x, 0 < f x) (hf'_pos : ∀ x, 0 < f' x)
  (hf''_pos : ∀ x, 0 < f'' x) (hf'''_pos : ∀ x, 0 < f''' x)
  (hf'''_le : ∀ x, f''' x ≤ f x) :
  ∀ x, f' x < 2 * f x :=
sorry

theorem exercise_2001_a5 :
  ∃ (a n : ℕ), a ^ (n + 1) - (a + 1) ^ n = 2001 ∧ ∀ (a' n' : ℕ), a' ^ (n' + 1) - (a' + 1) ^ n' = 2001 → a = a' ∧ n = n' :=
sorry

theorem exercise_2014_a5 {α : Type*} {p q : α → Prop}
  (h : ∃ x, p x ∧ q x) (h₁ : ∀ x, p x → r x) (h₂ : ∀ x, q x → s x) :
  ∃ x, r x ∧ s x :=
sorry

theorem exercise_2018_a5 
  {f : ℝ → ℝ} (hf : ∀ n : ℕ, differentiable_at ℝ f n) 
  (hf_nonneg : ∀ x : ℝ, 0 ≤ f x) (hf_0_0 : f 0 = 0) (hf_1_1 : f 1 = 1) :
  ∃ (n : ℕ) (x : ℝ), (derivative_within_at ℝ f n x (Icc 0 1)).comp_continuous_within_at 
  (Icc 0 1) x < 0 :=
sorry

theorem exercise_2018_b4 {a : ℝ} (h : ∃ n : ℕ, a ^ n = 0) :
  ∃ (p : ℕ), ∀ (n : ℕ), a ^ n = a ^ (n % p) :=
sorry