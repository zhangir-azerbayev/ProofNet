import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b {f : Ω → C} {a : Ω}
  (h : ∀ x ∈ f.image, f x = a) : constant_on f (f.image) :=
sorry

theorem exercise_1_19a {z : ℂ} (hz : z ∈ unit_circle) :
  ∃ n : ℕ, ∀ k : ℕ, k ≠ n → z ^ k ≠ z ^ n :=
sorry

theorem exercise_1_19c {z : ℂ} :
  ∀ (n : ℕ), z ^ n ≠ 1 → ∃ (r : ℝ), 0 < r ∧ ∀ (n : ℕ), |z ^ n - 1| < r :=
sorry

theorem exercise_2_2 {x : ℝ} (hx : 0 < x) :
  ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)⁻¹ x⁻¹ d x = ∫ (1 : ℝ)�:=
sorry

theorem exercise_2_13 {n : ℕ} {f : ℂ → ℂ}
  (h : ∀ z : ℂ, f z = 0 → z = 0) (h0 : f 0 = 0) :
  analytic_polynomial ℂ n :=
sorry

theorem exercise_3_4 :
  ∫ x in (-\infty, -a] : pi e - a = pi e - a :=
sorry

theorem exercise_3_14 {f : ℂ → ℂ} {a b : ℂ} (h : injective f) :
  f ∈ 𝓝 a → f ∈ 𝓝 b ↔ a = b :=
sorry

theorem exercise_5_1  {n : ℕ} {f : ℂ → ℂ} (hf : f.derivative.at_unity = 1) (hz : f.is_root_of_unity) :
  ∑ k in range n, f k * z ^ k = 1 :=
sorry