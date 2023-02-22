import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b {f : ℂ → ℂ} {s : set ℂ} (hf : holomorphic f s)
  (hIm : ∀ z ∈ s, ∀ w ∈ s, f z.im = f w.im) :
  ∀ z ∈ s, ∀ w ∈ s, f z = f w :=
sorry

theorem exercise_1_19a (z : ℂ) (hz : abs z = 1) :
  ¬converges_on ℂ (λ n, n * z ^ n) :=
sorry

theorem exercise_1_19c {z : ℂ} (hz : abs z < 1) (hz1 : z ≠ 1) :
  is_cau_seq (λ n, z ^ n / n) :=
sorry

theorem exercise_2_2 :
  ∫⁻ a in 0, ∞, sin a / a = π / 2 :=
sorry

theorem exercise_2_13 {f : ℂ → ℂ} (hf : analytic f)
  (h : ∀ z₀ : ℂ, ∃ n : ℕ, f.taylor_series z₀ n = 0) :
  is_polynomial f :=
sorry

theorem exercise_3_4 (a : ℝ) (ha : 0 < a) :
  ∫⁻∞ ∞ (λ x, x * sin x / (x ^ 2 + a ^ 2)) = π * exp (-a) :=
sorry

theorem exercise_3_14 {f : ℂ → ℂ} (hf : entire f) (hfi : function.injective f) :
  ∃ (a b : ℂ), a ≠ 0 ∧ f = λ z, a * z + b :=
sorry

theorem exercise_5_1  {f : ℂ → ℂ} (hf : holomorphic f) (hf_bd : metric.bounded (set.range f))
  (hf_nz : ¬ f = function.const ℂ 0) (hf_z : ∀ z, f z = 0 → abs z < 1) :
  ∑ z in set.range f, 1 - abs z < ⊤ :=
sorry