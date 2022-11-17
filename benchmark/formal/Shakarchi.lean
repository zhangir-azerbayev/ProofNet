import .common 

open complex filter function interval_integral
open_locale big_operators
open_locale filter
open_locale topological_space

theorem exercise_1_13a {f : ℂ → ℂ} (Ω : set ℂ) (a b : Ω) (h : is_open Ω)
  (hf : differentiable_on ℂ f Ω) (hc : ∃ (c : ℝ), ∀ z ∈ Ω, (f z).re = c) :
  f a = f b :=
sorry

theorem exercise_1_13b {f : ℂ → ℂ} (Ω : set ℂ) (a b : Ω) (h : is_open Ω)
  (hf : differentiable_on ℂ f Ω) (hc : ∃ (c : ℝ), ∀ z ∈ Ω, (f z).im = c) :
  f a = f b :=
sorry

theorem exercise_1_13c {f : ℂ → ℂ} (Ω : set ℂ) (a b : Ω) (h : is_open Ω)
  (hf : differentiable_on ℂ f Ω) (hc : ∃ (c : ℝ), ∀ z ∈ Ω, abs (f z) = c) :
  f a = f b :=
sorry

theorem exercise_1_19a (z : ℂ) (hz : abs z = 1) (s : ℕ → ℂ)
    (h : s = (λ n, ∑ i in (finset.range n), i * z ^ i)) :
    ¬ ∃ y, tendsto s at_top (𝓝 y) :=
sorry

theorem exercise_1_19b (z : ℂ) (hz : abs z = 1) (s : ℕ → ℂ)
    (h : s = (λ n, ∑ i in (finset.range n), i * z / i ^ 2)) :
    ∃ y, tendsto s at_top (𝓝 y) :=
sorry

theorem exercise_1_19c (z : ℂ) (hz : abs z = 1) (hz2 : z ≠ 1) (s : ℕ → ℂ)
    (h : s = (λ n, ∑ i in (finset.range n), i * z / i)) :
    ∃ z, tendsto s at_top (𝓝 z) :=
sorry

theorem exercise_1_22 (n : ℕ) (S : fin n → set ℕ) (f : fin n → ℕ × ℕ)
  (h : ∀ i, S i = set.range (λ j, (f i).fst + j * (f i).snd)) :
    ¬ (⋃ i, S i) = (set.univ : set ℕ) :=
sorry

theorem exercise_1_26
  (f F₁ F₂ : ℂ → ℂ) (Ω : set ℂ) (h1 : is_open Ω) (h2 : is_connected Ω)
  (hF₁ : differentiable_on ℂ F₁ Ω) (hF₂ : differentiable_on ℂ F₂ Ω)
  (hdF₁ : ∀ x ∈ Ω, deriv F₁ x = f x) (hdF₂ : ∀ x ∈ Ω, deriv F₂ x = f x)
  : ∃ c : ℂ, ∀ x, F₁ x = F₂ x + c :=
sorry

theorem exercise_2_2 :
  tendsto (λ y, ∫ x in 0..y, real.sin x / x) at_top (𝓝 (real.pi / 2)) :=
sorry

theorem exercise_2_9
  {f : ℂ → ℂ} (Ω : set ℂ) (b : metric.bounded Ω) (h : is_open Ω)
  (hf : differentiable_on ℂ f Ω) (z ∈ Ω) (hz : f z = z) (h'z : deriv f z = 1) :
  ∃ (f_lin : ℂ →L[ℂ] ℂ), ∀ x ∈ Ω, f x = f_lin x :=
sorry

theorem exercise_2_13 {f : ℂ → ℂ}
    (hf : ∀ z₀ : ℂ, ∃ (s : set ℂ) (c : ℕ → ℂ), is_open s ∧ z₀ ∈ s ∧
      ∀ z ∈ s, tendsto (λ n, ∑ i in finset.range n, (c i) * (z - z₀)^i) at_top (𝓝 (f z₀))
      ∧ ∃ i, c i = 0) :
    ∃ (c : ℕ → ℂ) (n : ℕ), f = λ z, ∑ i in finset.range n, (c i) * z ^ n :=
sorry


theorem exercise_3_3 (a : ℝ) (ha : 0 < a) :
    tendsto (λ y, ∫ x in -y..y, real.cos x / (x ^ 2 + a ^ 2))
      at_top (𝓝 (real.pi * (real.exp (-a) / a))) :=
sorry

theorem exercise_3_4 (a : ℝ) (ha : 0 < a) :
    tendsto (λ y, ∫ x in -y..y, x * real.sin x / (x ^ 2 + a ^ 2))
      at_top (𝓝 (real.pi * (real.exp (-a)))) :=
sorry

theorem exercise_3_9 : ∫ x in 0..1, real.log (real.sin (real.pi * x)) = - real.log 2 :=
  sorry

theorem exercise_3_14 {f : ℂ → ℂ} (hf : differentiable ℂ f)
    (hf_inj : function.injective f) :
    ∃ (a b : ℂ), f = (λ z, a * z + b) ∧ a ≠ 0 :=
sorry

open metric

theorem exercise_3_22 (D : set ℂ) (hD : D = ball 0 1) (f : ℂ → ℂ)
    (hf : differentiable_on ℂ f D) (hfc : continuous_on f (closure D)) :
    ¬ ∀ z ∈ (sphere (0 : ℂ) 1), f z = 1 / z :=
sorry

theorem exercise_5_1 (f : ℂ → ℂ) (hf : differentiable_on ℂ f (ball 0 1))
  (hb : bounded (set.range f)) (h0 : f ≠ 0) (zeros : ℕ → ℂ) (hz : ∀ n, f (zeros n) = 0)
  (hzz : set.range zeros = {z | f z = 0 ∧ z ∈ (ball (0 : ℂ) 1)}) :
  ∃ (z : ℂ), tendsto (λ n, (∑ i in finset.range n, (1 - zeros i))) at_top (𝓝 z) :=
sorry
