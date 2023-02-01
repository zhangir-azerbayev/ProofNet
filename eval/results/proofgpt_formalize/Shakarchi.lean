import .common 

open complex filter function interval_integral
open_locale big_operators
open_locale filter
open_locale topological_space

universes u v 

theorem exercise_1_13a {f : ℂ → ℂ} {z : ℂ} (h : z.re = f) : f = function.const ℂ z :=
sorry

theorem exercise_1_13b {f : ℂ → ℂ} {s : set ℂ} (hs : is_open s) (hf : ∀ (z : ℂ), z ∈ s → z.im = x) : f = function.const ℂ s (f z) :=
sorry

theorem exercise_1_13c {f : ℂ → ℂ} {z : ℂ} (h : complex.abs f = z) : f = function.const ℂ z :=
sorry

theorem exercise_1_19a (𝕜 : Type*) [normed_field 𝕜] (z : ↥(metric.sphere 0 1)) : ¬summable_on 𝕜 (λ (x : ↥(metric.sphere 0 1)), ↑z ^ x) (metric.sphere 0 1) :=
sorry

theorem exercise_1_19b (z : ℂˣ) : has_sum (λ (x : ℝ), ↑x * ↑x - z * ↑z) (↑z * ↑z - z * ↑z) :=
sorry

theorem exercise_1_19c (z : ℂ) : (∀ (x : ℂ), x ∈ metric.sphere z |1| → z = 1) → summable (λ (x : ℂ), x * ↑(z.re) / ↑(z.im)) :=
sorry