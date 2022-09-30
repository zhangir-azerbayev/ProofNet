import data.rat.basic
import data.real.basic
import data.real.irrational
import data.real.sqrt
import analysis.inner_product_space.basic
import analysis.inner_product_space.pi_L2
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import analysis.box_integral.basic
import data.set.intervals.basic
import topology.basic
import topology.bases
import topology.metric_space.basic
import topology.instances.real

open complex filter function
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

