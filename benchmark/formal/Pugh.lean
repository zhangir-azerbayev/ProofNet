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
import topology.metric_space.basic
import topology.instances.real
import dynamics.ergodic.measure_preserving

open real filter function
open_locale big_operators
open_locale filter
open_locale topological_space

theorem exercise_2_12a (f : ℕ → ℕ) (p : ℕ → ℝ) (a : ℝ)
    (hf : injective f) (hp : tendsto p at_top (𝓝 a)) :
    tendsto (λ n, p (f n)) at_top (𝓝 a) :=
sorry

theorem exercise_2_12b (f : ℕ → ℕ) (p : ℕ → ℝ) (a : ℝ)
    (hf : surjective f) (hp : tendsto p at_top (𝓝 a)) :
    tendsto (λ n, p (f n)) at_top (𝓝 a) :=
sorry

theorem exercise_2_26 {M : Type*} [topological_space M]
  (U : set M) : is_open U ↔ ∀ x ∈ U, ¬ cluster_pt x (𝓟 (set.compl U)) :=
sorry

theorem exercise_2_29 (M : Type*) [metric_space M]
  (O C : set (set M))
  (hO : O = {s | is_open s})
  (hC : C = {s | is_closed s}) :
  ∃ f : O → C, bijective f :=
sorry

theorem exercise_2_32a (A : set ℕ) : is_clopen A :=
sorry

theorem exercise_2_41 (m : ℕ) {X : Type*} [normed_space ℝ ((fin m) → ℝ)] :
  is_compact (metric.closed_ball 0 1) :=
sorry


