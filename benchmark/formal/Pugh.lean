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

theorem exercise_2_46 {M : Type*} [metric_space M]
    {A B : set M} (hA : is_compact A) (hB : is_compact B)
    (hAB : disjoint A B) (hA₀ : A ≠ ∅) (hB₀ : B ≠ ∅) :
    ∃ a₀ b₀, a₀ ∈ A ∧ b₀ ∈ B ∧ ∀ (a : M) (b : M),
    a ∈ A → b ∈ B → dist a₀ b₀ ≤ dist a b :=
sorry

theorem exercise_2_57 {X : Type*} [topological_space X]
    : ∃ (S : set X), is_connected S ∧ ¬ is_connected (interior S) :=
sorry

theorem exercise_2_79
    {M : Type*} [topological_space M] [compact_space M]
    [loc_path_connected_space M] (hM : nonempty M)
    (hM : connected_space M) : path_connected_space M :=
sorry

theorem exercise_2_85
    (M : Type*) [topological_space M] [compact_space M]
    (U : set (set M)) (hU : ∀ p, ∃ (U₁ U₂ ∈ U), p ∈ U₁ ∧ p ∈ U₂ ∧ U₁ ≠ U₂) :
    ∃ (V : set (set M)), set.finite V ∧
      ∀ p, ∃ (V₁ V₂ ∈ V), p ∈ V₁ ∧ p ∈ V₂ ∧ V₁ ≠ V₂ :=
sorry

