import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_12a {α : Type*} [linear_order α]
  {f : ℕ → α} {p : ℕ → ℝ} (hf : function.injective f) :
  tendsto (λ n, p (f n)) at_top (𝓝 (lim at_top p)) :=
sorry

theorem exercise_2_29 {M : Type*} [metric_space M] :
  function.bijective (λ (U : set M), Uᶜ) :=
sorry

theorem exercise_2_41 {m : ℕ} (n : ℝ → ℝ) [normed_space ℝ n] :
  compact_space (set.Icc (n 0) 1) :=
sorry

theorem exercise_2_57 {X : Type*} [topological_space X]
  (S : set X) (hS : is_connected S) (hS_int : is_connected (interior S)) :
  false :=
sorry

theorem exercise_2_126 {E : set ℝ} (hE : ¬ countable E) :
  ∃ (p : ℝ), condensation_point E p :=
sorry

theorem exercise_3_4  (n : ℕ) : tendsto (λ n, (sqrt (n + 1) - sqrt n)) at_top (𝓝 0) :=
sorry

theorem exercise_3_63b (p : ℝ) (h : p ≤ 1) :
  ∑ k in range 1, 1 / (k * (log k) ^ p) = ∞ :=
sorry