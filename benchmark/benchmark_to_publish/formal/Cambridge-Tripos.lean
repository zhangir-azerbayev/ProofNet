import .common

open real topological_space filter
open_locale topology big_operators complex_conjugate filter ennreal

theorem exercise_2022_IA_4_I_1E_a : ∀ N : ℕ, ∃ n ≥ N, (3*n+1).prime ∧ (3*n+1) ≥ N :=
sorry

theorem exercise_2022_IA_4_I_2D_a : irrational (2^((1:ℝ)/3) + 3^((1:ℝ)/3)) :=
sorry

theorem exercise_2022_IB_3_II_13G_a_i (U : set ℂ) (hU : is_open U)
  (hU1 : nonempty U) (hU2 : is_connected U) (f : ℕ → ℂ → ℂ) (f' : ℂ → ℂ)
  (hf : ∀ n : ℕ, differentiable_on ℂ (f n) U)
  (hf1 : ∀ X ⊂ U, compact_space X →
  (tendsto_uniformly (λ n, set.restrict X (f n)) (set.restrict X f') at_top)) :
  differentiable_on ℂ f' U :=
sorry
