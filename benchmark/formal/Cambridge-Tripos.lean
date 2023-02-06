import .common 

open real topological_space filter
open_locale topology big_operators complex_conjugate filter ennreal 

theorem exercise_2022_IA_1_II_9D_a (g : ℝ → ℝ)
  (hg1 : continuous g) (hg2 : metric.bounded (set.range g)) 
  (T : ℝ) (hT : T > 0) : 
  ∃ x : ℕ → ℝ, (∀ n : ℕ, x n > 0) ∧ (tendsto x at_top at_top) ∧ 
  tendsto (λ n : ℕ, g ((x n) + T) - g (x n)) at_top (𝓝 0) := sorry       