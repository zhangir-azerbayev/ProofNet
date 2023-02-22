import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_b6 {K V : Type*} 
  [division_ring K] [add_comm_group V] [module K V] [finite_dimensional K V] 
  {t : finset V} (h : finite_dimensional.finrank K V + 1 < t.card) :
  ∃ (f : V → K), t.sum (λ (e : V), f e • e) = 0 ∧ t.sum (λ (e : V), f e) = 0 
  ∧ ∃ (x : V) (H : x ∈ t), f x ≠ 0 :=
sorry

theorem exercise_1999_b4  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : f_pos_third_derivative_pos_third_derivative_comp hgc hY h) :
  f_pos_third_derivative_pos_third_derivative_comp hgc hY h :=
sorry

theorem exercise_2001_a5 {a : ℕ} (ha : a ≠ 1) :
  ∃ (n : ℕ), a ^ n = 2001 ∧ n ≠ 1 ∧ n ≠ a ^ (n - 1) :=
sorry

theorem exercise_2014_a5  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_2018_a5 {f : ℝ → ℝ}
  (hf : ∀ x, f x ≠ 0 → f' x ≠ 0) (h : ∀ x, f x ≠ 0 → f x < 0) :
  ∃ (n : ℕ), ∀ x, f x ≠ 0 → f' x ≠ 0 :=
sorry

theorem exercise_2018_b4 {a : ℝ} (ha : a ≠ 0) :
  periodic_sequence (λ n, x n) 0 :=
sorry