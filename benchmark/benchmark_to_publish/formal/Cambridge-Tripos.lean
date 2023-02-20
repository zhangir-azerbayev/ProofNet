import .common 

open real topological_space filter
open_locale topology big_operators complex_conjugate filter ennreal 

theorem exercise_2022_IA_1_II_9D_a (g : ℝ → ℝ)
  (hg1 : continuous g) (hg2 : metric.bounded (set.range g)) 
  (T : ℝ) (hT : T > 0) : 
  ∃ x : ℕ → ℝ, (∀ n : ℕ, x n > 0) ∧ (tendsto x at_top at_top) ∧ 
  tendsto (λ n : ℕ, g ((x n) + T) - g (x n)) at_top (𝓝 0) := sorry  

theorem exercise_2022_IA_4_I_1E_a : ∀ N : ℕ, ∃ n ≥ N, (3*n+1).prime ∧ (3*n+1) ≥ N :=
sorry 

theorem exercise_2022_IA_4_I_2D_a : irrational (2^(1/3) + 3^(1/3)) :=
  sorry      

theorem exercise_2022_IB_3_II_13G_a_i (U : set ℂ) (hU : is_open U) 
  (hU1 : nonempty U) (hU2 : is_connected U) (f : ℕ → ℂ → ℂ) (f' : ℂ → ℂ)
  (hf : ∀ n : ℕ, differentiable_on ℂ (f n) U) 
  (hf1 : ∀ X ⊂ U, compact_space X → 
  (tendsto_uniformly (λ n, set.restrict X (f n)) (set.restrict X f') at_top)) :
  differentiable_on ℂ f' U := 
sorry 

theorem exercise_2022_IB_3_II_11G_b (f : (fin 2 → ℝ) → (fin 2 → ℝ)) 
  (hf1 : ∀ x : fin 2 → ℝ, (f x) 0 = (cos (x 0) + cos (x 1) - 1)/2)
  (hf2 : ∀ x : fin 2 → ℝ, (f x) 1 = cos (x 0) - cos (x 1)) :
  ∃ x : fin 2 → ℝ, f x = x :=
sorry    

theorem exercise_2021_IIB_3_I_1G_i (G : Type*) [group G] (H : subgroup G)
  (hH : H ≠ ⊤) :
  ∃ (K : subgroup G) (hK : K.normal) (finK : fintype (G ⧸ K )), 
  (((@fintype.card (G ⧸ K) finK) ∣ (nat.factorial H.index)) ∧ 
  ((@fintype.card (G ⧸ K) finK) ≥ H.index)) :=
sorry 

theorem exercise_2021_IA_1_I_3E_b (f : ℝ → ℝ) 
  (hf : ∀ x y : ℝ, x < y → f x < f y)
  (x : ℕ → ℝ) (hx0 : x > 0) (hx1 : x 0 = 1) 
  (hx2 : ∀ n, x (n+1) = x n + f(x n)) :
  tendsto x at_top at_top :=
sorry 