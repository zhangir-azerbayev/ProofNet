theorem exercise_2.12a
 lim_rearrangement {α : Type*} [linear_order α] {f : ℕ → α} 
    (hf : injective f) (l : filter α) (hn : tendsto (λ n, f n) at_top l) :
    tendsto f at_top l 

theorem exercise_2.12b
 lim_rearrangement {α : Type*} [linear_order α] {f : ℕ → α} 
    (hf : surjective f) {l : filter α} {a : α} (h : tendsto (λ n, f n) at_top l) :
    tendsto (λ n, f (f.symm n)) at_top l 

theorem exercise_2.26
 is_open_iff_no_limit_points_of_complement {M : Type*} [topological_space M]
    (U : set M) : is_open U ↔ ∀ (x : M), x ∈ U → ¬ is_limit_point (set.compl U) x 

theorem exercise_2.29
 bijective_closure_of_open_subsets (M : Type*) [metric_space M] :
    function.bijective (λ (U : set M), closure U) 

theorem exercise_2.32a
 is_clopen_nat (A : set ℕ) : is_closed A ∧ is_open A 

theorem exercise_2.41
 compact_ball (m : ℕ) (n : ℝ) : compact (ball n m) 

theorem exercise_2.46
 exists_min_dist_of_disjoint_compact {M : Type*} [metric_space M]
    {A B : set M} (hA : compact A) (hB : compact B) (hAB : disjoint A B) 
    (hA0 : A ≠ ∅) (hB0 : B ≠ ∅) :
    ∃ (a0 : M) (b0 : M), a0 ∈ A ∧ b0 ∈ B ∧ ∀ (a : M) (b : M), a ∈ A → b ∈ B → 
    dist a0 b0 ≤ dist a b

theorem exercise_2.48
 exists_embedding_of_line_as_closed_subset_of_plane :
    ∃ (f : ℝ → ℝ²), is_closed (set.range f) 

theorem exercise_2.56
 sphere_not_homeomorphic_to_plane : 
  ¬ homeomorphic (sphere (0 : ℝ) 1) (euclidean_plane) 

theorem exercise_2.57
 not_interior_connected_of_connected {X : Type*} [topological_space X]
    (S : set X) (hS : is_connected S) :
    ¬ is_connected (interior S) 

theorem exercise_2.79
 path_connected_of_locally_path_connected_connected_nonempty_compact 
    {M : Type*} [topological_space M] [compact_space M] 
    [locally_path_connected_space M] (hM : nonempty M) 
    (hM : connected_space M) : path_connected_space M 

theorem exercise_2.85
 compact_redundant_open_cover_reduces_to_finite_subcover 
    (M : Type*) [topological_space M] [compact_space M] 
    (U : set (set M)) (hU : ∀ (p : M), ∃ (U₁ U₂ : set M), p ∈ U₁ ∧ p ∈ U₂ ∧ U₁ ∈ U ∧ U₂ ∈ U) :
    ∃ (V : set (set M)), finite V ∧ ∀ (p : M), ∃ (U₁ U₂ :

theorem exercise_2.92
 compact_nested_inter_of_covering_compact_nonempty {α : Type*} 
    [topological_space α] {s : set (set α)} (hs : ∀ t ∈ s, is_compact t) 
    (hc : ∀ t ∈ s, ∃ u ∈ s, t ⊆ u) (hne : ∀ t ∈ s, t.nonempty) :
    (⋂₀ s).nonempty 

theorem exercise_2.109
 totally_disconnected_of_ultrametric {M : Type*} [metric_space M] 
    (d : M → M → ℝ) (h : ultrametric d) : 
    totally_disconnected M 

theorem exercise_2.126
 exists_condensation_point_of_uncountable {E : set ℝ} 
    (hE : ¬ countable E) : ∃ (p : ℝ), condensation_point E p 

theorem exercise_2.137
 condensation_point_of_perfect_subset {M : Type*} [metric_space M] 
    [separable_space M] [complete_space M] {P : set M} (hP : is_closed P) 
    (hP' : is_perfect P) :
    ∀ (x : M), x ∈ P → is_condensation_point P x 

theorem exercise_2.138
 exists_path_in_epsilon_neighborhood_of_line_segment_disjoint_from_cantor_space 
    (M : set ℝ^2) (hM : is_cantor_space M) (p q : ℝ^2) (hpq : p ≠ q) 
    (hMpq : p ∉ M) (hMq : q ∉ M) (ε : ℝ) (hε : ε > 0) :
    ∃ (A : set ℝ^2), is_path A ∧ A ⊆ (set.Icc p q

theorem exercise_3.1
 is_constant_of_le_norm_squared {f : ℝ → ℝ} (hf : ∀ x y, abs (f x - f y) ≤ abs (x - y) ^ 2) :
  function.is_constant f 

theorem exercise_3.4
 sqrt_n_plus_1_sub_sqrt_n_tendsto_0_of_tendsto_infinity 
    (n : ℕ) (hn : tendsto (λ n, n) at_top (𝓝 ∞)) :
    tendsto (λ n, sqrt (n + 1) - sqrt n) at_top (𝓝 0) 

theorem exercise_3.11a
 exists_lim_of_deriv_two_exists {f : ℝ → ℝ} {a b : ℝ} (hf : ∀ x ∈ (a, b), deriv_two f x) :
  ∀ x ∈ (a, b), ∃ (l : ℝ), tendsto (λ h, (f (x - h) - 2 * f x + f (x + h)) / h ^ 2) (𝓝 0) (𝓝 l) 

theorem exercise_3.17c-i
 smooth_bump_function (x : ℝ) : smooth_at ℝ ℝ (λ x, exp 2 * exp (1 - x) * exp (x + 1)) x 

theorem exercise_3.17c-ii
 bump_function_is_zero_outside_interval (x : ℝ) : 
    x ≤ -1 ∨ x ≥ 1 → bump_function x = 0 

theorem exercise_3.18
 exists_smooth_function_of_closed_set {L : set ℝ} (hL : is_closed L) :
    ∃ (f : ℝ → ℝ), is_smooth f ∧ ∀ (x : ℝ), f x = 0 ↔ x ∈ L 

theorem exercise_3.43a
 riemann_integrable_comp_of_riemann_integrable {f : ℝ → ℝ} 
    (hf : riemann_integrable f) :
    riemann_integrable (f ∘ λ x, x * sin (1 / x)) 

theorem exercise_3.53
 max_min_integrable {α : Type*} [linear_ordered_field α] 
    {f g : ℝ → α} (hf : integrable f) (hg : integrable g) :
    integrable (λ x, max (f x) (g x)) ∧ integrable (λ x, min (f x) (g x)) 

theorem exercise_3.59
 converges_of_converges_sqrt_div_n {α : Type*} [linear_ordered_semiring α]
    {a : ℕ → α} (ha : ∀ n, 0 ≤ a n) (h : series a) :
    series (λ n, sqrt (a n) / n) 

theorem exercise_3.63
 sum_inv_log_pow_p_converges_of_p_gt_1 (p : ℕ) (h : p > 1) :
  series.converges (λ k, (1 : ℝ) / (k * (log k) ^ p)) 

theorem exercise_4.15a
 uniform_continuous_iff_has_modulus_of_continuity {α : Type*} 
    [linear_order α] [topological_space α] [uniform_space α] 
    {f : α → ℝ} (hf : continuous f) :
    uniform_continuous f ↔ ∃ (μ : (0, ∞) → (0, ∞)), 
    (∀ (s : (0, ∞)), tendsto μ s at_top (𝓝 0)) ∧ 
    (∀ (s t : α), s ≤ t → abs (f t - f s) ≤ μ (

theorem exercise_4.15b
 equicontinuous_iff_uniform_continuous_on {α : Type*} 
    [topological_space α] {β : Type*} [topological_space β] 
    {f : α → β} {s : set (α → β)} (h : ∀ x ∈ s, continuous_on f (set.univ)) :
    equicontinuous_on s f ↔ uniform_continuous_on s f 

theorem exercise_4.19
 exists_finite_subset_dense_of_dense {M : Type*} [metric_space M] 
    [compact_space M] (A : set M) (hA : dense A) (δ : ℝ) (hδ : δ > 0) :
    ∃ (a : finset M), ∀ (x : M), ∃ (a' : M), a' ∈ a ∧ dist x a' < δ 

theorem exercise_4.36a
 no_escape_to_infinity_of_bounded {f : ℝ → ℝ} (hf : ∀ x, abs (f x) ≤ M) :
    ∀ (x₀ : ℝ) (t₀ : ℝ), ∃ (t₁ : ℝ), t₁ > t₀ ∧ ∀ (t : ℝ), t₁ ≥ t → abs (x₀ + ∫ t₀ t f) ≤ M 

theorem exercise_4.42
 cantor_set_union_cantor_set_ne_univ (C : set ℝ) (hC : is_cantor_set C) :
    ∀ (n : ℕ), ∃ (Cn : set ℝ), is_cantor_set Cn ∧ Cn ⊆ C ∧ Cn ⊆ Ioo (-(1/n)) (1/n) 

theorem exercise_5.2
 normed_space.operator_norm_is_norm {V : Type*} [normed_group V] 
    [normed_space ℂ V] {W : Type*} [normed_group W] [normed_space ℂ W] :
    normed_space ℂ (continuous_linear_map V W) 

theorem exercise_5.20
 is_const_of_differentiable_zero {n m : ℕ} {U : set ℝ^n} 
    (hU : is_connected U) (hUo : is_open U) (f : U → ℝ^m) 
    (hf : ∀ p ∈ U, differentiable_at ℝ f p) (hfz : ∀ p ∈ U, (D f p) = 0) :
    ∃ (c : ℝ^m), f = function.const U c 

theorem exercise_5.22
 continuous_integral_of_continuous_function {Y : Type*} 
    [metric_space Y] {f : ℝ × Y → ℝ} (hf : continuous f) :
    continuous (λ y : Y, integral (f ∘ (λ x : ℝ, (x, y))) a b) 

theorem exercise_5.43a
 exists_delta_of_rank_lt {R : Type*} [normed_ring R] 
    {n m : ℕ} (T : matrix (fin n) (fin m) R) (k : ℕ) (hk : T.rank < k) :
    ∃ (δ : ℝ), δ > 0 ∧ ∀ (S : matrix (fin n) (fin m) R), 
    S.to_linear_map.op_norm < δ → S.rank ≥ k 

theorem exercise_6.38
 integrable_max_min {α : Type*} [measurable_space α] 
    [measure_space α] {f g : α → ℝ} (hf : integrable f) (hg : integrable g) :
    integrable (λ x, max (f x) (g x)) ∧ integrable (λ x, min (f x) (g x)) 

theorem exercise_6.39
 measurable_product_integrable_le_sqrt_product_sqrt_integral 
    {α : Type*} [measurable_space α] {μ : measure α} {f g : α → ℝ} 
    (hf : measurable f) (hg : measurable g) (hf2 : integrable μ (λ x, f x ^ 2)) 
    (hg2 : integrable μ (λ x, g x ^ 2)) :
    measurable (λ x, f x * g x) ∧ integrable μ (λ x, f x * g x) ∧ 
    integral μ (λ x,

theorem exercise_6.43
 diff_integral_sin_exp {y : ℝ} : differentiable ℝ (λ x, integral (λ x, exp (-x) * sin (x + y)) x) 

theorem exercise_6.49a
 is_measurable_iff_preimage_is_measurable {α : Type*} 
    [measurable_space α] {β : Type*} [measurable_space β] 
    {f : α → β} :
    is_measurable f ↔ ∀ (s : set β), is_measurable s → is_measurable (f ⁻¹' s) 