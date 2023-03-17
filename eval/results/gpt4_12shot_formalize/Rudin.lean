import .common

open real complex
open topological_space
open filter
open_locale real 
open_locale topology
open_locale big_operators
open_locale complex_conjugate
open_locale filter


noncomputable theory





theorem exercise_1_1b irrational_of_mul_rat_irrat {r : ℚ} {x : ℝ} (hr : r ≠ 0) (hx : irrational x) :
  irrational (r * x) :=
sorry

theorem exercise_1_4 lower_bound_le_upper_bound {α : Type*} [preorder α] {E : set α}
  (hE : E.nonempty) (α_lb : is_least (lower_bounds E) α)
  (β_ub : is_greatest (upper_bounds E) β) : α ≤ β :=
sorry

theorem exercise_1_8 no_order_complex : ¬∃ (r : ℂ → ℂ → Prop), @ordered_field ℂ ⟨r, _⟩ :=
sorry

theorem exercise_1_12 abs_sum_leq_sum_abs (n : ℕ) (f : ℕ → ℂ) :
  abs (∑ i in finset.range n, f i) ≤ ∑ i in finset.range n, abs (f i) :=
sorry

theorem exercise_1_14 complex_abs_square_sum_sub_square_eq_two (z : ℂ) (hz : abs z = 1) :
  abs (1 + z) ^ 2 + abs (1 - z) ^ 2 = 2 :=
sorry

theorem exercise_1_17 sum_add_square_sub_square_eq_sum_square_real (k : ℕ) (x y : euclidean_space ℝ (fin k)) :
  ‖x + y‖^2 + ‖x - y‖^2 = 2*‖x‖^2 + 2*‖y‖^2 :=
sorry

theorem exercise_1_18b no_nonzero_orthogonal_in_one_dimensional_space {R : Type*} [ring R]
  [no_zero_divisors R] [char_zero R] (x : euclidean_space R (fin 1)) :
  ¬∃ (y : euclidean_space R (fin 1)), y ≠ 0 ∧ x ⬝ y = 0 :=
sorry

theorem exercise_2_19a separated_of_disjoint_closed_sets {X : Type*} [metric_space X]
  (A B : set X) (hA : is_closed A) (hB : is_closed B) (h_disjoint : disjoint A B) :
  separated A B :=
sorry

theorem exercise_2_25 compact_space_has_countable_base {X : Type*} [metric_space X]
  [compact_space X] : ∃ (B : set (set X)), countable B ∧ is_topological_basis B :=
sorry

theorem exercise_2_27b at_most_countable_points_not_in_condensation_points {E : set ℝ}
  (hE : set.uncountable E) :
  set.countable (E \ condensation_points E) :=
sorry

theorem exercise_2_29 open_set_union_countable_disjoint_segments (U : set ℝ) (hU : is_open U) :
  ∃ (A : set (set ℝ)), countable A ∧
    (∀ S ∈ A, is_open S ∧ is_connected S) ∧
    U = ⋃₀ A ∧ pairwise_disjoint A :=
sorry

theorem exercise_3_2a topology.instances.real

theorem lim_sqrt_n2_plus_n_sub_n : tendsto (λ n : ℕ, real.sqrt (n^2 + n) - n) at_top (𝓝 (1/2)) :=
sorry

theorem exercise_3_5 limsup_add_leq_limsup_add_limsup {a b : ℕ → ℝ} :
  limsup at_top a + limsup at_top b ≠ ⊤ - ⊤ →
  limsup at_top (λ n, a n + b n) ≤ limsup at_top a + limsup at_top b :=
sorry

theorem exercise_3_7 sqrt_summable_of_summable_nonneg {a : ℕ → ℝ}
  (ha_nonneg : ∀ n, 0 ≤ a n) (ha_summable : summable a) :
  summable (λ n, (sqrt (a n)) / n) :=
sorry

theorem exercise_3_13 cauchy_product_abs_converges {α : Type*} [ring α] [topological_space α]
  [topological_ring α] {f g : ℕ → α} (hf : summable (λ n, abs (f n)))
  (hg : summable (λ n, abs (g n))) :
  summable (λ n, abs (∑ i in finset.range (n + 1), f i * g (n - i))) :=
sorry

theorem exercise_3_21 unique_point_of_intersection_of_decreasing_closed_sets
  {X : Type*} [metric_space X] [complete_space X] {E : ℕ → set X}
  (hE : ∀ n, is_closed (E n) ∧ set.nonempty (E n) ∧ bdd_below (E n))
  (hE_dec : ∀ n, E (n + 1) ⊆ E n)
  (hE_diam : tendsto (λ n, metric.diam (E n)) at_top (𝓝 0)) :
  ∃! x : X, x ∈ ⋂ n, E n :=
sorry

theorem exercise_4_1a The following example demonstrates that the given condition does not imply continuity of f.

example : ∃ (f : ℝ → ℝ), (∀ x : ℝ, filter.tendsto (λ h, f (x + h) - f (x - h)) (nhds 0) (nhds 0)) ∧ ¬continuous f :=
sorry

theorem exercise_4_3 zero_set_of_continuous_function_is_closed {X : Type*} [metric_space X]
  [topological_space ℝ] (f : X → ℝ) (hf : continuous f) :
  is_closed {x : X | f x = 0} :=
sorry

theorem exercise_4_4b continuous_eq_on_dense {X Y : Type*} [metric_space X] [metric_space Y]
  (f g : X → Y) (hf : continuous f) (hg : continuous g) (E : set X)
  (hE : dense E) (heq : set.eq_on f g E) : f = g :=
sorry

theorem exercise_4_5b exists_continuous_no_extension {E : set ℝ} (hE : E.dense) :
  ∃ (f : E → ℝ) (hf : continuous f), ¬∃ (g : ℝ → ℝ), continuous g ∧ ∀ x ∈ E, g x = f x :=
sorry

theorem exercise_4_8a bounded_of_uniform_continuous_on_bounded_set {E : set ℝ} (f : ℝ → ℝ)
  (hE : metric.bounded E) (hf : uniform_continuous_on f E) :
  metric.bounded (f '' E) :=
sorry

theorem exercise_4_11a cauchy_image_of_uniform_continuous {X Y : Type*} [metric_space X] [metric_space Y]
  (f : X → Y) (hf : uniform_continuous f) {x : ℕ → X} (hx : cauchy_seq x) :
  cauchy_seq (f ∘ x) :=
sorry

theorem exercise_4_15 continuous_open_map_is_monotonic {f : ℝ → ℝ}
  (hf : continuous f) (h_open : is_open_map f) :
  monotone f ∨ monotone (λ x, -f x) :=
sorry

theorem exercise_4_21a exists_delta_gt_dist_of_compact_closed_disjoint {X : Type*} [metric_space X]
  {K F : set X} (hK : is_compact K) (hF : is_closed F) (h_disjoint : K ∩ F = ∅) :
  ∃ δ > 0, ∀ p ∈ K, ∀ q ∈ F, dist p q > δ :=
sorry

theorem exercise_5_1 const_of_abs_leq_square_diff {f : ℝ → ℝ}
  (hf : ∀ x y : ℝ, abs (f x - f y) ≤ (x - y) ^ 2) :
  ∃ c : ℝ, ∀ x : ℝ, f x = c :=
sorry

theorem exercise_5_3 one_to_one_of_small_epsilon_bounded_derivative
  {g : ℝ → ℝ} {M : ℝ} (hg : ∀ x, ∥deriv g x∥ ≤ M)
  (ε : ℝ) (hε : 0 < ε) (hεM : ε * M < 1) :
  function.injective (λ x, x + ε * g x) :=
sorry

theorem exercise_5_5 diff_to_zero_implies_shifted_diff_to_zero {f : ℝ → ℝ}
  (hf : ∀ x > 0, has_deriv_at f (deriv f x) x)
  (hf' : filter.tendsto (deriv f) filter.at_top (nhds 0)) :
  filter.tendsto (λ x, f (x + 1) - f x) filter.at_top (nhds 0) :=
sorry

theorem exercise_5_7 lhopital_zero_at_top {f g : ℝ → ℝ} {x : ℝ}
  (hfx : differentiable_at ℝ f x) (hgx : differentiable_at ℝ g x)
  (hg'x : deriv g x ≠ 0) (hfxgx : f x = g x) (hfxgx0 : f x = 0) :
  filter.tendsto (λ t, f t / g t) (𝓝[{x}ᶜ] x) (𝓝 (deriv f x / deriv g x)) :=
sorry

theorem exercise_5_17 exists_third_derivative_geq_three {f : ℝ → ℝ}
  (hf : differentiable ℝ^[3] f) (hf1 : f (-1) = 0) (hf2 : f 0 = 0)
  (hf3 : f 1 = 1) (hf4 : deriv f 0 = 0) :
  ∃ x ∈ Ioo (-1 : ℝ) 1, deriv^[3] f x ≥ 3 :=
sorry