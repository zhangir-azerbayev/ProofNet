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





theorem exercise_1_1b {r x : ℚ}
  (hr : r ≠ 0) (hx : ¬ is_rat x) : ¬ is_rat (r * x) :=
sorry

theorem exercise_1_4 {α : Type*} [linear_order α]
  {E : set α} (hE : E.nonempty) (hα : ∀ x ∈ E, α ≤ x) (hβ : ∀ x ∈ E, x ≤ β) :
  α ≤ β :=
sorry

theorem exercise_1_8 : ¬ ∃ (r : ℂ → ℂ → Prop), is_linear_order r :=
sorry

theorem exercise_1_12 (n : ℕ) (f : ℕ → ℂ) :
  abs (∑ i in finset.range n, f i) ≤ ∑ i in finset.range n, abs (f i) :=
sorry

theorem exercise_1_14 (z : ℂ) (h : abs z = 1) :
  abs (1 + z)^2 + abs (1 - z)^2 = 2 :=
sorry

theorem exercise_1_17 (n : ℕ) (x y : euclidean_space ℝ (fin n)) :
  ∥x + y∥^2 + ∥x - y∥^2 = 2*∥x∥^2 + 2*∥y∥^2 :=
sorry

theorem exercise_1_18b {k : ℕ} {x y : fin k → ℝ}
  (hx : x = 0) (hy : y ≠ 0) (h : dot_product x y = 0) :
  k = 1 :=
sorry

theorem exercise_2_19a {X : Type*} [metric_space X]
  {A B : set X} (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
  separated A B :=
sorry

theorem exercise_2_25 {K : Type*} [metric_space K]
  (hK : compact_space K) :
  ∃ (B : set (set K)), countable B ∧ is_topological_basis B :=
sorry

theorem exercise_2_27b {E : Type*} 
  [uncountable E] [metric_space E] {P : set E} (hP : P = condensation_points E) :
  countable (E \ P) :=
sorry

theorem exercise_2_29 {s : set ℝ} (hs : is_open s) :
  ∃ (t : set (set ℝ)), t.finite ∧ t.pairwise_disjoint ∧ s = ⋃₀ t :=
sorry

theorem exercise_3_2a :
  tendsto (λ n : ℕ, (n ^ 2 + n) ^ (1 / 2) - n) at_top (𝓝 (1 / 2)) :=
sorry

theorem exercise_3_5 {α : Type*} [linear_order α]
  {f g : ℕ → α} (hf : ∀ n, f n ≤ f (n + 1)) (hg : ∀ n, g n ≤ g (n + 1)) :
  limsup (λ n, f n + g n) ≤ limsup f + limsup g :=
sorry

theorem exercise_3_7 {α : Type*} [linear_ordered_semiring α]
  (a : ℕ → α) (h : summable a) : summable (λ n, sqrt (a n) / n) :=
sorry

theorem exercise_3_13 {R : Type*} [comm_ring R]
  {f g : ℕ → R} (hf : abs_converges f) (hg : abs_converges g) :
  abs_converges (cauchy_product f g) :=
sorry

theorem exercise_3_21 {X : Type*} [metric_space X]
  [complete_space X] {E : ℕ → set X} (hE : ∀ n, is_closed (E n))
  (hE_nonempty : ∀ n, E n ≠ ∅) (hE_bounded : ∀ n, metric.bounded (E n))
  (hE_nested : ∀ n, E n ⊆ E (n + 1))
  (hE_diam_zero : tendsto (λ n, metric.diameter (E n)) at_top (𝓝 0)) :
  ∃ x, ∀ n, x ∈ E n :=
sorry

theorem exercise_4_1a {f : ℝ → ℝ}
  (hf : ∀ x : ℝ, tendsto (λ h : ℝ, f (x + h) - f (x - h)) at_top (𝓝 0)) :
  ¬ continuous f :=
sorry

theorem exercise_4_3 {X : Type*} [metric_space X]
  {f : X → ℝ} (hf : continuous f) (hfz : ∀ x, f x = 0 → x ∈ set.range f) :
  is_closed (set.range f) :=
sorry

theorem exercise_4_4b {X Y : Type*} [metric_space X] [metric_space Y]
  (f g : X → Y) (hf : continuous f) (hg : continuous g) (E : set X)
  (hE : dense E) (h : ∀ x ∈ E, f x = g x) : ∀ x, f x = g x :=
sorry

theorem exercise_4_5b  (E : set ℝ) (f : E → ℝ) (hf : continuous f) :
  ∃ (g : ℝ → ℝ), continuous g ∧ ∀ (x : ℝ), x ∈ E → g x ≠ f x :=
sorry

theorem exercise_4_8a {f : ℝ → ℝ} 
  (hf : uniform_continuous_on f (set.bounded_of_uniform_continuous_on f)) :
  metric.bounded (set.range f) :=
sorry

theorem exercise_4_11a {X Y : Type*} [metric_space X]
  [metric_space Y] (f : X → Y) (hf : uniform_continuous f)
  {x : ℕ → X} (hx : cauchy x) : cauchy (λ n, f (x n)) :=
sorry

theorem exercise_4_15 {f : ℝ → ℝ} (hf : continuous f)
  (hof : ∀ x, is_open (set.range f)) : monotone f :=
sorry

theorem exercise_4_21a {X : Type*} [metric_space X]
  {K F : set X} (hK : compact_space K) (hF : is_closed F) (hKF : disjoint K F) :
  ∃ (δ : ℝ), δ > 0 ∧ ∀ (p q : X), p ∈ K → q ∈ F → dist p q ≥ δ :=
sorry

theorem exercise_5_1 {f : ℝ → ℝ} (hf : ∀ x y, abs (f x - f y) ≤ (x - y)^2) :
  ∃ c : ℝ, ∀ x, f x = c :=
sorry

theorem exercise_5_3 {α : Type*} [linear_ordered_field α]
  {g : α → α} (hg : ∀ x, abs (g x) ≤ 1) (ε : α) (hε : abs ε < 1 / 2) :
  function.injective (λ x, x + ε * g x) :=
sorry

theorem exercise_5_5 {α : Type*} [linear_ordered_field α]
  {f : ℕ → α} (hf : tendsto f at_top (𝓝 0)) :
  tendsto (λ x, f (x + 1) - f x) at_top (𝓝 0) :=
sorry

theorem exercise_5_7 {α : Type*} [linear_ordered_field α]
  {f g : ℝ → α} (hf : tendsto f at_top (𝓝 0)) (hg : tendsto g at_top (𝓝 0))
  (hg' : tendsto (λ x, g' x) at_top (𝓝 0))
  (hg'_ne_0 : ∀ x, g' x ≠ 0) :
  tendsto (λ x, f x / g x) at_top (𝓝 (f' 0 / g' 0)) :=
sorry

theorem exercise_5_17  {f : ℝ → ℝ} (hf : three_times_differentiable_on ℝ f I)
  (hf_boundary : f (-1) = 0 ∧ f 0 = 0 ∧ f 1 = 1 ∧ f' 0 = 0) :
  ∃ (x : ℝ), x ∈ I ∧ f''' x ≥ 3 :=
sorry