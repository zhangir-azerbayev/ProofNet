import .common

open set real filter function ring_hom
open_locale big_operators
open_locale filter
open_locale topological_space
noncomputable theory 

theorem exercise_2_12a (f : ℕ → ℕ) (p : ℕ → ℝ) (a : ℝ)
  (hf : injective f) (hp : tendsto p at_top (𝓝 a)) :
  tendsto (λ n, p (f n)) at_top (𝓝 a) :=
sorry

theorem exercise_2_12b (f : ℕ → ℕ) (p : ℕ → ℝ) (a : ℝ)
  (hf : surjective f) (hp : tendsto p at_top (𝓝 a)) :
  tendsto (λ n, p (f n)) at_top (𝓝 a) :=
sorry

theorem exercise_2_26 {M : Type*} [topological_space M]
  (U : set M) : is_open U ↔ ∀ x ∈ U, ¬ cluster_pt x (𝓟 Uᶜ) :=
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

theorem exercise_2_92 {α : Type*} [topological_space α]
  {s : ℕ → set α}
  (hs : ∀ i, is_compact (s i))
  (hs : ∀ i, (s i).nonempty)
  (hs : ∀ i, (s i) ⊃ (s (i + 1))) :
  (⋂ i, s i).nonempty :=
sorry

theorem exercise_2_109
  {M : Type*} [metric_space M]
  (h : ∀ x y z : M, dist x z = max (dist x y) (dist y z)) :
  totally_disconnected_space M :=
sorry

theorem exercise_2_126 {E : set ℝ}
  (hE : ¬ set.countable E) : ∃ (p : ℝ), cluster_pt p (𝓟 E) :=
sorry

open topological_space

theorem exercise_2_137
  {M : Type*} [metric_space M] [separable_space M] [complete_space M]
  {P : set M} (hP : is_closed P)
  (hP' : is_closed P ∧ P = {x | cluster_pt x (𝓟 P)}) :
  ∀ x ∈ P, ∀ n ∈ (𝓝 x), ¬ set.countable n :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ}
  (hf : ∀ x y, |f x - f y| ≤ |x - y| ^ 2) :
  ∃ c, f = λ x, c :=
sorry

theorem exercise_3_4 (n : ℕ) :
  tendsto (λ n, (sqrt (n + 1) - sqrt n)) at_top (𝓝 0) :=
sorry

theorem exercise_3_11a
  {f : ℝ → ℝ} {a b x : ℝ}
  (h1 : differentiable_within_at ℝ f (set.Ioo a b) x)
  (h2 : differentiable_within_at ℝ (deriv f) (set.Ioo a b) x) :
  ∃ l, tendsto (λ h, (f (x - h) - 2 * f x + f (x + h)) / h ^ 2) (𝓝 0) (𝓝 l)
  ∧ deriv (deriv f) x = l :=
sorry

theorem exercise_3_63a (p : ℝ) (f : ℕ → ℝ) (hp : p > 1)
  (h : f = λ k, (1 : ℝ) / (k * (log k) ^ p)) :
  ∃ l, tendsto f at_top (𝓝 l) :=
sorry

theorem exercise_3_63b (p : ℝ) (f : ℕ → ℝ) (hp : p ≤ 1)
  (h : f = λ k, (1 : ℝ) / (k * (log k) ^ p)) :
  ¬ ∃ l, tendsto f at_top (𝓝 l) :=
sorry

theorem exercise_4_15a {α : Type*}
  (a b : ℝ) (F : set (ℝ → ℝ)) :
  (∀ (x : ℝ) (ε > 0), ∃ (U ∈ (𝓝 x)),
  (∀ (y z ∈ U) (f : ℝ → ℝ), f ∈ F → (dist (f y) (f z) < ε)))
  ↔
  ∃ (μ : ℝ → ℝ), ∀ (x : ℝ), (0 : ℝ) ≤ μ x ∧ tendsto μ (𝓝 0) (𝓝 0) ∧
  (∀ (s t : ℝ) (f : ℝ → ℝ), f ∈ F → |(f s) - (f t)| ≤ μ (|s - t|)) :=
sorry

theorem exercise_4_19 {M : Type*} [metric_space M]
  [compact_space M] (A : set M) (hA : dense A) (δ : ℝ) (hδ : δ > 0) :
  ∃ (A_fin : set M), A_fin ⊂ A ∧ set.finite A_fin ∧ ∀ (x : M), ∃ i ∈ A_fin, dist x i < δ :=
sorry

theorem exercise_5_2 {V : Type*} [normed_add_comm_group V]
  [normed_space ℂ V] {W : Type*} [normed_add_comm_group W] [normed_space ℂ W] :
  normed_space ℂ (continuous_linear_map (id ℂ) V W) :=
sorry
