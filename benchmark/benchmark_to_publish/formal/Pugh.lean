import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 

theorem exercise_2_12a (f : ℕ → ℕ) (p : ℕ → ℝ) (a : ℝ)
  (hf : injective f) (hp : tendsto p at_top (𝓝 a)) :
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

theorem exercise_2_92 {α : Type*} [topological_space α]
  {s : ℕ → set α}
  (hs : ∀ i, is_compact (s i))
  (hs : ∀ i, (s i).nonempty)
  (hs : ∀ i, (s i) ⊃ (s (i + 1))) :
  (⋂ i, s i).nonempty :=
sorry

theorem exercise_2_126 {E : set ℝ}
  (hE : ¬ set.countable E) : ∃ (p : ℝ), cluster_pt p (𝓟 E) :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ}
  (hf : ∀ x y, |f x - f y| ≤ |x - y| ^ 2) :
  ∃ c, f = λ x, c :=
sorry

theorem exercise_3_4 (n : ℕ) :
  tendsto (λ n, (sqrt (n + 1) - sqrt n)) at_top (𝓝 0) :=
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