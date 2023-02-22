import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_26 {M : Type*} [topological_space M]
  (U : set M) : is_open U ↔ ∀ (x : M), x ∈ U → ¬ x ∈ closure (set.compl U) :=
sorry

theorem exercise_2_32a (s : set ℕ) : is_clopen s :=
sorry

theorem exercise_2_46 {M : Type*} [metric_space M]
  {A B : set M} (hA : compact A) (hB : compact B) (hAB : disjoint A B)
  (hA0 : A ≠ ∅) (hB0 : B ≠ ∅) :
  ∃ (a0 : M) (b0 : M), a0 ∈ A ∧ b0 ∈ B ∧ ∀ (a : M) (b : M), a ∈ A → b ∈ B → dist a0 b0 ≤ dist a b :=
sorry

theorem exercise_2_92 {X : Type*} [topological_space X]
  [compact_space X] {I : Type*} [fintype I] {s : I → set X}
  (h : ∀ i j : I, i ≤ j → s i ⊆ s j) (hc : ∀ i : I, is_compact (s i))
  (hnc : ∀ i : I, s i ≠ ∅) :
  ∃ x : X, ∀ i : I, x ∈ s i :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ}
  (hf : uniform_continuous f) (hf' : ∀ x t, abs (f t - f x) ≤ abs (t - x) ^ 2) :
  ∃ c : ℝ, f = function.const ℝ c :=
sorry

theorem exercise_3_63a (p : ℝ) (hp : p > 1) :
  ∃ (N : ℕ), ∀ (n : ℕ), n ≥ N → (∑ k in finset.range n, 1 / (k * (log k) ^ p)) < ∞ :=
sorry

theorem exercise_4_15a {α : Type*}
  [linear_order α] [topological_space α] [metric_space α]
  {f : α → ℝ} (hf : continuous f) :
  uniform_continuous f ↔ ∃ (μ : ℝ → ℝ),
    (∀ (s : ℝ), 0 < s → 0 < μ s) ∧
    (∀ (s : ℝ), 0 < s → μ s < 1) ∧
    (∀ (s : ℝ), μ s = 0 → s = 0) ∧
    (∀ (s : ℝ), μ s = 1 → s ≠ 0) ∧
    (∀ (s : ℝ), μ s ≠ 0 → ∃ (t : ℝ), 0 < t ∧ t < s ∧ μ t = 0) ∧
    (∀ (s t : ℝ), 0 < s → 0 < t → μ (s + t) ≤ μ s + μ t) ∧
    (∀ (s t : ℝ), 0 < s → 0 < t → μ (s * t) ≤ μ s * μ t) ∧
    (∀ (s t : ℝ), 0 < s → 0 < t → μ (s / t) ≤ μ s / μ t) ∧
    (∀ (s t : ℝ), 0 < s → 0 < t → μ (s - t) ≤ μ s + μ t) ∧
    (∀ (s t : ℝ), 0 < s → 0 < t → μ (s / t) ≤ μ s / μ t):=
sorry