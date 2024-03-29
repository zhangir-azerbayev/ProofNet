import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 

theorem exercise_2_26 {M : Type*} [topological_space M] (U : set M) :
  is_open U ↔ ∀ x ∈ U, x ∉ closure (Uᶜ) :=
sorry

theorem exercise_2_32a (A : set ℕ) :
  is_clopen A :=
sorry

theorem exercise_2_46 {M : Type*} [metric_space M] [compact_space M]
  (A B : set M) (hA : A ≠ ∅) (hB : B ≠ ∅) (hAB : disjoint A B) :
  ∃ (a₀ ∈ A) (b₀ ∈ B), ∀ (a ∈ A) (b ∈ B), dist a₀ b₀ ≤ dist a b :=
sorry

theorem exercise_2_92
  {α : Type*} [topological_space α] [compact_space α]
  {S : ℕ → set α} (hS : ∀ n, is_compact (S n))
  (hS_cover : ∀ x, ∃ n, x ∈ S n)
  (hS_decreasing : ∀ n, S (n + 1) ⊆ S n) :
  set.nonempty (⋂ n, S n) :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ}
  (hf : ∀ t x : ℝ, abs (f t - f x) ≤ abs (t - x) ^ 2) :
  ∃ (c : ℝ), f = function.const ℝ c :=
sorry

theorem exercise_3_63a {p : ℝ} (hp : p > 1) :
  has_sum (λ k : ℕ, 1 / (k * (real.log k) ^ p)) :=
sorry

theorem exercise_4_15a
  {a b : ℝ} (f : ℝ → ℝ) (hf : continuous_on f (set.Icc a b)) :
  (∃ (μ : ℝ → ℝ), continuous μ ∧ strict_mono μ ∧
    (∀ s, 0 < s → μ s > 0) ∧ (∀ s, s > 0 → μ s → 0) ∧
    (∀ s t ∈ set.Icc a b, abs (f s - f t) ≤ μ (abs (s - t)))) ↔
  uniform_continuous_on f (set.Icc a b) :=
sorry