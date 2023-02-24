import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 

universes u v 



theorem exercise_2_26 {α : Type u} [topological_space α] [t2_space α]
  [compact_space α] (U : set α) :
  is_open U ↔ ∀ (F : ultrafilter α), F.Lim ∈ U → U ∈ F.to_filter :=
sorry

theorem exercise_2_32a {s : set ℕ} : is_clopen s :=
sorry

theorem exercise_2_46 {α : Type u} [pseudo_metric_space α]
  {s : set α} (h : is_compact s) (hne : s.nonempty) (x : α) :
  ∃ (y : α) (H : y ∈ s), metric.inf_dist x s = has_dist.dist x y :=
sorry

theorem exercise_2_92 {α : Type u} [topological_space α]
  {s : set α} (hs : is_compact s) (U : Π (x : α), x ∈ s → set α)
  (hU : ∀ (x : α) (H : x ∈ s), U x H ∈ nhds x) :
  ∃ (t : finset ↥s), s ⊆ ⋃ (x : ↥s) (H : x ∈ t), U ↑x _ :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ} (hf : ∀ (t x : ℝ),
  dist (f t) (f x) ≤ (t - x) ^ 2) : continuous f :=
sorry

theorem exercise_3_63a {p : ℝ} :
  summable (λ (n : ℕ), 1 / (↑n * (log ↑n) ^ p)) ↔ 1 < p :=
sorry

theorem exercise_4_15a {α : Type*}
  [metric_space α] {f : α → ℝ} :
  uniform_continuous f ↔ ∃ (μ : ℝ → ℝ), (∀ (s : ℝ), s > 0 → μ s > 0) ∧ (∀ (s t : ℝ), s > 0 → t > 0 → μ (s + t) ≤ μ s + μ t) ∧ (∀ (s t : ℝ), s > 0 → t > 0 → μ (s * t) ≤ μ s * μ t) ∧ (∀ (s : ℝ), s > 0 → μ s < s) ∧ (∀ (s t : ℝ), s > 0 → t > 0 → μ (s + t) ≤ μ s + μ t) ∧ (∀ (s t : ℝ), s > 0 → t > 0 → μ (s * t) ≤ μ s * μ t) ∧ (∀ (s : ℝ), s > 0 → μ s < s) ∧ (∀ (s t : ℝ), s > 0 → t > 0 → μ (s + t) ≤ μ s + μ t) ∧ (∀ (s t : ℝ), s > 0 → t > 0 → μ (s * t) ≤ μ s * μ t) ∧ (∀ (s : ℝ), s > 0 → μ s < s) ∧ (∀ (s t : ℝ), s > 0 → t > 0 → μ (s + t) ≤ μ s + μ t) ∧ (∀ (s t : ℝ), s > 0 →:=
sorry