import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_26 {M : Type*} [topological_space M]
	{U : set M} :
	is_open U ↔ ∀ (x : M), x ∉ U → (∃ (y : M), x ∈ U ∧ y ∉ U) :=
sorry

theorem exercise_2_32a {s : set ℕ} :
	∃ (n : ℕ), s = {↑n} :=
sorry

theorem exercise_2_46 {M : Type*} [metric_space M]
	{A B : set M} (hA : is_compact A) (hB : is_compact B) (hAB : disjoint A B)
	(h : A.nonempty) (h' : B.nonempty) :
	∃ (a₀ : M) (H : a₀ ∈ A) (b₀ : M) (H : b₀ ∈ B), has_dist.dist a₀ b₀ ≤ has_dist.dist a b :=
sorry

theorem exercise_2_92 {α : Type*}
	[conditionally_complete_linear_order α] [topological_space α]
	[order_topology α] [densely_ordered α] ( : ℕ → α) (h : ∀ (n : ℕ), c n ≠ 0) :
	(⋂ (n : ℕ), (finset.range n).nonempty ∧ covering_compact α c) ≠ ∅ :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ}
	(hf : ∀ (t x : ℝ), has_dist.dist (f t) (f x) ≤ has_dist.dist t x ^ 2)
	(t : ℝ) :
	f t = f t :=
sorry

theorem exercise_3_63a {p : ℝ} (hp : 1 < p) :
	summable (λ (k : ℕ), 1 / ↑k * (real.log ↑k) ^ p) :=
sorry

theorem exercise_4_15a {a b : ℝ}
	{μ : ℝ → ℝ} (hμ : strict_mono μ) (f : ℝ → ℝ) :
	uniform_continuous f ↔ (∀ (s t : ℝ), s < t → 0 ≤ ε → |f s - f t| ≤ ε) ∧ (∀ (s : ℝ), 0 ≤ ε → 0 ≤ μ s) ∧ has_modulus_of_continuous μ f :=
sorry