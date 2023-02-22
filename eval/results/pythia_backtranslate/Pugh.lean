import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_26 (M : Type*) [topological_space M]
	[comm_monoid M] [has_continuous_mul M] {s : set M} :
	is_open s ↔ (∀ (x : M), x ∈ s → ¬is_limit x (s.compl)) :=
sorry

theorem exercise_2_32a {s : set ℕ} (hs : s ⊆ ↑⊤) :
	is_clopen s :=
sorry

theorem exercise_2_46 {M : Type*}
	[topological_space M] [linear_ordered_add_comm_group M] [order_topology M]
	{A B : set M} (hA : is_compact A) (hB : is_compact B) (hAB : disjoint A B)
	(hAB : B.nonempty) :
	∃ (a₀ a₁ : M) (b₀ b₁ : M), a₀ ∈ A ∧ b₀ ∈ B ∧ ∀ (a : M), a ∈ A → ∀ (b : M), b ∈ B → has_dist.dist a₀ b₀ ≤ has_dist.dist a a₁ :=
sorry

theorem exercise_2_92 (X : Top)
	(ι : Type v) (U : ι → topological_space.nonempty_compacts ↥X) :
	(X.nonempty_compact_covering ι).nonempty :=
sorry

theorem exercise_3_1 (f : circle_deg1_lift) (x : ℝ)
	(h : has_dist.dist f f ≤ has_dist.dist x x) :
	⇑f x = f :=
sorry

theorem exercise_3_63a (p : ℝ) (hp : 1 < p) :
	summable (λ (k : ℕ), (↑k ^ (p - 1))⁻¹ / (↑(p - 1)) ^ p) :=
sorry

theorem exercise_4_15a : Type*} [uniform_space α] [linear_order α] [order_topology α]
	{m : measurable_space α} (μ : measure_theory.measure α)
	[measure_theory.is_locally_finite_measure μ] [densely_ordered α]
	[no_max_order α] (a b : α) :
	measure_theory.uniform_continuous_on (λ (x : α), set.Ioc (a x) (b x)) μ a b ↔ (∀ (s t : set α), s ∈ set.Ico a b → ⇑μ s ≤ ⇑μ t) ∧ continuous_on f (set.Ioc a b) :=
sorry