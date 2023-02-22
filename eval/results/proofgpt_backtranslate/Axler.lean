import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_2 (x : ℂ) :
	(complex.sqrt_three_add x⁻¹ / 2) * complex.sqrt_three_add x / 2 = 1 :=
sorry

theorem exercise_1_4 {α V : Type*} [semi_normed_group V]
	[normed_space α V] {a : α} {v : V} (h : a • v = 0) :
	a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_7 {𝕜 : Type*}
	[is_R_or_C 𝕜] {U : set (euclidean_space 𝕜 2)} (hU : is_open U)
	(hne : U.nonempty) :
	¬submodule.span 𝕜 U ≠ ⊤ :=
sorry

theorem exercise_1_9 {R : Type u} {M : Type v} {M₂ : Type w}
	[semiring R] [add_comm_monoid M] [add_comm_monoid M₂] [module R M]
	[module R M₂] {s t : submodule R M} :
	s ⊔ t ≤ s ⊔ t :=
sorry

theorem exercise_3_8 {V : Type u}
	[add_comm_group V] [module ℝ V] {W : Type v} [add_comm_group W] [module ℝ W]
	[finite_dimensional ℝ V] (T : linear_map ℝ V W) :
	∃ (U : submodule ℝ V), U ⊓ T.range = ⊥ ∧ T.range = ↑U :=
sorry

theorem exercise_5_1 {α : Type*} {m : measurable_space α}
	{μ : measure_theory.measure α} {V : Type*} [normed_group V]
	[normed_space ℝ V] [complete_space V] {T : set α → (V →L[ℝ] V)}
	(h_add : ∀ (s : set α) (x : V), x ∈ T s → ∀ (y : V), y ∈ T s → add_commute (x + y) (x + y))
	(h_smul : ∀ (c : ℝ) (s : set α) (x : V), x ∈ T s → c • x ∈ T s)
	(f : ↥(measure_theory.Lp V 1 μ)) :
	measure_theory.L1.is_invariant T (measure_theory.Lp.sum f) :=
sorry

theorem exercise_5_11 (R : Type u) (S T : Type v)
	[comm_ring R] [comm_ring S] [comm_ring T] [algebra R S] [algebra R T]
	[algebra S T] [is_scalar_tower R S T] (h : S = 0) :
	lie_algebra.adjoin R S T = ⊤ :=
sorry

theorem exercise_5_13 {V : Type (max v u)}
	[category_theory.large_category V] (T : V ⥤ V)
	(hT : ∀ (X : Type (max v u)) (f : X ⟶ X), f ≫ T.map f = T.map f ≫ f) :
	T.smul_invariant_projection = λ (X : Type (max v u)) (f : X ⟶ X), f ≫ T.map f :=
sorry

theorem exercise_5_24 {V : Type*}
	[inner_product_space ℝ V] {T : module.End ℝ V}
	[no_eigenvalue_finite_dimensional ℝ T] (P : submodule ℝ V) (hP : P ≠ ⊥)
	(hT : ∀ (μ : ℝ), module.End.eigenspace T μ = P) :
	even (finite_dimensional.finrank ℝ ↥P) :=
sorry

theorem exercise_6_3 {ι : Type*} [linear_ordered_field ι]
	(a b : ι → ℝ) :
	(∑' (j : ι), a j * b j) ^ 2 ≤ (∑' (j : ι), a j) ^ 2 * (∑' (j : ι), b j) ^ 2 :=
sorry

theorem exercise_6_13 {𝕜 E : Type*} [is_R_or_C 𝕜]
	[inner_product_space 𝕜 E] {ι : Type*} [dec_ι : decidable_eq ι] {v : E}
	(hv : orthonormal 𝕜 v) {s : finset ι}
	(hs : ∀ (i : ι), i ∈ s → has_inner.inner v (e i) = 0) :
	∥v∥ ^ 2 = s.sum (λ (i : ι), ∥v∥ ^ 2 * (e i).inner (e i)) ↔ v ∈ submodule.span 𝕜 (set.range (λ (i : ι), e i)) :=
sorry

theorem exercise_7_5 {K : Type*} [field K]
	{V : Type u} [add_comm_group V] [module K V] (h : 2 ≤ module.rank K V) :
	¬submodule.is_normal_operator K (set.range ⇑(linear_map.to_endomorphism K V)) :=
sorry

theorem exercise_7_9 {𝕜 : Type*}
	[is_R_or_C 𝕜] {E : Type*} [inner_product_space 𝕜 E] {n : ℕ} :
	inner_product_space.is_self_adjoint 𝕜 n ↔ ∀ (μ : 𝕜), module.End.eigenvalues 𝕜 μ = ↑n :=
sorry

theorem exercise_7_11 {V : Type*}
	[inner_product_space ℂ V] (T : inner_product_space ℂ V) :
	∃ (S : ↥(non_negative_subspace.Lp V)), S * S = ↑T :=
sorry