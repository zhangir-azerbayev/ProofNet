import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_2 (i : ℝ) (h₁ : -(1 + real.sqrt (3 * i)) / 2 < i)
	(h₂ : real.sqrt (1 + i) = 1) :
	is_cubine_root (real.sqrt (1 + i) / 2) 1 :=
sorry

theorem exercise_1_4 {α : Type*} [cancel_comm_monoid_with_zero α]
	(a : α) (v : α) (h : a * v = 0) :
	a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_7 {U : set ennreal}
	(hU : U ∈ nhds_within 2 {⊤, ⊤})
	(hU' : ∀ (x : ennreal), x ∈ U → ∀ (y : ennreal), y ∈ U → x < y → y < x) :
	¬is_subspace U :=
sorry

theorem exercise_1_9 {K : Type*} {V : Type u} [field K]
	[add_comm_group V] [module K V] :
	(V ⊔ V)ᗮ = ⊤ ↔ Vᗮ ⊆ V :=
sorry

theorem exercise_3_8 W : Type*} [inner_product_space ℝ V] [finite_dimensional ℝ V]
	(h : finite_dimensional.finrank ℝ V = finite_dimensional.finrank ℝ W)
	(T : ↥(submodule.span ℝ {T})ᗮ) :
	∃ (u : V →ₗ[ℝ] W), (∀ (v : V), v ∈ T.to_linear_map.range → (∃ (u : V), u.to_linear_map = v)) ∧ set.range coe = {0} ∧ ∀ (u : V), u ∈ T.to_linear_map.range → (∃ (v : V), v ∈ T.to_linear_map.range ∧ ⇑u v = 0) :=
sorry

theorem exercise_5_1 {𝕜 : Type*} [is_R_or_C 𝕜]
	{V : Type*} [inner_product_space 𝕜 V] (T : submodule 𝕜 V)
	(h : ∀ (u : finset (fin 2)), u ∈ finset.univ → ∀ (v : finset V), v ∈ finset.univ → T.is_scalar_tower 𝕜 V (↑u + ↑v)) :
	(⨆ (u : finset (fin 2)), ↑u) + T = ↑(⨆ (u : finset V), ↑u) :=
sorry

theorem exercise_5_11 {𝕜 V : Type*}
	[nondiscrete_normed_field 𝕜] [complete_space 𝕜] (S T : set (𝕜))
	(h_comm : ∀ (x : 𝕜), x ∈ S → ∀ (y : 𝕜), y ∈ S → has_inner.inner x y = has_inner.inner y x)
	(h_eigen : ∀ (x : 𝕜), x ∈ S → (function.is_conjugate_exponent 𝕜 x) → has_inner.inner x y = 0)
	(h_eigen_id : ∀ (x : 𝕜), x ∈ S → (function.is_conjugate_exponent 𝕜 x) → has_inner.inner x (⇑(inner_product_space.of_core.repr S T h_comm h_eigen) x) = 0)
	(h_eigen_id_comm : ∀ (x : 𝕜), x ∈ S → (function.is_conjugate_exponent 𝕜 x) → has_inner.inner x (⇑(inner_product_space.of_core.repr S T h_comm h_eigen) x) = 0)
	(h_eigen_id_comm_apply : �:=
sorry

theorem exercise_5_13 {K : Type*} [field K] {V : Type*}
	[add_comm_group V] [module K V] (T : set V →ₗ[K] V)
	(h : ∀ (s : submodule K V), submodule.is_invariantence K coe s → T.dim = s.dim - 1) :
	∃ (c : K), T = c • linear_map.id :=
sorry

theorem exercise_5_24 {V : Type*}
	[add_comm_group V] [module ℝ V] {T : set V}
	(hT : ∀ (μ : module.End ℝ V), μ ∈ T → module.End.eigenspace T μ = ⊥)
	(K : submodule ℝ V) [finite_dimensional ℝ ↥K] :
	∃ (n : ℕ), even (finite_dimensional.finrank ℝ ↥K) :=
sorry

theorem exercise_6_3 (n : ℕ) (a : fin n → ℝ) (b : fin n → ℝ) :
	(finset.range n).sum (λ (j : fin n), a j * b j) ^ 2 ≤ (finset.range n).sum (λ (j : fin n), a j * b j) ^ 2 :=
sorry

theorem exercise_6_13 {V : Type*} [inner_product_space ℝ V]
	{m : ℕ} (e : fin m.succ → V) (v : V)
	(h_e : ∀ (i : fin m.succ), has_inner.inner v (e i) = has_inner.inner (e i) v) :
	∥v∥ ^ 2 = (has_inner.inner v (e 1) ^ 2 + (finset.range m).sum (λ (i : ℕ), (has_inner.inner (e i) v) ^ 2)) :=
sorry

theorem exercise_7_5 {K : Type u} {V : Type v} [division_ring K]
	[add_comm_group V] [module K V] (h : 2 ≤ module.rank K V) :
	¬submodule.restrict_scalars K (submodule.of_dim K V) ≤ submodule.span K {v : V | v ≠ 0} :=
sorry

theorem exercise_7_9 eigenvalues_real
	(E' : Type*) [inner_product_space ℝ E'] [complete_space E'] :
	inner_product_space.is_self_adjoint E' ↔ ∀ (μ : module.End ℝ E'), is_R_or_C.is_integral (λ (x : E'), (⇑μ ↑x).re) :=
sorry

theorem exercise_7_11 {V : Type*}
	[inner_product_space ℂ V] [complete_space V] {T : inner_product_space.core V}
	(hT : ∀ (x : V), has_inner.inner x x = 0 → has_inner.inner x x = 0)
	(S : inner_product_space.core.submodule ℂ V)
	(hS : S ^ 2 = T) :
	inner_product_space.of_core.inner_products S T = S :=
sorry