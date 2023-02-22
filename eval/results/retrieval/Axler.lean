import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_2 :
	((-1 : ℂ) + complex.sqrt 3 * complex.I) / 2 = 1 :=
sorry

theorem exercise_1_4 {α : Type u} {β : Type v} [group α] [add_monoid β]
  [distrib_mul_action α β] (a : α) {x : β} :
  a • x = 0 ↔ a = 0 ∨ x = 0 :=
sorry

theorem exercise_1_7  {R : Type*} [comm_ring R] [add_comm_group R] [topological_space R]
  [topological_add_group R] [module R R] (U : set R)
  (hU : ∀ (r : R) (x : R), x ∈ U → r • x ∈ U) (hU0 : ∅ ≠ U) :
  ¬ is_submodule U :=
sorry

theorem exercise_1_9 {V : Type*} [add_comm_group V] [vector_space ℂ V]
  {U₁ U₂ : subspace V} :
  subspace.subtype (U₁.val ∪ U₂.val) ↔ U₁ ≤ U₂ ∨ U₂ ≤ U₁ :=
sorry

theorem exercise_3_8 {K : Type u} {V : Type v} [field K]
  [add_comm_group V] [module K V] [finite_dimensional K V] {W : Type w}
  [add_comm_group W] [module K W] (T : V →ₗ[K] W) :
  ∃ (U : submodule K V), U.comap T = ⊥ ∧ T.range = T '' U :=
sorry

theorem exercise_5_1 {R M : Type*} [semiring R] [add_comm_monoid M]
  [module R M] {ι : Type*} [fintype ι] {f : ι → M} {p : ι → submodule R M}
  (h : ∀ (i : ι), f i ∈ p i) :
  finset.univ.sum (λ (i : ι), f i) ∈ ⨆ (i : ι), p i :=
sorry

theorem exercise_5_11 {𝕜 : Type*}
	[is_R_or_C 𝕜] [dec_𝕜 : decidable_eq 𝕜] {E : Type*} [inner_product_space 𝕜 E]
	{S T : E →ₗ[𝕜] E} (hS : S.is_symmetric) (hT : T.is_symmetric)
	(hST : S.comm T) [finite_dimensional 𝕜 E] {n : ℕ}
	(hn : finite_dimensional.finrank 𝕜 E = n) (i : fin n) :
	module.End.has_eigenvalue (S.mul T) ↑(hS.eigenvalues hn i) :=
sorry

theorem exercise_5_13  {K : Type u} {V : Type v} [division_ring K] [add_comm_group V] [module K V]
  [finite_dimensional K V] (T : V →ₗ[K] V)
  (hT : ∀ (U : submodule K V), (U.dim = V.dim - 1) → T.range ⊆ U) :
  ∃ (c : K), T = c • 1 :=
sorry

theorem exercise_5_24  {𝕜 : Type*} [is_R_or_C 𝕜] {E : Type*} [inner_product_space 𝕜 E]
  [finite_dimensional 𝕜 E] {T : E →ₗ[𝕜] E} (hT : T.is_symmetric)
  (hT' : ∀ (μ : 𝕜), module.End.eigenspace T μ = ⊥) :
  subsingleton E :=
sorry

theorem exercise_6_3 {n : ℕ} {R : Type*} [comm_ring R]
  (a b : fin n → R) :
  (finset.univ.sum (λ (i : fin n), a i * b i)) ^ 2 ≤
  (finset.univ.sum (λ (i : fin n), i.val * a i ^ 2)) *
  (finset.univ.sum (λ (i : fin n), b i ^ 2 / i.val)) :=
sorry

theorem exercise_6_13 {𝕜 E : Type*}
  [is_R_or_C 𝕜] [inner_product_space 𝕜 E] {ι : Type*} {v : basis ι 𝕜 E}
  (hv : orthonormal 𝕜 ⇑v) (i : ι) (x : E) :
  ∥x∥ = ∑ i, ∥(⇑v i) • x∥ ^ 2 ↔ x ∈ submodule.span 𝕜 (set.range v) :=
sorry

theorem exercise_7_5 {K : Type u} {V : Type v}
  [division_ring K] [add_comm_group V] [module K V] (h : 2 ≤ module.rank K V) :
  ¬ submodule K (normal_ops K V) (linear_map K V V) :=
sorry

theorem exercise_7_9 {𝕜 : Type*} [is_R_or_C 𝕜]
  {E : Type*} [inner_product_space 𝕜 E] [complete_space E] {T : E →L[𝕜] E}
  (hT : is_normal T) : is_self_adjoint T ↔ ∀ (μ : module.End.eigenvalues T),
  is_real μ :=
sorry

theorem exercise_7_11 {𝕜 : Type*} [is_R_or_C 𝕜] {E : Type*}
  [inner_product_space 𝕜 E] [complete_space E] {T : E →L[𝕜] E}
  (hT : T.is_normal) : ∃ (S : E →L[𝕜] E), S.is_normal ∧ S ^ 2 = T :=
sorry