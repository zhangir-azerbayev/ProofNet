import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_3 {V : Type*} [add_comm_group V] (v : V) :
  -(-v) = v :=
sorry

theorem exercise_1_6 {U : set ℝ}
  (hU : ∀ (x : ℝ), x ∈ U → -x ∈ U) (hU' : ∀ (x y : ℝ), x ∈ U → y ∈ U → x + y ∈ U)
  (hU'' : ∀ (x : ℝ), x ∈ U → x ≠ 0) :
  ¬ is_subspace U :=
sorry

theorem exercise_1_8 {R : Type u} {M : Type v} [ring R]
  [add_comm_group M] [module R M] {ι : Sort u_1} (S : ι → submodule R M) :
  (⨅ (i : ι), S i).to_submodule = ⨅ (i : ι), S i.to_submodule :=
sorry

theorem exercise_3_1 {K V : Type*} [field K]
  [add_comm_group V] [module K V] (f : V →ₗ[K] V) (h : linear_map.ker f = ⊥) :
  ∃ (a : K), f = a • linear_map.to_span_singleton K V (f 1) :=
sorry

theorem exercise_4_4 {R : Type u}
  [comm_ring R] [is_domain R] {p : polynomial R} (hp : p ≠ 0) :
  p.distinct_roots ↔ p.roots ∩ (⇑polynomial.derivative p).roots = ∅ :=
sorry

theorem exercise_5_4 {K : Type*} {V : Type u} [field K]
  [add_comm_group V] [module K V] {S T : submodule K V}
  (hST : S.commutes T) (λ : K) :
  is_invariant S (T.ker (T.sub_self λ)) :=
sorry

theorem exercise_5_12 {𝕜 : Type*}
  [is_R_or_C 𝕜] [dec_𝕜 : decidable_eq 𝕜] {E : Type*} [inner_product_space 𝕜 E]
  {T : E →ₗ[𝕜] E} (hT : ∀ (v : E), module.End.has_eigenvector T v 1) :
  T = 1 :=
sorry

theorem exercise_5_20 {𝕜 : Type*}
	[is_R_or_C 𝕜] [dec_𝕜 : decidable_eq 𝕜] {E : Type*} [inner_product_space 𝕜 E]
	{T : E →ₗ[𝕜] E} (hT : T.is_symmetric) [finite_dimensional 𝕜 E] {n : ℕ}
	(hn : finite_dimensional.finrank 𝕜 E = n) (i : fin n) :
	⇑T (⇑(hT.eigenvector_basis hn) i) = ↑(hT.eigenvalues hn i) • ⇑(hT.eigenvector_basis hn) i :=
sorry

theorem exercise_6_2 {𝕜 : Type*}
  [is_R_or_C 𝕜] [inner_product_space 𝕜 E] {u v : E} :
  has_inner.inner u v = 0 ↔ ∀ (a : 𝕜), ‖u‖ ≤ ‖u + a • v‖ :=
sorry

theorem exercise_6_7 {V : Type*} [inner_product_space ℂ V] (u v : V) :
  has_inner.inner u v = (norm (u + v) ^ 2 - norm (u - v) ^ 2 + norm (u + I * v) ^ 2 * I - norm (u - I * v) ^ 2 * I) / 4 :=
sorry

theorem exercise_6_16 {𝕜 E : Type*} [is_R_or_C 𝕜]
  [inner_product_space 𝕜 E] (K : submodule 𝕜 E) :
  Kᗮ = ⊤ ↔ K = ⊥ :=
sorry

theorem exercise_7_6 {𝕜 E : Type*} [is_R_or_C 𝕜]
  [inner_product_space 𝕜 E] [complete_space E] {T : E →L[𝕜] E}
  (hT : is_normal T) :
  T.range = T.adjoint.range :=
sorry

theorem exercise_7_10 {𝕜 : Type*}
  [is_R_or_C 𝕜] {E : Type*} [inner_product_space 𝕜 E] [complete_space E]
  {T : E →L[𝕜] E} (hT : T.is_normal) (hT9 : T ^ 9 = T ^ 8) :
  is_self_adjoint T ∧ T ^ 2 = T :=
sorry

theorem exercise_7_14 {𝕜 : Type*}
  [is_R_or_C 𝕜] {E : Type*} [inner_product_space 𝕜 E] [complete_space E]
  {T : E →L[𝕜] E} (hT : is_self_adjoint T) {λ : ℝ} {ε : ℝ} (hε : 0 < ε)
  {x₀ : E} (hx₀ : ‖x₀‖ = 1) (h : ‖T x₀ - λ • x₀‖ < ε) :
  ∃ (λ' : ℝ), module.End.has_eigenvalue T ↑λ' ∧ abs (λ - λ') < ε :=
sorry