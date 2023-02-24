import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_3 {V : Type*} [add_comm_group V] [vector_space ℝ V] 
  (v : V) : -(-v) = v :=
sorry

theorem exercise_1_6 {U : Type*} [add_comm_group U] 
  [vector_space ℝ U] (hU : ∀ (u : U), -u ∈ U) (hU_nonempty : ∃ (u : U), u ≠ 0) :
  ¬ is_subspace U :=
sorry

theorem exercise_1_8 {R : Type*} [comm_ring R] {V : Type*} [add_comm_group V] 
  [module R V] {S : set (submodule R V)} :
  is_submodule R (⋂₀ S) :=
sorry

theorem exercise_3_1 {V : Type*} [field K] [add_comm_group V] 
  [vector_space K V] (h : vector_space.dim K V = 1) (T : V →ₗ[K] V) :
  ∃ (a : K), T = linear_map.id.smul a :=
sorry

theorem exercise_4_4 
  {α : Type*} [discrete_field α] {p : polynomial α} (hp : p.degree > 0) :
  p.has_n_distinct_roots p.degree ↔ p.roots.disjoint p.derivative.roots :=
sorry

theorem exercise_5_4 {V : Type*} [add_comm_group V] 
  [vector_space ℂ V] {S T : V →ₗ[ℂ] V} (hST : S.comm T) 
  (hTS : T.comm S) (λ : ℂ) :
  S.range_subset (null_space (T - λ • linear_map.id V)) :=
sorry

theorem exercise_5_12 {V : Type*} [add_comm_group V]
  [module ℂ V] [finite_dimensional ℂ V] (T : V →ₗ[ℂ] V)
  (hT : ∀ v : V, is_eigenvector ℂ T v) :
  ∃ (c : ℂ), T = c • linear_map.id :=
sorry

theorem exercise_5_20 {K : Type*} [field K] 
  [add_comm_group V] [module K V] [finite_dimensional K V] 
  (T : V →ₗ[K] V) (S : V →ₗ[K] V) (hT : T.dim_eigenvalues = V.dim K) 
  (hS : ∀ (v : V), v ≠ 0 → ∃ (a : K), T v = a • v → ∃ (a : K), S v = a • v) :
  S.comm T :=
sorry

theorem exercise_6_2 {V : Type*} 
  [inner_product_space ℝ V] {u v : V} :
  inner_product u v = 0 ↔ ∀ (a : ℝ), ∥u∥ ≤ ∥u + a • v∥ :=
sorry

theorem exercise_6_7 {V : Type*} 
  [inner_product_space ℂ V] (u v : V) :
  inner_product u v = (∥u + v∥^2 - ∥u - v∥^2 + ∥u + I • v∥^2 - ∥u - I • v∥^2) / 4 :=
sorry

theorem exercise_6_16 {K : Type*} [field K] 
  {V : Type*} [add_comm_group V] [vector_space K V] {U : subspace V} :
  U.orthogonal = {0} ↔ U = ⊤ :=
sorry

theorem exercise_7_6 {V : Type*} [inner_product_space ℂ V]
  [normed_group V] [normed_space ℂ V] {T : V →ₗ[ℂ] V} (hT : T.is_normal) :
  T.range = T.adjoint.range :=
sorry

theorem exercise_7_10 {V : Type*} 
  [inner_product_space ℂ V] [add_comm_group V] [vector_space ℂ V] 
  [module ℂ V] [normed_group V] [normed_space ℂ V] {T : V →ₗ[ℂ] V} 
  (hT : T.is_normal) (hT₁ : T ^ 9 = T ^ 8) (hT₂ : T ^ 8 = T ^ 7) :
  T.is_self_adjoint ∧ T ^ 2 = T :=
sorry

theorem exercise_7_14 
  {V : Type*} [normed_group V] [normed_space ℂ V] [inner_product_space ℂ V] 
  {T : V →ₗ[ℂ] V} (hT : T.is_self_adjoint) (λ : ℂ) (ε : ℝ) 
  (hε : ε > 0) (v : V) (hv : ∥v∥ = 1) (hvT : ∥T v - λ • v∥ < ε) :
  ∃ (λ' : ℂ), T.is_eigenvalue λ' ∧ abs (λ - λ') < ε :=
sorry