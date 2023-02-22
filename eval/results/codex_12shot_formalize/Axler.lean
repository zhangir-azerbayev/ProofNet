import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_2 :
  (complex.I * √3 / 2) ^ 3 = 1 :=
sorry

theorem exercise_1_4 {K V : Type*} [field K] [add_comm_group V] 
  [vector_space K V] (a : K) (v : V) :
  a • v = 0 ↔ a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_7 {U : Type*} [add_comm_group U]
  [vector_space ℝ U] (hU : ∀ (x : ℝ) (u : U), u ∈ U → x • u ∈ U)
  (hU_ne : U ≠ (0 : U)) : ¬ is_subspace U :=
sorry

theorem exercise_1_9 {K V : Type*} [field K] [add_comm_group V] 
  [vector_space K V] {U₁ U₂ : subspace V} :
  U₁ ⊔ U₂ = ⊤ ↔ U₁ ⊆ U₂ ∨ U₂ ⊆ U₁ :=
sorry

theorem exercise_3_8 {V W : Type*} 
  [add_comm_group V] [add_comm_group W] [vector_space ℝ V] [vector_space ℝ W] 
  [finite_dimensional ℝ V] {T : V →ₗ[ℝ] W} :
  ∃ (U : submodule ℝ V), U.comap T.to_fun = ⊥ ∧ range T = U.map T.to_fun :=
sorry

theorem exercise_5_1 {V : Type*} [add_comm_group V] 
  [module ℂ V] {T : V →ₗ[ℂ] V} {U : Type*} [fintype U] {f : U → submodule ℂ V} 
  (hf : ∀ (i : U), T.range ⊆ f i) :
  T.range ⊆ ⨆ (i : U), f i :=
sorry

theorem exercise_5_11 {V : Type*} [finite_dimensional V]
  [add_comm_group V] [vector_space ℂ V] {S T : V →ₗ[ℂ] V} :
  eigenvalues (S * T) = eigenvalues (T * S) :=
sorry

theorem exercise_5_13 {K V : Type*} 
  [field K] [add_comm_group V] [vector_space K V] [finite_dimensional K V] 
  (T : V →ₗ[K] V) (hT : ∀ (U : submodule K V), U.dim = V.dim - 1 → T.range ⊆ U) :
  ∃ (c : K), T = c • linear_map.id :=
sorry

theorem exercise_5_24 {V : Type*} [add_comm_group V] 
  [vector_space ℝ V] [finite_dimensional ℝ V] {T : V →ₗ[ℝ] V} 
  (hT : ∀ (v : V), v ≠ 0 → T v ≠ v) :
  ∀ (U : submodule ℝ V), U.is_invariant T → U.dim_eq 0 ∨ U.dim_eq 2 :=
sorry

theorem exercise_6_3 (n : ℕ) (a b : fin n → ℝ) :
  (∑ i in finset.range n, a i * b i) ^ 2 ≤
    (∑ i in finset.range n, i * a i ^ 2) * (∑ i in finset.range n, b i ^ 2 / i) :=
sorry

theorem exercise_6_13  {K : Type*} [field K] [inner_product_space K ℝ]
  {n : ℕ} (e : fin n → K) (h : is_orthonormal_basis K e)
  (v : K) :
  ∥v∥^2 = ∑ i in finset.range n, (v • e i)^2 ↔ v ∈ span K (e '' finset.range n) :=
sorry

theorem exercise_7_5 {V : Type*} [field K] [add_comm_group V] 
  [vector_space K V] [finite_dimensional K V] (h : 2 ≤ finite_dimensional.finrank K V) :
  ¬ submodule K (normal_ops K V) (linear_map.module K V) :=
sorry

theorem exercise_7_9 {V : Type*} [inner_product_space ℂ V]
  [normed_group V] [normed_space ℂ V] {T : V →ₗ[ℂ] V} (hT : T.is_normal)
  (hT_eigenvalues : ∀ (λ : ℂ), is_eigenvalue T λ → λ.is_real) :
  T.is_self_adjoint :=
sorry

theorem exercise_7_11 {V : Type*} [inner_product_space ℂ V] 
  [normed_group V] [normed_space ℂ V] {T : V →ₗ[ℂ] V} (hT : T.is_normal) :
  ∃ (S : V →ₗ[ℂ] V), S.is_normal ∧ S ^ 2 = T :=
sorry