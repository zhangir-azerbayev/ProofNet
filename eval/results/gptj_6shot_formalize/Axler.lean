import linear_algebra.finite_dimensional
import linear_algebra.eigenspace
import data.complex.basic
import analysis.inner_product_space.basic
import analysis.inner_product_space.projection
import analysis.inner_product_space.adjoint

open set fintype complex polynomial submodule linear_map
open finite_dimensional module module.End inner_product_space

open_locale big_operators


theorem exercise_1_2 {a : ℝ} : a ^ 3 = 1 ↔ a = 1 ∧ a = -1 :=
sorry

theorem exercise_1_3 {V : Type*} [field V] [add_comm_group V]
  [module V V] {v : V} (hv₁ : v = -v) (hv₂ : v = v) :
  v = 0 :=
sorry

theorem exercise_1_4 {F V : Type*} [field F] [module V F]
  (hv : vector.zero_divides v) :
  zero_divides (λ x, x • v) :=
sorry

theorem exercise_1_6 {U : Type*} [subset U] [add_comm_group U] [module U ℝ2]
  (h : U.is_closed_under_add_inverse) (h : U.is_closed_under_add)
  (h : U.is_closed_under_add_inverse_add) :
  non_subspace U :=
sorry

theorem exercise_1_7 {U : Type*} [subspace U]
  [non_empty U] (h : U.is_closed_under_scalar_mult) :
  non_subspace U :=
sorry

theorem exercise_1_8 {V : Type*} [subspace V] [subspace_intersection_subspace_def]
  (h : ∀ {A B : subspace V}, A ∩ B ⊆ A) :
  subspace (A ∩ B) :=
sorry

theorem exercise_1_9 {V : Type*} [vector_space V]
  [subspace_subspace_union_subspace_subspace_union_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_subspace_sub:=
sorry

theorem exercise_3_1 {V : Type*} 
  [field V] [add_comm_group V] [module V V] [one_dim_space V] 
  (hv : is_one_dim V) (hT : T.is_linear) (h : T.is_multiplication_by_scalar) :
  T.is_multiplication_by_scalar :=
sorry

theorem exercise_3_8 {V W : Type*} [fintype V] [fintype W]
  [finite_dimensional V] [finite_dimensional W] (T : linear_map V → W)
  (hT : T.is_linear) (h : T.is_injective) (hU : T.range = T.null)
  (hU : T.range ⊆ U) :
  exists U : subspace V, U ⊆ V, U ⊆ T.range ∧ U ⊆ U ∧ U ∩ T.null = {0} :=
sorry

theorem exercise_4_4 {p : complex_polynomial} 
  [degree p = m] [degree p^{\prime} = m] (h : p.degree = m) (hp : p.degree = m)
  (hdp : p.degree = m) (h : p.degree = m → p.degree = m) (hp : p.degree = m → p.degree = m)
  (h : distinct_roots p) (hdp : distinct_roots p^{\prime}) :
  distinct_roots p ↔ distinct_roots p^{\prime} :=
sorry

theorem exercise_5_1 {V : Type*} [vector_space V] 
  [subspace U V] [subspace U₁ V] [subspace U₂ V] [subspace U₃ V] 
  (hU₁ : subspace U₁ V) (hU₂ : subspace U₂ V) (hU₃ : subspace U₃ V)
  (h : subspace.sum U₁ U₂ U₃) : subspace.sum U₁ U₂ U₃ :=
sorry

theorem exercise_5_11 {V : Type*} [linear_space V]
  [linear_space_space V] [linear_space_space_space V]
  (hS : ∀ v : V, S.is_linear v) (hT : ∀ v : V, T.is_linear v)
  (hST : ∀ v : V, S.is_linear v ∧ T.is_linear v)
  (h : eigenvalues S T = eigenvalues T S) :
  eigenvalues (S T) = eigenvalues (T S) :=
sorry

theorem exercise_5_12 {V : Type*} [field K] 
  [add_comm_group V] [module K V] [eigen_vector_of_T V] 
  (hT : T.is_scalar_multiple_of_identity) :
  T.is_scalar_multiple_of_identity :=
sorry

theorem exercise_5_13 {V : Type*} [linear_space V]
  [subspace_dim V-1] (T : linear_space V) (h : T.is_scalar_multiple_of_identity) :
  T.is_scalar_multiple_of_identity :=
sorry

theorem exercise_5_20eig_vect_same_eig_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_vect_same_eig_:=
sorry

theorem exercise_5_24 {V : Type*} [vector_space V]
  [eigenvalue_free_operator T : operator V] [subspace V]
  (h : dimension V = 2) :
  dimension (subspace.invariant T) = 2 :=
sorry

theorem exercise_5_4 {V : Type*} [field V] 
  [add_comm_group V] [module V V] {S T : endofunctor V} 
  (hS : S.is_symmetric) (hT : T.is_symmetric) (h : S.is_endofunctor)
  (hλ : T.is_endofunctor) (hλ : T.is_symmetric) :
  null (T - λ I) = null (T - λ I) :=
sorry

theorem exercise_6_13 {V : Type*} 
  [vector_space V] [add_comm_group V] [module V ℝ] 
  {e : V} (h : ∀ (i : ℕ), e.is_orthonormal) 
  (hv : ∀ (i : ℕ), ∃ (v : V), v.is_orthonormal ∧ v = e.sum i) :
  ∀ v : V, v.norm² = ∑ i : ℕ, e.sum i.norm²
  :=
sorry

theorem exercise_6_16 {U V : Type*} [subspace U] [subspace V]
  (h : U^⊥ = {0}) : U = V :=
sorry

theorem exercise_6_2 {V : Type*} [field V] 
  [add_comm_group V] [module V ℝ] {u v : V} (hu : u.is_ortho v) 
  (hv : ∀ a : ℝ, u.norm ≤ a.norm) :
  u.norm ≤ ∞.norm (u + v) :=
sorry

theorem exercise_6_3lemma_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_le_sum_squared_:=
sorry

theorem exercise_6_7 {V : Type*} [complex_space V]
  [inner_product_space V] (h : complex_space.is_inner_product V)
  (hv₁ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₂ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₃ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₄ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₅ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₆ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₇ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₈ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₉ : ∀ (u : V), u.real = 0 → u.imag = 0)
  (hv₁₀ : ∀ (u : V), u.imag = 0 → u.real = 0)
  (hv₁₁ : ∀ (u : V), u.imag = 0 → u.real = 0)
  (hv₁₂ : ∀ (u : V), u.imag = 0 → u.real = 0)
  (hv₁):=
sorry

theorem exercise_7_10 {V : Type*} 
  [complex_inner_product V] [normal_operator V] [self_adjoint V]
  (h : T.is_normal) (hT : T.squared = T) (hT2 : T.squared = T)
  (hT3 : T.squared = T) (hT4 : T.squared = T) (hT5 : T.squared = T)
  (hT6 : T.squared = T) (hT7 : T.squared = T) (hT8 : T.squared = T)
  (hT9 : T.squared = T) (hT10 : T.squared = T) (hT11 : T.squared = T)
  (hT12 : T.squared = T) (hT13 : T.squared = T) (hT14 : T.squared = T)
  (hT15 : T.squared = T) (hT16 : T.squared = T) (hT17 : T.squared = T)
  (hT18 : T.squared = T) (hT19 : T.squared = T) (hT20 : T.squared = T)
  (hT21 : T.squared = T) (hT22 : T.squared = T) (hT23 : T.squared = T)
  (hT24 : T.squared = T) (hT25 : T.squared = T) (hT26 : T.squared = T)
  (hT27) :=
sorry

theorem exercise_7_11 {V : Type*} [complex_space V] [normal_operator V]
  (h : ∀ (T : normal_operator V), ∃ (S : V → V), S.is_square_root T) :=
sorry

theorem exercise_7_14 {V : Type*} 
  [field V] [add_comm_group V] [module V] [self_adjoint V] 
  (hv : norm 1 v = 1) (hT : T.is_self_adjoint) (hT.eig : T.eig v) 
  (hT.eig.eig : ∃ (λ : ℝ), norm 1 v = λ.norm v) (hT.eig.eig.eig : norm 1 v = λ.norm v) 
  (hT.eig.eig.eig : norm 1 v = λ.norm v) (hT.eig.eig.eig.eig : norm 1 v = λ.norm v) 
  (hT.eig.eig.eig.eig : norm 1 v = λ.norm v) (hT.eig.eig.eig.eig.eig : norm 1 v = λ.norm v) 
  (hT.eig.eig.eig.eig.eig : norm 1 v = λ.norm v) (hT.eig.eig.eig.eig.eig.eig : norm 1 v = λ.norm v) 
  (hT.eig.eig.eig.eig.eig.eig : norm 1 v = λ.norm v) (hT.eig.eig.eig.eig.eig.eig.eig : norm 1 v = λ.norm v) 
  (hT):=
sorry

theorem exercise_7_5 {V : Type*} [field K] 
  [add_comm_group V] [module K V] [finite_dimensional K V] 
  {n : ℕ} (h : n ≥ 2) :
  normal_operators K V ⊆ℝ_ℝ :=
sorry

theorem exercise_7_6 {V : Type*} [linear_space V] [linear_space V]
  (h : normal T) (hT : range T = range T.adjoint) :
  range T = range T.adjoint :=
sorry

theorem exercise_7_9 {A : Type*} [complex_space A]
  [normal_operator A] (h : self_adjoint A) :
  real_eigenvalues A.eigenspace A.eigenspace.real_eigenspace h.eigenspace :=
sorry