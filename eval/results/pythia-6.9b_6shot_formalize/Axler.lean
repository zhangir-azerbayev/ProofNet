import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_2 {x : ℝ} [field ℚ] : x ^ 3 = 1 :=
sorry

theorem exercise_1_4 {F V : Type*} [field F] [module V]
  [add_comm_group V] [finite_dimensional V] [finite_dimensional V]
  (a : F) (v : V) (h : a = 0 → v = 0) :
  a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_7 {R : Type*} [field R] [field R]
  [add_comm_group R] [module R R] [finite_dimensional R]
  (h : finite_dimensional.finrank R + 1 < 2) :
  ∃ (u : R → R), u • u ≠ 0 ∧ u • u = 0 :=
sorry

theorem exercise_1_9 {V : Type*} [vector_space V]
  [add_comm_group V] [module V V] [finite_dimensional V]
  (h : finite_dimensional.finrank V + 1 < ∞) :
  subspace V :=
sorry

theorem exercise_3_8 {V W : Type*} [finite_dimensional V] [finite_dimensional W]
  (T : linear_operator V W) (U : subspace V) (T_U : T = 0) :
  subspace.null (T_U) :=
sorry

theorem exercise_5_1 {V : Type*} [vector_space V]
  [add_comm_group V] [module V V] [linear_operator V] [linear_operator V V]
  (T : linear_operator V) (U1 : invariant_under T U1) (U2 : invariant_under T U2)
  (U3 : invariant_under T U3) :
  invariant_under T (U1 + U2 + U3) :=
sorry

theorem exercise_5_11 {V : Type*} [vector_space V] [field V]
  (S : V → V) (T : V → V) (h : S T = T S) :
  S T = T S :=
sorry

theorem exercise_5_13 {V : Type*} [vector_space V]
  [field V] (T : linear_operator V) (h : ∀ (v : V), T.v = v) :
  scalar_mult T.identity :=
sorry

theorem exercise_5_24 {V : Type*} [vector_space V] [linear_operator V]
  (T : linear_operator V) (W : subspace V) :
  dim W = dim W.subtype :=
sorry

theorem exercise_6_3  {n : ℕ} (a : ℝ) (b : ℝ) (h : ∀ j : ℕ, j < n → a < b) :
  ∀ j : ℕ, j < n → a < b → a < ∑ j : ℕ, j < n a j :=
sorry

theorem exercise_6_13span_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal_orthogonal:=
sorry

theorem exercise_7_5 {V : Type*} [vector_space V]
  [dim_space V] (dim : ℕ) (h : ∀ n : ℕ, dim.dim V n < 2) :
  normal_operator V :=
sorry

theorem exercise_7_9 {A : Type*} [complex_inner_product_space A]
  [complex_inner_product_space A] (h : normal A) (hA : self_adjoint A) :
  real_eigenvalues A = real_eigenvalues hA :=
sorry

theorem exercise_7_11 {V : Type*} [complex_inner_product_space V]
  [complex_inner_product_space V] (S : normal_operator V) (T : normal_operator V)
  (h : square_root S = square_root T) :
  square_root S = square_root T :=
sorry