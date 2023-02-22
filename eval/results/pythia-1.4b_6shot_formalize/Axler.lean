import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_2 :=
sorry

theorem exercise_1_4zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_zero_element_:=
sorry

theorem exercise_1_7 {U : Type*} [field R2] [module R2 U]
  [finite_dimensional R2 U] {x : U} (hx : ∀ (y : U), x.x = y.x) :
  ∃ (y : U), x.x = y.x :=
sorry

theorem exercise_1_9 {V : Type*} [field V] [module V V]
  [finite_dimensional V] {V : finite_dimensional V} (V1 : V.dim V = n) (V2 : V.dim V = m) :
  sub_union V1 V2 :=
sorry

theorem exercise_3_8 {V W : Type*} [field V] [module W V] [finite_dimensional V] 
  {T : ℤ} (h : finite_dimensional.finrank W + 1 < T.card) :
  ∃ (U : ℤ) (U : ℤ → V), U.range T = U.null ∧ U.range T = U.range W :=
sorry

theorem exercise_5_1invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_invariant_subspaces_:=
sorry

theorem exercise_5_11 {V : Type*} [field V] [module V V]
  [finite_dimensional V] [finite_dimensional V]
  (h : finite_dimensional.finrank V + 1 < finrank V) :
  eig_eigenvalues V :=
sorry

theorem exercise_5_13 {V : Type*} [field V] 
  [add_comm_group V] [module V V] [finite_dimensional V] 
  {t : finset V} (h : finite_dimensional.finrank V + 1 < t.card) :
  ∃ (f : V → K), t.sum (λ (e : V), f e • e) = 0 ∧ t.sum (λ (e : V), f e) = 0 
  ∧ ∃ (x : V) (H : x ∈ t), f x ≠ 0 :=
sorry

theorem exercise_5_24invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant_subspace_of_invariant:=
sorry

theorem exercise_6_3sum_of_square_of_square_of_sum_of_square_of_sum_of_square_of_sum_of_square_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_:=
sorry

theorem exercise_6_13 {V : Type*} [field V] [module V V]
  [finite_dimensional V] [fintype V] {m : ℕ} [fintype V] {n : ℕ} [fintype V] {p : ℕ} [fintype V] {q : ℕ}
  (h : finite_dimensional.finrank V + 1 < p.card) :
  orthonormal_span V :=
sorry

theorem exercise_7_5 {V : Type*} [group V] [module V]
  [finite_dimensional V] {n : ℕ} {m : ℕ} (h : n ≠ m) :
  ∃ (x : V), ∃ (y : V), ∃ (z : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),
  ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),
  ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),
  ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),
  ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),
  ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),
  ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),
  ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (:=
sorry

theorem exercise_7_9 {X : Type*} [inner_product_space X] [normal_operator X]
  (h : normal_operator X) :
  self_adjoint X :=
sorry

theorem exercise_7_11 {V : Type*} [inner_product_space V] 
  [module V V] [finite_dimensional V] [finite_dimensional V]
  (h : ∀ (x : V), x.norm (x) = 1) :
  ∃ (S : ∈ V, S.norm = 1), ∃ (T : ∈ V, T.norm = 1) :
  ∃ (x : V), ∃ (y : V), ∃ (z : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V), ∃ (w : V), ∃ (u : V), ∃ (v : V),:=
sorry