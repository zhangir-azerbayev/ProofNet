import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators





theorem exercise_1_2 {K : Type*} [field K] [add_comm_group K] [module K K]
  [finite_dimensional K K] (n : ℕ) :
  (1 : K) ^ n = 1 :=
sorry

theorem exercise_1_4 {V : Type*} [add_comm_group V] [module K V]
  {a : V} (ha : a ≠ 0) : is_zero a → is_zero (a • a) :=
sorry

theorem exercise_1_7 {U : set ℝ^2} (hU : U.nonempty) :
  (∀ (a : ℝ), a ∈ U → ∃ (b : ℝ), b ∈ U ∧ a * b ∈ U) ↔ U.nonempty :=
sorry

theorem exercise_1_9  {V W : Type*} [add_comm_group V] [module K V] [module K W] [finite_dimensional K V]
  [finite_dimensional K W] {U V W : Type*} [add_comm_group U] [module K U] [module K V]
  [module K W] [finite_dimensional K U] [finite_dimensional K V] [finite_dimensional K W]
  (hU : U.finite) (hV : V.finite) (hW : W.finite) :
  U ∪ V ∪ W.finite = U.finite ∪ V ∪ W :=
sorry

theorem exercise_3_8 {V W : Type*} [finite_dimensional V] [finite_dimensional W]
  {T : V → W} (hT : range T = {0}) :
  ∃ (U : submodule.span V W), U.dim < fintype.card V ∧ range T ⊆ U ∧ U.null :=
sorry

theorem exercise_5_1  {V : Type*} [add_comm_group V] [module K V] {T : L(V)} {U : submodule K V}
  (hT : T ∈ L(V)) (hU : U ∈ L(V)) :
  T.sum (λ (e : V), e) ∈ U :=
sorry

theorem exercise_5_11 {V W : Type*} [add_comm_group V] [module K V]
  [add_comm_group W] [module K W] {S T : L(V, W)} (hS : S.is_linear) (hT : T.is_linear) :
  (S.prod T).is_linear :=
sorry

theorem exercise_5_13  (T : V → V) (hT : is_scalar_multiple T) :
  ∃ (v : V), T v = v ∧ ∀ (v : V), T v = v :=
sorry

theorem exercise_5_24 {V : Type*} [inner_product_space V]
  {T : V → V} (hT : linear_map T = T) (v : V) :
  even_dim_subspace T v → even_dim_subspace T (T v) :=
sorry

theorem exercise_6_3 {n : ℕ} {a b : ℝ} {x y : ℝ}
  (hx : x ∈ [0, 1]) (hy : y ∈ [0, 1]) :
  (∑ i in range n, x i * y i) ^ 2 ≤ (∑ i in range n, i * (x i) ^ 2) * (∑ i in range n, (y i) ^ 2) :=
sorry

theorem exercise_6_13 :
  ∥v∥^{2} = ∑ i : fin m, ∥e_{i}∥^{2} :=
sorry

theorem exercise_7_5 {V : Type*} [nontrivial V] [finite_dimensional V] :
  ¬(subspace.span {normal_operator V}) :=
sorry

theorem exercise_7_9 {V : Type*} [inner_product_space V]
  {A : V → V} (hA : is_self_adjoint A) :
  is_real (λ x, ∥A x∥) ↔ ∀ x : V, ∥x∥ = ∥A x∥ :=
sorry

theorem exercise_7_11  {V : Type*} [inner_product_space V] [normed_space V] {T : V → V}
  (hT : ∀ x, ∥T x∥ = ∥x∥) :
  ∃ S : V → V, ∀ x, S x = x / ∥x∥ ∧ ∥S x∥ = ∥x∥ ∧ ∀ y, ∥S y∥ ≤ ∥y∥ → ∥S y∥ = ∥y∥ :=
sorry