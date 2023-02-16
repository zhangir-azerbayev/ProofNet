import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators

theorem exercise_1_2 :
  (⟨-1/2, real.sqrt 3 / 2⟩ : ℂ) ^ 3 = -1 :=
sorry

theorem exercise_1_3 {F V : Type*} [add_comm_group V] [field F]
  [module F V] {v : V} : -(-v) = v :=
sorry

theorem exercise_1_4 {F V : Type*} [add_comm_group V] [field F]
  [module F V] (v : V) (a : F): a • v = 0 ↔ a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_6 : ∃ U : set (ℝ × ℝ),
  (U ≠ ∅) ∧
  (∀ (u v : ℝ × ℝ), u ∈ U ∧ v ∈ U → u + v ∈ U) ∧
  (∀ (u : ℝ × ℝ), u ∈ U → -u ∈ U) ∧
  (∀ U' : submodule ℝ (ℝ × ℝ), U ≠ ↑U') :=
sorry

theorem exercise_1_7 : ∃ U : set (ℝ × ℝ),
  (U ≠ ∅) ∧
  (∀ (c : ℝ) (u : ℝ × ℝ), u ∈ U → c • u ∈ U) ∧
  (∀ U' : submodule ℝ (ℝ × ℝ), U ≠ ↑U') :=
sorry

theorem exercise_1_8 {F V : Type*} [add_comm_group V] [field F]
  [module F V] {ι : Type*} (u : ι → submodule F V) :
  ∃ U : submodule F V, (⋂ (i : ι), (u i).carrier) = ↑U :=
sorry

theorem exercise_1_9 {F V : Type*} [add_comm_group V] [field F]
  [module F V] (U W : submodule F V):
  ∃ U' : submodule F V, U'.carrier = ↑U ∩ ↑W ↔ U ≤ W ∨ W ≤ U :=
sorry

theorem exercise_3_1 {F V : Type*}  
  [add_comm_group V] [field F] [module F V] [finite_dimensional F V]
  (T : V →ₗ[F] V) (hT : finrank F V = 1) :
  ∃ c : F, ∀ v : V, T v = c • v:=
sorry

theorem exercise_3_8 {F V W : Type*}  [add_comm_group V]
  [add_comm_group W] [field F] [module F V] [module F W]
  (L : V →ₗ[F] W) :
  ∃ U : submodule F V, U ⊓ L.ker = ⊥ ∧
  linear_map.range L = range (dom_restrict L U):=
sorry

theorem exercise_4_4 (p : polynomial ℂ) :
  p.degree = @card (root_set p ℂ) (polynomial.root_set_fintype p ℂ) ↔
  disjoint
  (@card (root_set p.derivative ℂ) (polynomial.root_set_fintype p.derivative ℂ))
  (@card (root_set p ℂ) (polynomial.root_set_fintype p ℂ)) :=
sorry

theorem exercise_5_1 {F V : Type*} [add_comm_group V] [field F]
  [module F V] {L : V →ₗ[F] V} {n : ℕ} (U : fin n → submodule F V)
  (hU : ∀ i : fin n, map L (U i) = U i) :
  map L (∑ i : fin n, U i : submodule F V) =
  (∑ i : fin n, U i : submodule F V) :=
sorry

theorem exercise_5_4 {F V : Type*} [add_comm_group V] [field F]
  [module F V] (S T : V →ₗ[F] V) (hST : S ∘ T = T ∘ S) (c : F):
  map S (T - c • id).ker = (T - c • id).ker :=
sorry

theorem exercise_5_11 {F V : Type*} [add_comm_group V] [field F]
  [module F V] (S T : End F V) :
  (S * T).eigenvalues = (T * S).eigenvalues  :=
sorry

theorem exercise_5_12 {F V : Type*} [add_comm_group V] [field F]
  [module F V] {S : End F V}
  (hS : ∀ v : V, ∃ c : F, v ∈ eigenspace S c) :
  ∃ c : F, S = c • id :=
sorry

theorem exercise_5_13 {F V : Type*} [add_comm_group V] [field F]
  [module F V] [finite_dimensional F V] {T : End F V}
  (hS : ∀ U : submodule F V, finrank F U = finrank F V - 1 →
  map T U = U) : ∃ c : F, T = c • id :=
sorry

theorem exercise_5_20 {F V : Type*} [add_comm_group V] [field F]
  [module F V] [finite_dimensional F V] {S T : End F V}
  (h1 : @card T.eigenvalues (eigenvalues.fintype T) = finrank F V)
  (h2 : ∀ v : V, ∃ c : F, v ∈ eigenspace S c ↔ ∃ c : F, v ∈ eigenspace T c) :
  S * T = T * S :=
sorry

theorem exercise_5_24 {V : Type*} [add_comm_group V]
  [module ℝ V] [finite_dimensional ℝ V] {T : End ℝ V}
  (hT : ∀ c : ℝ, eigenspace T c = ⊥) {U : submodule ℝ V}
  (hU : map T U = U) : even (finrank U) :=
sorry

theorem exercise_6_2 {V : Type*} [add_comm_group V] [module ℂ V]
  [inner_product_space ℂ V] (u v : V) :
  ⟪u, v⟫_ℂ = 0 ↔ ∀ (a : ℂ), ∥u∥ ≤ ∥u + a • v∥ :=
sorry

theorem exercise_6_3 {n : ℕ} (a b : fin n → ℝ) :
  (∑ i, a i * b i) ^ 2 ≤ (∑ i : fin n, i * a i ^ 2) * (∑ i, b i ^ 2 / i) :=
sorry

theorem exercise_6_7 {V : Type*} [inner_product_space ℂ V] (u v : V) :
  ⟪u, v⟫_ℂ = (∥u + v∥^2 - ∥u - v∥^2 + I*∥u + I•v∥^2 - I*∥u-I•v∥^2) / 4 :=
sorry

theorem exercise_6_13 {V : Type*} [inner_product_space ℂ V] {n : ℕ}
  {e : fin n → V} (he : orthonormal ℂ e) (v : V) :
  ∥v∥^2 = ∑ i : fin n, ∥⟪v, e i⟫_ℂ∥^2 ↔ v ∈ span ℂ (e '' univ) :=
sorry

theorem exercise_6_16 {K V : Type*} [is_R_or_C K] [inner_product_space K V]
  {U : submodule K V} : 
  U.orthogonal = ⊥  ↔ U = ⊤ :=
sorry 

theorem exercise_7_5 {V : Type*} [inner_product_space ℂ V] 
  [finite_dimensional ℂ V] (hV : finrank V ≥ 2) :
  ∀ U : submodule ℂ (End ℂ V), U.carrier ≠
  {T | T * T.adjoint = T.adjoint * T} :=
sorry

theorem exercise_7_6 {V : Type*} [inner_product_space ℂ V]
  [finite_dimensional ℂ V] (T : End ℂ V)
  (hT : T * T.adjoint = T.adjoint * T) :
  T.range = T.adjoint.range :=
sorry

theorem exercise_7_9 {V : Type*} [inner_product_space ℂ V]
  [finite_dimensional ℂ V] (T : End ℂ V)
  (hT : T * T.adjoint = T.adjoint * T) :
  is_self_adjoint T ↔ ∀ e : T.eigenvalues, (e : ℂ).im = 0 :=
sorry

theorem exercise_7_10 {V : Type*} [inner_product_space ℂ V]
  [finite_dimensional ℂ V] (T : End ℂ V)
  (hT : T * T.adjoint = T.adjoint * T) (hT1 : T^9 = T^8) :
  is_self_adjoint T ∧ T^2 = T :=
sorry

theorem exercise_7_11 {V : Type*} [inner_product_space ℂ V]
  [finite_dimensional ℂ V] {T : End ℂ V} (hT : T*T.adjoint = T.adjoint*T) :
  ∃ (S : End ℂ V), S ^ 2 = T :=
sorry

theorem exercise_7_14 {𝕜 V : Type*} [is_R_or_C 𝕜]
  [inner_product_space 𝕜 V] [finite_dimensional 𝕜 V]
  {T : End 𝕜 V} (hT : is_self_adjoint T)
  {l : 𝕜} {ε : ℝ} (he : ε > 0) : ∃ v : V, ∥v∥ = 1 ∧ ∥T v - l • v∥ < ε →
  ∃ l' : T.eigenvalues, ∥l - l'∥ < ε :=
sorry