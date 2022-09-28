import linear_algebra.finite_dimensional
import linear_algebra.eigenspace
import data.complex.basic
import analysis.inner_product_space.basic
import analysis.inner_product_space.projection

open set fintype complex polynomial submodule linear_map 
open finite_dimensional module module.End inner_product_space

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

-- set builder
theorem exercise_2_1 {F V : Type*} [add_comm_group V] [field F]
  [module F V] {n : ℕ} (ι : fin n → V) (h : span (set.image ι (fin n))) :
   :=
sorry

-- same issue as above
theorem exercise_2_2 
    {V : Type*} [add_comm_group V] [vector_space ℂ V] 
    {v : list V} (hv : linear_independent ℂ v) :
    linear_independent ℂ (list.sub_list v) :=
sorry

-- space of continuous functions?
theorem exercise_2_6 : finite_dimensional ℝ 
  ({f ∣ λ f : set.Icc 0 1 → ℝ, (continuous f).is_open_preimage} ) → false :=
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

theorem exercise_3_9 
    {F : Type*} [field F] {n m : ℕ} (T : F^n → F^m) 
    (hT : T.null_space = span ({(5,1,0,0),(0,0,7,1)} : set (F^4))) : 
    T.surjective :=
sorry

theorem exercise_3_10 
    (F : Type*) [field F] (f : F → F) (hf : linear_map F F f) 
    (h : f.null_space = span F (set.range (λ (i : fin 5), 
    (λ (x : F) (i : fin 5), x) (3 : F) i))) : 
    injective f :=
sorry

theorem exercise_3_11 {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {W : Type*} 
    [add_comm_group W] [vector_space ℂ W] (f : V →ₗ[ℂ] W) 
    (hf : finite_dimensional ℂ (f.null_space) ∧ 
    finite_dimensional ℂ (f.range)) : finite_dimensional ℂ V :=
sorry

-- is this correct? 
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

theorem exercise_6_16 {V : Type*} [inner_product_space ℂ V] 
  {U : submodule ℂ V} :
  Uᗮ = ⊥ ↔ U = ⊤ :=
sorry

-- same problem as with group theory stuff
theorem exercise_6_17 {V : Type*} [inner_product_space ℂ V]
  [complete_space V]
  {P : End ℂ V} (hP : P ^ 2 = P) 
  (hP1 : ∀ u v : V, u ∈ P.ker → v ∈ linear_map.range P → ⟪u, v⟫_ℂ = 0) : 
  ∃ U : submodule ℂ V, complete_space U ∧ P = orthogonal_projection U :=
sorry

theorem exercise_6_18 {V : Type*} [inner_product_space ℂ V]
    {P : V → V} (hP : self_adjoint P) (hP2 : P ∘ P = P) 
    (hP_le : ∀ v : V, ∥P v∥ ≤ ∥v∥) : is_projection P :=
sorry

theorem exercise_6_19 {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {T : V →ₗ[ℂ] V} 
    (U : submodule ℂ V) :
    U.is_invariant T ↔ T.comp (U.subtype.projection) = 
    (U.subtype.projection).comp T :=
sorry

theorem exercise_6_20 {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] (T : V →ₗ[ℂ] V) (U : submodule ℂ V) :
    is_invariant U T ↔ T.comp (projection U) = projection U.comp T :=
sorry

theorem exercise_6_29 {V : Type*} 
    [inner_product_space ℂ V] {T : V →ₗ[ℂ] V} (U : submodule ℂ V) :
    U.is_invariant T ↔ U.orthogonal.is_invariant T.adjoint :=
sorry

theorem exercise_7_4 {V : Type*} [inner_product_space ℂ V]
    [normed_group V] [normed_space ℂ V] (P : V →ₗ[ℂ] V) (hP : P.is_projection) :
    P.is_self_adjoint ↔ P.is_projection :=
sorry

theorem exercise_7_5 {V : Type*} [finite_dimensional ℂ V]
    (hV : dim V ≥ 2) :
    ¬ (normal_ops V).is_subspace :=
sorry

theorem exercise_7_6 {V : Type*} [inner_product_space ℂ V] 
    [normed_space ℂ V] (T : V →ₗ[ℂ] V) (hT : T.is_normal) :
    T.range = T.adjoint.range :=
sorry

theorem exercise_7_8 {R : Type*} [ring R] 
    [semiring R] [add_comm_group R] [module R R] [vector_space ℝ R] 
    [inner_product_space ℝ R] [normed_group R] [normed_space ℝ R] 
    [complete_space ℝ R] (T : R →ₗ[R] R) (hT : T.is_self_adjoint) 
    (hT1 : T (1 : R) = 0) (hT2 : T (2 : R) = 2 :=
sorry

theorem exercise_7_9 {V : Type*} [inner_product_space ℂ V]
    [normed_group V] [normed_space ℂ V] {T : V →ₗ[ℂ] V} (hT : T.is_normal) :
    T.is_self_adjoint ↔ ∀ (λ : ℂ), T.is_eigenvalue λ → λ.is_real :=
sorry

theorem exercise_7_10 {V : Type*} [inner_product_space ℂ V]
    {T : V →ₗ[ℂ] V} (hT : is_normal T) (hT9 : T ^ 9 = T ^ 8) :
    T = T⁻¹ ↔ is_self_adjoint T :=
sorry

theorem exercise_7_11 {V : Type*} [inner_product_space ℂ V] 
    {T : V →ₗ[ℂ] V} (hT : T.is_normal) :
    ∃ (S : V →ₗ[ℂ] V), S.is_normal ∧ S ^ 2 = T :=
sorry

theorem exercise_7_14 
    {V : Type*} [inner_product_space ℂ V] [normed_group V] [normed_space ℂ V]
    {T : V →ₗ[ℂ] V} (hT : self_adjoint T) (λ : ℂ) (ε : ℝ) 
    (hε : ε > 0) (v : V) (hv : ∥v∥ = 1) (hvT : ∥T v - λ • v∥ < ε) : :=
sorry

theorem exercise_7_15 {U : Type*} 
    [finite_dimensional ℝ U] (T : linear_map ℝ U U) :
    ∃ (b : set U), is_basis ℝ b ∧ ∀ (v : U), v ∈ b → is_eigenvector ℝ T v ↔ 
    ∃ (ip : inner_product_space ℝ U), is_self_adjoint ℝ ip T :=
sorry

theorem exercise_7_17 {V : Type*} [inner_product_space ℂ V] 
    {A B : V →ₗ[ℂ] V} (hA : A.is_self_adjoint) (hB : B.is_self_adjoint) 
    (hApos : A.is_positive) (hBpos : B.is_positive) :
    (A + B).is_positive :=
sorry

theorem exercise_7_18 {V : Type*} [inner_product_space ℂ V] [normed_group V] 
    [normed_space ℂ V] (T : V →ₗ[ℂ] V) (hT : T.is_positive) (k : ℕ) :
    T^k.is_positive :=
sorry
