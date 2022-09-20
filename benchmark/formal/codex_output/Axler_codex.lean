theorem exercise_1_2 : is_cube_root_of_1 ⟨-1, 1⟩ :=
sorry

theorem exercise_1_3 (v : V) : -(-v) = v :=
sorry

theorem exercise_1_4 {F : Type*} [field F] {V : Type*} 
    [add_comm_group V] [vector_space F V] (a : F) (v : V) :
    a • v = 0 ↔ a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_6 {R : Type*} [ring R] {U : set (R × R)} 
    (hU : U ≠ ∅) (hU_add : ∀ (u v : R × R), u ∈ U → v ∈ U → u + v ∈ U) 
    (hU_neg : ∀ (u : R × R), u ∈ U → -u ∈ U) :
    ¬ is_submodule U :=
sorry

theorem exercise_1_7 (U : set ℝ^2) (hU : U ≠ ∅) (hU_scalar : ∀ (a : ℝ) (x : ℝ^2), x ∈ U → a • x ∈ U) :
  ¬ is_subspace U :=
sorry

theorem exercise_1_8 {R : Type*} [comm_ring R] {I : Type*} 
    [decidable_eq I] {V : Type*} [add_comm_group V] [module R V] 
    (S : I → submodule R V) :
    is_submodule R (⋂ i, S i) :=
sorry

theorem exercise_1_9 {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] (U W : submodule ℂ V) :
    submodule.is_subspace (U ⊔ W) ↔ U ≤ W ∨ W ≤ U :=
sorry

theorem exercise_2_1 {V : Type*} [add_comm_group V] [vector_space ℂ V]
    {v : ℕ → V} (hv : span ℂ (range v) = ⊤) :
    span ℂ (range (λ i, v i - v (i + 1))) = ⊤ :=
sorry

theorem exercise_2_2 
    {V : Type*} [add_comm_group V] [vector_space ℂ V] 
    {v : list V} (hv : linear_independent ℂ v) :
    linear_independent ℂ (list.sub_list v) :=
sorry

theorem exercise_2_6 : 
    infinite_dimensional ℝ (continuous_functions_on_01) :=
sorry

theorem exercise_3_1 {V : Type*} [field ℂ] [add_comm_group V] 
    [vector_space ℂ V] {T : V → V} (hT : linear_map ℂ V V T) 
    (hdim : dim ℂ V = 1) :
    ∃ (a : ℂ), ∀ (v : V), T v = a • v :=
sorry

theorem exercise_3_8 {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {W : Type*} 
    [add_comm_group W] [vector_space ℂ W] (T : V →ₗ[ℂ] W) 
    (hT : T.range = T.range) (hT' : T.null_space = ⊥) :
    ∃ (U : submodule ℂ V), U.comap T = ⊥ ∧ T.range = U.range :=
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

theorem exercise_4_4 
    (p : polynomial ℂ) (m : ℕ) (h : p.degree = m) :
    p.has_n_roots m ↔ p.derivative.roots.disjoint p.roots :=
sorry

theorem exercise_5_1 {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] {T : V →ₗ[ℂ] V} (U : set (submodule ℂ V)) 
    (hU : ∀ (u : submodule ℂ V), u ∈ U → T.range ⊆ u) :
    T.range ⊆ ⨆ u ∈ U, u :=
sorry

theorem exercise_5_4 {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] {S T : V →ₗ[ℂ] V} (hST : S.commutes T) 
    (λ : ℂ) :
    is_submodule (null_space (T - λ • linear_map.id V)) S :=
sorry

theorem exercise_5_11 {V : Type*} [add_comm_group V] [vector_space ℂ V]
    [finite_dimensional ℂ V] (S T : linear_map ℂ V ℂ V) :
    eigenvalues (S * T) = eigenvalues (T * S) :=
sorry

theorem exercise_5_12 {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] [finite_dimensional ℂ V] 
    (T : V →ₗ[ℂ] V) (hT : ∀ v : V, is_eigenvector ℂ T v) :
    ∃ (c : ℂ), T = c • linear_map.id :=
sorry

theorem exercise_5_13 {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {T : V →ₗ[ℂ] V} 
    (hT : ∀ (U : submodule ℂ V), dim ℂ U = dim ℂ V - 1 → U.is_invariant T) :
    ∃ (c : ℂ), T = c • linear_map.id :=
sorry

theorem exercise_5_20 {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] {T S : V →ₗ[ℂ] V} (hT : is_diagonalizable T) 
    (hS : is_diagonalizable S) (h : ∀ (v : V), is_eigenvector T v → is_eigenvector S v) :
    S.comp T = T.comp S :=
sorry

theorem exercise_5_24 {V : Type*} [add_comm_group V] 
    [vector_space ℝ V] {T : V → V} (hT : linear_map ℝ V V T) 
    (hT_no_eigenvalues : ∀ (v : V), v ≠ 0 → T v ≠ v) :
    ∀ (U : submodule ℝ V), T.range ⊆ U → T.ker ⊆ U → 
    (U.dim % 2 : ℕ) = 0 :=
sorry

theorem exercise_6_2 {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] [inner_product ℂ V] 
    (u v : V) :
    inner_product u v = 0 ↔ ∀ (a : ℂ), ∥u∥ ≤ ∥u + a • v∥ :=
sorry

theorem exercise_6_3 
    {α : Type*} [add_comm_monoid α] [semimodule ℝ α] 
    (a b : fin n → α) :
    (∑ i, a i * b i) ^ 2 ≤ (∑ i, i * a i ^ 2) * (∑ i, b i ^ 2 / i) :=
sorry

theorem exercise_6_7 
    {V : Type*} [inner_product_space ℂ V] (u v : V) :
    inner u v = (norm_squared (u + v) - norm_squared (u - v) + 
    norm_squared (u + I • v) * I - norm_squared (u - I • v) * I) / 4 :=
sorry

theorem exercise_6_13 {V : Type*} 
    [inner_product_space ℂ V] {e : fin m → V} (he : orthonormal e) 
    (v : V) :
    ∥v∥^2 = ∑ i, ∥inner_product ℂ v (e i)∥^2 ↔ v ∈ span ℂ (e '' univ) :=
sorry

theorem exercise_6_16 {V : Type*} [inner_product_space ℂ V] 
    {U : submodule ℂ V} :
    U.orthogonal = ⊥ ↔ U = ⊤ :=
sorry

theorem exercise_6_17 {V : Type*} [inner_product_space ℂ V]
    {P : V → V} (hP : is_projection P) (hP2 : P ∘ P = P) 
    (hnull : ∀ (v : V), v ∈ null_space P → ∀ (w : V), w ∈ range P → v ⊥ w) :
    is_orthogonal_projection P :=
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