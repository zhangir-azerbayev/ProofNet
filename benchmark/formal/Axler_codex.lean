theorem exercise_1.2
 cube_root_of_1_of_sqrt_3_i : is_cube_root_of_1 ⟨-1, 1⟩ 

theorem exercise_1.3
 neg_neg_eq_self (v : V) : -(-v) = v 

theorem exercise_1.4
 mul_zero_iff_zero_or_zero {F : Type*} [field F] {V : Type*} 
    [add_comm_group V] [vector_space F V] (a : F) (v : V) :
    a • v = 0 ↔ a = 0 ∨ v = 0 

theorem exercise_1.6
 not_subspace_of_add_inv_closed {U : Type*} [add_comm_group U] 
    [vector_space ℝ U] (hU : set.nonempty U) (hU_add : ∀ (u v : U), u + v ∈ U) 
    (hU_inv : ∀ (u : U), -u ∈ U) :
    ¬ is_subspace U 

theorem exercise_1.7
 exists_nonempty_subset_of_nonempty_set (U : Type*) (hU : nonempty U) :
    ∃ (V : set U), V ≠ ∅ 

theorem exercise_1.8
 is_open_inter_of_is_open_of_is_open {X : Type*} [topological_space X]
  (U V : set X) (hU : is_open U) (hV : is_open V) : is_open (U ∩ V) 

theorem exercise_1.9
 union_of_subspaces_is_subspace {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] (U W : submodule ℂ V) :
    submodule.is_subspace (U ⊔ W) ↔ U ≤ W ∨ W ≤ U 

theorem exercise_2.1
 span_sub_list {V : Type*} [add_comm_group V] [vector_space ℂ V]
    {v : list V} (hv : span ℂ v = ⊤) :
    span ℂ (list.sub_list v) = ⊤ 

theorem exercise_2.2
 linear_independent_sub_of_linear_independent 
    {V : Type*} [add_comm_group V] [vector_space ℂ V] 
    {v : list V} (hv : linear_independent ℂ v) :
    linear_independent ℂ (list.sub_list v) 

theorem exercise_2.6
 infinite_dim_continuous_functions_on_01 : 
    infinite_dimensional ℝ (continuous_functions_on_01) 

theorem exercise_3.1
 linear_map.one_dim_eq_smul {V : Type*} [field ℂ] [add_comm_group V] 
    [vector_space ℂ V] (T : V →ₗ[ℂ] V) (hT : T ≠ 0) :
    ∃ (a : ℂ), T = a • linear_map.id 

theorem exercise_3.8
 exists_subspace_of_range_eq_range_of_null_eq_zero {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {W : Type*} 
    [add_comm_group W] [vector_space ℂ W] (T : V →ₗ[ℂ] W) 
    (hT : T.range = T.range) (hT' : T.null_space = ⊥) :
    ∃ (U : submodule ℂ V), U.comap T = ⊥ ∧ T.range = U.range

theorem exercise_3.9
 linear_map.surjective_of_null_eq_span_of_two_vectors 
    {F : Type*} [field F] {n m : ℕ} (h : n > m) (T : F^n → F^m) 
    (hT : T.null_space = span ({(1,5,0,0),(0,0,1,7)} : set (F^4))) : 
    T.surjective 

theorem exercise_3.10
 not_exists_linear_map_of_null_space_eq_set {F : Type*} 
    [field F] (h : ∃ (f : F^5 → F^2), linear_map f ∧ 
    f.null_space = {v : F^5 | v.1 = 3 * v.2 ∧ v.3 = v.4 ∧ v.5 = v.4}) : false 

theorem exercise_3.11
 exists_linear_map_of_finite_dim_null_range {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {W : Type*} 
    [add_comm_group W] [vector_space ℂ W] (f : V →ₗ[ℂ] W) 
    (hf : finite_dimensional ℂ (f.null_space) ∧ 
    finite_dimensional ℂ (f.range)) : finite_dimensional ℂ V 

theorem exercise_4.4
 has_n_roots_iff_no_common_roots_with_derivative 
    (p : polynomial ℂ) (m : ℕ) (h : p.degree = m) :
    p.has_n_roots m ↔ p.derivative.roots.disjoint p.roots 

theorem exercise_5.1
 linear_map.sum_invariant {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] {T : V →ₗ[ℂ] V} (hT : linear_map ℂ T) 
    (U : ℕ → submodule ℂ V) (hU : ∀ i, T.range ⊆ U i) :
    T.range ⊆ ⨆ i, U i 

theorem exercise_5.4
 null_of_comm_of_mul_eq_mul_left {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] {S T : V →ₗ[ℂ] V} (hST : S.commutes T) 
    (λ : ℂ) :
    is_submodule (null_space (T - λ • linear_map.id V)) S 

theorem exercise_5.11
 eigenvalues_mul_comm {V : Type*} [add_comm_group V] [vector_space ℂ V]
    [finite_dimensional ℂ V] (S T : linear_map ℂ V ℂ V) :
    eigenvalues (S * T) = eigenvalues (T * S) 

theorem exercise_5.12
 is_scalar_multiple_of_id_of_all_eigenvectors {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] [finite_dimensional ℂ V] 
    (T : V →ₗ[ℂ] V) (hT : ∀ v : V, is_eigenvector ℂ T v) :
    ∃ (c : ℂ), T = c • linear_map.id 

theorem exercise_5.13
 linear_map.is_scalar_of_dim_sub_one_invariant {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {T : V →ₗ[ℂ] V} 
    (hT : ∀ (U : submodule ℂ V), dim ℂ U = dim ℂ V - 1 → U.is_invariant T) :
    ∃ (c : ℂ), T = c • linear_map.id 

theorem exercise_5.20
 same_eigenvectors_implies_commutes {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] {T S : V →ₗ[ℂ] V} (hT : is_diagonalizable T) 
    (hS : is_diagonalizable S) (h : ∀ (v : V), is_eigenvector T v → is_eigenvector S v) :
    S.comp T = T.comp S 

theorem exercise_5.24
 even_dim_of_no_eigenvalues {V : Type*} [add_comm_group V] 
    [vector_space ℝ V] {T : V → V} (hT : linear_map ℝ V V T) 
    (hT_no_eigenvalues : ∀ (v : V), v ≠ 0 → T v ≠ v) :
    ∀ (U : submodule ℝ V), T.range ⊆ U → U.dim % 2 = 0 

theorem exercise_6.2
 inner_product.eq_zero_iff_norm_le_of_add {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] [inner_product ℂ V] 
    (u v : V) :
    inner_product u v = 0 ↔ ∀ (a : ℂ), ∥u∥ ≤ ∥u + a • v∥ 

theorem exercise_6.3
 sum_squared_le_sum_squared_of_sum_squared_le_sum_squared 
    {α : Type*} [add_comm_monoid α] [semimodule ℝ α] 
    {a b : fin n → α} (h : ∀ i, ∑ j in finset.range n, a j * b j ≤ ∑ j in finset.range n, j * a j * a j) :
    ∑ j in finset.range n, a j * b j ≤ ∑ j in finset.range n, (1 / j) * b j * b j 

theorem exercise_6.7
 inner_product_space.inner_eq_frac_of_norm_squared_add_sub_add_sub_i 
    {V : Type*} [inner_product_space ℂ V] (u v : V) :
    inner u v = (norm_squared (u + v) - norm_squared (u - v) + 
    norm_squared (u + I • v) * I - norm_squared (u - I • v) * I) / 4 

theorem exercise_6.13
 norm_eq_sum_of_squares_of_inner_product_iff_in_span {V : Type*} 
    [inner_product_space ℂ V] {e : fin m → V} (he : orthonormal e) 
    (v : V) :
    ∥v∥^2 = ∑ i, ∥inner_product ℂ v (e i)∥^2 ↔ v ∈ span ℂ (e '' univ) 

theorem exercise_6.16
 orthogonal_iff_eq_bot {V : Type*} [inner_product_space ℂ V] 
    (U : submodule ℂ V) :
    U.orthogonal = ⊥ ↔ U = ⊤ 

theorem exercise_6.17
 is_orthogonal_projection {V : Type*} [inner_product_space ℂ V]
    {P : V → V} (hP : is_projection P) (hP2 : P ∘ P = P) 
    (hnull : ∀ (v : V), v ∈ null_space P → ∀ (w : V), w ∈ range P → v ⊥ w) :
    is_orthogonal_projection P 

theorem exercise_6.18
 is_projection_of_self_adjoint {V : Type*} [inner_product_space ℂ V]
    {P : V → V} (hP : self_adjoint P) (hP2 : P ∘ P = P) 
    (hP_le : ∀ v : V, ∥P v∥ ≤ ∥v∥) : is_projection P 

theorem exercise_6.19
 is_invariant_iff_eq_projection_mul_projection {V : Type*} 
    [add_comm_group V] [vector_space ℂ V] {T : V →ₗ[ℂ] V} 
    (U : submodule ℂ V) :
    U.is_invariant T ↔ T.comp (U.subtype.projection) = 
    (U.subtype.projection).comp T 

theorem exercise_6.20
 is_invariant_iff_projection_commutes {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] (T : V →ₗ[ℂ] V) (U : submodule ℂ V) :
    is_invariant U T ↔ T.comp (projection U) = projection U.comp T 

theorem exercise_6.29
 is_invariant_iff_orthogonal_is_invariant {V : Type*} 
    [inner_product_space ℂ V] {T : V →ₗ[ℂ] V} (U : submodule ℂ V) :
    U.is_invariant T ↔ U.orthogonal.is_invariant T.adjoint 

theorem exercise_7.4
 is_projection_iff_self_adjoint {V : Type*} [inner_product_space ℂ V]
    [add_comm_group V] [vector_space ℂ V] [module ℂ V] {P : V → V} 
    (hP : is_projection P) :
    self_adjoint P ↔ is_projection P 

theorem exercise_7.5
 not_subspace_of_normal_operators {V : Type*} [finite_dimensional ℂ V]
    (hV : dim V ≥ 2) :
    ¬ (normal_ops V).is_subspace 

theorem exercise_7.6
 range_eq_range_adjoint {V : Type*} [inner_product_space ℂ V] 
    [normed_space ℂ V] (T : V →ₗ[ℂ] V) (hT : T.is_normal) :
    T.range = T.adjoint.range 

theorem exercise_7.8
 self_adjoint_operator_not_exists {R : Type*} [ring R] 
    [semiring R] [add_comm_group R] [module R R] [vector_space ℝ R] 
    [inner_product_space ℝ R] [normed_group R] [normed_space ℝ R] 
    [complete_space ℝ R] (T : R →ₗ[R] R) (hT : T.is_self_adjoint) 
    (hT1 : T (1 : R) = 0) (hT2 : T (2 : R) = 2

theorem exercise_7.9
 is_self_adjoint_iff_eigenvalues_real {V : Type*} [inner_product_space ℂ V]
    [normed_group V] [normed_space ℂ V] {T : V →ₗ[ℂ] V} (hT : T.is_normal) :
    T.is_self_adjoint ↔ ∀ (λ : ℂ), T.is_eigenvalue λ → λ.is_real 

theorem exercise_7.10
 normal_pow_eq_self_iff_self_adjoint {V : Type*} [inner_product_space ℂ V]
    {T : V →ₗ[ℂ] V} (hT : is_normal T) (hT9 : T ^ 9 = T ^ 8) :
    T = T.adjoint ↔ T ^ 2 = T 

theorem exercise_7.11
 exists_sqrt_of_normal {V : Type*} [inner_product_space ℂ V] 
    {T : V →ₗ[ℂ] V} (hT : T.is_normal) :
    ∃ (S : V →ₗ[ℂ] V), S.is_normal ∧ S ^ 2 = T 

theorem exercise_7.14
 exists_eigenvalue_of_self_adjoint_of_norm_lt_epsilon 
    {V : Type*} [inner_product_space ℂ V] [normed_group V] [normed_space ℂ V]
    {T : V →ₗ[ℂ] V} (hT : self_adjoint T) (λ : ℂ) (ε : ℝ) 
    (hε : ε > 0) (v : V) (hv : ∥v∥ = 1) (hvT : ∥T v - λ • v∥ < ε) :

theorem exercise_7.15
 exists_basis_of_eigenvectors_iff_self_adjoint {U : Type*} 
    [finite_dimensional ℝ U] (T : linear_map ℝ U U) :
    ∃ (b : set U), is_basis ℝ b ∧ ∀ (v : U), v ∈ b → is_eigenvector ℝ T v ↔ 
    ∃ (ip : inner_product_space ℝ U), is_self_adjoint ℝ ip T 

theorem exercise_7.17
 pos_add {V : Type*} [inner_product_space ℂ V] 
    {A B : V →ₗ[ℂ] V} (hA : A.is_self_adjoint) (hB : B.is_self_adjoint) 
    (hApos : A.is_positive) (hBpos : B.is_positive) :
    (A + B).is_positive 

theorem exercise_7.18
 pos_pow {V : Type*} [inner_product_space ℂ V] [normed_space ℂ V]
    (T : bounded_linear_map V V) (hT : T.is_positive) (k : ℕ) :
    T.pow k.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ