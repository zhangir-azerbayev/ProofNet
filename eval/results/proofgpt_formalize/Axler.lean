

real.is_cubine_root_of_is_sqrt_three_add_inv_add_sqrt_three (i : ℝ) :
	is_cubine_root (λ (x : ℝ), -(x + (⇑real.sqrt_three_add_inv i) / 2)) 1 :=
sorry

inner_product_geometry.proof_ that_neg_neg {V : Type*}
	[inner_product_space ℝ V] (v : V) :
	(∀ (w : V), has_inner.inner (-v) (-w) = has_inner.inner v w) → v = -v :=
sorry

norm_num.int_fract_eq_zero {α : Type*} [linear_ordered_ring α]
	[floor_ring α] (a : α) (v : α) (h : int.fract a v = 0) :
	a = 0 ∨ v = 0 :=
sorry

ennreal.not_lt_top_of_mem_add_preimage_neg {u : ennreal}
	(h : u ∈ ennreal.of_real |(u, 0)|) :
	¬u < ⊤ :=
sorry

ennreal.example_nonempty_of_mem_nhds_within_2 {U : set (fin 2 → ℝ)}
	(hU : U ∈ nhds_within 2 (set.Ioi 0)) :
	U.nonempty :=
sorry

orthogonal_family.le_Inf {𝕜 V : Type*} [is_R_or_C 𝕜]
	[inner_product_space 𝕜 V] (K : orthogonal_family 𝕜 (λ (i : unit), ↥(K.subspace))) :
	K.subspace ≤ has_Inf.Inf (orthogonal_family.subspace K) :=
sorry

submodule.union_le_iff {K : Type u} {V : Type v} [field K]
	[add_comm_group V] [module K V] {S T : submodule K V} :
	S ∪ T ≤ T ↔ S ≤ T :=
sorry

one_dimensional.exists_linear_map_eq_smul_of_dim_eq_one {V : Type u}
	[add_comm_group V] [module ℕ V] [finite_dimensional ℕ V] :
	∃ (a : fin 1 → V), ∀ (v : V), T • v = a • v :=
sorry

linear_map.exists_submodule_range_eq_bot_and_range_eq_top_of_finite_dimensional
	{V W : Type*} [add_comm_group V] [module ℕ V] [add_comm_group W]
	[module ℕ W] [h : finite_dimensional ℕ V] (t : tensor_product ℕ V W) :
	(∃ (u : V), u ∈ t.to_linear_map.range ∧ set.range ⇑t = {0}) ∧ set.range ⇑t = {⊤ := ⇑(t.to_linear_map) u} :=
sorry

complex.roots_nodup_of_degree_eq_m {p : ℂ} {m : ℕ} (h : p.degree = ↑m) :
	(complex.coyoneda.obj p).roots.nodup ↔ ∀ (z : ℂ), z ∈ p.roots → z.im = 0 :=
sorry

subspace.supr_invariant_of_invariant {𝕜 : Type*} [is_R_or_C 𝕜]
	{V : Type*} [inner_product_space 𝕜 V] (T : submodule 𝕜 V) (m : ℕ)
	(h : ∀ (u : fin m → V), u ∈ T.is_internal → ∀ (v : fin m → V), v ∈ T.is_internal → has_inner.inner (u v) (⇑T v) = 0) :
	(⨆ (i : fin m), u i) = ⨆ (i : fin m), v i :=
sorry

inner_product_space.is_R_or_C.ext_eigenvalues {V : Type*}
	[inner_product_space ℝ V] {S T : set V}
	(hS : inner_product_space.is_R_or_C.S = S)
	(hT : inner_product_space.is_R_or_C.T = T) :
	∃ (x : ℝ), x ∈ S ∧ x ∈ T ∧ ⇑is_R_or_C.re (has_inner.inner x S) = ⇑is_R_or_C.re (has_inner.inner x T) ∧ ⇑is_R_or_C.im (has_inner.inner x T) = 0 :=
sorry

inner_product_space.is_scalar_tower.of_is_scalar_tower_id_left {𝕜 : Type*}
	[is_R_or_C 𝕜] {V : Type*} [inner_product_space 𝕜 V] (T : 𝕜)
	(hT : ∀ (x : V), ∃ (r : 𝕜), T = r • x) :
	inner_product_space.is_scalar_tower.of_is_scalar_tower T (𝟙 𝕜) hT = T :=
sorry

linear_map.map_range_eq_top_of_forall_is_compl {𝕜 : Type*}
	[nondiscrete_normed_field 𝕜] {V : Type*} [normed_group V] [normed_space 𝕜 V]
	(T : (V →ₗ[𝕜] V) →ₗ[𝕜] V →ₗ[𝕜] V)
	(h : ∀ (s : submodule 𝕜 V), is_compl s T.submodule) :
	submodule.map T.linear_map_range = ⊤ :=
sorry

eigenvalue_eq_zero_of_invertible {𝕜 : Type*} [is_R_or_C 𝕜] {V : Type*}
	[inner_product_space 𝕜 V] (T : ↥(module.End.eigenvalues T))
	[hd2 : fact (finite_dimensional.finrank 𝕜 V = 1)] (S : ↥(module.End.eigenspace T))
	(hS : ∀ (μ : 𝕜), S.val ∈ module.End.eigenspace T μ) (hT : T.val ∈ module.End.eigenspace S μ) :
	S.val = T.val :=
sorry

riesz_invariant_of_no_eigenvalue_finite_dimensional {V : Type u}
	[add_comm_group V] [module ℝ V] {T : set V} (hT : ∀ (x : V), x ∈ T → x = 0)
	(μ : measure_theory.measure V) [measure_theory.is_finite_measure μ]
	(hV : ∀ (x : V), x ∈ T → ∀ (r : ℝ), 0 < r → T.mutually_singular (⇑μ {x : V | x ∈ T ∧ r ≤ |x - x|}) :
	∃ (n : ℕ), (module.dual.to_continuous_linear_map '' T).topological_closure.dim = n :=
sorry

first_order.language.substructure.comap_inclusion_eq_of_eq_mul
	{V : Type*} [inner_product_space ℝ V]
	{S T : ↥(first_order.language.substructure.closure (first_order.language.is_scalar_tower ℝ V ↥(S.carrier) ⊤)}
	(h : S = T) :
	first_order.language.substructure.comap (first_order.language.hom.incl first_order.language.is_scalar_tower ℝ V ↥(T.carrier)) S = T :=
sorry

or_thonormal_list_of_mem_span {V : Type*} [inner_product_space ℝ V]
	{m : ℕ} (e : fin m → V) (v : V) (H : ∥v∥ ^ 2 = (list.of_fn (λ (i : fin m), ∥⇑(e i) v∥ ^ 2) i).sum) :
	v ∈ submodule.span ℝ (set.range e) :=
sorry

subspace.projection_eq_bot_iff {𝕜 : Type*} [is_R_or_C 𝕜]
	{U : subspace 𝕜} :
	U.projection = ⊥ ↔ U = ⊤ :=
sorry

proj_norm_eq_zero_iff {V : Type*} [semi_normed_group V] (u v : V) :
	↑(⇑(proj_norm V) u) = 0 ↔ ∀ (a : V), ∥u∥ ≤ ∥u + a * v∥ :=
sorry

real.arith_mean_sq_le_mean_sq_mul_mean {n : ℕ} (a : ℕ → ℝ)
	(b : ℕ → ℝ) :
	(finset.range n).sum (λ (j : ℕ), a j * b j) ^ 2 ≤ (finset.range n).sum (λ (j : ℕ), a j * b j) ^ 2 :=
sorry

inner_product_space.of_core.inner_eq_norm_add_norm_sub_add_norm_mul_self_sub_norm_mul_self_div_four
	{V : Type*} [inner_product_space.core V] (u v : V) :
	has_inner.inner u v = (∥u + v∥ ^ 2 - ∥u - v∥ ^ 2 + ∥u + complex.I * v∥ ^ 2 * complex.I - ∥u - complex.I * v∥ ^ 2 * complex.I) / 4 :=
sorry

inner_product_space.is_self_adjoint.of_normal_of_tower_of_self_adjoint
	{V : Type*} [inner_product_space ℂ V] {T : set V}
	(hT : measure_theory.dominated_fin_meas_additive (λ (x : V), T.re_apply_inner_self x) measure_theory.measure_space.volume)
	(hT_re : ∀ (x : V), ⇑is_R_or_C.re (has_inner.inner x (⇑T x)))
	(hT_im : measure_theory.integrable (λ (x : V), has_inner.inner x (⇑T x)) measure_theory.measure_space.volume) :
	inner_product_space.is_self_adjoint T :=
sorry

inner_product_space.of_core.inner_products_of_normal_sq {V : Type*}
	[inner_product_space ℂ V] [complete_space V]
	(hV : ∀ (v : V), v ∈ inner_product_space.of_core.V → ∃ (r : ℝ), r ∈ set.Ioo 0 1 ∧ has_inner.inner v (inner_product_space.of_core.T v r) = r)
	(S T : inner_product_space.of_core) :
	inner_product_space.of_core.inner_products_of_normal S T = S ^ 2 = T :=
sorry

inner_product_space.is_self_adjoint.has_eigenvalue_of_is_self_adjoint_of_exists_ne_one
	{𝕜 : Type*} [is_R_or_C 𝕜] {V : Type*} [inner_product_space 𝕜 V] {T : V →ₗ[𝕜] V}
	(hT : inner_product_space.is_self_adjoint T) {ε : ℝ} (hε : 0 < ε)
	(hε' : ∃ (v : V), ∥v∥ = 1 ∧ ∥T v - λ_hom.conj_ae T v∥ < ε) :
	module.End.has_eigenvalue T ↑(hT.eigenvalues_def ε hε') :=
sorry

two_dimensional.not_submodule_normal_operals (K : Type u)
	(V : Type v) [division_ring K] [add_comm_group V] [module K V]
	[topological_space K] [topological_add_group K] [densely_ordered K]
	[no_zero_smul_divisors K V] (h : 2 ≤ module.rank K V) :
	¬submodule.restrict_scalars K (add_comm_group.normal_operals K V) ≤ ⊤ :=
sorry

convex_hull_eq_of_normalise_eq (𝕜 : Type*) {E : Type*}
	[normed_linear_ordered_field 𝕜] [normed_group E] [normed_space 𝕜 E]
	{T : set E} (hT : T ∈ 𝕜.normalize_set) :
	⇑(convex_hull 𝕜) T = set.range T :=
sorry

inner_product_space.is_self_adjoint_iff_all_ eigenvalues_real
	(𝕜 : Type*) {E : Type*} [is_R_or_C 𝕜] [inner_product_space 𝕜 E]
	[finite_dimensional 𝕜 E] :
	inner_product_space.is_self_adjoint 𝕜 ↔ ∀ (μ : 𝕜), module.End.eigenspace 0 μ = ⊤ :=
sorry