

real.sqrt_three_mul_div_two_add_series_one :
	real.sqrt_three_mul_div_two_add_series 1 1 = real.sqrt_three_mul_div_two :=
sorry

neg_vsub {K : Type*} [division_ring K] {V : Type*} [add_comm_group V]
	[module K V] [is_empty K] :
	-vsub K V = v :=
sorry

eq_zero_or_eq_zero_of_mul_eq_zero {K : Type*} [division_ring K] {a : K}
	{v : K} (h : a * v = 0) :
	a = 0 ∨ v = 0 :=
sorry

ereal.coe_sub_closed_sub_eq_coe_sub_of_mem_nhds {r p : ℝ}
	(h : p ∈ nhds (r, 0)) (h₁ : ∀ (u : ℝ), u ∈ set.Ioo 0 1 → p < u)
	(h₂ : ∀ (u : ℝ), u ∈ set.Ioo (-1) 1 → p < u) :
	↑(h.sub_closed_sub_eq h₁ h₂) = ↑(h.sub_of_mem_nhds h₁) :=
sorry

ereal.coe_mul_self_nonempty {p : ℝ × ℝ} (hp : p.fst ≠ 0) :
	↑(p.mul_self).nonempty :=
sorry

submodule.infi_subtype {R M : Type*} [semiring R] [add_comm_monoid M]
	[module R M] {ι : Sort u_2} (p : ι → submodule R M) :
	(⨅ (i : ι), p i) = ⨅ (i : ι), p i :=
sorry

submodule.sup_le {R M : Type*} [semiring R] [add_comm_monoid M]
	[module R M] {V : submodule R M} {s t : submodule R M} :
	s ⊔ t ≤ V ↔ s ≤ V ∧ t ≤ V :=
sorry

finrank_eq_one_of_exists_linear_map_eq_smul {R : Type*}
	[comm_ring R] {n : Type*} [fintype n] [nontrivial R] [nonempty n] {V : Type*}
	[add_comm_group V] [module R V] [finite_dimensional R V]
	(h_dim : ∀ (f : (n → R) →ₗ[R] n → R), f.is_linear_map) (h_dim_R : fintype.card n = 1)
	{T : matrix n n R} (hT : T.mul_vec = 1) :
	∃ (a : n → R), ∀ (v : V), ⇑T v = a • v :=
sorry

finite_dimensional.inf_subtype_zero_eq_bot_of_finite_dimensional
	{V : Type u} [inner_product_space ℝ V] {W : Type v} [inner_product_space ℝ W]
	[finite_dimensional ℝ V] (T : submodule ℝ V) (h_dim : finite_dimensional.finrank ℝ V = 2) :
	(⨅ (U : submodule ℝ V) (hU : U ⊗ₜ[ℝ] ⊥), submodule.span ℝ {0} ⊓ T) = ⊥ :=
sorry

complex.mem_roots_of_unity_iff_nodup_roots {p : ℂ} {m : ℕ}
	(h : p ∈ complex.roots_of_unity m) :
	p.to_complex.mem_roots ↔ p.roots.nodup :=
sorry

is_Lprojection.of_submodule_eq_supr {L : Type*} [field L] {V : Type*}
	[add_comm_group V] [module L V] {T : set V} {m n : ℕ}
	(h_le : m ≤ n) (h_submodule : ∀ (s : finset V), s.card ≤ m → s.card ≤ n → T.submodule) :
	is_Lprojection T m → is_Lprojection T n → T.submodule = supr {to_submodule := (finset.range m).sup (λ (i : ℕ), (h_le i).submodule), map := (i.succ_above).subtypeₗ' := _} :=
sorry

lie_algebra.equiv_of_injective_symm_apply {R : Type u} {L : Type v}
	[comm_ring R] [lie_ring L] [lie_algebra R L] (S T : set L)
	(h : function.injective ⇑(lie_algebra.equiv_of_injective S T h)) :
	↑(⇑((lie_algebra.equiv_of_injective S T h).symm) = ⇑((lie_algebra.equiv_of_injective S T h).symm) :=
sorry

modular_group.coe_mk {V : Type*} [add_comm_group V] [module ℝ V]
	(T : ↥(matrix.GL_pos V)) (hT : ∀ (z : V), z ∈ T.spectrum) :
	↑{re := T, im := hT, im_mem := hT} = ↑T :=
sorry

tensor_product.exists_smul_eq_one_of_invariant_of_subsingleton
	{𝕜 : Type*} [is_R_or_C 𝕜] {V : Type*} [inner_product_space 𝕜 V]
	[finite_dimensional 𝕜 V] (T : V →ₗ[𝕜] V)
	(hT : ∀ (K : submodule 𝕜 V), T ≤ K) (hT' : ∀ (V : Type*) [_inst_3 : inner_product_space 𝕜 V] (K : submodule 𝕜 V), (∀ (V' : Type*) [_inst_4 : inner_product_space 𝕜 V'), (∀ (v : V'), v ∈ K → has_inner.inner (⇑T v) (⇑T v) = v)
	(hT' : ∀ (K : submodule 𝕜 V), (∀ (v : V), v ∈ K → has_inner.inner (⇑T v) (⇑T v) = v) :
	∃ (c : 𝕜), c • T = 1 :=
sorry

modular_group.exists_eq_smul_of_mem_spectrum {V : Type*}
	[category_theory.category V] [category_theory.preadditive V]
	[category_theory.limits.has_finite_products V] [category_theory.limits.has_kernels V]
	[category_theory.limits.has_cokernels V] (T : ↥(matrix.special_linear_group V))
	(hT : T ∈ modular_group.eigenvalues) (hS : S ∈ (matrix.special_linear_group.of T).spectrum)
	(hT' : T = 0) :
	∃ (c : ↥(matrix.special_linear_group.of T)), c • S = T • c :=
sorry

fst_of_no_eigenvector {V : Type*} [inner_product_space ℝ V]
	(T : ℝ) (hV : ∀ (x : V), x ∈ T → (∃ (n : ℕ), x = n • T))
	[finite_dimensional ℝ V] :
	(fst_of_no_eigenvector T hV).dim = 2 * finite_dimensional.finrank ℝ V :=
sorry

is_invariant_prop.comp_sub_left {𝕜 V : Type*} [normed_field 𝕜]
	[semi_normed_group V] [normed_space 𝕜 V] {S T : set V}
	(h : is_invariant_prop 𝕜 (has_norm.norm ∘ S)) (hst : S T = T * S) :
	is_invariant_prop (λ (x : V), h.comp (has_norm.norm x - 1)) T :=
sorry

matrix.norm_sq_eq_norm_sq_repr_of_is_or_mem_span {V : Type*}
	[inner_product_space ℝ V] {m : ℕ} (e : fin m → V) (he : is_or_mem_spanning_subspaces e)
	(v : V) :
	∥v∥ ^ 2 = ∥⇑(matrix.repr (basis.of_is_or_mem_spanning_subspaces e)) (e 1)∥ ^ 2 + ∥⇑(matrix.repr (basis.of_is_or_mem_spanning_subspaces e) (e m)) (matrix.repr v v)∥ ^ 2 :=
sorry

is_submodule.is_compl_iff_eq_bot {R : Type*} [comm_ring R]
	{V : Type*} [add_comm_group V] [module R V] {U : set V} (hU : is_submodule R U) :
	is_compl U ↔ U = ⊥ :=
sorry

inner_product_space.of_core.inner_eq_zero_iff {V : Type*}
	[inner_product_space ℝ V] {u v : V} (h : inner_product_space.core.inner u v = 0) :
	has_inner.inner u v = 0 ↔ ∀ (a : V), has_inner.inner u a v = 0 :=
sorry

real.angle.coe_mk (n : ℕ) (a : ℝ) (b : ℝ) (h : 0 < n) (i : ℕ) :
	↑(⟨n, a⟩) = ↑(a.val) * b :=
sorry

inner_eq_norm_sq_div_norm_sq_add_norm_sq_div_norm_sq_add_norm_sq_div_four
	{𝕜 : Type*} [is_R_or_C 𝕜] {V : Type*} [inner_product_space 𝕜 V]
	[complete_space V] {u v : V} :
	has_inner.inner u v = ↑∥u / ↑∥u + ↑∥v / ↑∥u - ↑∥v∥ ^ 2 - ↑∥u - v∥ ^ 2 + ↑∥u + v∥ ^ 2 * is_R_or_C.I :=
sorry

inner_product_space.is_self_adjoint.orthogonal_projection_self
	{V : Type*} [inner_product_space ℂ V] {T : set V}
	(hT : inner_product_space.is_self_adjoint T)
	(hT' : inner_product_space.is_self_adjoint T) :
	↑(hT.orthogonal_projection T) = ↑T :=
sorry

inner_product_space.is_normal.eq_smul_self_of_has_square_root {V : Type*}
	[inner_product_space ℝ V] (h : inner_product_space.is_normal V)
	{T : V} (hT : T ^ 2 = 1) (hT' : T ≠ 0) :
	T = h • T :=
sorry

inner_product_geometry.is_self_adjoint.exists_eigenvalue_eq {V : Type*}
	[inner_product_space ℝ V] [complete_space V] {T : set V}
	(hT : inner_product_geometry.is_self_adjoint T) {ε : ℝ} (hε : 0 < ε) {v : V}
	(hv : ∃ (w : V), ∥v∥ = 1 ∧ ∥T v - w∥ < ε) :
	∃ (c : V), c ≠ 0 ∧ ∀ (z : V), z ∈ T → ∥c • z - (λ (w : V), c) • v∥ < ε :=
sorry

not_submodule_normal_span_singleton_le_top {𝕜 V : Type*}
	[normed_group V] [normed_space 𝕜 V] [nontrivial V] :
	¬submodule.normal_span 𝕜 {x} ≤ ⊤ :=
sorry

set.range_eq_set_of_normal {α : Type*} {s : set α} [normal_space α] :
	set.range s = set.range sᶜ :=
sorry

inner_product_space.is_self_adjoint_iff_forall_eigenvector {𝕜 : Type*}
	[is_R_or_C 𝕜] [dec_𝕜 : decidable_eq 𝕜] {E : Type*} [inner_product_space 𝕜 E]
	[complete_space E] {x : E} :
	inner_product_space.is_self_adjoint x ↔ ∀ (v : E), v ≠ 0 → (∃ (c : 𝕜), c • x = v) :=
sorry