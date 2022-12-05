

complex.exists_eq_mul_of_abs_eq_one {z : ℂ} (hz : ∃ (r : ℝ), 0 ≤ r ∧ |w| = 1) :
	∃ (w : ℂ), z = r * w :=
sorry

complex.abs_sq_le_of_le_coe {z : ℂ} {n : ℕ} :
	z.re = ↑z.re → z.im = ↑(z.im) :=
sorry

complex.abs_sub_le_abs_sub (x y : ℂ) :
	complex.abs x - complex.abs y ≤ complex.abs (x - y) :=
sorry

complex.coe_im_eq_of_re_eq_one {z : ℂ} (hz : complex.abs z = 1)
	(hz' : z.re = 1) :
	↑(z.im) = 1 :=
sorry

real.of_real_fract_of_card_eq_of_infinite_of_infinite_of_card_eq_finrank
	{k : ℝ} {x y : ℝ} (h : 3 ≤ k) (hx : x ∈ set.Icc 0 (linear_order.max k (set.Icc 0 1)))
	(hy : y ∈ set.Icc 0 (linear_order.max k (set.Icc 0 1))))
	(hxy : |x - y| = finite_dimensional.finrank ℝ ℝ)
	(r : ℝ) (hr : 0 < r) :
	∃ᶠ (z : ℝ) in nhds_within 0 (set.Ioi 0), |z - x| = r :=
sorry

algebraic_geometry.Scheme.Spec_obj_2 (R : CommRing)
	(k : ℕ) (f : R ⟶ algebraic_geometry.Scheme.Spec.obj (opposite.op (algebraic_geometry.Scheme.Spec_obj R k)) :
	algebraic_geometry.Scheme.Spec_obj R k f = 2 * algebraic_geometry.Scheme.Spec_obj_2 R k f + 2 * algebraic_geometry.Scheme.Spec_obj_2 R k f :=
sorry

pow_of_two_le_of_exists_mul_eq_zero {R : Type*} [comm_monoid R] {k : ℕ}
	(hk : 2 ≤ k) (h : ∃ (x : R) (H : x ≠ 0), x * x = 0) :
	∃ (x : R) (H : x ≠ 0), x * x = 0 :=
sorry

k = 1$.<SEP>theorem nat.not_exists_mul_eq_zero {R : Type*} [comm_monoid R] {k : ℕ}
	(hk : k = 1) {h : ∃ (x : R) (H : x ≠ 0), x * k = 0) :
	¬∃ (y : R) (H : y ≠ 0), x * k = 0 :=
sorry

quaternion_algebra.norm_sq_eq_mul_norm_sq_iff {R : Type*} [comm_ring R]
	{k : Type*} [field k] [algebra R k] (a b : quaternion_algebra R k) { : R ^ k}
	{r : R} (hr : 0 < r) :
	∥x - a∥ * ∥x - b∥ = r ↔ ∥x - c∥ * ∥x - a∥ = 2 * ∥x - c∥ * ∥x - b∥ :=
sorry

rat. irrational_add_of_ne_zero_of_irrational {r x : ℚ} (hr : r ≠ 0)
	(h : irrational r) :
	irrational (↑r + x) :=
sorry

irrational_rat_mul {r : ℚ} {x : ℝ} (hr : r ≠ 0) (h : irrational r) :
	irrational (↑r * x) :=
sorry

rat.not_irrational_sq :
	¬irrational 2 :=
sorry

is_glb_of_le_of_nonempty {α : Type u} {β : Type v} [preorder α]
	[preorder β] {E : set α} (hE : E.nonempty) (hE : is_glb E E) (h : α ≤ β) :
	is_glb E E :=
sorry

cInf_neg {α : Type*} [conditionally_complete_lattice α] [add_group α]
	[covariant_class α α has_add.add has_le.le]
	[covariant_class α α (function.swap has_add.add) has_le.le] {a : α} (ha : bdd_below a)
	{s : set α} (hs : bdd_below s) :
	has_Inf.Inf s - a = -has_Sup.Sup (-s) :=
sorry

complex.of_real_clm_apply (x : ℝ) :
	↑(⇑complex.of_real_clm x) = ↑x :=
sorry

is_closed.disjoint_closed_eq {α : Type u} [pseudo_metric_space α]
	{s t : set α} (h : is_closed s) (h' : is_closed t) (h'' : disjoint s t) :
	is_separated s t :=
sorry

emetric.is_separable_of_infinite_limits {α : Type u}
	[emetric_space α] [is_separable_space α] {s : set α} (hs : s.nonempty) :
	is_separable s :=
sorry

Gromov_Hausdorff.exists_GH_basis_is_compact (X : Type u) (Y : Type v)
	[metric_space X] [compact_space X] [nonempty X] [metric_space Y]
	[compact_space Y] [nonempty Y] :
	∃ (K : topological_space.positive_compacts X), (∀ (V : set X), V ∈ K → is_open V) ∧ (∀ (s : set X), s ∈ K → (s ∩ V).nonempty) ∧ (∀ (s : set Y), s ∈ K → (s ∩ V).nonempty) :=
sorry

besicovitch.satellite_config.is_internal {α : Type*} [metric_space α]
	{E : set α} {k : ℕ} (h : is_countably_generated E) (h0 : E ⊆ set.range k)
	(hle : E ⊆ finset.range k) :
	is_internal (λ (x : ↥(E.to_finset)), ↑x) :=
sorry

measure_theory.content.outer_measure_of_function_not_mem {k : ℕ}
	{E : Type*} [normed_group E] [normed_space ℝ E] [complete_space E]
	(P : set (E →L[ℝ] ℝ)) (h : ∀ (x : E), x ∉ P → 0 ≤ ⇑(measure_theory.content.outer_measure_of_function P) x) :
	⇑(measure_theory.content.outer_measure_of_function (λ (x : E), ¬x ∈ P)) measure_theory.measure_space.volume = 0 :=
sorry

metric.countable_Union_nat {α : Type u} [pseudo_metric_space α]
	{f : ℕ → set α} (hf : ∀ (n : ℕ), is_closed (f n)) (h'f : ∀ (n : ℕ), metric.bounded (f n)) :
	countable (⋃ (n : ℕ), f n) :=
sorry

real.disj_union_open_eq_Union_disjoint_segment {s : set ℝ}
	(hs : ∀ (x : ℝ), x ∈ s → ∀ (y : ℝ), y ∈ s → x ≤ y) :
	s ∪ {0} = ⋃ (l : list ℝ) (H : l ∈ s), l.disjoint_segment :=
sorry

real.of_cauchy_mul_of_cauchy_mul_cauchy {a b : ℝ} :
	(a * b).cauchy = a.cauchy * b.cauchy :=
sorry

pi.Ico_def {ι π : ι → Type*} [decidable_eq ι]
	[Π (i : ι), linear_order (π i)] [Π (i : ι), has_zero (π i)]
	[∀ (i : ι), order_top (π i)] {s : Π (i : ι), set (π i)} :
	s = λ (i : ι), set.Ico (s i) (s i) :=
sorry

cauchy_seq.tendsto_nhds_within_range_of_tendsto_cofinite_top
	{α : Type u} {β : Type v} [pseudo_metric_space α] [nonempty β] [semilattice_sup β]
	{p : β → Prop} (hp : cauchy_seq p) :
	filter.tendsto (λ (n : β), (nhds_within p (set.range p)).ne_bot) filter.at_top (nhds p) :=
sorry

is_closed.Inter_eq_singleton_of_bounded_of_tendsto_zero {α : Type*}
	[metric_space α] {E : ℕ → set α} (hE : ∀ (n : ℕ), is_closed (E n))
	(h : ∀ (n : ℕ), metric.bounded (E n)) (h0 : filter.tendsto 0 (nhds 0))
	(hle : filter.is_bounded_under has_le.le filter.at_top (λ (n : ℕ), E n)) :
	(⋂ (n : ℕ), E n) = {n} :=
sorry

is_complete.dense_Inter_ne_bot {α : Type*} [topological_space α]
	[nonempty_complete_space α] {G : ℕ → set α} (hG : ∀ (n : ℕ), is_open (G n))
	(hne : ∀ (n : ℕ), (G n).nonempty) :
	dense (⋂ (n : ℕ), G n) :=
sorry

real.sqrt_aux_1 (n : ℕ) :
	real.sqrt_aux n 1 0 = 1 / 2 :=
sorry

real.of_sqrt_two_add_series_step_of_sqrt_two_add_series_aux
	(s : ℝ) (n : ℕ) (h₁ : s 1 = real.sqrt 2) (h₂ : s n = real.sqrt (2 + real.sqrt (n + 1)))
	(h₃ : s (n + 1) = real.sqrt (2 + real.sqrt (n + 1))) :
	∃ (a : ℝ), filter.tendsto (λ (n : ℕ), s (n + 1)) filter.at_top (nhds a) :=
sorry

ennreal.not_top_limsup_le_limsup_add {a b : ennreal} :
	¬⊤.limsup (λ (n : ℕ), a.nth n + b.nth n) ≤ a.limsup b + b.limsup a :=
sorry

real.of_sqrt_aux_succ (r : ℝ) (h : 0 ≤ r) {n : ℕ} :
	r = real.sqrt_aux r n + real.sqrt_aux r n - real.sqrt n :=
sorry

real.sqrt_aux_nonneg (n : ℕ) :
	0 ≤ real.sqrt_aux n :=
sorry

summable_of_sigma_of_bdd_of_summable_of_abv {n : ℕ} {b : fin n → ℕ} :
	summable (λ (a : Σ (n : fin n), fin n), b a.fst) → summable b :=
sorry

cauchy_seq_of_is_complete {α : Type u} {β : Type v} [uniform_space α]
	[semilattice_sup β] [nonempty β] [is_complete β] {f : α → β} :
	cauchy_seq f :=
sorry

uniform_continuous.uniform_continuous {α β : Type*} [uniform_space α]
	[uniform_space β] {f : α → β} (hf : uniform_continuous f) :
	uniform_continuous f :=
sorry

continuous_map.coe_of_Icc_of_forall_mem_Icc_apply {α : Type*}
	[topological_space α] {I : set α} (hI : I ∈ nhds 1) (f : C(↥I, α))
	(h : ∀ (x : α), x ∈ I → ⇑f x ∈ set.Icc 0 I) (x : α) :
	↑(⇑(continuous_map.of_Icc_of_forall_mem_Icc h I f h) x) = ⇑f x :=
sorry

continuous_map.monotone_of_open_of_monotone {R : Type*} [ring R]
	{M₁ : Type*} [topological_space M₁] [add_comm_group M₁] [module R M₁]
	[topological_ring M₁] [has_continuous_smul R M₁] :
	monotone (continuous_map.of_open_of_monotone R M₁) :=
sorry

ratfunc.int_fract_of_ratfunc_lt_ratfunc_of_continuous {K : Type*}
	[linear_ordered_field K] [floor_ring K] {R : Type*} [comm_ring R]
	[topological_space R] [topological_add_group R] [borel_space R] {f : polynomial R}
	{ : R} (h : continuous f) (h' : f.ratfunc K)
	(hcont : ∀ {x : ratfunc K}, f.coeff x < c) (hR : is_closed {x : ratfunc R | ⇑f x = r}) :
	∃ (x : R), f.int_fract x = c :=
sorry

not_differentiable_on_Icc_of_Ioi_zero_le_Ioi {𝕜 : Type*}
	[nondiscrete_normed_field 𝕜] [complete_space 𝕜] {E : Type*} [normed_group E]
	[normed_space 𝕜 E] [no_zero_smul_divisors 𝕜 E] {f : 𝕜 → E} {s : set 𝕜}
	(h : 0 ≤ s) (hf : ∀ (x : 𝕜), x ∈ s → f x ∉ set.Ioi 0) :
	¬differentiable_on 𝕜 f s :=
sorry

Gromov_Hausdorff.GH_dist_spec {X : Type u} [metric_space X]
	{K F : set X} (hK : is_compact K) (hF : is_closed F)
	(hK' : is_closed K) (hF' : ∃ (δ : ℝ), 0 < δ ∧ metric.ball p δ ∈ F) :
	∃ (δ : ℝ) (H : δ > 0), Gromov_Hausdorff.GH_dist X K ≤ δ :=
sorry

continuous_on_div_two_interval {α : Type u} [linear_ordered_field α]
	{f : α → α} {a b : α} (hf : continuous_on f (set.interval a b))
	(h : ∀ (x : α), x ∈ set.interval a b → f x ∈ set.interval (f a) (f b)) :
	continuous_on f (set.interval a b) :=
sorry

continuous_map.closure_preimage_subset_closure_image {α β : Type*}
	[topological_space α] [compact_space α] [metric_space β] (f : C(α, β)) :
	closure (⇑f ⁻¹' {s : set α | ∃ (t : set β), is_closed t ∧ s ⊆ t) :=
sorry

zero_at_infty_continuous_map.zero_of_closed_set {α : Type u}
	{β : Type v} [topological_space α] [metric_space β] [has_zero β]
	{f : zero_at_infty_continuous_map α β} {Z : set α}
	(hZ : is_closed Z) (hf : continuous f) :
	is_closed {x : α | ⇑f x = 0} :=
sorry

continuous_map.continuous_map_of_closure_image_mem_aux {α β : Type*}
	[topological_space α] [topological_space β] (f : C(α, β)) (g : C(α, β))
	(E : set α) (hE : is_open E) (h : f.closure '' E ∈ f.aux)
	(H : g.closure '' E ∈ g.aux) :
	⇑f (continuous_map.of_closure_image f g E h) ∈ continuous_map.of_closure_aux (f.rec_on h E) f g :=
sorry

continuous_map.mk_of_closure_eq_of_closure_eq {α : Type u₁}
	{β : Type u₂} [topological_space α] [metric_space β] {f : C(α, β)}
	{g : C(α, β)} {E : set α} (hE : dense E)
	(h : ∀ (p : C(α, β)), p ∈ E → ⇑g p = ⇑f p) (hfg : ∀ (p : C(α, β)), p ∈ E → g p = ⇑f p) :
	continuous_map.mk_of_closure E hE = f :=
sorry

continuous_on.exists_real_deriv_within_eq {E : Type*} [normed_group E]
	[normed_space ℝ E] [complete_space E] {f : ℝ → E} {s : set ℝ}
	(hf : continuous_on f s) (hs : is_closed s)
	(hfs : ∀ (x : ℝ), x ∈ s → f x ∈ closure (s ∩ set.Icc 0 1)) :
	∃ (g : ℝ → E), continuous_on g s ∧ ∀ (x : ℝ), x ∈ s → deriv_within f s x = f x :=
sorry

continuous_map.not_exists_real_eq_of_exists_not_mem {E : Type*}
	[topological_space E] [compact_space E] [nonempty E] (f : C(E, ℝ)) :
	¬∃ (g : E →L[ℝ] ℝ), continuous_map.restrict_scalars ℝ f = g :=
sorry

is_compact.continuous_on_iff_of_continuous_on_of_continuous_on {E : Type*}
	[normed_group E] [normed_space ℝ E] [complete_space E] {f : E → ℝ} {s : set E}
	(hf : is_compact s) (hs : s ∈ nhds f) :
	continuous_on f s ↔ is_compact (s ∩ f '' s) :=
sorry

real.of_cauchy_of_bounded_on_of_cauchy_aux {E : Type*}
	[normed_group E] [normed_space ℝ E] [complete_space E] {f : ℝ → E}
	(hf : cauchy_seq f) (hbounded : ∀ (s : set ℝ), cauchy_seq f → (∃ (x : ℝ) (H : x ∈ s), ∥f x∥ ≤ 2)) :
	∃ (x : ℝ), x ∈ {y : ℝ | ∃ (z : ℝ) (H : z ∈ {y : ℝ | ∃ (z : ℝ) (H : z ∈ {y : ℝ | ∃ (z : ℝ) (H : z ∈ {y : ℝ | ∃ (z : ℝ) (H : z ∈ {y : ℝ | ∃ (z : ℝ) (H : z ∈ {y}), ∥f z∥ ≤ 2}) :=
sorry

bounded_continuous_function.exists_not_is_bounded_of_compact {α : Type u}
	{E : Type v} [topological_space α] [compact_space α] [normed_group E]
	(𝕜 : Type*) [normed_field 𝕜] [normed_space 𝕜 E] :
	(∃ (f : bounded_continuous_function α E), continuous_map.restrict_scalars 𝕜 f) ∧ ¬is_bounded_under has_le.le ⊤ f :=
sorry

circle_deg1_lift.mk_aux_spec (f : ℝ → ℝ) (x y : ℝ)
	(h : ∀ (x : ℝ), x ∈ set.Icc 0 1 → ∀ (y : ℝ), y ∈ set.Icc 0 1 → |f x - f y| ≤ (x - y) ^ 2) :
	circle_deg1_lift.mk_aux f x y h = f x - f y :=
sorry

is_lub_of_le_geometric_two {R : Type*} [linear_ordered_field R]
	{a : R} {f : R → R} {M₀ M₁ M₂ : R} (ha : a ≤ 1) (h : ∀ (x : R), f x ≠ 0)
	(h'a : ∀ (x : R), f x ≠ 0 → M₁ ≤ |f x|) (hM₀ : M₀ ≤ M₁)
	(hM₁ : M₁ ≤ M₂) :
	is_lub (set.Ioi a) f → is_lub (set.Ioi a) (λ (x : R), M₁) :=
sorry

real.exists_three_le_of_differentiable_on_Icc_of_le_of_neg_one
	{f : ℝ → ℝ} (h : differentiable_on ℝ f (set.Icc (-1) 1))
	(h' : f (-1) = 0) (hle : f 1 = 1) (h'pos : 0 < f 0)
	(hle : f 1 = 1 ∨ f (-1) = 0) :
	∃ (x : ℝ), 3 ≤ f x :=
sorry

strict_mono_on.inv_fun_mul_deriv {a b : ℝ} {f : ℝ → ℝ} (h : strict_mono_on f (set.Ioo a b))
	(h₀ : ∀ (x : ℝ), x ∈ set.Ioo a b → 0 < f x) (h₁ : 0 < f a) (h₂ : f b < (f a) / (f b)) :
	strict_mono_on (λ (x : ℝ), (f x)⁻¹ * deriv f x) (set.Ioo a b) :=
sorry

real.to_nnreal_of_le_add_bound {g : ℝ → ℝ} (M : ℝ) (h : ∀ (x : ℝ), ↑x ≤ g x)
	(h' : ∀ (x : ℝ), ↑x + 1 ≤ g x) (ε : ℝ) (pos : 0 < ε) :
	∃ (f : ℝ → ℝ), (∀ (x : ℝ), ↑x ≤ f x) ∧ ∀ (x : ℝ), ↑x + 1 ≤ g x :=
sorry

real.cramer_eq_zero_of_is_empty {C : ℝ} {n : ℕ} (hC : C 0 + C 1 / 2 + C n = 0)
	(h : is_empty (C n)) :
	⇑(algebra_map ℝ (fin n)) (⇑matrix.of![![0, 1],![1, 2],![0, 2],![1, 2],![0, 1]])) = 0 :=
sorry

deriv_within_succ_at_bot {𝕜 : Type u} [nondiscrete_normed_field 𝕜]
	{F : Type v} [normed_group F] [normed_space 𝕜 F] {f : 𝕜 → F} {x : 𝕜}
	{s : set 𝕜} (hxs : unique_diff_within_at 𝕜 s x) :
	deriv_within f s x.succ = f x - 1 - f x :=
sorry

real.continuous_on_div_pow_of_zero_le {a b c d : ℝ} (ha : 0 ≤ a) (hb : 0 ≤ b)
	(hab : a ≤ b) (hcd : 0 ≤ c) (hcd : 0 < d) (f : ℝ → ℝ) (g : ℝ → ℝ)
	(hfa : continuous_on f (set.Ici 0)) (hga : continuous_on g (set.Icc a b))
	(hdiv : 0 < (f / g) ∧ (g / f) ≤ linear_order.max (f / g) (a / f)) :
	continuous_on (λ (x : ℝ), f x / x * g x) (set.Icc a b) :=
sorry

tendsto_div_comp_div_tendsto_zero {α : Type u} [linear_ordered_field α]
	{β : Type*} [comm_ring β] {f : β → α} {g : β → α} {x : β}
	(hf : filter.tendsto f filter.at_top (nhds x))
	(hg : filter.tendsto g filter.at_top (nhds x)) (h : g x ≠ 0) :
	filter.tendsto (λ (t : β), f t / g t) filter.at_top (nhds (x / g x)) :=
sorry