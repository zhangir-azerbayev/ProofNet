

euclidean_space.totally_disconnected {M : Type*} [metric_space M]
	[totally_disconnected_space M] (p : M × M) :
	totally_disconnected_space M :=
sorry

exists_mem_Icc_of_unbounded {E : set ℝ} (hE : set.unbounded has_lt.lt E) :
	∃ (p : ℝ), p ∈ set.Icc 0 1 :=
sorry

filter.map_le_map_iff_of_injective {α β : Type*} {r : α → α → Prop}
	{p : β → β → Prop} {f : filter β} (hf : function.injective f) :
	filter.map f (filter.map r r) ≤ filter.map r (filter.map f r) ↔ ∀ (k : β), r (f k) (f (f.map r k)) :=
sorry

filter.map_at_top_le_of_surjective {α β : Type*} {r : α → α → Prop}
	{p : β → β → Prop} (f : ℕ → β) (h : ∀ (k : ℕ), function.surjective (p (f k))) :
	filter.map (λ (k : ℕ), f k) filter.at_top ≤ filter.map (λ (k : ℕ), p (f k)) filter.at_top :=
sorry

is_closed.is_refl_trans_gen {M : Type*} [metric_space M]
	[topological_space.separable_space M] {P : set M} (hP : is_closed P)
	(hP' : ∀ (x : M), x ∈ P → x ∈ metric.sphere 0 1) :
	is_refl_trans_gen P P :=
sorry

submonoid.is_open_iff_compl_no_limits {M : Type*} [topological_space M]
	[comm_monoid M] {U : set M} :
	is_open U ↔ ∀ (x : M), x ∉ U → ¬x.is_limit :=
sorry

Gromov_Hausdorff.GH_space.exists_GH_space_equiv_coe {X : Type u}
	[metric_space X] (𝒯 : topological_space.opens X)
	(𝒰 : topological_space.closeds X) :
	∃ (s : topological_space.GH_space X), Gromov_Hausdorff.GH_space.to_GH_space = 𝒯 :=
sorry

nat.clopen_of_subset_pnat {s : set ℕ} :
	(∀ (x : ℕ), x ∈ s → nat.clopen x) → nat.clopen s :=
sorry

proj_norm_le_one {m : ℝ} {β : Type*} [normed_group β]
	(h : ∀ (x : β), ∥x∥ ≤ 1) {s : set (fin m → β)} (hs : is_compact s) :
	∥s.sum (λ (i : fin m), 1 - i)∥ ≤ 1 :=
sorry

measure_theory.measure.exists_disjoint_supersets_of_compact_of_exists_nndist_le
	{M : Type*} [topological_space M] [t2_space M] [compact_space M]
	[disjoint_space M] [nonempty M] (A B : set M) (hA : is_compact A)
	(hB : is_compact B) (hAB : disjoint A B) (hA₂ : B.nonempty) :
	∃ (a₀ a₁ : M) (b₀ b₁ : M), a₀ ∈ A ∧ b₀ ∈ B ∧ ∀ (a : M), a ∈ A → ∀ (b : M), b ∈ B → has_nndist.nndist a₀ b₀ ≤ has_nndist.nndist a₁ b₁ :=
sorry

is_connected.interior_not_general {α : Type u} [topological_space α]
	{s : set α} (h : is_connected s) :
	¬is_generalized_root (λ (u : α), interior s) s :=
sorry

is_path_connected_of_nonempty_compact_locally_path_connected
	{E : Type*} [inner_product_space ℝ E] (M : Type*) [topological_space M]
	[nonempty M] [locally_path_connected_space M] [connected_space M] :
	is_path_connected (set.range (λ (x : M), (continuous_linear_map.proj x).range)) :=
sorry

is_compact.finite_subcovering_reducible {M : Type*} [topological_space M]
	[compact_space M] {ι : Type*} {U : ι → set M} (hU : ∀ (i : ι), is_open (U i))
	(p : M) (hp : p ∈ nhds p) :
	∃ (s : finset ι) (H : s ∈ nhds p), s.card = p ∧ ∀ (i : ι), i ∈ s → U i ⊆ s :=
sorry

Top.direct_nonempty_compact_covering_nonempty {X : Top}
	(U : topological_space.opens ↥X) :
	(Top.direct_nonempty_compact_covering U).nonempty :=
sorry

circle_deg1_lift.constant_of_le_dist_sq (f : circle_deg1_lift) (x : ℝ)
	(h : ∀ (t : ℝ), has_dist.dist (⇑f t) (⇑f x) ≤ has_dist.dist t x ^ 2) :
	⇑f x = function.const ℝ (⇑f x) :=
sorry

real.tendsto_deriv2_of_Ioo {a b : ℝ} (h : a < b) (f : ℝ → ℝ) :
	filter.tendsto (λ (x : ℝ), deriv^[2] f x) (nhds_within a (set.Ioi a)) (nhds (deriv^[2] f a)) :=
sorry

exists_smooth_zero_one_of_closed {𝕜 : Type*}
	[nondiscrete_normed_field 𝕜] [complete_space 𝕜] {L : set 𝕜}
	(hL : is_closed L) (h_zero : ∀ (x : 𝕜), x ∈ L → 0 ∈ set.Icc 0 1)
	(h_one : ∀ (x : 𝕜), x ∈ L → 1 ∈ set.Icc 0 1) :
	∃ (f : 𝕜 → 𝕜), smooth_function 𝕜 f ∧ ∀ (x : 𝕜), x ∈ L → f x = 0 :=
sorry

tendsto_sqrt_succ_sub_sqrt_at_top_nhds_0 (n : ℕ) :
	filter.tendsto (λ (x : ℕ), real.sqrt (n + 1) - real.sqrt n) filter.at_top (nhds 0) :=
sorry

nnreal.summable_div_rpow_log_of_one_div_summable (p : ℝ)
	(hp : 1 < p) :
	summable (λ (k : ℕ), (↑k ^ p)⁻¹) :=
sorry

formal_multilinear_series.dvd_sum_rpow_p_of_le_one {p : ℝ}
	(hp : 1 ≤ p) {k : ℕ} :
	(finset.Ico 1 (k + 1)).sum (λ (i : ℕ), ↑k ^ p) ∣ 0 :=
sorry

measure_theory.mod_continuous_iff_of_tendsto_zero {α : Type*}
	[topological_space α] {m : measurable_space α} {μ : measure_theory.measure α}
	[linear_order α] [order_topology α] [topological_space.second_countable_topology α]
	[measure_theory.is_locally_finite_measure μ] {a b : α} {f : α → ℝ}
	(hf : filter.tendsto f (nhds_within a (set.Ioi a)) (nhds 0)) :
	measure_theory.mod_continuous f μ ↔ measure_theory.has_modulus μ f :=
sorry

dense_range_of_compact_of_finite_subset {M A : Type*}
	[topological_space M] [compact_space M] [nonempty M] [add_comm_group M]
	[topological_add_group M] (hA : dense_range A) (hM : is_compact M) :
	∃ (δ : ℝ) (k : ℕ), ∀ (a : M), ∃ (b : M) (H : b ∈ nhds_within a (set.Ici a)), ∀ (x : M), x ∈ M → (∃ (b' : M) (H : b' ∈ nhds_within x (set.Ici b'))), ∀ (b' : M), b' ∈ nhds_within x (set.Ici b') → b' - b = δ) :=
sorry

continuous_linear_map.is_Lprojection_norm {𝕜 V W : Type*}
	[normed_group V] [normed_group W] [nondiscrete_normed_field 𝕜]
	[normed_space 𝕜 V] [normed_space 𝕜 W] (L : V →L[𝕜] W) :
	is_Lprojection (λ (x : V), ∥L.to_linear_map x∥) L :=
sorry