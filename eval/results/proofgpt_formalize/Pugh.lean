import .common 

open set real filter function ring_hom
open_locale big_operators
open_locale filter
open_locale topological_space
noncomputable theory 

universes u v 

theorem exercise_2_109 {M : Type*} [metric_space M] [totally_disconnected_space M] (p : M × M) : totally_disconnected_space M :=
sorry

theorem exercise_2_126 {E : set ℝ} (hE : set.unbounded has_lt.lt E) : ∃ (p : ℝ), p ∈ set.Icc 0 1 :=
sorry

theorem exercise_2_12a {α β : Type*} {r : α → α → Prop} {p : β → β → Prop} {f : filter β} (hf : function.injective f) : filter.map f (filter.map r r) ≤ filter.map r (filter.map f r) ↔ ∀ (k : β), r (f k) (f (f.map r k)) :=
sorry

theorem exercise_2_12b {α β : Type*} {r : α → α → Prop} {p : β → β → Prop} (f : ℕ → β) (h : ∀ (k : ℕ), function.surjective (p (f k))) : filter.map (λ (k : ℕ), f k) filter.at_top ≤ filter.map (λ (k : ℕ), p (f k)) filter.at_top :=
sorry

theorem exercise_2_137 {M : Type*} [metric_space M] [topological_space.separable_space M] {P : set M} (hP : is_closed P) (hP' : ∀ (x : M), x ∈ P → x ∈ metric.sphere 0 1) : is_refl_trans_gen P P :=
sorry

theorem exercise_2_26 {M : Type*} [topological_space M] [comm_monoid M] {U : set M} : is_open U ↔ ∀ (x : M), x ∉ U → ¬x.is_limit :=
sorry

theorem exercise_2_29 {X : Type u} [metric_space X] (𝒯 : topological_space.opens X) (𝒰 : topological_space.closeds X) : ∃ (s : topological_space.GH_space X), Gromov_Hausdorff.GH_space.to_GH_space = 𝒯 :=
sorry

theorem exercise_2_32a {s : set ℕ} : (∀ (x : ℕ), x ∈ s → nat.clopen x) → nat.clopen s :=
sorry

theorem exercise_2_41 {m : ℝ} {β : Type*} [normed_group β] (h : ∀ (x : β), ∥x∥ ≤ 1) {s : set (fin m → β)} (hs : is_compact s) : ∥s.sum (λ (i : fin m), 1 - i)∥ ≤ 1 :=
sorry

theorem exercise_2_46 {M : Type*} [topological_space M] [t2_space M] [compact_space M] [disjoint_space M] [nonempty M] (A B : set M) (hA : is_compact A) (hB : is_compact B) (hAB : disjoint A B) (hA₂ : B.nonempty) : ∃ (a₀ a₁ : M) (b₀ b₁ : M), a₀ ∈ A ∧ b₀ ∈ B ∧ ∀ (a : M), a ∈ A → ∀ (b : M), b ∈ B → has_nndist.nndist a₀ b₀ ≤ has_nndist.nndist a₁ b₁ :=
sorry

theorem exercise_2_57 {α : Type u} [topological_space α] {s : set α} (h : is_connected s) : ¬is_generalized_root (λ (u : α), interior s) s :=
sorry

theorem exercise_2_79 {E : Type*} [inner_product_space ℝ E] (M : Type*) [topological_space M] [nonempty M] [locally_path_connected_space M] [connected_space M] : is_path_connected (set.range (λ (x : M), (continuous_linear_map.proj x).range)) :=
sorry

theorem exercise_2_85 {M : Type*} [topological_space M] [compact_space M] {ι : Type*} {U : ι → set M} (hU : ∀ (i : ι), is_open (U i)) (p : M) (hp : p ∈ nhds p) : ∃ (s : finset ι) (H : s ∈ nhds p), s.card = p ∧ ∀ (i : ι), i ∈ s → U i ⊆ s :=
sorry

theorem exercise_2_92 {X : Top} (U : topological_space.opens ↥X) : (Top.direct_nonempty_compact_covering U).nonempty :=
sorry

theorem exercise_3_1 (f : circle_deg1_lift) (x : ℝ) (h : ∀ (t : ℝ), has_dist.dist (⇑f t) (⇑f x) ≤ has_dist.dist t x ^ 2) : ⇑f x = function.const ℝ (⇑f x) :=
sorry

theorem exercise_3_11a {a b : ℝ} (h : a < b) (f : ℝ → ℝ) : filter.tendsto (λ (x : ℝ), deriv^[2] f x) (nhds_within a (set.Ioi a)) (nhds (deriv^[2] f a)) :=
sorry

theorem exercise_3_18 {𝕜 : Type*} [nondiscrete_normed_field 𝕜] [complete_space 𝕜] {L : set 𝕜} (hL : is_closed L) (h_zero : ∀ (x : 𝕜), x ∈ L → 0 ∈ set.Icc 0 1) (h_one : ∀ (x : 𝕜), x ∈ L → 1 ∈ set.Icc 0 1) : ∃ (f : 𝕜 → 𝕜), smooth_function 𝕜 f ∧ ∀ (x : 𝕜), x ∈ L → f x = 0 :=
sorry

theorem exercise_3_4 (n : ℕ) : filter.tendsto (λ (x : ℕ), real.sqrt (n + 1) - real.sqrt n) filter.at_top (nhds 0) :=
sorry

theorem exercise_3_63a (p : ℝ) (hp : 1 < p) : summable (λ (k : ℕ), (↑k ^ p)⁻¹) :=
sorry

theorem exercise_3_63b {p : ℝ} (hp : 1 ≤ p) {k : ℕ} : (finset.Ico 1 (k + 1)).sum (λ (i : ℕ), ↑k ^ p) ∣ 0 :=
sorry

theorem exercise_4_15a {α : Type*} [topological_space α] {m : measurable_space α} {μ : measure_theory.measure α} [linear_order α] [order_topology α] [topological_space.second_countable_topology α] [measure_theory.is_locally_finite_measure μ] {a b : α} {f : α → ℝ} (hf : filter.tendsto f (nhds_within a (set.Ioi a)) (nhds 0)) : measure_theory.mod_continuous f μ ↔ measure_theory.has_modulus μ f :=
sorry

theorem exercise_4_19 {M A : Type*} [topological_space M] [compact_space M] [nonempty M] [add_comm_group M] [topological_add_group M] (hA : dense_range A) (hM : is_compact M) : ∃ (δ : ℝ) (k : ℕ), ∀ (a : M), ∃ (b : M) (H : b ∈ nhds_within a (set.Ici a)), ∀ (x : M), x ∈ M → (∃ (b' : M) (H : b' ∈ nhds_within x (set.Ici b'))), ∀ (b' : M), b' ∈ nhds_within x (set.Ici b') → b' - b = δ) :=
sorry

theorem exercise_5_2 {𝕜 V W : Type*} [normed_group V] [normed_group W] [nondiscrete_normed_field 𝕜] [normed_space 𝕜 V] [normed_space 𝕜 W] (L : V →L[𝕜] W) : is_Lprojection (λ (x : V), ∥L.to_linear_map x∥) L :=
=======
euclidean_space.is_totally_disconnected_of_dist_le_max {M : Type*}
	[metric_space M] (x y z : M) :
	is_totally_disconnected (has_dist.dist x z) (linear_order.max (has_dist.dist x y) (has_dist.dist y z)) :=
sorry

exists_forall_mem_of_differentiable_within_at {E : Type*}
	[normed_group E] [normed_space ℝ E] [complete_space E] {s : set ℝ}
	(hs : s ∈ nhds_within 0 (set.Ioi 0)) :
	∃ (p : ℝ) (H : p ∈ set.Ioi 0), ∀ (x : ℝ), x ∈ s → p ≤ x :=
sorry

nat.partrec.code.eval_is_rotated_of_le_of_le {p : ℕ →. bool}
	{f : ℕ →. ℕ} {k : ℕ} (h : nat.partrec.code.eval f k ≤ p) :
	(nat.partrec.code.eval (λ (k : ℕ), (f k).dom) (p k)).is_rotated :=
sorry

nat.partrec.code.is_equiv_of_surjective_of_is_compl {p : ℕ →. bool}
	{f : ℕ →. ℕ} (hf : nat.partrec f) (hf' : function.surjective f)
	(h : ∀ (k : ℕ), (f k).is_compl) :
	(nat.partrec.code.of_surjective f hf').is_equiv (nat.partrec.code.of_surjective f hf').trans (nat.partrec.code.of_surjective f hf h) :=
sorry

is_closed.exists_prop_of_is_closed_of_exists_mem_is_separable
	{V : Type u} [metric_space V] [complete_space V] {P : set V}
	(hP : is_closed P) (hP' : ∀ (x : V), x ∈ P → P.nonempty)
	(hP'' : ∀ (x : V), x ∈ P → (∃ (r : ℝ), x = r • y)) :
	∃ (r : ℝ), P.rel r :=
sorry

is_open_iff_is_open_compl {M : Type*} [topological_space M]
	[t2_space M] {s : set M} :
	is_open s ↔ is_open sᶜ :=
sorry

measure_theory.measure.mk_of_open_subsets_to_is_open {α : Type*}
	{m0 : measurable_space α} {μ : measure_theory.measure α} {𝒜 : set (set α)}
	(h𝒜 : is_open 𝒜) (hμ : ∀ (s : set α), s ∈ 𝒜 → is_open s)
	(hμ' : ∀ (s : set α), s ∈ 𝒜 → is_closed s)
	(hμ'' : ∀ (s : set α), s ∈ 𝒜 → ⇑μ s ≠ ⊤) :
	(measure_theory.measure.mk_of_open_subsets h𝒜 hμ').to_is_open = μ :=
sorry

nat.clopen_of_clopen_set {n : ℕ} (hn : n ≠ 0) :
	is_clopen {n : ℕ | n ≤ n.clog n} :=
sorry

matrix.compact_of_bounded_of_one_le {m R : Type*} [fintype m]
	[semi_normed_group R] [compact_space R] (B : fin 1 → R) (hB : is_compact ↑B) :
	is_compact ↑B :=
sorry

is_compact.exists_forall_le_dist {M : Type*} [topological_space M]
	[compact_space M] {A B : set M} (hA : is_compact A) (hB : is_compact B)
	(hA' : A.nonempty) (hB' : B.nonempty) :
	(∃ (a₀ : M) (b₀ : M), ∀ (a : M), a ∈ A → b₀ ≤ a → has_dist.dist a₀ b₀ ≤ has_dist.dist a b) :=
sorry

is_connected_inter_not_inter_eq {α : Type u} [topological_space α]
	{s : set α} (h : is_connected s) :
	¬s ∩ s = ∅ :=
sorry

simple_graph.nonempty_compacts.coe_to_path_connected {V : Type u}
	[category_theory.category V] [category_theory.limits.has_zero_morphisms V]
	{G : simple_graph V} (M : simple_graph V) (h : M.nonempty)
	(h' : G.connected) :
	↑(M.to_path_connected h h') = set.range ⇑(h.to_path_connected_range) :=
sorry

compact_open_covering_of_open_covering {M : Type*} [topological_space M]
	[compact_space M] (h : ∀ (p : M), p ∈ open_covering M ↔ p ≤ 2) :
	compact_open_covering M :=
sorry

compact_nonempty_compact_covering {α : Type u} [topological_space α]
	{s : set α} [compact_space α] (h : s.nonempty) :
	nonempty ↥s :=
sorry

circle_deg1_lift.mk_apply (f : circle_deg1_lift) (x : ℝ) :
	⇑circle_deg1_lift.mk f x = ⟨f.trans, _⟩ - ⇑f x, _⟩ :=
sorry

ennreal.tendsto_nhds_zero_div_add {a b : ennreal} {f : ennreal → ennreal}
	(hf : ∀ (x : ennreal), f (x / a + x) = f x / a ^ 2) (h0 : f 0 = 0)
	(hI : filter.tendsto f (nhds 0) (nhds 0)) :
	filter.tendsto (λ (x : ennreal), f (x / a + x) - 2 * f x + f x / a ^ 2) (nhds 0) (nhds 0) :=
sorry

exists_smooth_zero_one_iff_mem_of_closed {E : Type*} [normed_group E]
	[normed_space ℝ E] {H : Type*} [topological_space H] {I : model_with_corners ℝ E H}
	{L : set H} [closed_space H] [L.is_closed] :
	(∃ (f : cont_mdiff_map I (model_with_corners_self ℝ ℝ) H ℝ ⊤), f = cont_mdiff_map I (model_with_corners_self ℝ ℝ) H ∧ ∀ (x : H), ⇑f x = 0 ↔ x ∈ L :=
sorry

nat.tendsto_succ_at_top_zero (n : ℕ) :
	filter.tendsto (λ (p : ℕ × ℕ), p.fst.succ - p.snd.succ) filter.at_top (nhds 0) :=
sorry

real.summable_one_div_pow_log_pow {p : ℝ} (hp : 1 < p) :
	summable (λ (k : ℕ), 1 / ↑k ^ p) :=
sorry

real.exp_bound_div_pow_of_sum_one_div_pow_le_one {p : ℝ} (k : ℕ) :
	p ≤ 1 → (finset.range k).prod (λ (i : ℕ), 1 / ↑(k.log ↑i)) ≤ 1 :=
sorry

continuous_on_Icc_iff_modulus {α : Type*} [linear_ordered_field α]
	[topological_space α] [order_topology α] [densely_ordered α]
	{μ : measure_theory.measure α} [measure_theory.is_locally_finite_measure μ]
	{f : α → ℝ} {s t : set α} (hf : continuous_on f s) (hs : s.nonempty)
	(ht : t.nonempty) :
	μ.restrict (set.Icc s t) = (μ.restrict s).to_real - (μ.restrict t).to_real ↔ ∀ (s : set α), s ∈ set.Icc a b → has_modulus_on f (μ.restrict s) s :=
sorry

continuous_map.exists_finset_forall_mem_closed_ball_of_compact_of_dense
	{M : Type*} [topological_space M] [compact_space M] [nonempty M] {A : set M}
	(hA : dense A) :
	∃ (δ : ℝ) (H : δ > 0), ∀ (a : M), a ∈ A → a ∈ metric.closed_ball a δ :=
sorry

continuous_linear_map.to_bilin_apply {𝕜₁ : Type*} [normed_field 𝕜₁]
	{𝕜₂ : Type*} [normed_field 𝕜₂] [complete_space 𝕜₂] (L : Type*)
	[normed_group L] [normed_space 𝕜₁ L] [normed_space 𝕜₂ L]
	[nontrivial L] (φ : 𝕜₁ →L[𝕜₂] L) (x : 𝕜₁) :
	⇑(⇑(continuous_linear_map.to_bilin L φ) x) = ⇑φ x :=
