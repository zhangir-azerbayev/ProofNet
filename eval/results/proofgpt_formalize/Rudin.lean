

complex.exists_mem_quadrant_II (z : ℂ) :
	∃ (r : ℝ) (w : ℂ), w.re ∈ set.Icc 0 1 ∧ z = ↑r * w :=
sorry

complex.abs_add_comm_group_le {ι : Type*} [fintype ι] (z : ι → ℂ) :
	(complex.add_comm_group z).abs ≤ (finset.univ.sum (λ (i : ι), z i)).card + (finset.univ.sum (λ (i : ι), z i)).card + -z :=
sorry

complex.abs_sub_abs_le {x y : ℂ} (hx : complex.abs x - complex.abs y ≤ complex.abs x - y) :
	||x - y|| ≤ complex.abs x - y :=
sorry

complex.of_real_norm_one_add_pow_two_add_abs_sub_of_abs_eq_one {z : ℂ}
	(hz : complex.abs z = 1) (h : z * z = 1) :
	↑(⇑complex.norm_sq z) = 1 :=
sorry

real.frequently_dist_eq_of_dist_gt_of_frequently_dist_eq {k : ℕ}
	{x y : ℝ} {r : ℝ} (h3 : 3 ≤ k) (hx : x ∈ set.Icc 0 1) (hy : y ∈ set.Icc 0 1)
	(hxy : has_dist.dist x y = r) :
	∃ᶠ (z : ℝ) in nhds_within 0 (set.Ioi 0), has_dist.dist z x = r :=
sorry

modular_group.m_add_m_sq_add_m_abs_sq_eq_two_mul_m_add_m_sq
	{R : Type*} [comm_ring R] [is_domain R] [is_principal_ideal_ring R]
	(hR : 0 < R) {k : ℕ} (x y : R) :
	|x + y| ^ 2 + |x - y| ^ 2 = 2 * |x| ^ 2 + 2 * |y| ^ 2 ↔ x ∈ modular_group.R k ∧ y ∈ modular_group.R k :=
sorry

pythagorean_formula.mul_eq_zero {R : Type*} [comm_ring R] {k : ℕ}
	(x : R ^ k) (h : 2 ≤ k) :
	∃ (y : R ^ k), y ≠ 0 ∧ x * y = 0 :=
sorry

exists_mul_eq_zero_of_eq_one {R : Type*} [comm_semiring R] {k : ℕ}
	(hk : k = 1) (x : R ^ k) :
	¬∃ (y : R ^ k), y ≠ 0 ∧ x * y = 0 :=
sorry

fiset.exists_mem_finset_three_mul_eq_four_mul_sub_iff_card_eq_two
	{α : Type*} [decidable_eq α] [fintype α] {a b c : α} {r : ℝ}
	(h : ∀ (x : α), x ∈ {a, b} → r ≤ |x - a| ↔ r ≤ |x - b|) :
	(∃ (c : α) (H : c ∈ finset.finset_image (λ (k : α), 3 * k) {a, b}), r = 2 * |b - a| ∧ 3 * r = 2 * |b - a|) ↔ r = 2 * |b - a| ∧ 3 * r = 2 * |b - a| :=
sorry

irrational.rat_add (r : ℚ) {x : ℝ} (h : irrational (↑r + x)) :
	irrational x :=
sorry

irrational.of_rat_mul {x : ℝ} {r : ℚ} (hr : rat.mk x r ≠ 0)
	(h : irrational (↑r * x)) :
	irrational x :=
sorry

real.sq_mul_self_of_empty_rational_sq :
	is_empty (rat.sqrt 2) → ∀ (x : ℝ), x ^ 2 = 12 :=
sorry

covby.le_of_is_glb_of_is_lub {α β : Type*} [linear_order α]
	[linear_order β] {E : set α} (hE : E.nonempty) (h₁ : is_glb E α)
	(h₂ : is_lub E β) :
	α ≤ β :=
sorry

cInf_neg {α : Type*} [conditionally_complete_lattice α] [add_group α]
	[covariant_class α α has_add.add has_le.le] {A : set α} (hA : A.nonempty)
	(hA' : bdd_below A) :
	has_Inf.Inf (-A) = -has_Sup.Sup A :=
sorry

complex.not_lt_of_denom_lt_one (x : ℂ) :
	¬x < 1 :=
sorry

is_closed.is_separated {α : Type u} [topological_space α]
	[t2_space α] {s t : set α} (hs : is_closed s) (ht : is_closed t)
	(hst : disjoint s t) :
	is_separated s :=
sorry

metric.Prove_separable {X : Type u} [metric_space X]
	[topological_space.separable_space X] (s : set X) [infinite ↥s] :
	topological_space.is_separable s :=
sorry

Kuratowski_embedding.exists_countable_base (K : Type u) [metric_space K]
	[compact_space K] :
	∃ (n : ℕ), nonempty (encodable.decode K n) :=
sorry

uncountable.perfect_cofinite_of_ condensation_points {α : Type*}
	[metric_space α] {E : set α} (hE : set.uncountable E)
	(hE : (set.range condensation_points).nonempty) (P : set α) :
	perfect_space ↥(set.cofinite E) :=
sorry

uncountable.many_not_mem_of_countable {α : Type*} [topological_space α]
	{E : set (α × α)} (hE : is_uncountable E) {P : set α}
	(hP : ∀ (x : α), x ∈ E → x ∉ P) :
	∃ (k : ℕ), (set.range (λ (x : α), (x, x) ∉ P)).countable ∧ k ≤ cardinal.mk ↥P :=
sorry

is_closed.exists_is_perfect_union_and_countable_of_is_separable
	{α : Type u} [pseudo_metric_space α] (hs : is_closed {s : set α | is_separable s}) :
	∃ (f : set α), is_perfect f ∧ s ⊆ f ∧ (∀ (ε : ℝ), ε > 0 → (∃ (i : ℕ), is_closed {s : set α | ε / 2 < i ∧ s ⊆ f ∧ i < ε)) ∧ ∀ (ε : ℝ), ε > 0 → (∃ (i : ℕ), is_closed {s : set α | ε / 2 < i ∧ s ⊆ f ∧ i < ε)) :=
sorry

real.ave_disjoint_of_open_of_countable {s : set ℝ} (hs : is_open s)
	(h : ∀ (t : set ℝ), t ∈ s → t.countable → disjoint (s ∩ ⋃ (x : ℝ) (H : x ∈ t), (set.Icc (-x) x).preimage s) s) :
	s.disjoint (⋃ (x : ℝ) (H : x ∈ s), (set.Icc (-x) x).preimage s) :=
sorry

real.absolutely_ convergent.prod_cau {α β : Type*} [comm_ring α]
	[comm_ring β] {abv : β → α} {cd : complex_shape β} {ε : ℝ} (hε : 0 < ε)
	(hα : α ≃ᵐ[ε] β) :
	∃ (b : β), abv b = cd :=
sorry

measure_theory.vector_measure.abs_tendsto_of_cover {α : Type*}
	{m : measurable_space α} {M : Type*} [add_comm_monoid M] [topological_space M]
	[t2_space M] {s : ℕ → set α} {t : set α}
	(h : ∀ (i : ℕ), s i ⊆ t) (ht : t.countable) :
	filter.tendsto (λ (i : ℕ), |s i|) filter.at_top (nhds (⇑measure_theory.vector_measure.restrict s t)) :=
sorry

metric.cau_seq.tendsto_nhds {α : Type u} {β : Type v}
	[pseudo_metric_space α] [nonempty β] [semilattice_sup β] {p : β → α}
	(hp : is_cau_seq has_abs.abs p) {l : β → ℕ} (hpl : filter.tendsto l filter.at_top (nhds p)) :
	filter.tendsto (λ (n : β), metric.cau_seq.const (p n) (l n)) filter.at_top (nhds p) :=
sorry

closed_nonempty_bInter_of_tendsto_diam_zero {α : Type*} [metric_space α]
	{E : ℕ → set α} (hE : (set.range E).nonempty)
	(h : ∀ (n : ℕ), metric.closed_ball (E n) (metric.diam (E n)) ⊆ metric.closed_ball (E (n + 1)) (metric.diam (E n)))
	(h' : filter.tendsto (λ (n : ℕ), metric.diam (E n)) filter.at_top (nhds 0)) :
	(⋂ (n : ℕ), E n).nonempty :=
sorry

dense_range_of_nonempty_complete_space {α : Type*}
	[metric_space α] [nonempty α] [complete_space α] {G : ℕ → set α}
	(H : ∀ (n : ℕ), is_open (G n)) (hd : ∀ (n : ℕ), dense (G n)) :
	(⋂ (n : ℕ), G n).nonempty :=
sorry

surreal.tendsto_pow_half_succ_nhds_one_div_two :
	filter.tendsto (λ (x : ℕ), real.sqrt (↑x ^ 2 + ↑x)) filter.at_top (nhds 1 / 2) :=
sorry

real.tendsto_sqrt_two_add_series_step_down (s : ℝ) (h : s 1 = real.sqrt 2)
	(n : ℕ) :
	filter.tendsto (λ (k : ℕ), s (n + 1)) filter.at_top (nhds (real.sqrt (2 + real.sqrt 2))) :=
sorry

ennreal.limsup_add_le_limsup_add_limsup {a b : ℕ → ennreal}
	(h : filter.tendsto a filter.at_top (nhds 0)) (h' : filter.tendsto b filter.at_top (nhds 0)) :
	(a + b).limsup ≤ a.limsup + b.limsup :=
sorry

ennreal.tendsto_sum_sqrt_add_one_at_top (a : ℕ → ennreal) :
	filter.tendsto (λ (n : ℕ), (finset.range n).sum (λ (i : ℕ), a i)) filter.at_top (nhds (⊤ + 1)) :=
sorry

nnreal.tendsto_sqrt_mul_antitone (a : ℕ → nnreal) :
	filter.tendsto (λ (n : ℕ), ⟨↑(a n), _⟩) filter.at_top (nhds (⟨↑a, _⟩)) :=
sorry

monotonic_bounded_of_tendsto_sigma {α β : Type*} [topological_space α]
	[linear_ordered_cancel_add_comm_monoid α] [order_topology α] [nonempty β]
	{f : β → α} (sigma_f : filter.tendsto f (filter.cofinite.sigma (λ (n : β), {b n})) (nhds (f b)))
	(hα : ∀ (n : β), monotone (λ (a : α), b n))
	(hβ : filter.is_bounded_under has_le.le filter.at_top (λ (n : β), ∑' (a : α), b n)) :
	∃ (a : α), filter.tendsto f (filter.cofinite.sigma (λ (n : β), {b n})) (nhds a) :=
sorry

metric.uniform_inducing.cauchy_seq_image {α : Type u} {β : Type v}
	[metric_space α] [nonempty α] [semilattice_sup β] {f : α → β}
	(hf : uniform_inducing f)
	(H : ∀ (x : α), cauchy_seq (λ (n : β), f (x n))) :
	cauchy_seq (λ (n : β), f (x n)) :=
sorry

uniform_continuous_map_of_uniform_continuous {α β : Type*}
	[uniform_space α] [uniform_space β] (f : α → β) (hf : uniform_continuous f) :
	uniform_continuous (uniform_continuous_map f) :=
sorry

continuous_map.exists_of_Icc_eq_one (α β : Type*)
	[linear_ordered_add_comm_group α] [topological_space α] [order_topology α]
	[topological_add_group α] [nonempty α] [densely_ordered α]
	(I : model_with_corners α β) (hI : is_closed I.to_topological_space)
	(f : C(↥I, α)) (hf : ∀ (x : α), ⇑f x = x) :
	∃ (x : α), ⇑f x = x :=
sorry

continuous_open_map.monotone (R : Type*) [topological_space R]
	[linear_ordered_field R] [order_topology R] :
	monotone continuous_open_map :=
sorry

has_deriv_at.of_Ioo_of_rat_lt {f : ℝ → ℝ} {a b c : ℝ}
	(hab : a < b) (hfc : has_deriv_at f (f a) c)
	(hfIoo : ∀ (r : ℚ), f a < ↑r → is_closed {x : ℝ | f x = ↑r})
	(hfb : continuous_on f (set.Ioo a b)) :
	∃ (x : ℝ) (H : x ∈ set.Ioo a b), f x = c :=
sorry

not_continuous_of_tendsto_zero_add_add {α : Type u}
	[topological_space α] [linear_ordered_add_comm_group α] [order_topology α]
	{f : α → α} (h : filter.tendsto (λ (x : α), f (x + h)) (nhds 0) (nhds 0))
	(hf : ∀ (x₀ : α), nhds x₀ = filter.map f (nhds x₀)) :
	¬continuous f :=
sorry

exists_pos_dist_lt_of_disjoint {α : Type u} [metric_space α]
	{K F : set α} (h : disjoint K F) [proper_space α] [compact_space α]
	[closed_space α] {p q : α} (hp : p ∈ K) (hq : q ∈ F) :
	∃ (δ : ℝ), 0 < δ ∧ ∀ (x : α), x ∈ K → ∀ (y : α), y ∈ F → has_dist.dist p q < δ :=
sorry

strict_convex_on_Ioc_of_add_div_two_le {a b : ℝ}
	(h : ∀ (x y : ℝ), x ∈ set.Ioo a b → y ∈ set.Ioo a b → f (x + y) / 2 ≤ f x + f y / 2) :
	strict_convex_on ℝ (set.Ioc a b) f :=
sorry

continuous_map.closure_image_subset_closure_image {α β : Type*}
	[topological_space α] [compact_space α] [metric_space β] (f : C(α, β))
	{s : set α} (h : ∀ (x : α), ⇑f x ∈ s) :
	⇑(f.closure) '' s ⊆ closure (⇑f '' s) :=
sorry

is_closed.zero_sub_preimage_of_continuous_on {α : Type u}
	{β : Type v} [topological_space α] [metric_space β] [has_zero β] {f : α → β}
	{Z : set α → β} (hZ : is_closed Z) (hf : continuous_on f Z) :
	is_closed (Z (f ⁻¹' {0})) :=
sorry

continuous_map.dense_range_of_eval_mem_closure {α β : Type*}
	[topological_space α] [metric_space β] {f g : C(α, β)} {E : set α}
	(hE : dense_range ⇑f) (h : set.range ⇑f ∈ closure (set.range ⇑g)) :
	dense_range ⇑g :=
sorry

continuous_map.extend_diam_of_surjective {α β : Type*}
	[topological_space α] [metric_space β] {f g : C(α, β)} {E : set α}
	(hE : dense E) (h : ∀ (p : α × α), p ∈ uniformity α → g p.fst = f p.snd) :
	continuous_map.extend_diam f g E = E :=
sorry

real.exists_continuous_on_Icc_of_closed {E : set ℝ} (f : ℝ → ℝ)
	(hE : is_closed E) (hf : ∃ (g : ℝ → ℝ), continuous_on g (set.Icc (-f 0) f)) :
	∃ (g : ℝ → ℝ), continuous_on g (set.Icc (-f 0) f) ∧ ∀ (x : ℝ), g x = f x :=
sorry

exists_exists_eq_of_exists_not_exists_continuous_real (E : Type*)
	(f : E → ℝ) (hE : ∃ (x : E), ¬∃ (r : ℝ), f x = ↑r) :
	∃ (g : E → ℝ), continuous g ∧ ∀ (x : E), g x = f x :=
sorry

complex.continuous_on_iff_graph_subset_plane {E : set ℝ}
	(f : ℝ → ℝ) (hE : is_compact E) :
	continuous_on f E ↔ (∀ (x : ℝ), x ∈ E → (f x, f x)) ∧ continuous_on (λ (x : ℝ), (f x, f x)) E :=
sorry

real.uniform_continuous_on_of_bounded_of_compact (f : ℝ → ℝ)
	(hf : uniform_continuous_on f (set.Icc 0 1)) (h : ∀ (x : ℝ), x ∈ set.Ico 0 1 → ∀ (y : ℝ), y ∈ set.Icc 0 1 → f x ≤ f y) :
	uniform_continuous_on f (set.Icc 0 1) :=
sorry

bernstein_approximation.exists_uniform_continuous_not_bounded_on
	{𝕜 E : Type*} [normed_field 𝕜] [add_comm_group E] [module 𝕜 E]
	[uniform_space E] [uniform_add_group E] [has_continuous_smul 𝕜 E]
	(x : bernstein_approximation 𝕜 E) :
	∃ (f : E → 𝕜), uniform_continuous f ∧ ¬metric.bounded (set.range f) :=
sorry

circle_deg1_lift.const_apply (f : circle_deg1_lift) (x y : ℝ)
	(h : ∀ (z : ℝ), z ∈ set.Icc x y → |⇑f z - ⇑f y| ≤ (x - y) ^ 2) :
	⇑f x = ⇑f y :=
sorry

real.Prove_le_of_two_le_of_tendsto {a : ℝ} (ha : a ≠ 0) {f : ℝ → ℝ}
	(hfd : 2 ≤ᶠ[filter.at_top] f) {M₀ M₁ M₂ : ℝ}
	(hM₀ : filter.tendsto M₀ filter.at_top (nhds 0))
	(hM₁ : filter.tendsto M₁ filter.at_top (nhds 0))
	(hM₂ : filter.tendsto M₂ filter.at_top (nhds 0)) (hM₁₂ : M₁ ^ 2 ≤ 4 * M₀ * M₂) :
	M₁ ^ 2 ≤ 4 * M₀ * M₂ :=
sorry

exists_deriv_within_Iic_of_fderiv_within_Iic {f : ℝ → ℝ}
	(h : ∃ (x : ℝ), differentiable_within_at ℝ f (set.Iic x) x)
	(h' : differentiable_within_at ℝ f (set.Iic x) x)
	(h'' : differentiable_within_at ℝ f (set.Iic x) x)
	(h'h'' : ∃ (x : ℝ), differentiable_within_at ℝ f (set.Iic x) x) :
	∃ (x : ℝ), fderiv_within ℝ f (set.Iic x) x ≤ 3 :=
sorry

strict_mono_on.inv_of_deriv_within_Ioo {a b : ℝ} (hab : a < b)
	{f' : ℝ → ℝ} (h' : 0 < f') (hf : strict_mono_on f (set.Ioo a b))
	(g' : ℝ → ℝ) (hg : differentiable_on ℝ g (set.Ioo a b))
	(hgf' : ∀ (x : ℝ), x ∈ set.Ioo a b → deriv_within g (set.Ioo a b) x < f') :
	strict_mono_on (λ (x : ℝ), (f' x)⁻¹) (set.Ioo a b) :=
sorry

has_deriv_at_filter_inverse {𝕜 : Type u} [nondiscrete_normed_field 𝕜]
	(g : 𝕜 → 𝕜) (g' : 𝕜) {M : nnreal} (Mpos : 0 < M)
	(hM : ∃ (c : 𝕜), ∥g'∥ ≤ c * ∥g∥) {ε : 𝕜} (hε : 0 < ε)
	(hmg : ∀ᶠ (x : 𝕜) in nhds 0, has_deriv_at_filter g g' x)
	(hg' : ∀ᶠ (x : 𝕜) in nhds 0, ∥g'∥ ≤ M) :
	has_deriv_at_filter g g' x :=
sorry

polynomial.exists_nat_scale_roots_of_even_odd_zero {C : ℕ → Type*}
	[category_theory.category C] [category_theory.preadditive C]
	[category_theory.limits.has_scale_roots C] (n : ℕ) :
	(∃ (r : ℕ), 0 < r ∧ (finset.range n).sum (λ (i : ℕ), C i) / 2 + (finset.range n).sum (λ (i : ℕ), C i) / (2 * n) + C n = 0) :=
sorry

has_deriv_at.lim_zero {f : ℝ → ℝ} {f' x : ℝ} (hx : 0 < x)
	(hf : has_deriv_at f f' x) :
	filter.tendsto (λ (x : ℝ), f x + 1 - f x) filter.at_top (nhds 0) :=
sorry

ennreal.continuous_of_div_tendsto_zero {a b c d : ennreal}
	{f : c → ennreal} (h : a < b) (h' : 0 < x) (g : ℝ → ennreal)
	(hg : filter.tendsto g (nhds_within x (set.Ioi 0)) (nhds 0))
	(h'' : monotone g) :
	continuous (λ (x : ennreal), f x / x) :=
sorry

deriv.lhopital_zero_nhds {x : ℝ} {l : filter ℝ} {f g : ℝ → ℝ}
	(hx : filter.tendsto f l (nhds x)) (hg : filter.tendsto g l (nhds x))
	(h : f x = 0) :
	filter.tendsto (λ (t : ℝ), deriv f t / deriv g t) l (nhds x) :=
sorry