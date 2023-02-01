import .common 

open real
open topological_space
open filter
open_locale topological_space
open_locale big_operators
open_locale complex_conjugate
open_locale filter

universes u v

theorem exercise_1_11a (z : ℂ) : ∃ (r : ℝ) (w : ℂ), w.re ∈ set.Icc 0 1 ∧ z = ↑r * w :=
sorry

theorem exercise_1_12 {ι : Type*} [fintype ι] (z : ι → ℂ) : (complex.add_comm_group z).abs ≤ (finset.univ.sum (λ (i : ι), z i)).card + (finset.univ.sum (λ (i : ι), z i)).card + -z :=
sorry

theorem exercise_1_13 {x y : ℂ} (hx : complex.abs x - complex.abs y ≤ complex.abs x - y) : ||x - y|| ≤ complex.abs x - y :=
sorry

theorem exercise_1_14 {z : ℂ} (hz : complex.abs z = 1) (h : z * z = 1) : ↑(⇑complex.norm_sq z) = 1 :=
sorry

theorem exercise_1_16a {k : ℕ} {x y : ℝ} {r : ℝ} (h3 : 3 ≤ k) (hx : x ∈ set.Icc 0 1) (hy : y ∈ set.Icc 0 1) (hxy : has_dist.dist x y = r) : ∃ᶠ (z : ℝ) in nhds_within 0 (set.Ioi 0), has_dist.dist z x = r :=
sorry

theorem exercise_1_17 {R : Type*} [comm_ring R] [is_domain R] [is_principal_ideal_ring R] (hR : 0 < R) {k : ℕ} (x y : R) : |x + y| ^ 2 + |x - y| ^ 2 = 2 * |x| ^ 2 + 2 * |y| ^ 2 ↔ x ∈ modular_group.R k ∧ y ∈ modular_group.R k :=
sorry

theorem exercise_1_18a {R : Type*} [comm_ring R] {k : ℕ} (x : R ^ k) (h : 2 ≤ k) : ∃ (y : R ^ k), y ≠ 0 ∧ x * y = 0 :=
sorry

theorem exercise_1_18b {R : Type*} [comm_semiring R] {k : ℕ} (hk : k = 1) (x : R ^ k) : ¬∃ (y : R ^ k), y ≠ 0 ∧ x * y = 0 :=
sorry

theorem exercise_1_19 {α : Type*} [decidable_eq α] [fintype α] {a b c : α} {r : ℝ} (h : ∀ (x : α), x ∈ {a, b} → r ≤ |x - a| ↔ r ≤ |x - b|) : (∃ (c : α) (H : c ∈ finset.finset_image (λ (k : α), 3 * k) {a, b}), r = 2 * |b - a| ∧ 3 * r = 2 * |b - a|) ↔ r = 2 * |b - a| ∧ 3 * r = 2 * |b - a| :=
sorry

theorem exercise_1_1a (r : ℚ) {x : ℝ} (h : irrational (↑r + x)) : irrational x :=
sorry

theorem exercise_1_1b {x : ℝ} {r : ℚ} (hr : rat.mk x r ≠ 0) (h : irrational (↑r * x)) : irrational x :=
sorry

theorem exercise_1_2 : is_empty (rat.sqrt 2) → ∀ (x : ℝ), x ^ 2 = 12 :=
sorry

theorem exercise_1_4 {α β : Type*} [linear_order α] [linear_order β] {E : set α} (hE : E.nonempty) (h₁ : is_glb E α) (h₂ : is_lub E β) : α ≤ β :=
sorry

theorem exercise_1_5 {α : Type*} [conditionally_complete_lattice α] [add_group α] [covariant_class α α has_add.add has_le.le] {A : set α} (hA : A.nonempty) (hA' : bdd_below A) : has_Inf.Inf (-A) = -has_Sup.Sup A :=
sorry

theorem exercise_1_8 (x : ℂ) : ¬x < 1 :=
sorry

theorem exercise_2_19a {α : Type u} [topological_space α] [t2_space α] {s t : set α} (hs : is_closed s) (ht : is_closed t) (hst : disjoint s t) : is_separated s :=
sorry

theorem exercise_2_24 {X : Type u} [metric_space X] [topological_space.separable_space X] (s : set X) [infinite ↥s] : topological_space.is_separable s :=
sorry

theorem exercise_2_25 (K : Type u) [metric_space K] [compact_space K] : ∃ (n : ℕ), nonempty (encodable.decode K n) :=
sorry

theorem exercise_2_27a condensation_points {α : Type*} [metric_space α] {E : set α} (hE : set.uncountable E) (hE : (set.range condensation_points).nonempty) (P : set α) : perfect_space ↥(set.cofinite E) :=
sorry

theorem exercise_2_27b {α : Type*} [topological_space α] {E : set (α × α)} (hE : is_uncountable E) {P : set α} (hP : ∀ (x : α), x ∈ E → x ∉ P) : ∃ (k : ℕ), (set.range (λ (x : α), (x, x) ∉ P)).countable ∧ k ≤ cardinal.mk ↥P :=
sorry

theorem exercise_2_28 {α : Type u} [pseudo_metric_space α] (hs : is_closed {s : set α | is_separable s}) : ∃ (f : set α), is_perfect f ∧ s ⊆ f ∧ (∀ (ε : ℝ), ε > 0 → (∃ (i : ℕ), is_closed {s : set α | ε / 2 < i ∧ s ⊆ f ∧ i < ε)) ∧ ∀ (ε : ℝ), ε > 0 → (∃ (i : ℕ), is_closed {s : set α | ε / 2 < i ∧ s ⊆ f ∧ i < ε)) :=
sorry

theorem exercise_2_29 {s : set ℝ} (hs : is_open s) (h : ∀ (t : set ℝ), t ∈ s → t.countable → disjoint (s ∩ ⋃ (x : ℝ) (H : x ∈ t), (set.Icc (-x) x).preimage s) s) : s.disjoint (⋃ (x : ℝ) (H : x ∈ s), (set.Icc (-x) x).preimage s) :=
sorry

theorem exercise_3_13 convergent.prod_cau {α β : Type*} [comm_ring α] [comm_ring β] {abv : β → α} {cd : complex_shape β} {ε : ℝ} (hε : 0 < ε) (hα : α ≃ᵐ[ε] β) : ∃ (b : β), abv b = cd :=
sorry

theorem exercise_3_1a {α : Type*} {m : measurable_space α} {M : Type*} [add_comm_monoid M] [topological_space M] [t2_space M] {s : ℕ → set α} {t : set α} (h : ∀ (i : ℕ), s i ⊆ t) (ht : t.countable) : filter.tendsto (λ (i : ℕ), |s i|) filter.at_top (nhds (⇑measure_theory.vector_measure.restrict s t)) :=
sorry

theorem exercise_3_20 {α : Type u} {β : Type v} [pseudo_metric_space α] [nonempty β] [semilattice_sup β] {p : β → α} (hp : is_cau_seq has_abs.abs p) {l : β → ℕ} (hpl : filter.tendsto l filter.at_top (nhds p)) : filter.tendsto (λ (n : β), metric.cau_seq.const (p n) (l n)) filter.at_top (nhds p) :=
sorry

theorem exercise_3_21 {α : Type*} [metric_space α] {E : ℕ → set α} (hE : (set.range E).nonempty) (h : ∀ (n : ℕ), metric.closed_ball (E n) (metric.diam (E n)) ⊆ metric.closed_ball (E (n + 1)) (metric.diam (E n))) (h' : filter.tendsto (λ (n : ℕ), metric.diam (E n)) filter.at_top (nhds 0)) : (⋂ (n : ℕ), E n).nonempty :=
sorry

theorem exercise_3_22 {α : Type*} [metric_space α] [nonempty α] [complete_space α] {G : ℕ → set α} (H : ∀ (n : ℕ), is_open (G n)) (hd : ∀ (n : ℕ), dense (G n)) : (⋂ (n : ℕ), G n).nonempty :=
sorry

theorem exercise_3_2a : filter.tendsto (λ (x : ℕ), real.sqrt (↑x ^ 2 + ↑x)) filter.at_top (nhds 1 / 2) :=
sorry

theorem exercise_3_3 (s : ℝ) (h : s 1 = real.sqrt 2) (n : ℕ) : filter.tendsto (λ (k : ℕ), s (n + 1)) filter.at_top (nhds (real.sqrt (2 + real.sqrt 2))) :=
sorry

theorem exercise_3_5 {a b : ℕ → ennreal} (h : filter.tendsto a filter.at_top (nhds 0)) (h' : filter.tendsto b filter.at_top (nhds 0)) : (a + b).limsup ≤ a.limsup + b.limsup :=
sorry

theorem exercise_3_6a (a : ℕ → ennreal) : filter.tendsto (λ (n : ℕ), (finset.range n).sum (λ (i : ℕ), a i)) filter.at_top (nhds (⊤ + 1)) :=
sorry

theorem exercise_3_7 (a : ℕ → nnreal) : filter.tendsto (λ (n : ℕ), ⟨↑(a n), _⟩) filter.at_top (nhds (⟨↑a, _⟩)) :=
sorry

theorem exercise_3_8 {α β : Type*} [topological_space α] [linear_ordered_cancel_add_comm_monoid α] [order_topology α] [nonempty β] {f : β → α} (sigma_f : filter.tendsto f (filter.cofinite.sigma (λ (n : β), {b n})) (nhds (f b))) (hα : ∀ (n : β), monotone (λ (a : α), b n)) (hβ : filter.is_bounded_under has_le.le filter.at_top (λ (n : β), ∑' (a : α), b n)) : ∃ (a : α), filter.tendsto f (filter.cofinite.sigma (λ (n : β), {b n})) (nhds a) :=
sorry

theorem exercise_4_11a {α : Type u} {β : Type v} [metric_space α] [nonempty α] [semilattice_sup β] {f : α → β} (hf : uniform_inducing f) (H : ∀ (x : α), cauchy_seq (λ (n : β), f (x n))) : cauchy_seq (λ (n : β), f (x n)) :=
sorry

theorem exercise_4_12 {α β : Type*} [uniform_space α] [uniform_space β] (f : α → β) (hf : uniform_continuous f) : uniform_continuous (uniform_continuous_map f) :=
sorry

theorem exercise_4_14 (α β : Type*) [linear_ordered_add_comm_group α] [topological_space α] [order_topology α] [topological_add_group α] [nonempty α] [densely_ordered α] (I : model_with_corners α β) (hI : is_closed I.to_topological_space) (f : C(↥I, α)) (hf : ∀ (x : α), ⇑f x = x) : ∃ (x : α), ⇑f x = x :=
sorry

theorem exercise_4_15 (R : Type*) [topological_space R] [linear_ordered_field R] [order_topology R] : monotone continuous_open_map :=
sorry

theorem exercise_4_19 {f : ℝ → ℝ} {a b c : ℝ} (hab : a < b) (hfc : has_deriv_at f (f a) c) (hfIoo : ∀ (r : ℚ), f a < ↑r → is_closed {x : ℝ | f x = ↑r}) (hfb : continuous_on f (set.Ioo a b)) : ∃ (x : ℝ) (H : x ∈ set.Ioo a b), f x = c :=
sorry

theorem exercise_4_1a {α : Type u} [topological_space α] [linear_ordered_add_comm_group α] [order_topology α] {f : α → α} (h : filter.tendsto (λ (x : α), f (x + h)) (nhds 0) (nhds 0)) (hf : ∀ (x₀ : α), nhds x₀ = filter.map f (nhds x₀)) : ¬continuous f :=
sorry

theorem exercise_4_21a {α : Type u} [metric_space α] {K F : set α} (h : disjoint K F) [proper_space α] [compact_space α] [closed_space α] {p q : α} (hp : p ∈ K) (hq : q ∈ F) : ∃ (δ : ℝ), 0 < δ ∧ ∀ (x : α), x ∈ K → ∀ (y : α), y ∈ F → has_dist.dist p q < δ :=
sorry

theorem exercise_4_24 {a b : ℝ} (h : ∀ (x y : ℝ), x ∈ set.Ioo a b → y ∈ set.Ioo a b → f (x + y) / 2 ≤ f x + f y / 2) : strict_convex_on ℝ (set.Ioc a b) f :=
sorry

theorem exercise_4_2a {α β : Type*} [topological_space α] [compact_space α] [metric_space β] (f : C(α, β)) {s : set α} (h : ∀ (x : α), ⇑f x ∈ s) : ⇑(f.closure) '' s ⊆ closure (⇑f '' s) :=
sorry

theorem exercise_4_3 {α : Type u} {β : Type v} [topological_space α] [metric_space β] [has_zero β] {f : α → β} {Z : set α → β} (hZ : is_closed Z) (hf : continuous_on f Z) : is_closed (Z (f ⁻¹' {0})) :=
sorry

theorem exercise_4_4a {α β : Type*} [topological_space α] [metric_space β] {f g : C(α, β)} {E : set α} (hE : dense_range ⇑f) (h : set.range ⇑f ∈ closure (set.range ⇑g)) : dense_range ⇑g :=
sorry

theorem exercise_4_4b {α β : Type*} [topological_space α] [metric_space β] {f g : C(α, β)} {E : set α} (hE : dense E) (h : ∀ (p : α × α), p ∈ uniformity α → g p.fst = f p.snd) : continuous_map.extend_diam f g E = E :=
sorry

theorem exercise_4_5a {E : set ℝ} (f : ℝ → ℝ) (hE : is_closed E) (hf : ∃ (g : ℝ → ℝ), continuous_on g (set.Icc (-f 0) f)) : ∃ (g : ℝ → ℝ), continuous_on g (set.Icc (-f 0) f) ∧ ∀ (x : ℝ), g x = f x :=
sorry

theorem exercise_4_5b (E : Type*) (f : E → ℝ) (hE : ∃ (x : E), ¬∃ (r : ℝ), f x = ↑r) : ∃ (g : E → ℝ), continuous g ∧ ∀ (x : E), g x = f x :=
sorry

theorem exercise_4_6 {E : set ℝ} (f : ℝ → ℝ) (hE : is_compact E) : continuous_on f E ↔ (∀ (x : ℝ), x ∈ E → (f x, f x)) ∧ continuous_on (λ (x : ℝ), (f x, f x)) E :=
sorry

theorem exercise_4_8a (f : ℝ → ℝ) (hf : uniform_continuous_on f (set.Icc 0 1)) (h : ∀ (x : ℝ), x ∈ set.Ico 0 1 → ∀ (y : ℝ), y ∈ set.Icc 0 1 → f x ≤ f y) : uniform_continuous_on f (set.Icc 0 1) :=
sorry

theorem exercise_4_8b {𝕜 E : Type*} [normed_field 𝕜] [add_comm_group E] [module 𝕜 E] [uniform_space E] [uniform_add_group E] [has_continuous_smul 𝕜 E] (x : bernstein_approximation 𝕜 E) : ∃ (f : E → 𝕜), uniform_continuous f ∧ ¬metric.bounded (set.range f) :=
sorry

theorem exercise_5_1 (f : circle_deg1_lift) (x y : ℝ) (h : ∀ (z : ℝ), z ∈ set.Icc x y → |⇑f z - ⇑f y| ≤ (x - y) ^ 2) : ⇑f x = ⇑f y :=
sorry

theorem exercise_5_15 {a : ℝ} (ha : a ≠ 0) {f : ℝ → ℝ} (hfd : 2 ≤ᶠ[filter.at_top] f) {M₀ M₁ M₂ : ℝ} (hM₀ : filter.tendsto M₀ filter.at_top (nhds 0)) (hM₁ : filter.tendsto M₁ filter.at_top (nhds 0)) (hM₂ : filter.tendsto M₂ filter.at_top (nhds 0)) (hM₁₂ : M₁ ^ 2 ≤ 4 * M₀ * M₂) : M₁ ^ 2 ≤ 4 * M₀ * M₂ :=
sorry

theorem exercise_5_17 {f : ℝ → ℝ} (h : ∃ (x : ℝ), differentiable_within_at ℝ f (set.Iic x) x) (h' : differentiable_within_at ℝ f (set.Iic x) x) (h'' : differentiable_within_at ℝ f (set.Iic x) x) (h'h'' : ∃ (x : ℝ), differentiable_within_at ℝ f (set.Iic x) x) : ∃ (x : ℝ), fderiv_within ℝ f (set.Iic x) x ≤ 3 :=
sorry

theorem exercise_5_2 {a b : ℝ} (hab : a < b) {f' : ℝ → ℝ} (h' : 0 < f') (hf : strict_mono_on f (set.Ioo a b)) (g' : ℝ → ℝ) (hg : differentiable_on ℝ g (set.Ioo a b)) (hgf' : ∀ (x : ℝ), x ∈ set.Ioo a b → deriv_within g (set.Ioo a b) x < f') : strict_mono_on (λ (x : ℝ), (f' x)⁻¹) (set.Ioo a b) :=
sorry

theorem exercise_5_3 {𝕜 : Type u} [nondiscrete_normed_field 𝕜] (g : 𝕜 → 𝕜) (g' : 𝕜) {M : nnreal} (Mpos : 0 < M) (hM : ∃ (c : 𝕜), ∥g'∥ ≤ c * ∥g∥) {ε : 𝕜} (hε : 0 < ε) (hmg : ∀ᶠ (x : 𝕜) in nhds 0, has_deriv_at_filter g g' x) (hg' : ∀ᶠ (x : 𝕜) in nhds 0, ∥g'∥ ≤ M) : has_deriv_at_filter g g' x :=
sorry

theorem exercise_5_4 {C : ℕ → Type*} [category_theory.category C] [category_theory.preadditive C] [category_theory.limits.has_scale_roots C] (n : ℕ) : (∃ (r : ℕ), 0 < r ∧ (finset.range n).sum (λ (i : ℕ), C i) / 2 + (finset.range n).sum (λ (i : ℕ), C i) / (2 * n) + C n = 0) :=
sorry

theorem exercise_5_5 {f : ℝ → ℝ} {f' x : ℝ} (hx : 0 < x) (hf : has_deriv_at f f' x) : filter.tendsto (λ (x : ℝ), f x + 1 - f x) filter.at_top (nhds 0) :=
sorry

theorem exercise_5_6 {a b c d : ennreal} {f : c → ennreal} (h : a < b) (h' : 0 < x) (g : ℝ → ennreal) (hg : filter.tendsto g (nhds_within x (set.Ioi 0)) (nhds 0)) (h'' : monotone g) : continuous (λ (x : ennreal), f x / x) :=
sorry

theorem exercise_5_7 {x : ℝ} {l : filter ℝ} {f g : ℝ → ℝ} (hx : filter.tendsto f l (nhds x)) (hg : filter.tendsto g l (nhds x)) (h : f x = 0) : filter.tendsto (λ (t : ℝ), deriv f t / deriv g t) l (nhds x) :=
=======
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
