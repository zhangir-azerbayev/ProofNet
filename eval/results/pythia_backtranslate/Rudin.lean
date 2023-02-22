import .common

open real complex
open topological_space
open filter
open_locale real 
open_locale topology
open_locale big_operators
open_locale complex_conjugate
open_locale filter


noncomputable theory





theorem exercise_1_1b {x : ℝ} {r : ℚ} (hr : r ≠ 0) (h : irrational x) :
	irrational (↑r * x) :=
sorry

theorem exercise_1_4 {α β : Type*}
	[conditionally_complete_linear_order α] [conditionally_complete_linear_order β]
	{E : set α} (hE : E.nonempty) (h_le : ∀ (x : α), x ∈ E → α ≤ β)
	(h_above : ∀ (x : α), x ∈ E → β ≤ x) :
	has_Sup.Sup E ≤ β :=
sorry

theorem exercise_1_8 (K : Type*) [field K]
	[ordered_semiring K] [nontrivial K] :
	¬∃ (f : K), is_ordered_field f ∧ function.surjective f :=
sorry

theorem exercise_1_12 {z : ℂ} (h : ∀ (n : ℕ), complex.abs (z - z^[n]) ≤ complex.abs (z - z^[n + 1])) :
	|z - z^[n]| ≤ |z - z^[n + 1]| + |z - z^[n]| :=
sorry

theorem exercise_1_14 {z : ℂ}
	(hz : complex.abs z = 1) (h : z * z = 1) :
	↑(⇑complex.norm_sq z) = 1 + z.im ^ 2 + |1 - z.im| ^ 2 :=
sorry

theorem exercise_1_17 (k : ℕ) (R : Type*)
	[comm_ring R] [is_domain R] [decidable_eq R] (x y : R) :
	|x + y| ^ 2 + |x - y| ^ 2 = 2 * |x| ^ 2 + 2 * |y| ^ 2 :=
sorry

theorem exercise_1_18b {k : ℕ} {x y : fin k} :
	x * y = 0 → (∃ (h : k = 1), x = y) :=
sorry

theorem exercise_2_19a {α : Type u} [pseudo_metric_space α] {A B : set α}
	(hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
	is_separated A :=
sorry

theorem exercise_2_25 (α : Type u)
	[topological_space α] [compact_space α] :
	∃ (s : set α) (b : set (α × α)), is_compact b ∧ s ⊆ b ∧ s.nonempty ∧ b.nonempty :=
sorry

theorem exercise_2_27b : Type*} [normed_group α] [topological_space α] [order_topology α]
	{E : set α} (hE : (set.range E).uncountable) {P : set α | P ∈ nhds_within 0 (set.Ioi 0)}
	(hP : summable (λ (x : ↥P), ∥↑x∥)) :
	∃ (r : ℝ) (H : r > 0), ∀ (x : α), x ∈ P → r < ∥↑x∥ :=
sorry

theorem exercise_2_29 {s : set ℝ} (hs : is_open s)
	(h : s.nonempty) :
	∃ (t : ℝ → set ℝ), (∀ (x : ℝ), x ∈ t x → is_open (t x)) ∧ t.countable ∧ (∀ (x : ℝ), x ∈ t x → ∀ (y : ℝ), y ∈ t x → y ≠ 0 → (∃ (z : ℝ), z ∈ t x ∧ z.countable)) ∧ ∀ (x : ℝ), x ∈ s → ∀ (y : ℝ), y ∈ s → y ≠ 0 → (∃ (z : ℝ), z ∈ t x ∧ z.countable) :=
sorry

theorem exercise_3_2a (n : ℕ) :
	(real.sqrt_aux n 1 0).inv = -n + 1 / 2 :=
sorry

theorem exercise_3_5 {a b : ℕ → ennreal}
	(h : filter.tendsto a filter.at_top (nhds 0)) (h' : filter.tendsto b filter.at_top (nhds 0)) :
	(⨆ (n : ℕ), a n + b n) ≤ ⨆ (n : ℕ), a n + ⨆ (n : ℕ), b n :=
sorry

theorem exercise_3_7 {a : ℝ}
	(h : 0 ≤ a) :
	filter.tendsto (λ (n : ℕ), (finset.range n).sum (λ (i : ℕ), (a ^ i) / ↑n)) filter.at_top (nhds (a)) :=
sorry

theorem exercise_3_13 (f g : ℝ → ℝ) :
	|f * g| ≤ |f| * |g| :=
sorry

theorem exercise_3_21 {α : Type*}
	[metric_space α] {E : ℕ → set α} (hE : ∀ (n : ℕ), is_closed (E n))
	(h'E : ∀ (n : ℕ), metric.bounded (E n))
	(h_sup : filter.tendsto (λ (n : ℕ), metric.diam (E n)) filter.at_top (nhds 0))
	(h_lim : filter.tendsto (λ (n : ℕ), metric.diam (E n)) filter.at_top (nhds 0)) :
	is_closed (⋂ (n : ℕ), E n) :=
sorry

theorem exercise_4_1a {α β : Type*}
	[topological_space α] [add_group α] [topological_add_group α]
	[topological_space β] [add_group β] [topological_add_group β]
	(f : α → β) (hf : filter.tendsto f (nhds 0) (nhds 0)) (x : α) :
	¬continuous f :=
sorry

theorem exercise_4_3 {α : Type u} {β : Type v}
	[topological_space α] {p : α} [metric_space β] {f : α → β}
	(h : continuous f) (Z : set α) (hZ : 0 ∈ Z)
	(hZ' : filter.tendsto f (nhds_within 0 {0}ᶜ) (nhds 0)) :
	is_closed {x : α | f x = 0} :=
sorry

theorem exercise_4_4b {α β : Type*} [topological_space α]
	[topological_space β] [metric_space β] {P : set α} (f : C(α, β))
	(g : C(α, β)) (E : set α) (p : α) (H : p ∈ P)
	((hg : ∀ (x : α), x ∈ E → ⇑g x = ⇑f x) :
	continuous_map.extend f g E p = ⇑f p :=
sorry

theorem exercise_4_5b {α : Type u}
	[topological_space α] {β : Type*} [linear_order α] [topological_space β]
	(f : α → β) (h : ∃ (x : α), ¬∃ (y : α) (H : y ∈ set.Icc x (linear_order.max x y)), f y = f x) :
	∃ (g : α → β), continuous g ∧ ∀ (x : α), g x = f x :=
sorry

theorem exercise_4_8a : set ℝ} {f : ℝ → ℝ} (hf : metric.bounded (set.range f))
	(hfc : uniform_continuous_on f (set.Icc (-∥f∥) ∥f∥)) :
	metric.bounded (set.range f) :=
sorry

theorem exercise_4_11a {α : Type u} {β : Type v}
	[metric_space α] [nonempty β] [semilattice_sup β] {f : β → α}
	(hf : uniform_inducing f) {s : set α} (hs : is_complete s) :
	cauchy_seq_on (λ (n : β), f n) (f '' s) :=
sorry

theorem exercise_4_15 (R : Type*) [topological_space R]
	[linear_ordered_field R] [order_topology R] :
	monotone continuous_open_map :=
sorry

theorem exercise_4_21a {α : Type u}
	[metric_space α] {K F : set α} (h : disjoint K F) (hK : is_compact K)
	(hF : is_closed F) {p q : α × α} (hp : p ∈ K) (hq : q ∈ F) :
	0 < has_dist.dist p q ↔ p ∈ K ∧ q ∈ F :=
sorry

theorem exercise_5_1 {K : Type*} [is_R_or_C K] {f : K}
	{x y : K} (hf : ∀ (x : K), x = y → |f x - f y| ≤ (x - y) ^ 2)
	(hxy : x = y) :
	f = function.const K (f x) :=
sorry

theorem exercise_5_3 {g : ℝ → ℝ}
	(hg : has_deriv_at_filter g 0 (nhds 0)) {M : ℝ} (hM : ∥g'∥ ≤ M) {ε : ℝ}
	(hε : 0 < ε) (f : ℝ → ℝ)
	(hf : ∀ᶠ (x : ℝ) in nhds 0, has_deriv_at f (g x) x)
	(hL : ∀ᶠ (x : ℝ) in nhds 0, ∥g'∥ ≤ L * ∥f x∥) :
	∃ (x : ℝ) (H : x ∈ set.Icc 0 L), f x = x + ε * g x :=
sorry

theorem exercise_5_5 {f : ℝ → ℝ} {x : ℝ} (hf : ∀ (x : ℝ), x > 0 → has_deriv_at f (f x) x)
	(hx : filter.tendsto f filter.at_top (nhds 0)) :
	filter.tendsto (λ (x : ℝ), f (x + 1) - f x) filter.at_top (nhds 0) :=
sorry

theorem exercise_5_7 {𝕜 : Type u} [nondiscrete_normed_field 𝕜] {F : Type v}
	[normed_group F] [normed_space 𝕜 F] {f : 𝕜 → F} {x : 𝕜} {g : 𝕜 → F}
	(hg : deriv g x ≠ 0) (hf : filter.tendsto f (nhds x) (nhds (f x)))
	(hx : filter.tendsto g (nhds x) (nhds (f x))) :
	deriv f x⁻¹ = deriv g x⁻¹ :=
sorry

theorem exercise_5_17 : ℝ → ℝ}
	(hf : ∃ (x : ℝ) (H : x ∈ set.Icc (-1) 1), differentiable_within_at ℝ f (set.Icc (-1) 1) x)
	(hfc : ∀ (x : ℝ), x ∈ set.Icc (-1) 1 → f x = 0)
	(hgc : ∀ (x : ℝ), x ∈ set.Icc (-1) 1 → g x = 0)
	(hfd : ∀ (x : ℝ), x ∈ set.Icc (-1) 1 → f x = 1)
	(hfe : ∀ (x : ℝ), x ∈ set.Icc (-1) 1 → f x = 0) :
	0 ≤ fderiv_within ℝ f (set.Icc (-1) 1) x :=
sorry