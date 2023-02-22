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





theorem exercise_1_1b {r : ℚ} (hr : r ≠ 0) {x : ℝ} (h : irrational x) :
	irrational (↑r * x) :=
sorry

theorem exercise_1_4 {α : Type u} {β : Type v}
	[preorder α] [preorder β] {E : set α} (hE : E.nonempty)
	(hα : ∀ (a : α), a ∈ lower_bounds E → a ∈ upper_bounds E) {a : α}
	(ha : a ∈ E) :
	a ∈ lower_bounds (upper_bounds E) :=
sorry

theorem exercise_1_8 :
	¬no_order :=
sorry

theorem exercise_1_12 (z : ℂ) (n : ℕ) :
	complex.abs (z.sum n) ≤ complex.abs z + n :=
sorry

theorem exercise_1_14 (z : ℂ) (h : complex.abs z = 1) :
	complex.abs (1 + z) ^ 2 + complex.abs (1 - z) ^ 2 = 2 * (complex.abs (1 + z) * complex.abs (1 + z) + complex.abs (1 - z) * complex.abs (1 - z)) :=
sorry

theorem exercise_1_17 : Type*} [ordered_ring R] {k : ℕ} (x y : euclidean_space R k) :
	euclidean_space.add_sq x y + euclidean_space.add_sq x y = 2 * (euclidean_space.single 0 k x * euclidean_space.single 0 k y) + 2 * (euclidean_space.single 1 k x * euclidean_space.single 1 k y) :=
sorry

theorem exercise_1_18b {α : Type u} {k : ℕ}
	{R : Type*} [semiring R] [fintype k] {x : fin k → R} (h : 1 ≤ fintype.card k) :
	(∃ (y : fin k → R), y ≠ 0 ∧ x * y = 0) ↔ ¬∃ (y : fin k → R), x ≠ 0 ∧ y * y = 0 :=
sorry

theorem exercise_2_19a {X : Type*} [emetric_space X]
	{A B : set X} (h : is_metric_separated A B) :
	disjoint A B :=
sorry

theorem exercise_2_25 (α : Type u) [metric_space α]
	[topological_space.compact_space α] :
	∃ (b : set (α × α)), b.countable ∧ ∀ (k : α), k ∈ b → (∃ (r : ℝ), 0 < r ∧ metric.ball k r = set.univ) :=
sorry

theorem exercise_2_27b {α : Type*}
	[measurable_space α] {E : set (α × α)} (hE : cardinal.mk ↥E ≤ cardinal.mk α)
	(P : ↥E → Prop) (hP : ∀ (x : ↥E), (∃ (y : α × α), y ∈ E ∧ y.fst = x ∧ y.snd = x) → P y) :
	cardinal.mk ↥{x : α | ¬P x} ≤ cardinal.mk ↥E :=
sorry

theorem exercise_2_29 {α : Type*}
	[topological_space α] [linear_order α] [order_topology α] [no_max_order α]
	{m : measurable_space α} [no_min_order α] (U : set α) (hU : is_open U) :
	∃ (S : set (set α)), S.countable ∧ (∀ (s : set α), s ∈ S → s.nonempty) ∧ (⋃ (s : set α) (H : s ∈ S), s.ite (linear_order.min s) (linear_order.max s)) = U :=
sorry

theorem exercise_3_2a (n : ℕ) :
	filter.tendsto (λ (a : ℝ), real.sqrt (a * a + a)) filter.at_top (nhds (1 / 2)) :=
sorry

theorem exercise_3_5 {α : Type*} [encodable α]
	{f g : α → ennreal} (h : ∑' (i : α), f i ≠ ⊤) (h' : ∑' (i : α), g i ≠ ⊤) :
	f.limsup + g.limsup ≤ (f + g).limsup :=
sorry

theorem exercise_3_7 {a : ℕ → ℝ}
	(h : ∀ (n : ℕ), 0 ≤ a n) :
	(∃ (b : ℕ), 0 ≤ b ∧ b ≤ a ∧ b.sum (λ (n : ℕ), real.sqrt (a n) / ↑n) ≤ real.sqrt (a 0) / 0) :=
sorry

theorem exercise_3_13 {f g : ℝ} :
	summable (λ (p : ℝ × ℝ), f p.fst * g p.snd) :=
sorry

theorem exercise_3_21 {X : Type u}
	[metric_space X] [complete_space X] {n : ℕ} {E : ℕ → set X}
	(hE : ∀ (n : ℕ), is_closed (E n)) (hne : ∃ (n : ℕ), nonempty (E n))
	(h_bound : ∀ (n : ℕ), metric.bounded (E n))
	(h_lim : filter.tendsto (λ (n : ℕ), metric.diam (E n)) filter.at_top (nhds 0)) :
	(⋂ (n : ℕ), set.Ioi (E n)) = {x : X | x ∈ set.Ioi x} :=
sorry

theorem exercise_4_1a {α : Type u}
	[linear_ordered_add_comm_group α] [topological_space α] [order_topology α]
	{f : α → α} (hf : filter.tendsto (λ (x : α), f (x + h)) (nhds 0) (nhds 0)) :
	¬continuous f :=
sorry

theorem exercise_4_3 {α : Type u}
	[metric_space α] {f : α → ℝ} (hf : continuous_on f (set.univ \ {0}))
	(h'f : ∀ (x : α), f x ≠ 0) :
	is_closed {p : α | f p = 0} :=
sorry

theorem exercise_4_4b {α : Type*} [metric_space α] {s : set α}
	{E : set α} [topological_space E] {f g : α → E} {P : set α}
	(h : cont_diff_on ℝ 1 f s) (h' : ∀ (p : α), p ∈ P → g p = f p) :
	cont_diff_on ℝ 1 (function.extend g f P s) s :=
sorry

theorem exercise_4_5b {E : Type*}
	[normed_group E] [normed_space ℝ E] [complete_space E] :
	∃ (f : E → ℝ), continuous_on f (set.univ \ {x : E | f x ≠ x}) ∧ ∃ (g : E → ℝ), continuous g ∧ ∀ (x : E), g x = f x :=
sorry

theorem exercise_4_8a {E : Type*} [normed_group E]
	[normed_space ℝ E] [finite_dimensional ℝ E] {f : E → ℝ} {s : set E}
	(hs : metric.bounded s) :
	uniform_continuous_on f s :=
sorry

theorem exercise_4_11a {α : Type u} {β : Type v}
	[metric_space α] [nonempty α] [semilattice_sup β] {f : α → β}
	(hf : uniform_continuous f) (x : ℕ → α) :
	cauchy_seq (λ (n : ℕ), f (x n)) :=
sorry

theorem exercise_4_15 (R : Type*) [topological_space R]
	[monotone_class R] :
	monotone continuous_open_map.to_continuous_map :=
sorry

theorem exercise_4_21a {α : Type u} [metric_space α]
	{K F : set α} (hK : is_compact K) (hF : is_closed F) (h : disjoint K F) :
	∃ (δ : ℝ), 0 < δ ∧ ∀ (p : α), p ∈ K → ∀ (q : α), q ∈ F → has_dist.dist p q < δ :=
sorry

theorem exercise_5_1 {f : ℝ → ℝ}
	(hf : ∀ (x : ℝ), |f x - f x| ≤ (x - x) ^ 2) (x y : ℝ) :
	real.arccos_aux x = real.arccos_aux y :=
sorry

theorem exercise_5_3 {α : Type u} {β : Type v}
	[topological_space α] [linear_ordered_add_comm_group α] [order_topology α]
	[module ℝ α] [has_continuous_smul ℝ α] {g : β → α} {f : β → β} {s : set β}
	(h : ∀ (x : β), x ∈ s → has_deriv_within_at g (f x) s x) (ε : ℝ)
	(hε : 0 < ε) (hs : s ∈ nhds 0) :
	one_one_of_has_deriv_within_at f g s ε :=
sorry

theorem exercise_5_5 {𝕜 : Type*}
	[nondiscrete_normed_field 𝕜] {F : Type*} [normed_group F] [normed_space 𝕜 F]
	{f : 𝕜 → F} (hdf : ∀ᶠ (x : 𝕜) in nhds_within 0 (set.Ioi 0), differentiable_at 𝕜 f x)
	(h : filter.tendsto (λ (x : 𝕜), deriv f x) filter.at_top (nhds 0)) :
	filter.tendsto g filter.at_top (nhds 0) :=
sorry

theorem exercise_5_7 {l : filter ℕ} {f g : ℝ → ℝ}
	(hdf : ∀ᶠ (x : ℝ) in nhds 0, differentiable_at ℝ f x)
	(hg' : ∀ᶠ (x : ℝ) in nhds 0, deriv g x ≠ 0)
	(hftop : filter.tendsto f filter.at_top (nhds 0))
	(hgtop : filter.tendsto g filter.at_top (nhds 0)) :
	filter.tendsto (λ (t : ℝ), deriv f t / deriv g t) filter.at_top (nhds (f x / g x)) :=
sorry

theorem exercise_5_17 {f : ℝ → ℝ}
	(hf : real.three_deriv f ≤ 3) (h₀ : f (-1) = 0) (h₁ : f 0 = 0) (h₂ : f 1 = 1)
	(h₃ : f' 0 = 0) (h₄ : f'' 0 = 0) :
	3 ≤ f'' 0 :=
sorry