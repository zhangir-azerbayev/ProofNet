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





theorem exercise_1_1a (q : ℚ) {x : ℝ} (h : irrational x) :
	irrational (↑q + x) :=
sorry

theorem exercise_1_2 : irrational (real.sqrt 12) :=
sorry

theorem exercise_1_5 {α : Type*} [complete_linear_order α] [add_group α]
  [covariant_class α α has_add.add has_le.le]
  [covariant_class α α (function.swap has_add.add) has_le.le] {s : set α}
  (hs₀ : s.nonempty) (hs₁ : bdd_below s) :
  has_Inf.Inf (-s) = -has_Sup.Sup s :=
sorry

theorem exercise_1_11a {z : ℂ} :
  ∃ (r : ℝ) (w : ℂ), r ≥ 0 ∧ w.abs = 1 ∧ z = r * w :=
sorry

theorem exercise_1_13 {x y : ℂ} :
	|⇑complex.abs x - ⇑complex.abs y| ≤ ⇑complex.abs (x - y) :=
sorry

theorem exercise_1_16a (E : Type*)
	[seminormed_add_comm_group E] [uniform_convex_space E] {ε : ℝ}
	[normed_space ℝ E] (hε : 0 < ε) (r : ℝ) :
	∃ (δ : ℝ), 0 < δ ∧ ∀ ⦃x : E⦄, ‖x‖ ≤ r → ∀ ⦃y : E⦄, ‖y‖ ≤ r → ε ≤ ‖x - y‖ → ‖x + y‖ ≤ 2 * r - δ :=
sorry

theorem exercise_1_18a {α : Type*}
  [non_unital_non_assoc_ring α] [no_zero_divisors α] {k : ℕ} (hk : k ≥ 2)
  {x : fin k → α} (hx : x ≠ 0) :
  ∃ (y : fin k → α), y ≠ 0 ∧ x.dot y = 0 :=
sorry

theorem exercise_1_19 {R : Type*} [comm_ring R] (a b : R) :
  ∃ (c : R) (r : R), (∀ (x : R), |x - a| = 2 * |x - b| ↔ |x - c| = r) ∧
    3 * c = 4 * b - a ∧ 3 * r = 2 * |b - a| :=
sorry

theorem exercise_2_24 {α : Type u}
  [pseudo_metric_space α] [proper_space α]
  (h : ∀ (s : set α), s.infinite → ∃ (x : α), x ∈ closure s) :
  separable_space α :=
sorry

theorem exercise_2_27a {E : set ℝ^k}
  (hE : uncountable E) : perfect (condensation_points E) :=
sorry

theorem exercise_2_28 {α : Type*}
  [metric_space α] [topological_space α] {C : set α}
  [topological_space.second_countable_topology α] (hclosed : is_closed C) :
  ∃ (V D : set α), V.countable ∧ perfect D ∧ C = V ∪ D :=
sorry

theorem exercise_3_1a {α : Type*} [linear_ordered_field α]
  [topological_space α] [order_topology α] {f : ℕ → α} {l : filter α}
  (hf : tendsto f l (𝓝 0)) : tendsto (λ (n : ℕ), abs (f n)) l (𝓝 0) :=
sorry

theorem exercise_3_3 {x : ℝ} (h : x ^ 2 < 2) :
	real.sqrt x < 2 :=
sorry

theorem exercise_3_6a :
  filter.tendsto (λ (n : ℕ), (finset.range n).sum (λ (i : ℕ), (sqrt (i + 1) - sqrt i))) filter.at_top filter.at_top :=
sorry

theorem exercise_3_8 {α : Type*} [normed_ring α]
  {f g : ℕ → α} (hf : summable f) (hg : monotone g) (hg₀ : ∀ (x : ℕ), 0 ≤ g x)
  (hg₁ : ∀ (x : ℕ), g x ≤ 1) :
  summable (λ (n : ℕ), f n * g n) :=
sorry

theorem exercise_3_20 {α : Type u} {β : Type v}
  [uniform_space α] {γ : Type*} [semilattice_sup β] [semilattice_sup γ]
  [nonempty γ] {f : β → α} (hf : cauchy_seq f) {g : γ → β}
  (hg : filter.tendsto g filter.at_top filter.at_top) :
  cauchy_seq (f ∘ g) :=
sorry

theorem exercise_3_22 {α : Type*}
  [pseudo_emetric_space α] [complete_space α] :
  baire_space α :=
sorry

theorem exercise_4_2a {α β : Type*} [metric_space α]
  [metric_space β] {f : α → β} (hf : continuous f) (s : set α) :
  f '' closure s ⊆ closure (f '' s) :=
sorry

theorem exercise_4_4a {X Y : Type*} [metric_space X] [metric_space Y]
  {f : X → Y} (hf : continuous f) (hf' : dense_range f) {E : set X}
  (hE : dense E) : dense (f '' E) :=
sorry

theorem exercise_4_5a {α β : Type*} [topological_space α]
  [topological_space β] {f : α → β} (hf : closed_embedding f) :
  continuous f :=
sorry

theorem exercise_4_6 {X Y : Type*} [topological_space X]
  [topological_space Y] (hX : compact_space X) (f : X → Y) :
  continuous f ↔ compact (graph f) :=
sorry

theorem exercise_4_8b {E : set ℝ}
  (hE : metric.bounded E) :
  ∃ (f : ℝ → ℝ), uniform_continuous f ∧ ¬ metric.bounded (f '' E) :=
sorry

theorem exercise_4_12 {α β γ : Type*} [uniform_space α]
  [uniform_space β] [uniform_space γ] {g : β → γ} {f : α → β}
  (hg : uniform_continuous g) (hf : uniform_continuous f) :
  uniform_continuous (g ∘ f) :=
sorry

theorem exercise_4_19  {α : Type u} [conditionally_complete_linear_order α] [topological_space α]
  [order_topology α] [densely_ordered α] {δ : Type*} [linear_order δ]
  [topological_space δ] [order_closed_topology δ] {f : α → δ}
  (hf : continuous_on f (set.Ioo (f ⊥) (f ⊤)))
  (hf_rat : ∀ (r : ℚ), is_closed (f ⁻¹' {r})) :
  continuous f :=
sorry

theorem exercise_4_24 {f : ℝ → ℝ} (hf : continuous f)
  (h : ∀ (x y : ℝ), a < x → x < y → y < b →
    f ((x + y) / 2) ≤ (f x + f y) / 2) :
  convex_on ℝ set.univ f :=
sorry

theorem exercise_5_2 {𝕜 : Type*}
  [nontrivially_normed_field 𝕜] [cs : complete_space 𝕜] {f : 𝕜 → 𝕜} {f' a : 𝕜}
  (hf : has_strict_deriv_at f f' a) (hf' : f' ≠ 0) {g : 𝕜 → 𝕜}
  (hg : ∀ᶠ (x : 𝕜) in nhds a, g (f x) = x) :
  has_strict_deriv_at g f'⁻¹ (f a) :=
sorry

theorem exercise_5_4 {n : ℕ} (h0 : n ≠ 0)
  (h : (finset.range (n + 1)).sum (λ (m : ℕ), (-1) ^ m * ↑(n.choose m)) = 0) :
  ∃ (x : ℝ), 0 < x ∧ x < 1 ∧ (polynomial.C 0 + polynomial.C 1 * x +
  polynomial.C 2 * x ^ 2 + polynomial.C 3 * x ^ 3 + polynomial.C 4 * x ^ 4 +
  polynomial.C 5 * x ^ 5 + polynomial.C 6 * x ^ 6 + polynomial.C 7 * x ^ 7 +
  polynomial.C 8 * x ^ 8 + polynomial.C 9 * x ^ 9 + polynomial.C 10 * x ^ 10 +
  polynomial.C 11 * x ^ 11 + polynomial.C 12 * x ^ 12 + polynomial.C 13 * x ^ 13 +
  polynomial.C 14 * x ^ 14 + polynomial.C 15 * x ^ 15 + polynomial.C 16 * x ^ 16 +
  polynomial.C 17 * x ^ 17 + polynomial.C 18 * x ^ 18 + polynomial.C 19 * x ^ 19 +
  polynomial.C 20 * x ^ 20 + polynomial.C 21 * x ^ 21 + polynomial.C 22 * x ^ 22 +
  polynomial.C 23 * x ^ 23 + polynomial.C 24 * x ^ 24 + polynomial.C 25 * x ^ 25 +
  polynomial.C 26 * x ^:=
sorry

theorem exercise_5_6  {α : Type*} [linear_order α] [conditionally_complete_linear_order α]
  [topological_space α] [order_topology α] {f : α → α} (hf : continuous_at f 0)
  (hf' : deriv_at f 0) (hf'_mono : monotone (deriv_at f)) :
  monotone f :=
sorry

theorem exercise_5_15 {α : Type u} [linear_ordered_semiring α]
	{a b c : α} (h : a * c ≤ b) (hb : 0 ≤ b) (hc : 1 ≤ c) :
	a ≤ b :=
sorry