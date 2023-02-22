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





theorem exercise_1_1b {x : ℝ} (h : irrational x) {q : ℚ} (hq : q ≠ 0) :
	irrational (↑q * x) :=
sorry

theorem exercise_1_4 {α : Type u} [linear_order α]
  [densely_ordered α] {a₁ a₂ : α} (h : ∀ (a₃ : α), a₃ < a₁ → a₃ ≤ a₂) :
  a₁ ≤ a₂ :=
sorry

theorem exercise_1_8 : ¬(∃ (r : ℂ → ℂ → Prop), is_linear_order ℂ r) :=
sorry

theorem exercise_1_12 {ι : Type*} (s : finset ι) (f : ι → ℤ) :
	(s.sum (λ (i : ι), f i)).nat_abs ≤ s.sum (λ (i : ι), (f i).nat_abs) :=
sorry

theorem exercise_1_14 {z : ℂ} (h : z.abs = 1) :
  (1 + z).abs ^ 2 + (1 - z).abs ^ 2 = 4 :=
sorry

theorem exercise_1_17 {α : Type u}
  [normed_group α] [normed_space ℝ α] (x y : α) :
  (norm (x + y))^2 + (norm (x - y))^2 = 2 * (norm x)^2 + 2 * (norm y)^2 :=
sorry

theorem exercise_1_18b {α : Type*} [semiring α] {x y : α}
  (hx : x ≠ 0) (hxy : y ≠ 0) (hxyz : x * y = 0) :
  false :=
sorry

theorem exercise_2_19a {X : Type*} [emetric_space X]
  {A B : set X} (hA : is_closed A) (hB : is_closed B) (h : disjoint A B) :
  is_metric_separated A B :=
sorry

theorem exercise_2_25 {α : Type u} [metric_space α]
  (hK : is_compact K) :
  has_countable_basis (nhds_set K) :=
sorry

theorem exercise_2_27b {α : Type u}
  [pseudo_emetric_space α] (E : set α) (hE : E.uncountable) :
  (E \ condensation_points E).countable :=
sorry

theorem exercise_2_29 {α : Type*}
  [topological_space α] [t2_space α] {s : set α} (hs : is_open s) :
  ∃ (U : α → set α), (∀ (x : α), x ∈ U x ∧ is_open (U x)) ∧ s.pairwise_disjoint U :=
sorry

theorem exercise_3_2a :
	filter.tendsto (λ (n : ℕ), nat.sqrt (n ^ 2 + n) - n) filter.at_top (nhds (1 / 2)) :=
sorry

theorem exercise_3_5 {α : Type*} [linear_ordered_field α]
  {f g : cau_seq α has_abs.abs} (hf : f.lim_zero) (hg : g.lim_zero) :
  (f ⊔ g).lim_zero :=
sorry

theorem exercise_3_7 {α : Type*} [non_unital_non_assoc_semiring α]
  [topological_space α] [topological_semiring α] {f : ℕ → α} (hf : summable f)
  (hf0 : ∀ n, 0 ≤ f n) :
  summable (λ n, (f n)^(1/2) / n) :=
sorry

theorem exercise_3_13 {α : Type*} [normed_ring α]
  {f g : ℕ → α} (hf : summable (λ (x : ℕ), ‖f x‖))
  (hg : summable (λ (x : ℕ), ‖g x‖)) :
  summable (λ (n : ℕ), ‖(finset.range (n + 1)).sum (λ (k : ℕ), f k * g (n - k))‖) :=
sorry

theorem exercise_3_21 {X : Type*}
  [metric_space X] {E : ℕ → set X} (hE : ∀ (n : ℕ), is_closed (E n))
  (hEb : ∀ (n : ℕ), nonempty (E n)) (hEbnd : ∀ (n : ℕ), bounded_of_nonempty (E n))
  (hEn : ∀ (n : ℕ), E n ⊆ E (n + 1)) (hEd : lim_diam_zero E) :
  ∃! (x : X), ∀ (n : ℕ), x ∈ E n :=
sorry

theorem exercise_4_1a {f : ℝ → ℝ}
  (hf : ∀ (x : ℝ), tendsto (λ (h : ℝ), f (x + h) - f (x - h)) (nhds 0) (nhds 0))
  (hfnn : ∀ (x : ℝ), 0 ≤ f x) :
  ¬continuous f :=
sorry

theorem exercise_4_3 {X : Type*} [metric_space X] {f : X → ℝ}
  (hf : continuous f) : is_closed {x : X | f x = 0} :=
sorry

theorem exercise_4_4b {X Y : Type*} [metric_space X]
  [metric_space Y] {f g : X → Y} (hf : continuous f) (hg : continuous g)
  (E : set X) (hE : dense E) (h : ∀ (p : E), f p = g p) :
  ∀ (p : X), f p = g p :=
sorry

theorem exercise_4_5b  {E : set ℝ} (hE : ∃ (f : E → ℝ), continuous f) :
  ∃ (f : E → ℝ), continuous f ∧ ∀ (g : ℝ → ℝ), continuous g → ∃ (x : ℝ), x ∈ E ∧ g x ≠ f x :=
sorry

theorem exercise_4_8a {α : Type u} {β : Type v}
  [topological_space α] [metric_space β] (f : α → β) (E : set α)
  (hf : uniform_continuous_on f E) (hE : metric.bounded E) :
  metric.bounded (f '' E) :=
sorry

theorem exercise_4_11a {α : Type u} {β : Type v}
  [uniform_space α] {γ : Type*} [uniform_space β] [semilattice_sup γ] {f : α → β}
  (hf : uniform_continuous f) {u : γ → α} (hu : cauchy_seq u) :
  cauchy_seq (f ∘ u) :=
sorry

theorem exercise_4_15 {R : Type*}
  [topological_space R] [division_ring R] [has_continuous_sub R]
  [has_continuous_add R] [has_continuous_smul R] (f : R → R)
  (hf : is_open_map f) (hfc : continuous f) : monotone f :=
sorry

theorem exercise_4_21a {α : Type u} [pseudo_emetric_space α]
  {s t : set α} (hst : disjoint s t) (hs : is_compact s) (ht : is_closed t) :
  ∃ (δ : ℝ), 0 < δ ∧ disjoint (metric.thickening δ s) (metric.thickening δ t) :=
sorry

theorem exercise_5_1 {f : ℝ → ℝ} (hf : differentiable ℝ f)
  (h : ∀ (x y : ℝ), x ≤ y → abs (f y - f x) ≤ (y - x) ^ 2) :
  is_constant f :=
sorry

theorem exercise_5_3 {α : Type v} {β : Type w} [small β] {f : α → β}
  (hf : function.injective f) :
  small α :=
sorry

theorem exercise_5_5 {α β : Type*}
  [ordered_add_comm_monoid β] {l : filter α} {f g : α → β}
  (hf : filter.tendsto f l filter.at_top) (hg : ∀ (x : α), 0 ≤ g x) :
  filter.tendsto (λ (x : α), f x + g x) l filter.at_top :=
sorry

theorem exercise_5_7 {α 𝕜 : Type*} [normed_field 𝕜]
  [topological_space α] [topological_space 𝕜] [has_deriv_at 𝕜 α]
  {f g : α → 𝕜} {x : α} (hf : deriv_at f x = 0) (hg : deriv_at g x = 0)
  (hgx : g x ≠ 0) :
  filter.tendsto (λ (t : α), f t / g t) (nhds x) (nhds 0) :=
sorry

theorem exercise_5_17 {x y : ℝ} {f : ℝ → ℝ}
  (hf : continuous_on f (set.Icc x y)) (hxy : x < y)
  (hf'_mono : strict_mono_on (deriv f) (set.Ioo x y))
  (h : ∀ (w : ℝ), w ∈ set.Ioo x y → deriv f w ≠ 0) :
  ∃ (a : ℝ) (H : a ∈ set.Ioo x y), deriv f a < (f y - f x) / (y - x) :=
sorry