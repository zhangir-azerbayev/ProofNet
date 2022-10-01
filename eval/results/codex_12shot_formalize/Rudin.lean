import tactic
import data.rat.basic
import data.real.basic
import data.real.irrational
import data.real.sqrt
import analysis.inner_product_space.basic
import analysis.inner_product_space.pi_L2
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import analysis.box_integral.basic
import data.set.intervals.basic
import topology.basic
import topology.metric_space.basic
import topology.instances.real
import dynamics.ergodic.measure_preserving

open real
open_locale topological_space
open_locale big_operators
open_locale complex_conjugate

noncomputable theory

theorem exercise_1_11a {z : ℂ} :
  ∃ (r : ℝ) (w : ℂ), r ≥ 0 ∧ abs w = 1 ∧ z = r * w :=
sorry

theorem exercise_1_12 (n : ℕ) (f : ℕ → ℂ) :
  abs (∑ i in finset.range n, f i) ≤ ∑ i in finset.range n, abs (f i) :=
sorry

theorem exercise_1_13 (x y : ℂ) :
  abs (abs x - abs y) ≤ abs (x - y) :=
sorry

theorem exercise_1_14 (z : ℂ) (h : abs z = 1) :
  abs (1 + z)^2 + abs (1 - z)^2 = 2 :=
sorry

theorem exercise_1_16a {k : ℕ} 
  (hk : k ≥ 3) (x y : ℝ^k) (hxy : ∥x - y∥ = d) (hd : d > 0) (r : ℝ) (hr : r > 0) 
  (h2r : 2*r > d) :
  ∃ (z : ℝ^k), ∥z - x∥ = r ∧ ∥z - y∥ = r ∧ ∀ (z' : ℝ^k), ∥z' - x∥ = r ∧ ∥z' - y∥ = r → z = z' :=
sorry

theorem exercise_1_17 (n : ℕ) (x y : euclidean_space ℝ (fin n)) :
  ∥x + y∥^2 + ∥x - y∥^2 = 2*∥x∥^2 + 2*∥y∥^2 :=
sorry

theorem exercise_1_18a {k : ℕ} (hk : k ≥ 2) (x : ℝ^k) :
  ∃ (y : ℝ^k), y ≠ 0 ∧ x ⬝ y = 0 :=
sorry

theorem exercise_1_18b {k : ℕ} {x y : fin k → ℝ}
  (hx : x = 0) (hy : y ≠ 0) (h : dot_product x y = 0) :
  k = 1 :=
sorry

theorem exercise_1_19 {k : ℕ} {R : Type*} [ring R] {a b : vector R k} :
  ∃ (c : vector R k) (r : R),
  ∀ (x : vector R k),
  ∥x - a∥ = 2 * ∥x - b∥ ↔ ∥x - c∥ = r :=
sorry

theorem exercise_1_1a {r : ℚ} (hr : r ≠ 0) {x : ℚ} 
  (hx : ¬is_rat x) : ¬is_rat (r + x) :=
sorry

theorem exercise_1_1b {r x : ℚ} 
  (hr : r ≠ 0) (hx : ¬ is_rat x) : ¬ is_rat (r * x) :=
sorry

theorem exercise_1_2 : ¬ ∃ (q : ℚ), q ^ 2 = 12 :=
sorry

theorem exercise_1_4 {α : Type*} [linear_order α]
  {E : set α} (hE : E.nonempty) (hα : ∀ x ∈ E, α ≤ x) (hβ : ∀ x ∈ E, x ≤ β) :
  α ≤ β :=
sorry

theorem exercise_1_5 {α : Type*} [linear_order α] [decidable_linear_order α]
  (s : set α) (hs : s.nonempty) (hsb : s.bdd_below) :
  inf s = -sup (-s) :=
sorry

theorem exercise_1_8 : ¬ ∃ (r : ℂ → ℂ → Prop), is_linear_order r :=
sorry

theorem exercise_2_19a {X : Type*} [metric_space X]
  {A B : set X} (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
  separated A B :=
sorry

theorem exercise_2_24 {X : Type*} [metric_space X]
  (h : ∀ (s : set X), infinite s → ∃ (x : X), x ∈ closure s) :
  separable_space X :=
sorry

theorem exercise_2_25 {K : Type*} [metric_space K]
  (hK : compact_space K) :
  ∃ (B : set (set K)), countable B ∧ is_topological_basis B :=
sorry

theorem exercise_2_27a {E : Type*} [metric_space E]
  [fintype E] (hE : ¬ countable E) (P : set E) (hP : condensation_points P) :
  perfect P :=
sorry

theorem exercise_2_27b {E : Type*} 
  [uncountable E] [metric_space E] {P : set E} (hP : P = condensation_points E) :
  countable (E \ P) :=
sorry

theorem exercise_2_28 {X : Type*} [metric_space X]
  (hX : separable_space X) (s : set X) (hs : is_closed s) :
  ∃ (p : set X) (c : set X), s = p ∪ c ∧ is_perfect p ∧ c.countable :=
sorry

theorem exercise_2_29 {s : set ℝ} (hs : is_open s) :
  ∃ (t : set (set ℝ)), t.finite ∧ s = ⋃₀ t ∧ ∀ (a b : ℝ), a < b → {a, b} ∈ t :=
sorry

theorem exercise_3_13 {R : Type*} [comm_ring R]
  {f g : ℕ → R} (hf : abs_converges f) (hg : abs_converges g) :
  abs_converges (cauchy_product f g) :=
sorry

theorem exercise_3_1a {α : Type*} [linear_ordered_field α]
  {s : ℕ → α} (hs : tendsto s at_top (𝓝 0)) :
  tendsto (λ n, abs (s n)) at_top (𝓝 0) :=
sorry

theorem exercise_3_20 {X : Type*} [metric_space X]
  {f : ℕ → X} (hf : cauchy f) {g : ℕ → ℕ} (hg : g.strict_mono)
  (h : tendsto f (nhds (f (g 0))) (nhds (f (g 0)))) :
  tendsto f (nhds (f 0)) (nhds (f 0)) :=
sorry

theorem exercise_3_21 {X : Type*} [metric_space X]
  [complete_space X] {E : ℕ → set X} (hE : ∀ n, is_closed (E n))
  (hE_nonempty : ∀ n, E n ≠ ∅) (hE_bounded : ∀ n, metric.bounded (E n))
  (hE_nested : ∀ n, E n ⊆ E (n + 1))
  (hE_diam_zero : tendsto (λ n, metric.diameter (E n)) at_top (𝓝 0)) :
  ∃ x, ∀ n, x ∈ E n :=
sorry

theorem exercise_3_22 {X : Type*} [metric_space X] [complete_space X]
  (G : ℕ → set X) (hG : ∀ n, is_open (G n) ∧ dense (G n)) :
  ∃ x, ∀ n, x ∈ G n :=
sorry

theorem exercise_3_2a :
  tendsto (λ n : ℕ, (n ^ 2 + n) ^ (1 / 2) - n) at_top (𝓝 (1 / 2)) :=
sorry

theorem exercise_3_3 
  (h : √2 < 2) : √2 < 2 :=
sorry

theorem exercise_3_5 {α : Type*} [linear_order α]
  {f g : ℕ → α} (hf : ∀ n, f n ≤ f (n + 1)) (hg : ∀ n, g n ≤ g (n + 1)) :
  limsup (λ n, f n + g n) ≤ limsup f + limsup g :=
sorry

theorem exercise_3_6a :
  tendsto (λ n, ∑ i in finset.range n, √(i + 1) - √i) at_top at_top :=
sorry

theorem exercise_3_7 {α : Type*} [linear_ordered_semiring α]
  (a : ℕ → α) (h : summable a) : summable (λ n, sqrt (a n) / n) :=
sorry

theorem exercise_3_8 {α : Type*} [linear_ordered_semiring α]
  {f : ℕ → α} {g : ℕ → α} (hf : f.converges) (hg : g.monotone)
  (hg_bdd : g.bounded) :
  (λ n, f n * g n).converges :=
sorry

theorem exercise_4_11a {X Y : Type*} [metric_space X]
  [metric_space Y] (f : X → Y) (hf : uniform_continuous f)
  {x : ℕ → X} (hx : cauchy x) : cauchy (λ n, f (x n)) :=
sorry

theorem exercise_4_12 {X Y Z : Type*}
  [metric_space X] [metric_space Y] [metric_space Z]
  (f : X → Y) (g : Y → Z) (hf : uniform_continuous f) (hg : uniform_continuous g) :
  uniform_continuous (g ∘ f) :=
sorry

theorem exercise_4_14 {X : Type*}
  [topological_space X] [compact_space X] (f : X → X) (hf : continuous f) :
  ∃ x : X, f x = x :=
sorry

theorem exercise_4_15 {f : ℝ → ℝ} (hf : continuous f)
  (hof : ∀ x, is_open (set.range f)) : monotone f :=
sorry

theorem exercise_4_19  {f : ℝ → ℝ} (hf : ∀ a b c, a < b → f a < c → c < f b → ∃ x, a < x ∧ x < b ∧ f x = c)
  (hf_closed : ∀ r : ℚ, is_closed {x | f x = r}) : continuous f :=
sorry

theorem exercise_4_1a {f : ℝ → ℝ}
  (hf : ∀ x : ℝ, tendsto (λ h : ℝ, f (x + h) - f (x - h)) at_top (𝓝 0)) :
  ¬ continuous f :=
sorry

theorem exercise_4_21a {X : Type*} [metric_space X]
  {K F : set X} (hK : compact_space K) (hF : is_closed F) (hKF : disjoint K F) :
  ∃ (δ : ℝ), δ > 0 ∧ ∀ (p q : X), p ∈ K → q ∈ F → dist p q ≥ δ :=
sorry

theorem exercise_4_24 {f : ℝ → ℝ} (hf : continuous f)
  (h : ∀ x y : ℝ, x < y → f ((x + y) / 2) ≤ (f x + f y) / 2) :
  convex f :=
sorry

theorem exercise_4_2a {X Y : Type*} [metric_space X]
  [metric_space Y] {f : X → Y} (hf : continuous f) {E : set X} :
  closure (f '' E) ⊆ closure (f '' closure E) :=
sorry

theorem exercise_4_3 {X : Type*} [metric_space X]
  {f : X → ℝ} (hf : continuous f) (hfz : ∀ x, f x = 0 → x ∈ set.range f) :
  is_closed (set.range f) :=
sorry

theorem exercise_4_4a {X Y : Type*} [metric_space X] [metric_space Y]
  (f : X → Y) (g : X → Y) (hf : continuous f) (hg : continuous g)
  (hE : dense (set.range f)) : dense (set.range g) :=
sorry

theorem exercise_4_4b {X Y : Type*} [metric_space X] [metric_space Y]
  (f g : X → Y) (hf : continuous f) (hg : continuous g) (E : set X)
  (hE : dense E) (h : ∀ x ∈ E, f x = g x) : ∀ x, f x = g x :=
sorry

theorem exercise_4_5a {E : Type*} [linear_order E]
  [topological_space E] [compact_space E] {f : E → ℝ} (hf : continuous f)
  (hE : is_closed (set.range f)) :
  ∃ (g : ℝ → ℝ), continuous g ∧ ∀ (x : E), g x = f x :=
sorry

theorem exercise_4_5b  (E : set ℝ) (f : E → ℝ) (hf : continuous f) :
  ∃ (g : ℝ → ℝ), continuous g ∧ ∀ (x : ℝ), x ∈ E → g x ≠ f x :=
sorry

theorem exercise_4_6 {X : Type*} [metric_space X]
  {Y : Type*} [metric_space Y] {f : X → Y} (hf : continuous f)
  (hX : compact_space X) :
  compact_space (graph f) :=
sorry

theorem exercise_4_8a {f : ℝ → ℝ} 
  (hf : uniform_continuous_on f (set.bounded_of_uniform_continuous_on f)) :
  metric.bounded (set.range f) :=
sorry

theorem exercise_4_8b {E : Type*} [metric_space E]
  [compact_space E] :
  ∃ (f : E → ℝ), uniform_continuous f ∧ ¬ metric.bounded (set.range f) :=
sorry

theorem exercise_5_1 {f : ℝ → ℝ}
  (hf : uniform_continuous f) (h : ∀ x y : ℝ, abs (f x - f y) ≤ (x - y) ^ 2) :
  ∃ c : ℝ, f = function.const ℝ c :=
sorry

theorem exercise_5_15le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_:=
sorry

theorem exercise_5_17  {f : ℝ → ℝ} (hf : three_times_differentiable_on ℝ f I)
  (hf_boundary : f (-1) = 0) (hf_boundary : f 0 = 0) (hf_boundary : f 1 = 1)
  (hf_boundary : f' 0 = 0) :
  ∃ (x : ℝ), x ∈ I ∧ f''' x ≥ 3 :=
sorry

theorem exercise_5_2 {α : Type*} [linear_order α]
  [topological_space α] [metric_space α] [has_deriv_at α]
  {f : α → ℝ} (hf : strict_mono f) (hf' : ∀ x, has_deriv_at f (f' x) x)
  (hf'pos : ∀ x, 0 < f' x) :
  ∀ x, has_deriv_at (λ x, (f⁻¹ x)) (1 / f' (f⁻¹ x)) x :=
sorry

theorem exercise_5_3 {α : Type*} [linear_ordered_field α]
  {g : α → α} (hg : ∀ x, abs (g x) ≤ 1) (ε : α) (hε : abs ε < 1 / 2) :
  function.injective (λ x, x + ε * g x) :=
sorry

theorem exercise_5_4 {n : ℕ} 
  (C : fin n → ℝ) (hC : ∑ i in finset.range n, C i / (i + 1) = 0) :
  ∃ (x : ℝ), 0 < x ∧ x < 1 ∧ ∑ i in finset.range n, C i * x ^ i = 0 :=
sorry

theorem exercise_5_5 {α : Type*} [linear_ordered_field α]
  {f : ℕ → α} (hf : tendsto f at_top (𝓝 0)) :
  tendsto (λ x, f (x + 1) - f x) at_top (𝓝 0) :=
sorry

theorem exercise_5_6 {α : Type*} [linear_order α]
  [topological_space α] [metric_space α] [has_deriv_at α]
  {f : α → ℝ} (hf : ∀ x, 0 ≤ x → continuous_at f x)
  (hf' : ∀ x, 0 < x → has_deriv_at f (f' x) x)
  (hf0 : f 0 = 0) (hf'_mono : ∀ x y, 0 < x → 0 < y → x < y → f' x < f' y) :
  ∀ x y, 0 < x → 0 < y → x < y → f x / x < f y / y :=
sorry

theorem exercise_5_7 {α : Type*} [linear_ordered_field α]
  {f g : ℝ → α} (hf : tendsto f at_top (𝓝 0)) (hg : tendsto g at_top (𝓝 0))
  (hg' : tendsto (λ x, g' x) at_top (𝓝 0))
  (hg'_ne_0 : ∀ x, g' x ≠ 0) :
  tendsto (λ x, f x / g x) at_top (𝓝 (f' 0 / g' 0)) :=
sorry