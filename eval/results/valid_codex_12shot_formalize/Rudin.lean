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





theorem exercise_1_1a {r : ℚ} (hr : r ≠ 0) {x : ℚ} 
  (hx : ¬is_rat x) : ¬is_rat (r + x) :=
sorry

theorem exercise_1_2 : ¬ ∃ (q : ℚ), q ^ 2 = 12 :=
sorry

theorem exercise_1_5 {α : Type*} [linear_order α] [decidable_linear_order α]
  (s : set α) (hs : s.nonempty) (hsb : s.bdd_below) :
  inf s = -sup (-s) :=
sorry

theorem exercise_1_11a {z : ℂ} :
  ∃ (r : ℝ) (w : ℂ), r ≥ 0 ∧ abs w = 1 ∧ z = r * w :=
sorry

theorem exercise_1_13 (x y : ℂ) :
  abs (abs x - abs y) ≤ abs (x - y) :=
sorry

theorem exercise_1_16a {k : ℕ} 
  (hk : k ≥ 3) (x y : ℝ^k) (hxy : ∥x - y∥ = d) (hd : d > 0) (r : ℝ) (hr : r > 0) 
  (h2r : 2*r > d) :
  ∃ (z : ℝ^k), ∥z - x∥ = r ∧ ∥z - y∥ = r ∧ ∀ (z' : ℝ^k), ∥z' - x∥ = r ∧ ∥z' - y∥ = r → z = z' :=
sorry

theorem exercise_1_18a {k : ℕ} (hk : k ≥ 2) (x : ℝ^k) :
  ∃ (y : ℝ^k), y ≠ 0 ∧ x ⬝ y = 0 :=
sorry

theorem exercise_1_19 {k : ℕ} {R : Type*} [ring R] {a b : vector R k} :
  ∃ (c : vector R k) (r : R),
  ∀ (x : vector R k),
  ∥x - a∥ = 2 * ∥x - b∥ ↔ ∥x - c∥ = r :=
sorry

theorem exercise_2_24 {X : Type*} [metric_space X]
  (h : ∀ (s : set X), infinite s → ∃ (x : X), x ∈ closure s) :
  separable X :=
sorry

theorem exercise_2_27a {E : Type*} [metric_space E]
  [fintype E] (hE : ¬ countable E) (P : set E) (hP : condensation_points P) :
  perfect P :=
sorry

theorem exercise_2_28 {X : Type*} [metric_space X] 
  [separable_space X] (A : set X) (hA : is_closed A) :
  ∃ (P : set X) (C : set X), A = P ∪ C ∧ is_perfect P ∧ countable C :=
sorry

theorem exercise_3_1a {α : Type*} [linear_ordered_field α]
  {s : ℕ → α} (hs : tendsto s at_top (𝓝 0)) :
  tendsto (λ n, abs (s n)) at_top (𝓝 0) :=
sorry

theorem exercise_3_3 
  (h : √2 < 2) : √2 < 2 :=
sorry

theorem exercise_3_6a :
  tendsto (λ n, ∑ i in finset.range n, √(i + 1) - √i) at_top at_top :=
sorry

theorem exercise_3_8 {α : Type*} [linear_ordered_semiring α]
  {f : ℕ → α} {g : ℕ → α} (hf : f.converges) (hg : g.monotone)
  (hg_bdd : g.bounded) :
  (λ n, f n * g n).converges :=
sorry

theorem exercise_3_20 {X : Type*} [metric_space X]
  {f : ℕ → X} (hf : cauchy f) {g : ℕ → ℕ} (hg : g.strict_mono)
  (h : tendsto f (nhds (f (g 0))) (nhds (f (g 0)))) :
  tendsto f (nhds (f 0)) (nhds (f 0)) :=
sorry

theorem exercise_3_22 {X : Type*} [metric_space X] [complete_space X]
  (G : ℕ → set X) (hG : ∀ n, is_open (G n) ∧ dense (G n)) :
  ∃ x, ∀ n, x ∈ G n :=
sorry

theorem exercise_4_2a {X Y : Type*} [metric_space X]
  [metric_space Y] {f : X → Y} (hf : continuous f) {E : set X} :
  closure (f '' E) ⊆ closure (f '' closure E) :=
sorry

theorem exercise_4_4a {X Y : Type*} [metric_space X] [metric_space Y]
  (f : X → Y) (g : X → Y) (hf : continuous f) (hg : continuous g)
  (hE : dense (set.range f)) : dense (set.range g) :=
sorry

theorem exercise_4_5a {E : Type*} [linear_order E]
  [topological_space E] [compact_space E] {f : E → ℝ} (hf : continuous f)
  (hE : is_closed (set.range f)) :
  ∃ (g : ℝ → ℝ), continuous g ∧ ∀ (x : E), g x = f x :=
sorry

theorem exercise_4_6 {X : Type*} [metric_space X]
  {Y : Type*} [metric_space Y] {f : X → Y} (hf : continuous f)
  (hX : compact_space X) :
  compact_space (graph f) :=
sorry

theorem exercise_4_8b {E : Type*} [metric_space E]
  [compact_space E] :
  ∃ (f : E → ℝ), uniform_continuous f ∧ ¬ metric.bounded (set.range f) :=
sorry

theorem exercise_4_12 {X Y Z : Type*}
  [metric_space X] [metric_space Y] [metric_space Z]
  (f : X → Y) (g : Y → Z) (hf : uniform_continuous f) (hg : uniform_continuous g) :
  uniform_continuous (g ∘ f) :=
sorry

theorem exercise_4_19  {f : ℝ → ℝ} (hf : ∀ a b c, a < b → f a < c → c < f b → ∃ x, a < x ∧ x < b ∧ f x = c)
  (hf_closed : ∀ r : ℚ, is_closed {x | f x = r}) : continuous f :=
sorry

theorem exercise_4_24 {f : ℝ → ℝ} (hf : continuous f)
  (h : ∀ x y : ℝ, x < y → f ((x + y) / 2) ≤ (f x + f y) / 2) :
  convex f :=
sorry

theorem exercise_5_2 {α : Type*} [linear_order α]
  [topological_space α] [metric_space α] [has_deriv_at α]
  {f : α → ℝ} (hf : strict_mono f) (hf' : ∀ x, has_deriv_at f (f' x) x)
  (hf'pos : ∀ x, 0 < f' x) :
  ∀ x, has_deriv_at (λ x, (f⁻¹ x)) (1 / f' (f⁻¹ x)) x :=
sorry

theorem exercise_5_4 {n : ℕ} (C : fin n → ℝ)
  (hC : ∑ i in finset.range n, C i / (i + 1) = 0) :
  ∃ (x : ℝ), 0 < x ∧ x < 1 ∧ ∑ i in finset.range n, C i * x ^ i = 0 :=
sorry

theorem exercise_5_6 {α : Type*} [linear_order α]
  [topological_space α] [metric_space α] [has_deriv_at α]
  {f : α → ℝ} (hf : ∀ x, 0 ≤ x → continuous_at f x)
  (hf' : ∀ x, 0 < x → has_deriv_at f (f' x) x)
  (hf0 : f 0 = 0) (hf'_mono : ∀ x y, 0 < x → 0 < y → x < y → f' x < f' y) :
  ∀ x y, 0 < x → 0 < y → x < y → f x / x < f y / y :=
sorry

theorem exercise_5_15le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_le_of_:=
sorry