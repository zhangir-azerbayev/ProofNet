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
  ∃ (r : ℝ) (w : ℂ), | w | = 1 ∧ z = rw :=
sorry

theorem exercise_1_12 {z : ℂ} {n : ℕ} :
  |z_1 + z_2 + \ldots + z_n| ≤ |z_1| + |z_2| + \cdots + |z_n| :=
sorry

theorem exercise_1_13 {x y : ℂ} :
  abs (x - y) ≤ |x - y| :=
sorry

theorem exercise_1_14 (z : complex) :
  (|1+z|^2 + |1-z|^2) = 2 (|z|^2 - 1) :=
sorry

theorem exercise_1_16a {k : ℕ} {x y : ℝ^k} {r d : ℝ}
  (h : 2 r > d) :
  ∃ z : ℝ^k, |z - x| = r ∧ |z - y| = r :=
sorry

theorem exercise_1_17 {k : ℕ}
  [real_vector_space R k] [real_vector_space R k] [real_vector_space R k]
  (h : norm_sum (λ x, x + y) (λ x, x - y) = 2 * norm_sum x y) :
  norm_sum (λ x, x + y) (λ x, x - y) = 2 * norm_sum x y :=
sorry

theorem exercise_1_18a {k R : Type*} [field R] [vector R k]
  (h : k ≥ 2) (hx : ∃ (x : R^k), x ≠ 0) :
  ∃ (y : R^k), x · y = 0 :=
sorry

theorem exercise_1_19 {R : Type*} 
  [field R] [add_comm_group R] [module R R] [finite_dimensional R] 
  {a b : R^k} (h : finite_dimensional.finrank R^k + 1 < a.card) (h2 : a.card = b.card) :
  distance_of_two_vectors a b = distance_of_two_vectors a b.squared :=
sorry

theorem exercise_1_1a {r x : ℚ} (h : r ≠ 0) : r + x ≠ r := sorry 

Natural language version: "If $x$ is irrational, prove that $x$ is not a rational number." Translate the natural language version to a Lean mathlib version:
theorem irrational_not_rational {x : ℚ} (h : x ≠ 0) : x ∉ ℚ

Natural language version: "If $x$ is irrational, prove that $x$ is not a rational number." Translate the natural language version to a Lean mathlib version:
theorem irrational_not_rational {x : ℚ} (h : x ≠ 0) : x ∉ ℚ

Natural language version: "If $x$ is irrational, prove that $x$ is not a rational number." Translate the natural language version to a Lean mathlib version:
theorem irrational_not_rational {x : ℚ} (h : x ≠ 0) : x ∉ ℚ

Natural language version: "If $x$ is irrational, prove that $x$ is not a rational number." Translate the natural language version to a Lean mathlib version:
theorem irrational_not_rational {x : ℚ} (h : x ≠ 0) : x ∉ ℚ

Natural language version: "If $x$ is irrational, prove that $x$ is not a rational number." Translate the natural language version to a Lean mathlib version:
theorem irrational_not_rational {x : ℚ} (h : x ≠ 0) : x ∉ ℚ

Natural language version: "If $x$ is irrational, prove that $x$ is not a rational number." Translate:=
sorry

theorem exercise_1_1b {r : ℚ} {x : ℚ} (hx : x ≠ 0) :
  irrational (r * x) :=
sorry

theorem exercise_1_2 {R : Type*} [rational R] :
  ∃ (x : R), x ^ 2 = 12 :=
sorry

theorem exercise_1_4 {E : Type*} [ordered E] [lower_bound E] [upper_bound E]
  (h : E.has_le_lt_le_gt) (hα : lower_bound E) (hβ : upper_bound E) :
  α ≤ β :=
sorry

theorem exercise_1_5 {A : set real} :
  inf A = -sup (-A) :=
sorry

theorem exercise_1_8 (C : Type*) [field C] :
  no_order C :=
sorry

theorem exercise_2_19a {X : Type*} [metric_space X] 
  (hX : closed_space X) (hA : closed_space A) (hB : closed_space B)
  (hA.disjoint_from hB) :
  closed_space (A ∪ B) :=
sorry

theorem exercise_2_24 {X : Type*} [metric_space X] :
  ∃ (x : X), ∀ (A : ℕ → X) (n : ℕ), A n → x ∈ A n → A n → x ∈ A (n + 1) → A (n + 1) → x ∈ A (n + 2) → ⋯ → x ∈ A (n + n) → x ∈ A (n + n + 1) → ⋯
  :=
sorry

theorem exercise_2_25 {K : Type*} [metric_space K] :
  countable base (compact_space K) :=
sorry

theorem exercise_2_27a {k E : ℕ} [field E] [field E.subfield ℤ]
  [uncountable E] (h : uncountable E) (hP : perfect E) :
  perfect E :=
sorry

theorem exercise_2_27b {E : Type*} 
  [uncountable E] [cardinal E] [cardinal P] (h : uncountable E →* P)
  (hP : card P ≤ card E) :
  card (E ∖ P) ≤ card E :=
sorry

theorem exercise_2_28 {X : Type*} [metric_space X]
  (hX : compact_space X) (h : closed_set X)
  (hps : perfect_set X) (hcs : countable_set X) :
  closed_set X :=
sorry

theorem exercise_2_29 {U : Type*} [open U] [at_most_countable U]
  (h : open U) :
  open U :=
sorry

theorem exercise_3_13  {a b : ℝ} [field ℝ] [series a] [series b] [series_absolutely_convergent a] [series_absolutely_convergent b]
  (h : series_absolutely_convergent a) (hb : series_absolutely_convergent b)
  (h : series_cauchy a) (hb : series_cauchy b)
  (h : series_cauchy (a ⊗ b)) : series_cauchy (a ⊗ b) :=
sorry

theorem exercise_3_1a {s : ℝ} (h : s.is_finite) :
  ∀ n : ℕ, ∃ m : ℕ, ∀ k : ℕ, k < m → s.abs (s.abs n) < s.abs (s.abs k) :=
sorry

theorem exercise_3_20 {X : Type*} [metric_space X]
  [metric_space Y] (p : X) (h : cauchy p) (hp : p = p) :
  cauchy p → p = p :=
sorry

theorem exercise_3_22 {X : Type*} [metric_space X] [complete_space X]
  (hX : nonempty_complete_space X) (hG : dense_open_sets G)
  (h : ∀ n, ∃ x, x ∈ G ⧸ x ∈ hG) :
  ∃ x, x ∈ ∩ G ⧸ x ∈ hG :=
sorry

theorem exercise_3_2asqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n_limit_n_sqrt_n:=
sorry

theorem exercise_3_3 {s : ℕ} (h : s = 2) :
  ∀ n : ℕ, s.sqrt_2 = s.sqrt_2 + s.sqrt_2
  :=
sorry

theorem exercise_3_5 {A B : ℕ} (h : A < B) :
  ∀ (a : ℕ), ∃ (b : ℕ), a + b = ∃ (c : ℕ), a + b = ∃ (d : ℕ), b + d = ∃ (e : ℕ), a + b + d = ∃ (f : ℕ), a + b + d = ∃ (g : ℕ), c + g = ∃ (h : ℕ), a + b + c + g = ∃ (i : ℕ), f + i = ∃ (j : ℕ), a + b + c + f + i = ∃ (k : ℕ), e + k = ∃ (l : ℕ), a + b + c + e + k = ∃ (m : ℕ), b + m = ∃ (n : ℕ), a + b + c + e + k + m = ∃ (o : ℕ), d + o = ∃ (p : ℕ), a + b + c + e + k + m + d + o = ∃ (q : ℕ), c + q = ∃ (r : ℕ), a + b + c + e + k + m + d + o + q = ∃ (s : ℕ), f + s = ∃ (t : ℕ), a + b + c + e + k + m + d + o + q + f + s = ∃ (u : ℕ), e + u = ∃ (v : ℕ), a + b + c + e + k + m + d + o + q + f:=
sorry

theorem exercise_3_6a {n : ℕ} (a : ℝ) :
  ∀ n, ∃ i, i < n → a + (sqrt i + sqrt (i + 1)) = ∞ :=
sorry


theorem exercise_3_8 {a : ℕ} {b : ℕ} {c : ℕ}
  (h : ∀ n, a < b → c < a + b)
  (hb : ∀ n, b < a + b)
  (hc : ∀ n, c < a + b)
  (h : ∀ n, a + b < c) :
  ∃ n, a + b = c :=
sorry

theorem exercise_4_11a {X Y : Type*} [metric_space X] [metric_space Y]
  (hX : compact_space X) (f : X → Y) (hf : continuous f)
  (h : uniform_continuous f)
  (hX : cauchy_seq (f.domain X)) : cauchy_seq f.image :=
sorry

theorem exercise_4_12 {X Y Z : Type*} 
  [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_4_14 {I : Type*} [continuous I]
  (f : I → I) (h : continuous f) :
  ∃ x, f x = x :=
sorry

theorem exercise_4_15 {R : Type*} [continuous_open_mapping R]
  (f : R → R) (h : continuous f) : monotonic f :=
sorry

theorem exercise_4_19 {R : Type*} 
  [continuous_function R] [continuous_function_domain R] [continuous_function_codomain R]
  (f : R → R) (r : R) (hf : continuous f) (h : closed_set_of_f_r r)
  (h : ∀ (x : R), f x = r → x = f (f x)) : continuous f :=
sorry

theorem exercise_4_1a  {f : ℝ → ℝ} (h : limit_difference f (limit_difference f 0) = 0) :
  not_continuous f :=
sorry

theorem exercise_4_21a  {X K F : Type*} [metric_space X] [metric_space K] [metric_space F]
  (hK : compact_space K) (hF : closed_set F) (h : d K F <> 0)
  (h : d K F <> 0) :
  d K F <> 0 :=
sorry

theorem exercise_4_24 {a b : ℝ} [continuous f : (a, b) → ℝ]
  (h : f (⌊x, y⌋) ≤ ⌊f x, f y⌋) : f (⌊x, y⌋) ≤ ⌊f x, f y⌋ :=
sorry

theorem exercise_4_2a {X Y : Type*} [metric_space X] [metric_space Y]
  (hX : compact_space X) (f : X → Y) (hf : continuous f)
  (h : f (closure X) ⊆ closure (f (closure X))) :
  hf (closure X) ⊆ closure (f (closure X)) :=
sorry

theorem exercise_4_3 {X : Type*} [metric_space X]
  (hX : compact_space X) (f : X → ℝ) (hZ : closed f.zero_set) : hZ ⊆ X :=
sorry

theorem exercise_4_4a {X Y : Type*} [metric_space X] [metric_space Y]
  (hX : dense_set X) (hY : dense_set Y) (f : X → Y)
  (hf : function.continuous f)
  (h : dense_image f.image hX) : dense_image f.image hY :=
sorry

theorem exercise_4_4bcontinuous_mapping_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_dense_set_of_d:=
sorry

theorem exercise_4_5a {E : Type*} [continuous E]
  [continuous_function E] (f : E →* ℝ) (hf : continuous f) :
  continuous (λ x, f x) :=
sorry

theorem exercise_4_5b  {E : Type*} [continuous E] [continuous E]
  (f : E →* ℝ) (hf : continuous f)
  (h : ∀ x, f x = f (x.real))
  (hE : ∀ x, x ∈ E → f x = f (x.real))
  (h : ∀ x, f x = f (x.real)) :
  continuous f :=
sorry

theorem exercise_4_6 {E : Type*} [continuous E] [compact E]
  (h : continuous E) (hE : graph E ⊆ E × E) :
  continuous E ↔ graph E is compact :=
sorry

theorem exercise_4_8a {E : Type*} 
  [bounded_set E] [uniform_continuous E] [bounded E]
  (f : E → R) (hf : bounded f) : bounded f :=
sorry

theorem exercise_4_8b {E : ℝ} [bounded_set E] :
  unbounded_continuous (λ x, x) ∧ unbounded_continuous (λ x, x) → unbounded_continuous (λ x, x)
  :=
sorry

theorem exercise_5_1  {X : Type*} [metric_space X] [continuous X] [continuous_injective X]
  [continuous_uniform_continuous X] [uniform_continuous X]
  (h : continuous_injective (continuous_uniform_continuous X))
  (hf : function.injective (continuous_uniform_continuous X))
  (hg : function.continuous (continuous_uniform_continuous X))
  (h : constant (continuous_uniform_continuous X)) : constant (continuous_uniform_continuous X) :=
sorry

theorem exercise_5_2 {a b : ℝ}
  [finite_interval a b] [finite_interval_derivative a b]
  (f : a → b) (hf : f.derivative > 0) :
  strictly_increasing f :=
sorry

theorem exercise_5_3 {g : R → R} [bounded_derivative g]
  (ε : ℝ) (M : ℝ) (h : ∀ x, |g (x)| ≤ M) :
  ∃ ε′, ε′ < ε → ∃ ε′′, ε′′ < ε′ → ∃ ε′′′, ε′′′ < ε′′ → ∃ ε′′′′, ε′′′′ < ε′′′ → ∃ ε′′′′′, ε′′′′′ < ε′′′′ → ∃ ε′′′′′′, ε′′′′′′ < ε′′′′′ → ∃ ε′′′′′′′, ε′′′′′′′ < ε′′′′′′ → ∃ ε′′′′′′′′, ε′′′′′′′′ < ε′′′′′′′ → ∃ ε′′′′′′′′′, ε′′′′′′′′′ < ε′′′′′′′′ → ∃ ε′′′′′′′′′′, ε′′′′′′′′′′′ < ε′′′′′′′′′′ → ∃ ε′′′′′′′′′′′′′, ε′′′′′′′′′′′′′′′ < ε′′′′′′′′′′′′′ → ∃ ε′′′′′′′′′′′′′:=
sorry

theorem exercise_5_4 {C : ℝ} [real C] [real_roots_of_unity C]
  (h : C.zero + C.one / 2 + ⋯ + C.one / (n + 1) = 0)
  (h0 : C.zero = 0)
  (h1 : C.one = 1)
  (h2 : C.one / 2 = 0)
  (h3 : C.one / (n + 1) = 0)
  (h : real_roots_of_unity.root_of_sum_of_powers_of_x_between_0_and_1 h0 h1 h2 h3) :
  real_roots_of_unity.root_of_sum_of_powers_of_x_between_0_and_1 h0 h1 h2 h3

theorem exercise_5_5 {f : ℕ → ℝ} [continuous f] (h : ∀ x, f x → 0) :
  lim x→∞ f x = 0 :=
sorry

theorem exercise_5_6 {X : Type*} [continuous X]
  [continuous_for_x_geq_0 X] (f : X → ℝ) (hf : f.is_continuous)
  (h : monotonically_increasing (f.derivative)) : monotonically_increasing f :=
sorry

theorem exercise_5_7 {X Y Z : Type*} 
  [metric_space X] [metric_space Y] [metric_space Z]
  (hX : compact_space X) (f : X → Y) (g : Y → Z) (hg : continuous g)
  (h : function.injective g)
  (hf : function.zero f)
  (hgf : function.zero g)
  (h : limit_of_ratio_of_zero f g) : limit_of_ratio_of_zero f g :=
sorry