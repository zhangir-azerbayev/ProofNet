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





theorem exercise_1_1birrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_irrational_mul_ir:=
sorry

theorem exercise_1_4 {E : set (set α)}
  (hE : nonempty E) (h : ∀ x ∈ E, x ≤ x) :
  ∀ x ∈ E, x ≤ x :=
sorry

theorem exercise_1_8  {K : Type*} [field K] [add_comm_group K] [module K K] [finite_dimensional K K] :
  ∀ (x : K), ¬order_on K x
| 0 :=
sorry

theorem exercise_1_12complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_complex_add_complex_mul_:=
sorry

theorem exercise_1_14  {z : ℂ} (hz : z ∈ ℂ) :
  (norm z.squared + norm z.squared - 2 * norm z.squared) = 1 :=
sorry

theorem exercise_1_17 {k : Type*} [field k]
  {V : Type*} [add_comm_group V] [module k V] [finite_dimensional k V]
  {x y : V} (hx : x ∈ k) (hy : y ∈ k) :
  (x + y) * (x - y) = 2 * x * y :=
sorry

theorem exercise_1_18b {k : ℕ} (h : k ≠ 0) :
  ∀ {R : Type*} [ring R] [add_comm_group R] [module R R] [is_scalar_tower R k]
  (x : R^k), x ≠ 0 → x ≠ 0 → x ≠ 0 :=
sorry

theorem exercise_2_19a {X : Type*} [metric_space X] {A B : set X}
  (hA : A.nonempty) (hB : B.nonempty) :
  (A ∩ B).nonempty → (A.nonempty ∩ B.nonempty) :=
sorry

theorem exercise_2_25 {K : Type*} [metric_space K] [compact_space K] :
  ∃ (b : ℕ) (b_lt : b.nat_abs.lt.one),
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.ball x) ∧
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.ball x) ∧
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.ball x) ∧
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.ball x) ∧
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.ball x) ∧
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.ball x) ∧
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.ball x) ∧
  (∀ (x : K), ∃ (y : K), y ∈ b.nat_abs.ball x ∧ y ∈ b.nat_abs.:=
sorry

theorem exercise_2_27b {E : Type*} [metric_space E]
  (hE : uncountable E) :
  (∀ x ∈ E, ∃ y ∈ E, y ≠ x ∧ y ∈ P) → uncountable E :=
sorry

theorem exercise_2_29 {X : Type*} [topological_space X]
  {Y : Type*} [topological_space Y] [topological_space Z]
  (hY : topological_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : ∀ (s : set X), s.countable → ∃ (s' : set Y), s = g ⁻¹' s') :
  ∃ (s : set X) (s' : set Y), s.countable ∧ s.disjoint ∧ s' = g ⁻¹' s' :=
sorry

theorem exercise_3_2a : lim_n_sqrt_n - n = 1/2 :=
sorry

theorem exercise_3_5 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : limsup (λ n, f n + g n) ≤ limsup (λ n, f n) + limsup (λ n, g n)) :
  limsup (λ n, f n + g n) ≤ limsup (λ n, f n) + limsup (λ n, g n) :=
sorry

theorem exercise_3_7sum_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_nontrivial_div_:=
sorry

theorem exercise_3_13 {α : Type*}
  {s : series α} {f : α → series α} {g : α → series α}
  (h : ∀ᶠ n in 𝓝 (0 : series α), ∃ N, ∀ m ≥ N, ∥s.sum m - s.sum n∥ < ∥s.sum m∥) :
  cauchy_product s f g :=
sorry

theorem exercise_3_21 {X : Type*} [complete_space X] {x : X}
  (hx : x ∈ closure {x}) :
  ∃! y, y ∈ closure {x} ∧ y ∈ {x} :=
sorry

theorem exercise_4_1a {f : ℝ → ℝ}
  (hf : ∀ x, f x ≠ 0 → ∃ y, f y = 0) :
  ¬continuous_at_zero f :=
sorry

theorem exercise_4_3 {X : Type*} [metric_space X]
  {f : X → ℝ} (hf : continuous f) : closed (f '' {0}) :=
sorry

theorem exercise_4_4b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : dense_image (g ∘ f)) : dense_image f :=
sorry

theorem exercise_4_5b {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : not_exists_continuous_real_function_on_compact_space f g hgc h) :
  not_exists_continuous_real_function_on_compact_space f g hgc h :=
sorry

theorem exercise_4_8a {E : Type*} [metric_space E] [bounded_space E]
  (f : E → R) (hf : ∀ x, ∥f x∥ ≤ M) :
  bounded_on f E ↔ bounded_image f E :=
sorry

theorem exercise_4_11a {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : compact_space Y) (f : X → Y) (g : Y → ℝ) (hgc : continuous g)
  (hgi : function.injective g)
  (h : Cauchy_seq (g ∘ f)) : Cauchy_seq f :=
sorry

theorem exercise_4_15 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : open_map f) : open_map f :=
sorry

theorem exercise_4_21a {X K F : Type*} [metric_space X] [metric_space K] [metric_space F]
  (hK : compact K) (hF : closed F) (hX : X) (hF_compact : F.compact) :
  ∃ (δ : ℝ) (δ_pos : 0 < δ), ∀ x ∈ K, ∃ y ∈ F, d(x, y) < δ ∧ d(x, y) < δ_pos :=
sorry

theorem exercise_5_1 {f : ℝ → ℝ} {a b : ℝ}
  (h : ∀ x ∈ (a, b), f x ∈ (a, b)) :
  continuous_on f (a, b) ↔ continuous_on f (a, b) :=
sorry

theorem exercise_5_3  {R : Type*} [metric_space R] {f : R → R} {f' : R → R} {M : ℝ} {x : R}
  (hM : ∀ x, ∥f' x∥ ≤ M) (hx : x ∈ Icc (0 : ℝ)) :
  1 ≤ ∥f x∥ ≤ M * ∥x∥ + ∥f 0∥ :=
sorry

theorem exercise_5_5 {f : ℝ → ℝ} {x : ℝ} (hx : tendsto f x at_top (𝓝 0)) :
  tendsto (λ x, f x) at_top (𝓝 0) :=
sorry

theorem exercise_5_7  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : differentiable_on g)
  (hgi : differentiable_on (g ∘ f)) :
  differentiable_on f ↔ differentiable_on g :=
sorry

theorem exercise_5_17 :
  ∀ x ∈ (-1,1), f3 x > 0

Natural language version: "Suppose $f$ is a real, three times differentiable function on $[-1,1]$, such that $f(-1)=0, \quad f(0)=0, \quad f(1)=1, \quad f^{\prime}(0)=0.$ Prove that $f^{(3)}(x) \geq 3$ for some $x \in(-1,1)$." Translate the natural language version to a Lean mathlib version:
theorem f3_pos_on_interior_of_f3_pos_on_boundary :
  ∀ x ∈ (-1,1), f3 x > 0

Natural language version: "Suppose $f$ is a real, three times differentiable function on $[-1,1]$, such that $f(-1)=0, \quad f(0)=0, \quad f(1)=1, \quad f^{\prime}(0)=0.$ Prove that $f^{(3)}(x) \geq 3$ for some $x \in(-1,1)$." Translate the natural language version to a Lean mathlib version:
theorem f3_pos_on_interior_of_f3_pos_on_boundary :
  ∀ x ∈ (-1,1), f3 x > 0

Natural language version: "Suppose $f$ is a real, three times differentiable function on $[-1,1]$, such that $f(-1)=0, \quad f(0)=0, \quad f(1)=1, \quad f^{\prime}(0)=0.$ Prove that $f^{(3)}(x) \geq 3$ for some:=
sorry