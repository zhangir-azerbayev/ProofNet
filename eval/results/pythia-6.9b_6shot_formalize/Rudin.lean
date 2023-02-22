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





theorem exercise_1_1b {r : ℕ} (x : r) (y : r) :
  irrational x * irrational y = irrational (x * y) :=
sorry

theorem exercise_1_4  {E : Type*} [ordered_set E] [ordered_set E]
  (α : lower_bound E) (β : upper_bound E) (h : α ≤ β) :
  α ≤ β :=
sorry

theorem exercise_1_8  {C : Type*} [field C] [field C] [field C] [field C] [field C] [field C]
  (h : complex_field C) :
  no_order C :=
sorry

theorem exercise_1_12  {z_1 z_2 : ℂ} [complex_number z_1] [complex_number z_2]
  (h : |z_1| + |z_2| ≤ |z_1| + |z_2|) : |z_1 + z_2| ≤ |z_1| + |z_2| :=
sorry

theorem exercise_1_14  {z : ℂ} (h : z = 1) :
  (1 + z) ^ 2 + (1 - z) ^ 2 = 1 + 2 * z + z ^ 2 :=
sorry

theorem exercise_1_17 {R : Type*} [ring R]
  [field R] [finite_dimensional R] [finite_dimensional R]
  (x : R) (y : R) (h : norm_square x + norm_square y = norm_square (x + y)) :
  norm_square x = norm_square y :=
sorry

theorem exercise_1_18b {R : Type*} [ring R] [field R]
  (x : R) (y : R) (h : x * y = 0) :
  ∀ (z : R), x * z ≠ 0 :=
sorry

theorem exercise_2_19a {X : Type*} [metric_space X]
  [metric_space X] (A : closed_set X) (B : closed_set X) :
  disjoint_closed_sets X A B :=
sorry

theorem exercise_2_25 {K : Type*} [metric_space K]
  [compact_space K] [finite_dimensional K] [finite_dimensional K]
  (h : finite_dimensional.finrank K + 1 < ∞) :
  ∃ (f : K →*), finite_dimensional.finrank K + 1 < ∞ :=
sorry

theorem exercise_2_27b {E : Type*} [metric_space E]
  [metric_space_set E] (P : set.of_points E) (n : ℕ) (h : ∃ x : E, x ∉ P) :
  ∃ x : E, x ∉ P :=
sorry

theorem exercise_2_29 {R : Type*} [field R] [field R]
  (h : open_set R) : open_set R :=
sorry

theorem exercise_3_2a {n : ℕ} :
  ∃ (x : ℝ), √(n^2 + n) - n = 1/2 * x :=
sorry

theorem exercise_3_5limsup_sum_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_of_limsup_:=
sorry

theorem exercise_3_7 {A : Type*} [field A] 
  [add_comm_group A] [module A A] [finite_dimensional A] 
  (a : A) (h : ∀ n : ℕ, a ^ n ≥ 0) :
  ∃ (f : A → ℝ), ∑ (λ (n : ℕ), a ^ n) = ∑ (λ (n : ℕ), √ a ^ n) :=
sorry

theorem exercise_3_13 {X Y : Type*} [metric_space X] [metric_space Y]
  (hX : Cauchy_product X Y) (hY : Cauchy_product Y X) (h : Cauchy_product.absolutely_convergent hX hY)
  (h : Cauchy_product.absolutely_convergent hY hX) : Cauchy_product.absolutely_convergent hX :=
sorry

theorem exercise_3_21 {X : Type*} [metric_space X]
  [complete_metric_space X] (E : closed_bounded_sets_in_complete_metric_space X)
  (E_n : E ⊆ E_{n+1}) (E_n_inf : ∀ n, ∀ x : X, x ∈ E_n → x ∈ E) :
  ∃ x : X, ∃ n : ℕ, x ∈ E_n :=
sorry

theorem exercise_4_1a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : not_continuous f :=
sorry

theorem exercise_4_3 {X : Type*} [metric_space X]
  [metric_space X] (f : X → ℝ) (g : continuous f) (h : continuous g) :
  closed (Z(f) ∪ {x : X | f(x) = 0}) :=
sorry

theorem exercise_4_4b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_4_5b  {E : Type*} [real_closed_field E] [real_closed_field E]
  (f : E → E) (g : E → E) (h : continuous g) :
  no_continuous_real_function f = no_continuous_real_function g :=
sorry

theorem exercise_4_8a  {E : Type*} [metric_space] [bounded_set E] (f : E → R) (g : E → R)
  (hgc : continuous g) (hgi : function.injective g) :
  bounded_on_bounded_set f :=
sorry

theorem exercise_4_11a  {X Y : Type*} [metric_space X] [metric_space Y]
  (f : X → Y) (g : Y → Y) (hgc : continuous g) (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) :
  uniform_continuous f → uniform_continuous g :=
sorry

theorem exercise_4_15  {R : Type*} [field R] [field R] [field R] [field R] [field R]
  (h : continuous_open_mapping R → R) (f : R → R) (g : R → R) :
  monotonic (h ∘ f) ∧ monotonic (g ∘ f) :=
sorry

theorem exercise_4_21a  {X : Type*} [metric_space X] [metric_space X] [metric_space X]
  (K : compact_space X) (F : closed_space X) (d : metric_space X)
  (p : X) (q : X) (δ : d.dist > 0) :
  d.dist (p, q) > δ → p ∈ K ∧ q ∈ F :=
sorry

theorem exercise_5_1 {X : Type*} [metric_space X]
  [metric_space X] (f : X → X) (g : X → X) (h : bounded_difference g)
  (hgi : function.injective g) : constant f :=
sorry

theorem exercise_5_3  {R : Type*} [field R] [metric_space R] [metric_space R]
  (g : R → R) (h : ∀ x, ∃ y, g(x) = y) (hgc : ∀ x, ∃ y, g(x) = y)
  (hgi : ∀ x, ∃ y, g(x) = y) (h : ∀ x, ∃ y, ∃ z, g(x) = y + z)
  (h : ∀ x, ∃ y, ∃ z, ∃ t, g(x) = y + t) (h : ∀ x, ∃ y, ∃ z, ∃ t, g(x) = y + t)
  (h : ∀ x, ∃ y, ∃ z, ∃ t, ∃ u, ∃ v, ∃ w, ∃ x, ∃ y, ∃ z, ∃ t, ∃ u, ∃ v, ∃ w, ∃ x, ∃ y, ∃ z, ∃ t, ∃ u, ∃ v, ∃ w, ∃ x, ∃ y, ∃ z, ∃ t, ∃ u, ∃ v, ∃ w, ∃ x, ∃ y, ∃ z, ∃ t, ∃ u, ∃ v, ∃ w, ∃ x, ∃ y, ∃ z, ∃ t, ∃ u, ∃ v, ∃ w, ∃ x, ∃ y, ∃ z, ∃ t, ∃ u, ∃ v, ∃ w, ∃ x,:=
sorry

theorem exercise_5_5  {X Y : Type*} [metric_space X] [metric_space Y] [metric_space Y]
  (f : X → Y) (g : Y → Y) (h : continuous g) (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_5_7  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (f : X → Y) (g : Y → Z) (h : continuous g) (hgc : continuous gc)
  (hgi : function.injective g) (h : uniform_continuous (g ∘ f)) :
  uniform_continuous (λ x, (f (x) / g (x)) • (g (x) / gc (x))) :=
sorry

theorem exercise_5_17 {X : Type*} [metric_space X]
  [metric_space X] [metric_space X] [metric_space X]
  (f : X → X) (g : X → X) (h : g.is_continuous) (hgc : continuous g)
  (hgi : function.injective g) (h : g.is_differentiable) :
  g.is_differentiable :=
sorry