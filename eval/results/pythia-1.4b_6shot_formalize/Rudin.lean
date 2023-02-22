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





theorem exercise_1_1birrational_of_rational_in_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational_irrational:=
sorry

theorem exercise_1_4 {E : Type*} [ordered_set E] [ordered_set E]
  [lower_bound E] [upper_bound E] (h : E ≤ E) :
  lower_bound E ≤ upper_bound E :=
sorry

theorem exercise_1_8 {X : Type*} [field X] 
  [add_comm_group X] [module X] [finite_dimensional X] 
  (h : ∀ x, x ≠ 0 → x ≠ 0) :
  ∃ (x : X), x ≠ 0 :=
sorry

theorem exercise_1_12  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_1_14complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_of_unit_norm_of_complex_number_:=
sorry

theorem exercise_1_17 {R : Type*} [ring R] [add_comm_group R] [module R R]
  [finite_dimensional R] {x y : R} (hx : ∀ (i : ℕ), x i = y i) :
  ∀ (x : R), ∀ (y : R), x + y = x + y + x y :=
sorry

theorem exercise_1_18bnonzero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_zero_product_:=
sorry

theorem exercise_2_19a {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (hZ : compact_space Z) (h : disjoint_closed_sets (hY ∪ hZ)) :
  disjoint_closed_sets (hY ∪ hZ) ≠ ∅ :=
sorry

theorem exercise_2_25 {K : Type*} [group K] 
  [fintype K] {p n : ℕ} [fintype K] {K : compact_space K} 
  (hK : card K = p ^ n) :
  countable_base K :=
sorry

theorem exercise_2_27b {E : Type*} [set E] [set P]
  (h : E.is_countable) (hP : P ≠ ∅) :
  condensation_points E ≠ ∅ :=
sorry

theorem exercise_2_29 {X Y Z : Type*} [open_set X] [open_set Y] [open_set Z]
  (hY : open_set Y) (hZ : open_set Z) (h : disjoint_segments (open_set Y) (open_set Z)) :
  ∀ (x y z : X), disjoint_segments (open_set Y) (open_set Z) (open_set x y) :=
sorry

theorem exercise_3_2alimit_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_square_root_of_:=
sorry

theorem exercise_3_5limsup_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum_of_sum:=
sorry

theorem exercise_3_7  {G : Type*} [group G] [fintype G] [finite_type G]
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (h : finite_type G)
  (h : finite_type G) (h : finite_type G) (h : finite_type G) (:=
sorry

theorem exercise_3_13 {X Y Z : Type*} 
  [fintype X] [fintype Y] [fintype Z] (hX : X → Y) (hY : Y → Z) (hZ : Z → X)
  (h : Cauchy_product (hX hY) (hZ hX)) :
  absolutely_convergent_series X Y Z :=
sorry

theorem exercise_3_21 {X Y Z : Type*} 
  [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : closed_nonempty_bounded_sets (g ∘ f)) : closed_nonempty_bounded_sets f :=
sorry

theorem exercise_4_1a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : continuous (g ∘ f)) : continuous f :=
sorry

theorem exercise_4_3 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : uniform_continuous (g ∘ f)) : closed_zero_set f :=
sorry

theorem exercise_4_4b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : uniform_continuous (g ∘ f)) : dense_subset f :=
sorry

theorem exercise_4_5b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : continuous (g ∘ f)) : continuous f :=
sorry

theorem exercise_4_8a {E : Type*} [bounded_set E] [bounded_continuous E]
  (f : bounded_set E → E) (f : bounded_continuous E) : bounded_set E :=
sorry

theorem exercise_4_11a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_4_15monotonic_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_mapping_of_open_:=
sorry

theorem exercise_4_21a {X : Type*} [metric_space X] [compact_space X]
  (K : compact_space X) (F : closed_set X) (h : disjoint_closed_sets K F) :
  ∀ (p : X), d(p, K) > 0 ∧ d(p, F) > 0 :=
sorry

theorem exercise_5_1  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : constant_function (g ∘ f)) : constant_function f :=
sorry

theorem exercise_5_3one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_one_to_one_of_:=
sorry

theorem exercise_5_5  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_5_7  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_5_17hree_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_differentiable_of_three_times_:=
sorry