import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3bnot_topological_space_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_of_not_empty_of_not_infinite_or_empty_or_all_:=
sorry

theorem exercise_13_4a2  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_13_4b2  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : unique_topology_of_uniform_continuous_of_continuous_injective_uniform_continuous_comp hY hgc hgi :=
sorry

theorem exercise_13_5b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_13_8a {a b : ℝ} (h : a < b) :
  basis_of_open_sets ℝ (open_interval a b) :=
sorry

theorem exercise_16_1 {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : compact_space Y) (f : X → Y) (g : Y → X) (hgc : continuous g)
  (h : uniform_continuous (g ∘ f)) : compact_space X :=
sorry

theorem exercise_16_6 {V : Type*} [add_comm_group V] [module V V]
  {B : basis V} {B' : basis V} {B'' : basis V}
  (hB : ∀ (v : V), ∃ (b : B), b ∈ v) (hB' : ∀ (v : V), ∃ (b' : B'), b' ∈ v)
  (hB'' : ∀ (v : V), ∃ (b'' : B''), b'' ∈ v) :
  B = B' ∪ B'' :=
sorry

theorem exercise_18_8a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : closed_set (g ∘ f)) : closed_set f :=
sorry

theorem exercise_18_13  {X Y : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (A : set X) (hA : A ⊆ f.range)
  (h : continuous_on f A) :
  unique_extension f A :=
sorry

theorem exercise_20_2  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_21_6b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_22_2a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : quotient_map (g ∘ f)) : quotient_map f :=
sorry

theorem exercise_22_5open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_of_open_map_:=
sorry

theorem exercise_23_3  {X : Type*} [topological_space X] [compact_space X]
  {A : set X} (hA : A.nonempty) (hA_compact : A.compact)
  (hA_nonempty : A.nonempty) (hA_nonempty_compact : A.compact)
  (hA_nonempty_nonempty : A.nonempty_nonempty) (hA_nonempty_nonempty_compact : A.nonempty_nonempty_compact)
  (hA_nonempty_compact_nonempty : A.nonempty_compact) (hA_compact_nonempty : A.compact_nonempty)
  (hA_compact_nonempty_compact : A.compact_nonempty_compact)
  (hA_compact_nonempty_compact_nonempty : A.compact_nonempty_compact_nonempty)
  (hA_compact_nonempty_compact_nonempty_compact : A.compact_nonempty_compact_nonempty_compact)
  (hA_compact_nonempty_compact_nonempty_compact_nonempty : A.compact_nonempty_compact_nonempty_compact_nonempty)
  (hA_compact_nonempty_compact_nonempty_compact_nonempty_compact : A.compact_nonempty_compact_nonempty_compact_nonempty_compact)
  (hA_compact_nonempty_compact_nonempty_compact_nonempty_compact_nonempty : A.compact_nonempty_compact_nonempty_compact_nonempty_compact_nonempty)
  (hA_compact_nonempty_compact_nonempty_compact_nonempty_:=
sorry

theorem exercise_23_6  {X Y : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hX : compact_space X) (hY : compact_space Y) (hZ : compact_space Z)
  (h : X ⊆ Y) (hYZ : Y ⊆ Z) (hXZ : X ⊆ Z)
  (hX : X ⊆ ⋃ (x : X), x ∈ Y) (hY : Y ⊆ ⋃ (x : X), x ∈ Z)
  (h : X ⊆ ⋃ (x : X), x ∈ Y ∩ Z) :
  connected X :=
sorry

theorem exercise_23_11connected_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_connected_comp_quotient_of_:=
sorry

theorem exercise_24_3a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : function.continuous (g ∘ f)) : function.continuous f :=
sorry

theorem exercise_25_9 {G : Type*} [group G]
  [topological_space G] [topological_group G] [topological_space.uniform_space G]
  (H : subgroup G) (C : set G) (hC : C ∈ 𝓝 (e : G)) :
  normal_subgroup H :=
sorry

theorem exercise_26_12 {X Y : Type*} [metric_space X] [metric_space Y]
  (p : X → Y) (h : perfect_map p) : compact_space X :=
sorry

theorem exercise_28_4 {X : Type*} [topological_space X] :
  limit_point_compact X ↔ countably_compact X ↔ compact_space X :=
sorry

theorem exercise_28_6 {X : Type*} [metric_space X] [compact_space X]
  (f : X → X) (hf : isometry f) : isometry f :=
sorry

theorem exercise_29_4  {X Y : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) :
  not_locally_compact_space f :=
sorry

theorem exercise_30_10countable_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable_product_of_dense_subset_of_countable:=
sorry

theorem exercise_31_1  {X Y : Type*} [topological_space X] [topological_space Y]
  (hX : regular X) (hY : regular Y) (h : disjoint_closed_sets X Y) :
  disjoint_closed_sets X Y :=
sorry

theorem exercise_31_3 {G : Type*} [group G] [order_topology G]
  (H : order_topology G) : regular H :=
sorry

theorem exercise_32_2a  {X : Type*} [topological_space X] [nonempty X]
  (hX : compact_space X) (f : X → prod X_\alpha) (hf : function.injective f) :
  uniform_continuous f :=
sorry

theorem exercise_32_2cnormal_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_compact_space_of_:=
sorry

theorem exercise_33_7 {X : Type*} [topological_space X] [Hausdorff_space X]
  (hX : locally_compact_space X) : completely_regular X :=
sorry

theorem exercise_34_9  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_43_2  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry