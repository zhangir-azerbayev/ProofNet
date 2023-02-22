import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3b {X : Type*} [set X] [set X]
  (h : ∃ U : X, U ∈ \mathcal{T}_\infty) :
  ∃ U : X, U ∈ \mathcal{T}_\infty :=
sorry

theorem exercise_13_4a2 {X Y Z : Type*} [topology X] [topology Y] [topology Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : union (g ∘ f)) : union (f ∘ g) :=
sorry

theorem exercise_13_4b2 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : largest_topology (g ∘ f)) : largest_topology f :=
sorry

theorem exercise_13_5b {X Y Z : Type*} [subbasis_space X] [subbasis_space Y] [subbasis_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : subbasis_generated (g ∘ f)) : subbasis_generated f :=
sorry

theorem exercise_13_8a {R : Type*} [field R] [module R] [finite_dimensional R] 
  {a b : ℤ} (h : finite_dimension R + 1 < a.card) :
  basis (R.standard_topology) :=
sorry

theorem exercise_16_1 {X Y Z : Type*} [subspace X] [subspace Y] [subspace Z]
  (hY : subspace Y ⊆ X) (hZ : subspace Z ⊆ Y) (h : subspace Y ⊆ X) (hgc : continuous h)
  (hgi : function.injective h) : subspace Y ⊆ X :=
sorry

theorem exercise_16_6 {R : Type*} [field R] [module R] [finite_dimensional R] 
  {a b c d : ℤ} (h : finite_dimensional.finrank R + 1 < a.card) :
  basis R :=
sorry

theorem exercise_18_8aclosed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_order_topology_closed_in_:=
sorry

theorem exercise_18_13  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : continuous (g ∘ f)) : continuous f :=
sorry

theorem exercise_20_2 {R : Type*} [dic_order_topology R]
  [dic_order_topology R] (h : R.is_metrizable) :
  ∀ (x : R), x.is_metrizable :=
sorry

theorem exercise_21_6b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_22_2a {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : quotient_map (g ∘ f)) : quotient_map f ≡ quotient_map g :=
sorry

theorem exercise_22_5open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_open_map_:=
sorry

theorem exercise_23_3 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : uniform_continuous (g ∘ f)) : connected_union (f ∘ g) :=
sorry

theorem exercise_23_6 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : connected_subspace (g ∘ f)) : connected_subspace f ⊆ connected_subspace g :=
sorry

theorem exercise_23_11 {X Y Z : Type*} 
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_map] [quotient_map_map_quotient_map]
  [quotient_map_map_quotient_:=
sorry

theorem exercise_24_3a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : uniform_continuous (g ∘ f)) : fixed_point f :=
sorry

theorem exercise_25_9 {G : Type*} [group G] [group C]
  (h : G.component_containing_identity_element C) :
  normal_subgroup G :=
sorry

theorem exercise_26_12 {X Y Z : Type*} 
  [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : closed_continuous_surjective_map (g ∘ f)) : compact_space f :=
sorry

theorem exercise_28_4 {X : Type*} [space X] [space X]
  (hX : limit_point_compact X) (hX : X is countably compact) :
  limit_point_compact X :=
sorry

theorem exercise_28_6 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : isometry (g ∘ f)) : isometry f :=
sorry

theorem exercise_29_4 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : uniform_continuous (g ∘ f)) :
  not_locally_compact_in_uniform_topology :=
sorry

theorem exercise_30_10 {X Y Z : Type*} 
  [countable_product_of_countable_dense_sets X] [countable_product_of_countable_dense_sets Y] [countable_product_of_countable_dense_sets Z]
  (hX : countable_product_of_countable_dense_sets X) (hY : countable_product_of_countable_dense_sets Y) (hZ : countable_product_of_countable_dense_sets Z)
  (h : countable_product_of_countable_dense_sets (X ∘ Y) (X ∘ Z)) : countable_product_of_countable_dense_sets (X ∘ Y) (X ∘ Z) :=
sorry

theorem exercise_31_1 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : disjoint_neighborhoods (g ∘ f)) : disjoint_neighborhoods f :=
sorry

theorem exercise_31_3 {T : Type*} [order_topology T]
  [order_topology T] (h : order_topology T) :
  regular_order_topology T :=
sorry

theorem exercise_32_2a {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (hZ : compact_space Z) (h : Hausdorff (X ∘ Y)) (hgc : continuous h)
  (hgi : function.injective h) : Hausdorff (X ∘ Y) :=
sorry

theorem exercise_32_2c {X Y Z : Type*} [group X] [group Y] [group Z]
  (hX : normal_space X) (hY : normal_space Y) (hZ : normal_space Z) (h : normal_space (X ∘ Y)) :
  normal_space (X ∘ Y) :=
sorry

theorem exercise_33_7 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : is_completely_regular (g ∘ f)) : is_completely_regular f :=
sorry

theorem exercise_34_9 {X Y Z : Type*} [compact_space X] [compact_space Y] [compact_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : metrizable_union (g ∘ f)) : metrizable_union f :=
sorry

theorem exercise_43_2  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry