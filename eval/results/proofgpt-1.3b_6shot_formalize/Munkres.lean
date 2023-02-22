import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3b {X : Type*} [topological_space X] :
  X \notin \mathcal{T}_\infty ↔ ∀ U ∈ \mathcal{T}_\infty, U \text{ is open or all of } X :=
sorry

theorem exercise_13_4a2 {X : Type*} [topological_space X]
  {T : set X → Type*} [topological_space T] (hT : ∀ x ∈ T, is_open x) :
  ∃ (U : set X) (hU : U ∈ T), U ∉ T :=
sorry

theorem exercise_13_4b2 {X : Type*} [topological_space X]
  {T : ι → Type*} [Π i, topological_space (T i)]
  (hT : ∀ i, is_topological_space (T i)) :
  topological_space (⨁ i, T i) :=
sorry

theorem exercise_13_5b {X : Type*} [topological_space X] {A : set X}
  (hA : A.is_basis) :
  topological_space (⋃ (B : set X) (B' : set B), A.is_basis.comap B.is_basis.comap B'.is_basis) :=
sorry

theorem exercise_13_8a {X : Type*} [metric_space X] [metric_space X]
  (hX : X ≃ₗᵢ[ℝ] X) :
  (basis_rational_basis hX).to_basis :=
sorry

theorem exercise_16_1 {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : compact_space Y) (f : X → Y) (g : Y → X) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) :
  uniform_continuous f :=
sorry

theorem exercise_16_6 {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : compact_space Y) (f : X → Y) (g : Y → X) (hgc : continuous g)
  (hgi : function.injective g) :
  countable_basis (g ∘ f) :=
sorry

theorem exercise_18_8a {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : ordered_set Y) (f : X → Y) (g : Y → ℝ) (hgc : continuous g)
  (hgi : function.injective g) : closed {x | f x ≤ g x} :=
sorry

theorem exercise_18_13  {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : compact_space Y) (f : X → Y) (g : Y → ℝ) (hgc : continuous g)
  (h : unique_extension f g) :
  unique_extension f g :=
sorry

theorem exercise_20_2  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : metric_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : metric_space (g ∘ f)) : metric_space f :=
sorry

theorem exercise_21_6b {n : ℕ} :
  ∀ x : [0,1], ∃ y : ℝ, 0 < y ∧ y < 1 ∧ ∀ ε > 0, ∃ N : ℕ, N > 0 ∧ ∀ m, m ≥ N → |f_{n}^{m}(x) - y| < ε :=
sorry

theorem exercise_22_2a {X Y : Type*} [metric_space X] [metric_space Y]
  (p : X → Y) (h : continuous p) :
  p.is_quotient ↔ p.is_quotient_comp :=
sorry

theorem exercise_22_5  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : open_map (g ∘ f)) : open_map f :=
sorry

theorem exercise_23_3 {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : disjoint (g ∘ f) (g '' A)) : connected (g ∘ f) :=
sorry

theorem exercise_23_6  {X : Type*} [topological_space X] {A : set X} {C : set X}
  (hC : connected C) (hA : A ⊆ C) (hAC : A ⊆ C) (hAC' : C ⊆ A) :
  connected A :=
sorry

theorem exercise_23_11  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : connected (g ∘ f)) : connected f :=
sorry

theorem exercise_24_3afixed_point_of_continuous_on_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_of_fixed_point_:=
sorry

theorem exercise_25_9 {G : Type*} [topological_group G]
  (C : set G) (hC : is_closed C) : is_closed (normalizer C) :=
sorry

theorem exercise_26_12 {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : compact_space Y) (f : X → Y) :
  compact_space X ↔ compact_space Y :=
sorry

theorem exercise_28_4 {X : Type*} [topological_space X]
  (hX : is_countably_compact X) :
  is_limit_point_compact X ↔ limit_point_compact X :=
sorry

theorem exercise_28_6 {X : Type*} [metric_space X] [compact_space X]
  (hX : isometry X) (f : X → X) : isometry X :=
sorry

theorem exercise_29_4 {X : Type*} [metric_space X] [topological_space X]
  (hX : is_compact X) : ¬locally_compact X :=
sorry

theorem exercise_30_10 {X : Type*} [topological_space X]
  (hX : countable_dense_sets X) :
  countable_dense_sets (X × X) :=
sorry

theorem exercise_31_1  {X : Type*} [topological_space X] [metric_space X] [metric_space X]
  (hX : regular_space X) (x y : X) (hx : x ∈ closure x) (hy : y ∈ closure y) :
  disjoint (closures x) (closures y) :=
sorry

theorem exercise_31_3 {X : Type*} [topological_space X] [order_topology X]
  (hX : X.is_compact) : regular_space X :=
sorry

theorem exercise_32_2a {X : Type*} [topological_space X]
  {x : X} (hx : x ∈ X) :
  ∃ (x_\alpha : X_\alpha) (x_\alpha' : X_\alpha), x = x_\alpha ∧ x_\alpha' = x_\alpha' ∧ x_\alpha ∈ X_\alpha :=
sorry

theorem exercise_32_2c {X : Type*} [topological_space X]
  {Y : Type*} [topological_space Y] [topological_space Z]
  (hY : nonempty_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : normal_space (g ∘ f)) : normal_space f :=
sorry

theorem exercise_33_7 {X : Type*} [topological_space X]
  (hX : locally_compact_space X) :
  completely_regular X :=
sorry

theorem exercise_34_9  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : metrizable X) : metrizable X :=
sorry

theorem exercise_43_2 {X Y : Type*}
  [metric_space X] [complete_space X] [metric_space Y] [complete_space Y]
  (hY : complete_space Y) (f : X → Y) (g : Y → X) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) :
  uniform_continuous f :=
sorry