import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3b {X : Type*} [set X]
  (h : ∀ U : ℕ → X - U, ∃ U : ℕ → X - U) :
  ∃ U : ℕ → X - U :=
sorry

theorem exercise_13_4a2 {X : Type*} [metric_space X]
  [metric_space X] [metric_space X] [metric_space X] [metric_space X]
  (hX : X) (hX_alpha : ∀ α, X_α ⊆ X) (hX_union : ∀ α, X_α = X) :
  ∃ α, X_α = X :=
sorry

theorem exercise_13_4b2 {X : Type*} [metric_space X]
  [metric_space X] [metric_space X] [metric_space X] [metric_space X]
  (α : ℕ) (α_top : α = ℕ) (α_top_top : α_top = ℕ) (α_top_top_top : α_top_top = ℕ)
  (α_top_top_top_top : α_top_top_top = ℕ) (α_top_top_top_top_top : α_top_top_top_top = ℕ)
  (α_top_top_top_top_top_top : α_top_top_top_top_top = ℕ) (α_top_top_top_top_top_top_top : α_top_top_top_top_top_top = ℕ)
  (α_top_top_top_top_top_top_top : α_top_top_top_top_top_top_top = ℕ) (α_top_top_top_top_top_top_top_top : α_top_top_top_top_top_top_top_top = ℕ)
  (α_top_top_top_top_top_top_top_top : α_top_top_top_top_top_top_top_top_top = ℕ) (α_top_top_top_top_top_top_top_top_top_top : α_top_top_top_top_top_top_top_top_top_top = ℕ):=
sorry

theorem exercise_13_5b {X : Type*} [topology X]
  [subbasis X] (A : subbasis X) (T : topology X) :
  T = ∩ {T' : topology X | T ⊆ T'} :=
sorry

theorem exercise_13_8a {R : Type*} [field R]
  [add_comm_group R] [module R R] [finite_dimensional R]
  (a b : R) (h : a < b) :
  ∃ (f : R → R), f a = b :=
sorry

theorem exercise_16_1 {X Y : Type*} [metric_space X] [metric_space Y]
  (A : subspace Y) (B : subspace X) (h : A = B) :
  A = B :=
sorry

theorem exercise_16_6basis_of_R2_rational_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair_pair:=
sorry

theorem exercise_18_8a  {X Y : Type*} [metric_space X] [metric_space Y] [ordered_set Y]
  (f : X → Y) (g : Y → Y) (hgc : continuous g) (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : closed_in_ordered_set (g ∘ f) :=
sorry

theorem exercise_18_13 {X Y : Type*} [metric_space X] [metric_space Y]
  (f : X → Y) (g : Y → Y) (h : continuous g) (hgc : continuous g)
  (hgi : function.injective g) :
  continuous (g ∘ f) :=
sorry

theorem exercise_20_2 {R : Type*} [field R]
  [add_comm_group R] [module R R] [metric_space R] [metrizable R]
  (h : dictionary_order_topology R) : metrizable R :=
sorry

theorem exercise_21_6b {n : ℕ} (f : ℝ → ℝ) :
  ∀ x : ℝ, ∃ n : ℕ, f (x ^ n) ≠ x ^ n :=
sorry

theorem exercise_22_2a  {X Y : Type*} [metric_space X] [metric_space Y] [metric_space Y]
  (p : continuous_map X Y) (f : continuous_map Y X) (g : continuous_map Y Y)
  (h : continuous_identity_map g) :
  continuous_identity_map (p ∘ f) = p ∘ f ∘ g :=
sorry

theorem exercise_22_5 {X Y : Type*} [metric_space X] [metric_space Y]
  (p : open_map X) (q : open_map Y) (A : open_set X) (B : open_set Y) :
  open_map (p ◦ q) (A ⊆ p(A)) (B ⊆ q(B)) :=
sorry

theorem exercise_23_3  {X : Type*} [metric_space X] [metric_space X] [metric_space X]
  (A : connected subset X) (A_alpha : connected subset X)
  (h : A ∩ A_alpha ≠ ∅) (h_alpha : A_alpha ∩ A ≠ ∅) :
  connected_union A ∪ (∪ A_alpha) = connected_union A ∪ (∪ A_alpha) :=
sorry

theorem exercise_23_6 {X : Type*} [metric_space X]
  [metric_space X] [connected_space X] (A : X →* X) (B : X →* X)
  (C : connected_space X) (D : X →* X) (E : X →* X) (F : X →* X)
  (h : connected_intersection C ∩ (B ∩ D) ∩ (E ∩ F)) :
  connected_intersection C ∩ (B ∩ D) ∩ (E ∩ F) = C ∩ (B ∩ D) ∩ (E ∩ F) :=
sorry

theorem exercise_23_11 {X Y : Type*} [metric_space X] [metric_space Y]
  (p : X → Y) (q : Y → Y) (h : continuous.injective p) (h' : continuous.injective q)
  (h'p : continuous.injective (p ∘ q)) (h'q : continuous.injective (q ∘ p)) :
  connected (p ∘ q) :=
sorry

theorem exercise_24_3a {X : Type*} [metric_space X]
  [metric_space X] (f : X → X) (x : X) :
  ∃ y : X, f.injective x = y :=
sorry

theorem exercise_25_9 {G : Type*} [group G] [group G]
  [topological_group G] (C : component G) (h : normal_subgroup C) :
  normal_subgroup C :=
sorry

theorem exercise_26_12 {X Y : Type*} [metric_space X] [metric_space Y]
  (p : X → Y) (hY : compact_space Y) (h : perfect_map p) :
  compact_space X :=
sorry

theorem exercise_28_4 {X : Type*} [T_1 space X]
  [metric_space X] [metric_space X] [metric_space X]
  (hX : X is countably compact) : limit_point_compact X :=
sorry

theorem exercise_28_6  {X Y : Type*} [metric_space X] [metric_space Y] [metric_space Y]
  (f : X → Y) (g : Y → Y) (h : isometry g) (hgi : function.injective g)
  (h : isometry.is_isometry g) : isometry.is_isometry g :=
sorry

theorem exercise_29_4  {X : Type*} [metric_space X] [metric_space X] [metric_space X]
  (hX : X) (f : X → X) (g : X → X) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) :
  not (uniform_continuous f) :=
sorry

theorem exercise_30_10 {X : Type*} [metric_space X]
  [metric_space X] [countable_space X] (hX : countable_space X)
  (hX_dense : countable_dense_subset X) :
  countable_space X :=
sorry

theorem exercise_31_1 {X : Type*} [metric_space X]
  [regular_space X] (hX : regular_space X) (x y : X) :
  disjoint_neighborhoods X x y :=
sorry

theorem exercise_31_3 {T : Type*} [topology T]
  (h : regular T) : regular T :=
sorry

theorem exercise_32_2a {X : Type*} [set X]
  [hausdorff_space X] (h : Hausdorff X) : Hausdorff (X × X) :=
sorry

theorem exercise_32_2c {X : Type*} [set X] [group X]
  [is_normal X] (h : normal_subgroup X) : normal_subgroup X :=
sorry

theorem exercise_33_7 {X : Type*} [topological_space X]
  [topological_space X] (hX : completely_regular X) :
  locally_compact_hausdorff X :=
sorry

theorem exercise_34_9 {X : Type*} [compact_space X]
  [metrizable_closed_space X] (X1 X2 : closed_subspaces X) :
  metrizable X :=
sorry

theorem exercise_43_2 {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : complete_space Y) (f : X → Y) (g : Y → Y) (hgc : continuous g)
  (hgi : function.injective g) (h : uniform_continuous (g ∘ f)) :
  uniform_continuous f → uniform_continuous g :=
sorry