import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3b (X : Type*) :
  ¬ topological_space (⋃ (U : set X), {U | X - U ∈ {∅, univ, {x | x.infinite}}}) :=
sorry

theorem exercise_13_4a2 {X : Type*} {α : Type*} 
  (T : α → set (set X)) (hT₁ : ∀ (α : Type*), ∀ (T : α → set (set X)), 
  ∀ (U : set X), U ∈ T α → is_open U) (hT₂ : ∀ (α : Type*), ∀ (T : α → set (set X)), 
  ∀ (U : set X), U ∈ T α → is_open U) (hT₃ : ∀ (α : Type*), ∀ (T : α → set (set X)), 
  ∀ (U : set X), U ∈ T α → is_open U) :
  ¬ is_topology (⋃ (α : Type*), T α) :=
sorry

theorem exercise_13_4b2 {X : Type*} 
  (T : Type*) [∀ α, topological_space (T α)] :
  topological_space X :=
sorry

theorem exercise_13_5b 
  {X : Type*} {A : set (set X)} (hA : is_subbasis A) :
  topology.generated A = ⋂ (T : set (set X)) (hT : is_topology T) 
  (hT_A : A ⊆ T) :=
sorry

theorem exercise_13_8a :
  is_topological_basis (set.range (λ (p : ℚ × ℚ), {a | p.1 < a ∧ a < p.2})) :=
sorry

theorem exercise_16_1 {X Y : Type*} [topological_space X]
  [topological_space Y] (hY : subspace Y X) (A : set Y) :
  subspace.topology A = subspace.topology (hY.subtype A) :=
sorry

theorem exercise_16_6 (a b c d : ℚ) 
  (hab : a < b) (hcd : c < d) :
  is_topological_basis (Icc a b) (Icc c d) :=
sorry

theorem exercise_18_8a {X Y : Type*} [topological_space X]
  [topological_space Y] [order_topology Y] (f g : X → Y) (hf : continuous f)
  (hg : continuous g) :
  is_closed {x | f x ≤ g x} :=
sorry

theorem exercise_18_13 {X Y : Type*} 
  [topological_space X] [topological_space Y] (hY : hausdorff Y) 
  (A : set X) (f : A → Y) (hf : continuous f) (g : closure A → Y) 
  (hg : continuous g) (hgf : ∀ x ∈ A, g x = f x) :
  g = f ∘ subtype.val :=
sorry

theorem exercise_20_2 {α β : Type*} 
  [topological_space α] [topological_space β] :
  metrizable_space (prod_dict_order_topology α β) :=
sorry

theorem exercise_21_6b (n : ℕ) :
  ¬ uniform_limit (λ n : ℕ, (λ x : ℝ, x ^ n)) (λ n : ℕ, (λ x : ℝ, x ^ (n + 1)))
    (λ x : ℝ, x ^ n) (Ioo 0 1) :=
sorry

theorem exercise_22_2a {X Y : Type*}
  [topological_space X] [topological_space Y] (p : X → Y) (hpc : continuous p)
  (f : Y → X) (hfc : continuous f) (h : p ∘ f = id) :
  quotient_map p :=
sorry

theorem exercise_22_5 {X Y : Type*} [topological_space X]
  [topological_space Y] (p : X → Y) (h : open_map p) (A : set X) (hA : is_open A) :
  open_map (p.restrict A) :=
sorry

theorem exercise_23_3 {X : Type*} 
  [topological_space X] {A : set X} (hA : is_connected A) 
  {Aα : Type*} {B : Aα → set X} (hB : ∀ (α : Aα), is_connected (B α)) 
  (h : ∀ (α : Aα), A ∩ B α ≠ ∅) :
  is_connected (A ∪ (⋃ (α : Aα), B α)) :=
sorry

theorem exercise_23_6 {X : Type*} [topological_space X]
  (A : set X) (C : set X) (hC : is_connected C) (hC_inter_A : C ∩ A ≠ ∅)
  (hC_inter_compl_A : C ∩ set.compl A ≠ ∅) :
  C ∩ set.boundary A ≠ ∅ :=
sorry

theorem exercise_23_11 {X Y : Type*} [topological_space X]
  [topological_space Y] (p : X → Y) (hp : quotient_map p)
  (hY : connected_space Y) (hX : ∀ y : Y, connected_space (p ⁻¹' {y})) :
  connected_space X :=
sorry

theorem exercise_24_3a {X : Type*} [topological_space X]
  [compact_space X] {f : X → X} (hf : continuous f) :
  ∃ (x : X), f x = x :=
sorry

theorem exercise_25_9 (G : Type*) [topological_space G] [group G]
  [topological_group G] (C : set G) (hC : is_connected C)
  (hC_id : (1 : G) ∈ C) : is_normal_subgroup C :=
sorry

theorem exercise_26_12 {X Y : Type*} [topological_space X]
  [topological_space Y] [compact_space Y] (p : X → Y) (hc : continuous p)
  (hsc : surjective p) (hpc : ∀ y : Y, compact_space (p ⁻¹' {y})) :
  compact_space X :=
sorry

theorem exercise_28_4 {X : Type*} [topological_space X]
  (hX : t1_space X) (hX₁ : limit_point_compact X) :
  countably_compact X :=
sorry

theorem exercise_28_6 {X : Type*} [metric_space X]
  (hX : compact_space X) (f : X → X) (hf : ∀ x y : X, dist x y = dist (f x) (f y)) :
  function.bijective f :=
sorry

theorem exercise_29_4 :
  ¬ locally_compact_space (uniform_space.prod_Ioo_omega) :=
sorry

theorem exercise_30_10 {ι : Type*} {X : ι → Type*}
  [∀ i, topological_space (X i)] [∀ i, countable_topology (X i)]
  [∀ i, dense_subset (X i)] :
  dense_subset (prod_topology X) :=
sorry

theorem exercise_31_1 {X : Type*} [topological_space X]
  (hX : regular_space X) (x y : X) :
  ∃ (U V : set X), is_open U ∧ is_open V ∧ x ∈ U ∧ y ∈ V ∧ closure U ∩ closure V = ∅ :=
sorry

theorem exercise_31_3 {α : Type*} [linear_order α] :
  regular_space (order_topology α) :=
sorry

theorem exercise_32_2a {ι : Type*} {X : ι → Type*}
  [∀ i, nonempty (X i)] [∀ i, topological_space (X i)]
  (h : hausdorff_space (Π i, X i)) :
  ∀ i, hausdorff_space (X i) :=
sorry

theorem exercise_32_2c {α : Type*} {X : α → Type*} [∀ a, topological_space (X a)]
  [∀ a, nonempty (X a)] [∀ a, normal_space (X a)]
  (h : normal_space (prod_topology X)) :
  ∀ a, normal_space (X a) :=
sorry

theorem exercise_33_7 {X : Type*} 
  [topological_space X] [locally_compact_space X] [hausdorff_space X] :
  completely_regular_space X :=
sorry

theorem exercise_34_9  {X : Type*} [topological_space X] [compact_space X] [t2_space X]
  {X₁ X₂ : set X} (hX₁ : is_closed X₁) (hX₂ : is_closed X₂)
  (hX₁m : metrizable_space X₁) (hX₂m : metrizable_space X₂)
  (hX₁X₂ : X₁ ∪ X₂ = univ) : metrizable_space X :=
sorry

theorem exercise_43_2 {X Y : Type*} [metric_space X] 
  [metric_space Y] [complete_space Y] (A : set X) (f : A → Y) 
  (hf : uniform_continuous f) :
  ∃ (g : closure A → Y), continuous g ∧ uniform_continuous g ∧ 
  ∀ (x : closure A), x ∈ A → g x = f x :=
sorry