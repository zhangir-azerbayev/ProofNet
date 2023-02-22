import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3b {α : Type*}
  [topological_space α] (U : topological_space.opens α) :
  ¬↑U.nonempty ↔ U = ⊥ :=
sorry

theorem exercise_13_4a2 {α : Type u} {ι : Sort w}
  [topological_space α] {f : ι → set (set α)} (h : ∀ (i : ι), is_topology (f i)) :
  ¬ is_topology (⋃ (i : ι), f i) :=
sorry

theorem exercise_13_4b2 {α : Type u}
  {X : Type u} {T : α → topological_space X} :
  unique (λ (t : topological_space X), ∀ (a : α), T a ≤ t) :=
sorry

theorem exercise_13_5b  {α : Type u} [t : topological_space α] {B : set (set α)}
  (hB : topological_space.is_topological_basis B) :
  t = ⋂ (t' : topological_space α), t'.is_topological_basis B :=
sorry

theorem exercise_13_8a :
  topological_space.is_topological_basis (⋃ (a b : ℚ) (h : a < b), {set.Ioo ↑a ↑b}) :=
sorry

theorem exercise_16_1 {X : Type*} [topological_space X]
  {Y : set X} (hY : is_open Y) {A : set Y} (hA : is_open A) :
  subtype.topology A = subtype.topology (subset.val '' A) :=
sorry

theorem exercise_16_6 {α β : Type*}
  [topological_space α] [topological_space β]
  (hα : is_basis_of_topology (λ (a b : α), {a .. b}) (λ (a b : α), is_open_Ioo a b))
  (hβ : is_basis_of_topology (λ (a b : β), {a .. b}) (λ (a b : β), is_open_Ioo a b)) :
  is_basis_of_topology (λ (a b : α × β), {a .. b}) (λ (a b : α × β), is_open_Ioo a b) :=
sorry

theorem exercise_18_8a {α : Type u} {β : Type v} [topological_space α]
  [preorder α] [t : order_closed_topology α] [topological_space β] {f g : β → α}
  (hf : continuous f) (hg : continuous g) :
  is_closed {b : β | f b ≤ g b} :=
sorry

theorem exercise_18_13 {X Y : Type*} [topological_space X]
  [topological_space Y] [t2_space Y] {f : X → Y} {A : set X}
  (hf : continuous_on f A) (h : ∃ (g : X → Y), continuous g ∧ ∀ (x : X), x ∈ A → g x = f x) :
  ∃! (g : X → Y), continuous g ∧ ∀ (x : X), x ∈ A → g x = f x :=
sorry

theorem exercise_20_2 :
  topological_space.metrizable_space (ℝ × ℝ) :=
sorry

theorem exercise_21_6b {α : Type*} [uniform_space α] [group α]
  [uniform_group α] (n : ℕ) :
  ¬ uniform_limit (λ (x : α), x ^ n) (λ (x : α), 0) :=
sorry

theorem exercise_22_2a {X Y : Type*}
  [topological_space X] [topological_space Y] (p : X → Y) (f : Y → X)
  (hf : continuous f) (h : p ∘ f = id) : quotient_map p :=
sorry

theorem exercise_22_5 {X Y : Top} (p : X ⟶ Y) (h : is_open_map p)
  {A : set ↥X} (hA : is_open A) : is_open_map (p.restrict A) :=
sorry

theorem exercise_23_3 {α : Type u}
  [topological_space α] {s : set α} {t : α → set α}
  (h : ∀ (a : α), s ∩ t a ≠ ∅) (hs : is_connected s)
  (ht : ∀ (a : α), is_connected (t a)) :
  is_connected (s ∪ (⋃ (a : α), t a)) :=
sorry

theorem exercise_23_6 {X : Type*} [topological_space X]
  {A C : set X} (hC : connected C) (hCA : C ∩ A ≠ ∅) (hCC : C ∩ -A ≠ ∅) :
  C ∩ closure (-A) ≠ ∅ :=
sorry

theorem exercise_23_11 {X : Type*}
  [topological_space X] {Y : Type*} [topological_space Y] {f : X → Y}
  (hf : quotient_map f) (hY : connected_space Y)
  (h : ∀ (y : Y), connected_space (f ⁻¹' {y})) : connected_space X :=
sorry

theorem exercise_24_3a {X : Type*}
  [topological_space X] [compact_space X] [t2_space X] {f : X → X}
  (hf : continuous f) :
  ∃ (x : X), f x = x :=
sorry

theorem exercise_25_9 {G : Type*} [topological_group G]
  (C : set G) (hC : is_component C) :
  is_normal_subgroup C :=
sorry

theorem exercise_26_12 {X Y : Type*} [topological_space X]
  [topological_space Y] (p : X → Y) (hc : continuous p) (hsc : surjective p)
  (hpc : ∀ (y : Y), compact (p ⁻¹' {y})) (hY : compact_space Y) :
  compact_space X :=
sorry

theorem exercise_28_4 {X : Type*}
  [topological_space X] [t1_space X] :
  countably_compact_space X ↔ limit_point_compact_space X :=
sorry

theorem exercise_28_6 {X : Type*} [emetric_space X]
  (f : X → X) (hf : isometry f) (hX : compact_space X) :
  function.bijective f :=
sorry

theorem exercise_29_4 :
  ¬locally_compact_space (set.Ioo 0 1) ^ (ℕ : Type*) :=
sorry

theorem exercise_30_10  {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)]
  [∀ i, topological_space.first_countable_topology (X i)]
  [∀ i, topological_space.t2_space (X i)]
  [∀ i, topological_space.countable_topology (X i)]
  [∀ i, topological_space.separated (X i)]
  [∀ i, topological_space.t2_space (X i)]
  [∀ i, topological_space.countable_topology (X i)]
  [∀ i, topological_space.separated (X i)]
  [∀ i, topological_space.t2_space (X i)]
  [∀ i, topological_space.countable_topology (X i)]
  [∀ i, topological_space.separated (X i)]
  [∀ i, topological_space.t2_space (X i)]
  [∀ i, topological_space.countable_topology (X i)]
  [∀ i, topological_space.separated (X i)]
  [∀ i, topological_space.t2_space (X i)]
  [∀ i, topological_space.countable_topology (X i)]
  [∀ i, topological_space.separated (X i)]
  [∀ i, topological_space.t2_space (X i)]
  [∀ i:=
sorry

theorem exercise_31_1 {α : Type u} [topological_space α]
  [regular_space α] {x y : α} (h : x ≠ y) :
  ∃ (s : set α) (H : s ∈ nhds x) (t : set α) (H : t ∈ nhds y), disjoint (closure s) (closure t) :=
sorry

theorem exercise_31_3 {α : Type u} [topological_space α]
  [linear_order α] [order_topology α] : regular_space α :=
sorry

theorem exercise_32_2a {α : Type*} {X : α → Type*}
  [∀ a, nonempty (X a)] [∀ a, topological_space (X a)]
  (h : is_Hausdorff (prod.topological_space X)) :
  ∀ a, is_Hausdorff (X a) :=
sorry

theorem exercise_32_2c {α : Type u} {X : α → Type*}
  [topological_space X] [normal_space (Π a, X a)] (h : ∀ a, nonempty (X a)) :
  ∀ a, normal_space (X a) :=
sorry

theorem exercise_33_7 {X : Type*} [topological_space X]
  [locally_compact_space X] [t2_space X] :
  is_completely_regular X :=
sorry

theorem exercise_34_9 {X : Type*}
  [topological_space X] [compact_space X] [t2_space X]
  {X₁ X₂ : set X} (hX₁ : is_closed X₁) (hX₂ : is_closed X₂)
  (hX₁m : is_metrizable X₁) (hX₂m : is_metrizable X₂)
  (hX : X = X₁ ∪ X₂) : is_metrizable X :=
sorry

theorem exercise_43_2 {X Y : Type*}
  [metric_space X] [metric_space Y] (hY : complete_space Y)
  (A : set X) (f : A → Y) (hf : uniform_continuous f) :
  ∃ (g : closure A → Y), uniform_continuous g ∧
  ∀ (x : closure A), g x = extend_from A f x :=
sorry