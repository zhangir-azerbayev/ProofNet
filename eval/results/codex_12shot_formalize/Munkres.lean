

theorem exercise_13_1 (X : Type*) [topological_space X]
  (A : set X) (hA : ∀ x ∈ A, ∃ U : set X, is_open U ∧ x ∈ U ∧ U ⊆ A): 
  is_open A :=
sorry

theorem exercise_13_3a {X : Type*} :
  is_topology {U : set X // X \ U.val ∈ @set.countable X ⊕ set.univ} :=
sorry

theorem exercise_13_3b (X : Type*) :
  ¬ topological_space (⋃ (U : set X), {U | X - U ∈ {∅, univ, {x | x.infinite}}}) :=
sorry

theorem exercise_13_4a1 {X : Type*} 
  {α : Type*} [fintype α] (T : α → topological_space X) :
  topological_space X :=
sorry

theorem exercise_13_4a2 {X : Type*} {α : Type*} 
  (T : α → set (set X)) (hT₁ : ∀ (α : Type*), ∀ (T : α → set (set X)), 
  ∀ (U : set X), U ∈ T α → is_open U) (hT₂ : ∀ (α : Type*), ∀ (T : α → set (set X)), 
  ∀ (U : set X), U ∈ T α → is_open U) (hT₃ : ∀ (α : Type*), ∀ (T : α → set (set X)), 
  ∀ (U : set X), U ∈ T α → is_open U) :
  ¬ is_topology (set.Union T) :=
sorry

theorem exercise_13_4b1 {X : Type*} {α : Type*} 
  (T : α → topological_space X) :
  ∃! (t : topological_space X), ∀ (a : α), t.is_topology_of (T a) :=
sorry

theorem exercise_13_4b2 {X : Type*} 
  (T : Type*) [∀ α, topological_space (T α)] :
  ∃ (T' : topological_space X), ∀ α, T' ≤ T α :=
sorry

theorem exercise_13_5a {X : Type*} 
  {A : set (set X)} (hA : is_topological_basis A) :
  topology.generated A = ⋂ (T : set (set X)) (hT : is_topology T) 
  (hT_A : A ⊆ T) :=
sorry

theorem exercise_13_5b 
  {X : Type*} {A : set (set X)} (hA : is_subbasis A) :
  topology.generated A = ⋂ (T : set (set X)) (hT : is_topology T) 
  (hT_A : A ⊆ T) :=
sorry

theorem exercise_13_6 :
  ¬ (topological_space.is_topological_basis ℝ (lower_limit_topology ℝ) 
  ∨ topological_space.is_topological_basis ℝ (K_topology ℝ)) :=
sorry

theorem exercise_13_8a {α : Type*} 
  [linear_order α] [densely_ordered α] [decidable_linear_order α] :
  is_topological_basis {p : α × α | p.1 < p.2 ∧ p.1.is_rat ∧ p.2.is_rat} :=
sorry

theorem exercise_13_8b :
  is_topological_basis (set.range (λ (p : ℚ × ℚ), {a | p.1 < a ∧ a < p.2})) :=
sorry

theorem exercise_16_1 {X Y : Type*} [topological_space X]
  [topological_space Y] (hY : subspace Y X) (A : set Y) :
  subspace.topology A = subspace.topology (A ∩ univ) :=
sorry

theorem exercise_16_4 {X Y : Type*} [topological_space X] [topological_space Y] :
  is_open (pi_one : X × Y → X) :=
sorry

theorem exercise_16_6 (a b c d : ℚ) 
  (hab : a < b) (hcd : c < d) :
  is_topological_basis (Icc a b) (Icc c d) :=
sorry

theorem exercise_17_4 {X : Type*} [topological_space X]
  (U : set X) (hU : is_open U) (A : set X) (hA : is_closed A) :
  is_open (U \ A) ∧ is_closed (A \ U) :=
sorry

theorem exercise_18_13 {X Y : Type*}
  [topological_space X] [topological_space Y] (hY : hausdorff Y)
  (A : set X) (f : A → Y) (hf : continuous f)
  (g : closure A → Y) (hg : continuous g)
  (h : ∀ x ∈ A, g x = f x) :
  ∀ x ∈ closure A, g x = f x :=
sorry

theorem exercise_18_8a {X Y : Type*} [topological_space X]
  [topological_space Y] [order_topology Y] (f g : X → Y) (hf : continuous f)
  (hg : continuous g) :
  is_closed {x | f x ≤ g x} :=
sorry

theorem exercise_18_8b {X Y : Type*} [topological_space X] [topological_space Y]
  [ordered_topology Y] (f g : X → Y) (hf : continuous f) (hg : continuous g) :
  continuous (λ x, min (f x) (g x)) :=
sorry

theorem exercise_19_6a {α : Type*} {β : α → Type*}
  [∀ a, metric_space (β a)] {x : α → ℝ} {y : α → ℝ}
  (hx : ∀ a, tendsto (λ n, x n a) at_top (𝓝 (y a))) :
  tendsto (λ n, (x n) a) at_top (𝓝 (y a)) :=
sorry

theorem exercise_20_2 {α β : Type*} 
  [topological_space α] [topological_space β] :
  metrizable_space (prod_dict_order_topology α β) :=
sorry

theorem exercise_21_6a {x : ℝ} (hx : 0 ≤ x) 
  (hx1 : x ≤ 1) :
  ∀ (n : ℕ), tendsto (λ (n : ℕ), x ^ n) at_top (𝓝 (x ^ n)) :=
sorry

theorem exercise_21_6b (n : ℕ) :
  ¬ uniform_limit (λ (n : ℕ), (λ (x : ℝ), x ^ n) ∘ (λ (x : ℝ), x / 2))
    (λ (x : ℝ), x ^ n) (Icc 0 1) :=
sorry

theorem exercise_21_8  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_22_2a {X Y : Type*}
  [topological_space X] [topological_space Y] (p : X → Y) (hpc : continuous p)
  (f : Y → X) (hfc : continuous f) (h : p ∘ f = id) :
  quotient_map p :=
sorry

theorem exercise_22_2b {X Y : Type*} [topological_space X] 
  [topological_space Y] (f : X → Y) (hf : continuous f) (hf_eq : ∀ x : X, f x = x) :
  quotient_map f :=
sorry

theorem exercise_22_5 {X Y : Type*} [topological_space X]
  [topological_space Y] (p : X → Y) (h : open_map p) (A : set X) (hA : is_open A) :
  open_map (p.restrict A) :=
sorry

theorem exercise_23_11 {X Y : Type*} [topological_space X]
  [topological_space Y] (p : X → Y) (hp : quotient_map p)
  (hY : connected_space Y) (hX : ∀ y : Y, connected_space (p ⁻¹' {y})) :
  connected_space X :=
sorry

theorem exercise_23_2 {X : Type*} [topological_space X]
  {n : ℕ} (h : ∀ n : ℕ, connected (A n)) (h₁ : ∀ n : ℕ, A n ∩ A (n + 1) ≠ ∅) :
  connected (⋃ n, A n) :=
sorry

theorem exercise_23_3 {X : Type*} 
  [topological_space X] {A : set X} (hA : is_connected A) 
  {Aα : Type*} {B : Aα → set X} (hB : ∀ (α : Aα), is_connected (B α)) 
  (h : ∀ (α : Aα), A ∩ B α ≠ ∅) :
  is_connected (A ∪ (⋃ (α : Aα), B α)) :=
sorry

theorem exercise_23_4 {X : Type*} [fintype X] (hX : ¬fintype.card X = 1) :
  connected_space (finite_compl_topology X) :=
sorry

theorem exercise_23_6 {X : Type*} [topological_space X]
  (A : set X) (C : set X) (hC : is_connected C) (hC_inter_A : C ∩ A ≠ ∅)
  (hC_inter_compl_A : C ∩ set.compl A ≠ ∅) :
  C ∩ set.boundary A ≠ ∅ :=
sorry

theorem exercise_23_9  {X Y : Type*} [topological_space X] [topological_space Y]
  (hX : connected_space X) (hY : connected_space Y)
  (A : set X) (hA : is_proper_subset A) (B : set Y) (hB : is_proper_subset B) :
  connected_space ((X × Y) \ (A × B)) :=
sorry

theorem exercise_24_2 {f : circle → ℝ} 
  (hf : continuous f) :
  ∃ (x : circle), f x = f (-x) :=
sorry

theorem exercise_24_3a {X : Type*} [topological_space X]
  [compact_space X] {f : X → X} (hf : continuous f) :
  ∃ (x : X), f x = x :=
sorry

theorem exercise_25_4 
  {X : Type*} [topological_space X] [locally_path_connected_space X] 
  (U : set X) (hU : is_open U) (hUc : is_connected U) :
  path_connected_space U :=
sorry

theorem exercise_25_9 {G : Type*} [topological_space G]
  [topological_group G] (C : set G) (hC : is_connected C)
  (hC_comp : C ∈ components G) :
  is_normal_subgroup C :=
sorry

theorem exercise_26_11  {X : Type*} [topological_space X] [compact_space X] [t2_space X]
  {Y : Type*} [topological_space Y] [t2_space Y]
  {f : X → Y} (hf : continuous f) (hf_conn : ∀ x, is_connected (f ⁻¹' {x}))
  (hf_closed : ∀ x, is_closed (f ⁻¹' {x}))
  (hf_simply_ordered : ∀ x y, x ≠ y → ∃ z, f ⁻¹' {z} ⊆ f ⁻¹' {x} ∧ f ⁻¹' {z} ⊆ f ⁻¹' {y}) :
  is_connected (f ⁻¹' univ) :=
sorry

theorem exercise_26_12 {X Y : Type*} [topological_space X]
  [topological_space Y] [compact_space Y] (p : X → Y) (hc : continuous p)
  (hsc : surjective p) (hpc : ∀ y : Y, compact_space (p ⁻¹' {y})) :
  compact_space X :=
sorry

theorem exercise_27_4 {X : Type*} [metric_space X]
  (hX : connected X) (hX_ne_singleton : X ≠ {x : X | x = x}) :
  nonempty (fintype.card_pos_iff_ne_zero.1 (fintype.card_univ X)) :=
sorry

theorem exercise_28_4 {X : Type*} [topological_space X]
  (hX : t1_space X) (hX₁ : limit_point_compact X) :
  countably_compact X :=
sorry

theorem exercise_28_5  {X : Type*} [topological_space X] (hX : countably_compact X) :
  ∀ (C : ℕ → set X), (∀ n, is_closed (C n)) → (∀ n, C n ≠ ∅) →
  ∃ (x : X), ∀ n, x ∈ C n :=
sorry

theorem exercise_28_6 {X : Type*} [metric_space X]
  (hX : compact_space X) (f : X → X) (hf : ∀ x y : X, dist x y = dist (f x) (f y)) :
  function.bijective f :=
sorry

theorem exercise_29_1 : ¬ locally_compact_space ℚ :=
sorry

theorem exercise_29_10 {X : Type*} 
  [topological_space X] [t2_space X] [locally_compact_space X] (x : X) 
  (U : set X) (hU : is_open U) (hxU : x ∈ U) :
  ∃ (V : set X), is_open V ∧ compact_space (closure V) ∧ closure V ⊆ U :=
sorry

theorem exercise_29_4 :
  ¬ locally_compact_space (uniform_space.prod_Ioo_omega) :=
sorry

theorem exercise_30_10 {ι : Type*} {X : ι → Type*}
  [∀ i, topological_space (X i)] [∀ i, countable_topology (X i)]
  [∀ i, dense_subset (X i)] :
  dense_subset (prod_topology X) :=
sorry

theorem exercise_30_13 {X : Type*} 
  [topological_space X] (hX : countable (dense_countable X)) 
  (h : ∀ (U : set X), is_open U → ∀ (V : set X), is_open V → U ≠ V → U ∩ V = ∅) :
  countable (set.range (λ (U : set X), U)) :=
sorry

theorem exercise_31_1 {X : Type*} [topological_space X]
  (hX : regular_space X) (x y : X) :
  ∃ (U V : set X), is_open U ∧ is_open V ∧ x ∈ U ∧ y ∈ V ∧ closure U ∩ closure V = ∅ :=
sorry

theorem exercise_31_2 
  {X : Type*} [topological_space X] (hX : normal_space X) 
  {A B : set X} (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
  ∃ (U V : set X), is_open U ∧ is_open V ∧ A ⊆ U ∧ B ⊆ V ∧ disjoint (closure U) (closure V) :=
sorry

theorem exercise_31_3 {α : Type*} [linear_order α] :
  regular_space (order_topology α) :=
sorry

theorem exercise_32_1 {X : Type*} [topological_space X]
  [normal_space X] (Y : set X) (hY : is_closed Y) : normal_space Y :=
sorry

theorem exercise_32_2a {ι : Type*} {X : ι → Type*}
  [∀ i, nonempty (X i)] [∀ i, topological_space (X i)]
  (h : hausdorff_space (Π i, X i)) :
  ∀ i, hausdorff_space (X i) :=
sorry

theorem exercise_32_2b {α : Type*} {X : α → Type*} [∀ a, regular_space (X a)]
  [∀ a, nonempty (X a)] (h : regular_space (Π a, X a)) :
  ∀ a, regular_space (X a) :=
sorry

theorem exercise_32_2c {α : Type*} {X : α → Type*} [∀ a, topological_space (X a)]
  [∀ a, nonempty (X a)] [∀ a, normal_space (X a)]
  (h : normal_space (prod_topology X)) :
  ∀ a, normal_space (X a) :=
sorry

theorem exercise_32_3 {X : Type*} [topological_space X]
  (hX : locally_compact_space X) (hX' : hausdorff_space X) :
  regular_space X :=
sorry

theorem exercise_33_7 {X : Type*} 
  [topological_space X] [locally_compact_space X] [t2_space X] :
  completely_regular_space X :=
sorry

theorem exercise_33_8 {X : Type*} [topological_space X]
  (hX : completely_regular_space X) (A B : set X) (hA : is_closed A) 
  (hB : is_closed B) (hAB : A ∩ B = ∅) (hA_compact : compact_space A) :
  ∃ (f : X → ℝ), continuous f ∧ f '' A = {0} ∧ f '' B = {1} :=
sorry

theorem exercise_34_9  {X : Type*} [topological_space X] [compact_space X] [t2_space X]
  {X₁ X₂ : set X} (hX₁ : is_closed X₁) (hX₂ : is_closed X₂)
  (hX₁m : metrizable_space X₁) (hX₂m : metrizable_space X₂)
  (hX₁X₂ : X₁ ∪ X₂ = univ) : metrizable_space X :=
sorry

theorem exercise_38_4 
  {X Y : Type*} [compact_space Y] [compactification X Y] :
  ∃ (g : β X → Y), continuous g ∧ surjective g ∧ closed_map g ∧ g ∘ i = id :=
sorry

theorem exercise_38_6 {X : Type*} [topological_space X]
  [comp_reg_space X] (hX : connected_space X) :
  connected_space (compactification X) :=
sorry

theorem exercise_43_2 {X Y : Type*} [metric_space X] 
  [metric_space Y] [complete_space Y] (A : set X) (f : A → Y) 
  (hf : uniform_continuous f) :
  ∃ (g : closure A → Y), continuous g ∧ uniform_continuous g ∧ 
  ∀ (x : closure A), x ∈ A → g x = f x :=
sorry