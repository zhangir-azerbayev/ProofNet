theorem exercise_13_1 (X : Type*) [topological_space X]
  (A : set X) (hA : ∀ x ∈ A, ∃ U : set X, is_open U ∧ x ∈ U ∧ U ⊆ A): 
  is_open A :=
sorry

theorem exercise_13_5a {X : Type*} 
    [topological_space X] (A : set (set X)) (hA : is_basis A) :
    topological_space.generate_from A = 
    {T : set (set X) | ∀ (U : set X), U ∈ T → ∃ (V : set X), is_open V ∧ V ⊆ U ∧ ∃ (a : set X), a ∈ A ∧ a ⊆ V} :=
sorry

theorem exercise_13_5b (X : Type*) (A : set (set X)) 
    (hA : is_subbasis A) :
    @topology.generate X A = ⋂₀ {U | is_topology U ∧ A ⊆ U} :=
sorry

theorem exercise_16_1 {X : Type*} [topological_space X] 
    {Y : set X} [is_subspace Y] {A : set Y} [is_subspace A] (hA : A ⊆ Y) :
    subtype.topology A = subtype.topology (A : set X) :=
sorry

theorem exercise_16_4 {X Y : Type*} [topological_space X] [topological_space Y] :
    is_open (pi_1 : X × Y → X) :=
sorry

theorem exercise_16_6 : 
    is_topological_basis (set.range (λ (p : ℕ × ℕ), 
    (Icc (p.1 : ℚ) (p.2 : ℚ)) × (Icc (p.1 : ℚ) (p.2 : ℚ)))) :=
sorry

theorem exercise_16_9 (X Y : Type*) 
    [topological_space X] [topological_space Y] :
    topological_space.induced (prod.lex X Y) = 
    topological_space.prod (topological_space.discrete X) 
    (topological_space.induced Y) :=
sorry

theorem exercise_17_2 {X : Type*} [topological_space X]
    {Y : Type*} [topological_space Y] {A : set Y} (hA : is_closed A)
    (hY : is_closed (set.range (subtype.val : Y → X))) :
    is_closed (subtype.val '' A) :=
sorry

theorem exercise_17_3 {X Y : Type*} [topological_space X] [topological_space Y]
    (A : set X) (B : set Y) (hA : is_closed A) (hB : is_closed B) :
    is_closed (set.prod A B) :=
sorry

theorem exercise_17_4 {X : Type*} [topological_space X] 
    (U : set X) (hU : is_open U) (A : set X) (hA : is_closed A) :
    is_open (U \ A) ∧ is_closed (A \ U) :=
sorry

theorem exercise_18_8a {X : Type*} [topological_space X] {Y : Type*} 
    [topological_space Y] [ordered_topology Y] {f g : X → Y} 
    (hf : continuous f) (hg : continuous g) : 
    is_closed {x | f x ≤ g x} :=
sorry

theorem exercise_18_8b {X : Type*} [topological_space X] {Y : Type*} 
    [topological_space Y] [ordered_topology Y] {f g : X → Y} 
    (hf : continuous f) (hg : continuous g) :
    continuous (λ x, min (f x) (g x)) :=
sorry

theorem exercise_18_13 
    {X : Type*} [topological_space X] {Y : Type*} [topological_space Y] 
    [t2_space Y] {A : set X} {f : A → Y} (hf : continuous f) 
    (hA : is_compact A) (h : ∃ (g : closure A → Y), continuous g ∧ 
    ∀ (x : closure A), x ∈ A → g x = f x) :
    ∃! (g : closure A → Y), continuous g ∧ ∀ :=
sorry

theorem exercise_19_4 {α : Type u} {β : Type v} {γ : Type w} 
    [topological_space α] [topological_space β] [topological_space γ] 
    (h : α ≃ β) (x : α) : 
    (β × γ) ≃ (α × γ) :=
sorry

theorem exercise_19_6a {ι : Type*} 
    {f : ι → Type*} [∀ i, topological_space (f i)] {s : set ι} 
    {x : Π i, f i} {y : Π i, f i} :
    tendsto (λ i, (x i, y i)) (prod_topology s) (prod.topological_space (λ i, f i)) 
    ↔ ∀ i ∈ s, tendsto (λ i, x i) (prod_topology s :=
sorry

theorem exercise_19_9 {α : Type u} {β : α → Type v} 
    (h : ∀ a, nonempty (β a)) :
    nonempty (Π a, β a) ↔ ∀ a, nonempty (β a) :=
sorry

theorem exercise_20_2 (X Y : Type*) [topological_space X] 
    [topological_space Y] : is_metrizable_space (X × Y) :=
sorry

theorem exercise_20_5 : 
    closure (set.range (λ (n : ℕ), 0)) = set.range (λ (n : ℕ), 0) :=
sorry

theorem exercise_21_6a {x : ℝ} (hx : 0 ≤ x) (hx1 : x ≤ 1) 
    (h : tendsto (λ n, x ^ n) at_top (𝓝 1)) :
    tendsto (λ n, x ^ n) at_top (𝓝 x) :=
sorry

theorem exercise_21_6b (n : ℕ) : 
    ∀ (ε : ℝ), ε > 0 → ∃ (x : ℝ), x ∈ Icc 0 1 ∧ abs (x ^ n - 1) ≥ ε :=
sorry

theorem exercise_21_8 
    {X : Type*} [topological_space X] {Y : Type*} [metric_space Y] 
    {f : ℕ → X → Y} {x : ℕ → X} (hf : uniform_limit f (λ n, f n x n)) 
    (hx : tendsto x at_top (𝓝 x)) :
    tendsto (λ n, f n (x n)) at_top (𝓝 (f x)) :=
sorry

theorem exercise_22_2a {X Y : Type*} [topological_space X] 
    [topological_space Y] (p : X → Y) (h : continuous p) :
    quotient_map p ↔ ∃ (f : Y → X), continuous f ∧ p ∘ f = id :=
sorry

theorem exercise_22_2b {X : Type*} [topological_space X] 
    {A : set X} (r : X → A) (hr : continuous r) (h : ∀ x ∈ A, r x = x) :
    quotient_map r :=
sorry

theorem exercise_22_5 {X Y : Type*} [topological_space X] 
    [topological_space Y] (p : X → Y) (hp : is_open_map p) 
    (A : set X) (hA : is_open A) : is_open_map (p ∘ subtype.val : A → Y) :=
sorry

theorem exercise_23_2 {X : Type*} 
    [topological_space X] {A : ℕ → set X} (hA : ∀ n, is_connected (A n)) 
    (hAn : ∀ n, A n ∩ A (n + 1) ≠ ∅) :
    is_connected (⋃ n, A n) :=
sorry

theorem exercise_23_3 {X : Type*} [topological_space X]
    (A : set X) (hA : is_connected A) (As : set (set X)) 
    (hAs : ∀ (A' : set X), A' ∈ As → is_connected A') 
    (hAAs : ∀ (A' : set X), A' ∈ As → A ∩ A' ≠ ∅) :
    is_connected (A ∪ (⋃₀ As)) :=
sorry

theorem exercise_23_4 {X : Type*} [fintype X] :
  connected (finite_compl_topology X) ↔ infinite X :=
sorry

theorem exercise_23_6 {X : Type*} 
    [topological_space X] {A : set X} (hA : is_open A) (hc : is_connected X) 
    (C : set X) (hC : is_connected C) (hCX : C ⊆ X) (hCA : C ∩ A ≠ ∅) 
    (hCXA : C ∩ (X \ A) ≠ ∅) : C ∩ (boundary A) ≠ ∅ :=
sorry

theorem exercise_23_9 {X Y : Type*} 
    [topological_space X] [topological_space Y] (hX : connected X) 
    (hY : connected Y) (A : set X) (B : set Y) (hA : is_proper_subset A X) 
    (hB : is_proper_subset B Y) :
    connected ((X × Y) \ (A × B)) :=
sorry

theorem exercise_23_11 {X : Type*} [topological_space X]
    {Y : Type*} [topological_space Y] (p : X → Y) (hq : quotient_map p)
    (hY : connected Y) (hX : ∀ y : Y, connected (p ⁻¹' {y})) :
    connected X :=
sorry

theorem exercise_23_12 {X Y : Type*} [topological_space X] [topological_space Y]
    (hX : connected X) (hY : connected Y) (hXY : Y ⊆ X) (A B : set X) 
    (hA : is_open A) (hB : is_open B) (hAB : A ∪ B = X) (hAB' : A ∩ B = ∅) 
    (hAY : A ∩ Y = ∅) (hBY : B ∩ Y = ∅) :
    connected (Y ∪ A) ∧ connected ( :=
sorry

theorem exercise_24_2 {f : sphere 1 → ℝ} 
    (hf : continuous f) : ∃ (x : sphere 1), f x = f (-x) :=
sorry

theorem exercise_24_3a {X : Type*} [topological_space X] 
    (f : X → X) (hf : continuous f) (hX : X = Icc 0 1) :
    ∃ (x : X), f x = x :=
sorry

theorem exercise_24_4 {X : Type*} [linear_order X] 
    [topological_space X] [order_topology X] (hX : connected_space X) :
    linear_continuum X :=
sorry

theorem exercise_24_6 {X : Type*} [linear_order X] 
    (hX : well_order X) : linear_continuum (X × Icc 0 1) :=
sorry

theorem exercise_25_4 {X : Type*} [topological_space X]
    [locally_path_connected_space X] (U : set X) (hU : is_open U) 
    (hcU : connected U) : path_connected U :=
sorry

theorem exercise_25_9 (G : Type*) [topological_group G] 
    (C : set G) (hC : is_connected G C) : 
    is_normal G C :=
sorry

theorem exercise_26_9 {X : Type*} 
    [topological_space X] {Y : Type*} [topological_space Y] 
    {A : set X} {B : set Y} (hA : is_compact A) (hB : is_compact B) 
    (N : set (X × Y)) (hN : is_open N) (hAB : A × B ⊆ N) :
    ∃ (U : set X) (V : set Y), is_open U ∧ is_open V ∧ A × B :=
sorry

theorem exercise_26_11 
    {X : Type*} [topological_space X] [compact_space X] [t2_space X] 
    (A : set (set X)) (hA : ∀ (a b : set X), a ∈ A → b ∈ A → a ⊆ b ∨ b ⊆ a) 
    (hA' : ∀ (a : set X), a ∈ A → is_closed a) (hA'' : ∀ (a : set X), a ∈ A → connected a) :
    connected (⋂ (a : set :=
sorry

theorem exercise_26_12 {X Y : Type*} [topological_space X]
    [topological_space Y] (p : X → Y) (hc : continuous p) (hp : ∀ y : Y, 
    is_compact (p ⁻¹' {y})) (hY : is_compact Y) : is_compact X :=
sorry

theorem exercise_27_1 (X : Type*) [linear_order X] 
    (hX : ∀ (a b : X), is_compact (Icc a b)) :
    has_lub X :=
sorry

theorem exercise_27_4 
    (X : Type*) [metric_space X] [connected_space X] (hX : ∃ x y : X, x ≠ y) :
    uncountable X :=
sorry

theorem exercise_28_4 {X : Type*} 
    [topological_space X] (hT1 : t1_space X) :
    countably_compact_space X ↔ limit_point_compact_space X :=
sorry

theorem exercise_28_5 
    (X : Type*) [topological_space X] :
    countably_compact X ↔ ∀ (C : ℕ → set X), (∀ n, is_closed (C n)) ∧ 
    (∀ n, C n ≠ ∅) ∧ (∀ n, C n ⊆ C (n + 1)) → ∃ x, ∀ n, x ∈ C n :=
sorry

theorem exercise_28_6 {X : Type*} [metric_space X] 
    [compact_space X] {f : X → X} (hf : isometry f) : 
    function.bijective f :=
sorry

theorem exercise_29_1 : ¬ locally_compact_space ℚ :=
sorry

theorem exercise_29_4 : 
  ¬ locally_compact_space (uniform_space.product_topology 
    (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) 
    (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) 
    (Ioo 0 1) (Ioo 0 1) (Ioo 0 1 :=
sorry

theorem exercise_29_5 {X₁ : Type*} 
    [topological_space X₁] [locally_compact_space X₁] 
    [compact_space X₁] [t2_space X₁] {X₂ : Type*} [topological_space X₂] 
    [locally_compact_space X₂] [compact_space X₂] [t2_space X₂] 
    {f : X₁ → X₂} (hf : homeomorphism f :=
sorry

theorem exercise_29_6 :
  one_point_compactification ℝ ≃_h circle :=
sorry

theorem exercise_29_10 {X : Type*} 
    [topological_space X] [t2_space X] (x : X) (hx : is_locally_compact_at x) 
    (U : set X) (hU : is_open U) (hxU : x ∈ U) :
    ∃ (V : set X), is_open V ∧ x ∈ V ∧ compact (closure V) ∧ closure V ⊆ U :=
sorry

theorem exercise_30_10 
    {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)] 
    (h : ∀ i, ∃ (s : set (X i)), countable s ∧ dense s) :
    ∃ (s : set (Π i, X i)), countable s ∧ dense s :=
sorry

theorem exercise_30_13 {X : Type*} [topological_space X]
    (hX : countable_dense_subset X) (U : set (set X)) 
    (hU : ∀ (x y : set X), x ∈ U → y ∈ U → x ≠ y → x ∩ y = ∅) :
    countable U :=
sorry

theorem exercise_31_1 {X : Type*} [topological_space X] 
    (hX : regular_space X) (x y : X) :
    ∃ (U V : set X), is_open U ∧ is_open V ∧ x ∈ U ∧ y ∈ V ∧ closure U ∩ closure V = ∅ :=
sorry

theorem exercise_31_2 {X : Type*} 
    [topological_space X] [normal_space X] {A B : set X} 
    (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
    ∃ (U V : set X), is_open U ∧ is_open V ∧ A ⊆ U ∧ B ⊆ V ∧ disjoint (closure U) (closure V) :=
sorry

theorem exercise_31_3 {α : Type*} [partial_order α] 
    [topological_space α] (h : order_topology α) : regular_space α :=
sorry

theorem exercise_32_1 {X : Type*} [topological_space X] 
    (hX : normal_space X) (A : set X) (hA : is_closed A) : 
    normal_space (subtype.topological_space A) :=
sorry

theorem exercise_32_2 
    {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)] 
    (h : ∀ i, nonempty (X i)) (hreg : regular_space (Π i, X i)) :
    ∀ i, regular_space (X i) :=
sorry

theorem exercise_32_2 {X : Type*} [topological_space X]
    (hX : locally_compact_space X) (hX' : hausdorff_space X) :
    regular_space X :=
sorry

theorem exercise_33_7 
    (X : Type*) [topological_space X] (hX : is_locally_compact_hausdorff X) :
    is_completely_regular X :=
sorry

theorem exercise_33_8 
    (X : Type*) [topological_space X] [t2_space X] [compact_space X] 
    (A B : set X) (hA : is_closed A) (hB : is_closed B) 
    (hAB : disjoint A B) :
    ∃ (f : X → ℝ), continuous f ∧ f '' A = {0} ∧ f '' B = {1} :=
sorry

theorem exercise_34_9 
    (X : Type*) [topological_space X] [compact_space X] 
    (X1 X2 : set X) (hX1 : is_closed X1) (hX2 : is_closed X2) 
    (hX : X1 ∪ X2 = univ) (hX1m : metrizable_space X1) 
    (hX2m : metrizable_space X2) : metrizable_space X :=
sorry

theorem exercise_37_2 {X : Type*} 
    [topological_space X] :
    lindelof X ↔ ∀ (A : set (set X)), (∀ (a : set X), a ∈ A → is_closed a) → 
    (∀ (a : set X), a ∈ A → a ≠ ∅) → (∃ (a : set X), a ∈ A) → 
    (∃ (a : set X), a ∈ A ∧ a ≠ ∅) :=
sorry

theorem exercise_38_4 {X : Type*} 
    [topological_space X] {Y : Type*} [compact_space Y] 
    (hY : is_compactification X Y) :
    ∃ (g : β X → Y), continuous g ∧ surjective g ∧ closed_map g ∧ g ∘ β.extend = id :=
sorry

theorem exercise_38_6 {X : Type*} 
    [topological_space X] (hX : completely_regular_space X) :
    connected X ↔ connected (β X) :=
sorry

theorem exercise_39_5 
    {X : Type*} [topological_space X] (hX : countable_basis X) 
    (A : set (set X)) (hA : countably_locally_finite A) :
    countable A :=
sorry

theorem exercise_43_2 {X : Type*} [metric_space X]
    {Y : Type*} [metric_space Y] [complete_space Y] (A : set X) 
    (f : A → Y) (hf : uniform_continuous f) :
    ∃ (g : closure A → Y), continuous g ∧ uniform_continuous g ∧ 
    ∀ (x : A), g x = f x :=
sorry

theorem exercise_43_7 (X : Type*) [normed_group X] [normed_space ℝ X] :
  complete_space (l2_seq X) :=
sorry