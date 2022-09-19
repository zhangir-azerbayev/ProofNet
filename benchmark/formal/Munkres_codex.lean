theorem exercise_13.1
 munkres_13_1 (X : Type*) [topological_space X] (A : set X) 
    (hA : is_open A) (B : set X) (hB : is_open B) (C : set X) 
    (hC : is_open C) (hAB : A ∩ B = ∅) (hBC : B ∩ C = ∅) : 
    A ∩ C = ∅ 

theorem exercise_13.5a
 topological_space.generate_from_eq_of_basis {α : Type u} 
    [topological_space α] (s : set (set α)) (h : is_topological_basis s) :
    topological_space.generate_from s = 
    {t : set (set α) | ∀ u ∈ s, u ⊆ t → t ∈ s} 

theorem exercise_13.5b
 subbasis_topology_eq_inter_nhds (X : Type*) (A : set (set X)) 
    (hA : is_subbasis A) :
    @topology.generate X A = ⋂₀ {U | is_topology U ∧ A ⊆ U} 

theorem exercise_16.1
 subspace.topology_eq_of_subset {X : Type*} [topological_space X] 
    {Y : set X} [is_subspace Y] {A : set Y} [is_subspace A] (hA : A ⊆ Y) :
    subtype.topology A = subtype.topology (A : set X) 

theorem exercise_16.4
 is_open_pi_1 {X Y : Type*} [topological_space X] [topological_space Y] :
    is_open (pi_1 : X × Y → X) 

theorem exercise_16.6
 is_topological_basis_of_rational_intervals (a b c d : ℚ) 
    (hab : a < b) (hcd : c < d) :
    is_topological_basis (set.prod (set.Ioo a b) (set.Ioo c d)) 

theorem exercise_16.9
 dictionary_order_topology_eq_prod_topology (X Y : Type*) 
    [topological_space X] [topological_space Y] :
    topological_space.induced (prod.lex X Y) = 
    topological_space.prod (topological_space.discrete X) 
    (topological_space.induced Y) 

theorem exercise_17.2
 is_closed_of_is_closed_of_is_closed {X : Type*} [topological_space X]
    {Y : Type*} [topological_space Y] {A : set Y} (hA : is_closed A) 
    (hY : is_closed (set.range (λ x : X, x))) :
    is_closed (set.range (λ x : X, x)) 

theorem exercise_17.3
 is_closed_prod {X Y : Type*} [topological_space X] [topological_space Y]
    (A : set X) (B : set Y) (hA : is_closed A) (hB : is_closed B) :
    is_closed (set.prod A B) 

theorem exercise_17.4
 is_open_diff_closed {X : Type*} [topological_space X] 
    (U : set X) (hU : is_open U) (A : set X) (hA : is_closed A) :
    is_open (U \ A) ∧ is_closed (A \ U) 

theorem exercise_18.8a
 is_closed_le {X : Type*} [topological_space X] {Y : Type*} 
    [topological_space Y] [ordered_topology Y] {f g : X → Y} 
    (hf : continuous f) (hg : continuous g) : 
    is_closed {x | f x ≤ g x} 

theorem exercise_18.8b
 continuous_min {X : Type*} [topological_space X] {Y : Type*} 
    [topological_space Y] [ordered_topology Y] {f g : X → Y} 
    (hf : continuous f) (hg : continuous g) :
    continuous (λ x, min (f x) (g x)) 

theorem exercise_18.13
 unique_extension_of_continuous_function_of_compact_subset 
    {X : Type*} [topological_space X] {Y : Type*} [topological_space Y] 
    [t2_space Y] {A : set X} {f : A → Y} (hf : continuous f) 
    (hA : is_compact A) (h : ∃ (g : closure A → Y), continuous g ∧ 
    ∀ (x : closure A), x ∈ A → g x = f x) :
    ∃! (g : closure A → Y), continuous g ∧ ∀

theorem exercise_19.4
 prod_assoc {α : Type u} {β : Type v} {γ : Type w} 
    [topological_space α] [topological_space β] [topological_space γ] :
    (α × β) × γ ≃ α × (β × γ) 

theorem exercise_19.6a
 prod_topology.tendsto_iff_tendsto_finset {ι : Type*} 
    {f : ι → Type*} [∀ i, topological_space (f i)] {s : set ι} 
    {x : Π i, f i} {y : Π i, f i} :
    tendsto (λ i, (x i, y i)) (prod_topology s) (prod.topological_space (λ i, f i)) 
    ↔ ∀ i ∈ s, tendsto (λ i, x i) (prod_topology s

theorem exercise_19.9
 choice_iff_prod_nonempty {α : Type u} {β : α → Type v} 
    (h : ∀ a, nonempty (β a)) :
    nonempty (Π a, β a) ↔ ∀ a, nonempty (β a) 

theorem exercise_20.2
 is_metrizable_prod_dict_order (X Y : Type*) [topological_space X] 
    [topological_space Y] : is_metrizable_space (X × Y) 

theorem exercise_20.5
 closure_of_R_infty_in_R_omega : 
    closure (set.range (λ (n : ℕ), 0)) = set.range (λ (n : ℕ), 0) 

theorem exercise_21.6a
 converges_to_pow_n_of_converges_to_one {x : ℝ} (hx : 0 ≤ x) (hx1 : x ≤ 1) 
    (h : tendsto (λ n, x ^ n) at_top (𝓝 1)) :
    tendsto (λ n, x ^ n) at_top (𝓝 x ^ 1) 

theorem exercise_21.6b
 not_uniform_converges_to_pow_n (n : ℕ) : 
    ∀ (ε : ℝ), ε > 0 → ∃ (x : ℝ), x ∈ Icc 0 1 ∧ abs (x ^ n - 1) ≥ ε 

theorem exercise_21.8
 uniform_limit_of_continuous_functions_converges_to_limit_of_sequence 
    {X : Type*} [topological_space X] {Y : Type*} [metric_space Y] 
    {f : ℕ → X → Y} {x : ℕ → X} (hf : uniform_limit f f) 
    (hx : tendsto x at_top (𝓝 x)) :
    tendsto (λ n, f n (x n)) at_top (𝓝 (f 0 x)) 

theorem exercise_22.2a
 continuous_iff_quotient_map {X Y : Type*} [topological_space X] 
    [topological_space Y] (p : X → Y) (h : continuous p) :
    quotient_map p ↔ ∃ (f : Y → X), continuous f ∧ p ∘ f = id 

theorem exercise_22.2b
 retraction_is_quotient_map {X : Type*} [topological_space X] 
    {A : set X} (r : X → A) (hr : continuous r) (h : ∀ x ∈ A, r x = x) :
    quotient_map r 

theorem exercise_22.5
 is_open_of_is_open_map {X Y : Type*} [topological_space X] 
    [topological_space Y] (p : X → Y) (hp : is_open_map p) 
    (A : set X) (hA : is_open A) : is_open_map (p ∘ subtype.val : A → Y) 

theorem exercise_23.2
 is_connected_of_connected_inter_connected {X : Type*} 
    [topological_space X] {A : ℕ → set X} (hA : ∀ n, is_connected (A n)) 
    (hAn : ∀ n, A n ∩ A (n + 1) ≠ ∅) :
    is_connected (⋃ n, A n) 

theorem exercise_23.3
 connected_union_of_connected_subsets {X : Type*} [topological_space X]
    (A : set X) (hA : is_connected A) (As : set (set X)) 
    (hAs : ∀ (A' : set X), A' ∈ As → is_connected A') 
    (hAAs : ∀ (A' : set X), A' ∈ As → A ∩ A' ≠ ∅) :
    is_connected (A ∪ (⋃₀ As)) 

theorem exercise_23.4
 connected_fintype_iff_infinite {X : Type*} [fintype X] :
  connected (finite_compl_topology X) ↔ infinite X 

theorem exercise_23.6
 connected_of_connected_of_subset {X : Type*} [topological_space X]
    {A B : set X} (hA : is_connected A) (hB : is_connected B) (hAB : A ⊆ B) :
    is_connected B 

theorem exercise_23.9
 connected_of_connected_prod_of_connected_subset {X Y : Type*} 
    [topological_space X] [topological_space Y] (hX : connected X) 
    (hY : connected Y) (A : set X) (B : set Y) (hA : is_proper_subset A X) 
    (hB : is_proper_subset B Y) :
    connected ((X × Y) \ (A × B)) 

theorem exercise_23.11
 connected_of_connected_quotient_map {X : Type*} [topological_space X]
    {Y : Type*} [topological_space Y] (p : X → Y) (hq : quotient_map p)
    (hY : connected Y) (hX : ∀ y : Y, connected (p ⁻¹' {y})) :
    connected X 

theorem exercise_23.12
 connected_of_connected_of_separation {X Y : Type*} [topological_space X] [topological_space Y]
    (hX : connected X) (hY : connected Y) (hXY : Y ⊆ X) (A B : set X) 
    (hA : is_open A) (hB : is_open B) (hAB : A ∪ B = X) (hAB' : A ∩ B = ∅) 
    (hAY : A ∩ Y = ∅) (hBY : B ∩ Y = ∅) :
    connected (Y ∪ A) ∧ connected (

theorem exercise_24.2
 exists_of_continuous_map_of_sphere_to_real {f : sphere 1 → ℝ} 
    (hf : continuous f) : ∃ (x : sphere 1), f x = f (-x) 

theorem exercise_24.3a
 exists_fixed_point {X : Type*} [topological_space X] 
    (f : X → X) (hf : continuous f) (hX : X = Icc 0 1) :
    ∃ (x : X), f x = x 

theorem exercise_24.4
 connected_linear_continuum {X : Type*} [linear_order X] 
    [topological_space X] [order_topology X] (hX : connected_space X) :
    linear_continuum X 

theorem exercise_24.6
 linear_continuum_of_well_order {X : Type*} [linear_order X] 
    (hX : well_order X) : linear_continuum (X × Icc 0 1) 

theorem exercise_25.4
 connected_of_locally_path_connected {X : Type*} [topological_space X]
    [locally_path_connected_space X] (U : set X) (hU : is_open U) 
    (hcU : connected U) : path_connected U 

theorem exercise_25.9
 component_is_normal (G : Type*) [topological_group G] 
    (C : set G) (hC : is_connected G C) : 
    is_normal G C 

theorem exercise_26.9
 compact_prod_compact_subset_of_open_prod_open {X : Type*} 
    [topological_space X] {Y : Type*} [topological_space Y] 
    {A : set X} {B : set Y} (hA : is_compact A) (hB : is_compact B) 
    (N : set (X × Y)) (hN : is_open N) (hAB : A × B ⊆ N) :
    ∃ (U : set X) (V : set Y), is_open U ∧ is_open V ∧ A × B

theorem exercise_26.11
 connected_of_connected_inter_closed_connected_subsets 
    {X : Type*} [topological_space X] [compact_space X] [t2_space X] 
    (A : set (set X)) (hA : ∀ (a b : set X), a ∈ A → b ∈ A → a ⊆ b ∨ b ⊆ a) 
    (hA' : ∀ (a : set X), a ∈ A → is_closed a) (hA'' : ∀ (a : set X), a ∈ A → connected a) :
    connected (⋂ (a : set

theorem exercise_26.12
 compact_of_perfect_map_of_compact {X Y : Type*} [topological_space X]
    [topological_space Y] (p : X → Y) (hc : continuous p) (hp : ∀ y : Y, 
    is_compact (p ⁻¹' {y})) (hY : is_compact Y) : is_compact X 

theorem exercise_27.1
 exists_sup_of_bdd_above {X : Type*} [linear_order X] 
    (s : set X) (hs : bdd_above s) :
    ∃ (x : X), is_lub s x 

theorem exercise_27.4
 uncountable_of_connected_metric_space_with_more_than_one_point 
    (X : Type*) [metric_space X] [connected_space X] (hX : ∃ x y : X, x ≠ y) :
    uncountable X 

theorem exercise_28.4
 countably_compact_iff_limit_point_compact {X : Type*} 
    [topological_space X] (hT1 : t1_space X) :
    countably_compact_space X ↔ limit_point_compact_space X 

theorem exercise_28.5
 countably_compact_iff_seq_compact {X : Type*} [topological_space X] :
  countably_compact X ↔ seq_compact X 

theorem exercise_28.6
 isometry_of_compact_is_homeomorphism {X : Type*} [metric_space X] 
    [compact_space X] {f : X → X} (hf : isometry f) : 
    function.bijective f ∧ homeomorphism f 

theorem exercise_29.1
 not_locally_compact_rat : ¬ locally_compact_space ℚ 

theorem exercise_29.4
 not_locally_compact_uniform_product_Ioo_omega : 
  ¬ locally_compact_space (uniform_space.product_topology 
    (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) 
    (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) (Ioo 0 1) 
    (Ioo 0 1) (Ioo 0 1) (Ioo 0 1

theorem exercise_29.5
 homeomorphism_of_one_point_compactifications {X₁ : Type*} 
    [topological_space X₁] [locally_compact_space X₁] 
    [compact_space X₁] [t2_space X₁] {X₂ : Type*} [topological_space X₂] 
    [locally_compact_space X₂] [compact_space X₂] [t2_space X₂] 
    (f : X₁ → X₂) (hf : homeomorphism f

theorem exercise_29.6
 homeomorphic_one_point_compactification_real_circle :
  one_point_compactification ℝ ≃_h circle 

theorem exercise_29.10
 exists_compact_closure_subset_of_neighborhood {X : Type*} 
    [topological_space X] [t2_space X] (x : X) (hx : is_locally_compact_at x) 
    (U : set X) (hU : is_open U) (hxU : x ∈ U) :
    ∃ (V : set X), is_open V ∧ x ∈ V ∧ compact (closure V) ∧ closure V ⊆ U 

theorem exercise_30.10
 countable_dense_subset_of_countable_prod_of_countable_dense_subsets 
    {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)] 
    (h : ∀ i, ∃ (s : set (X i)), countable s ∧ dense s) :
    ∃ (s : set (Π i, X i)), countable s ∧ dense s 

theorem exercise_30.13
 countable_of_countable_dense_subset {X : Type*} [topological_space X]
    (hX : countable_dense_subset X) (U : set (set X)) 
    (hU : ∀ (x y : set X), x ∈ U → y ∈ U → x ≠ y → x ∩ y = ∅) :
    countable U 

theorem exercise_31.1
 regular_of_disjoint_closure {X : Type*} [topological_space X] 
    (hX : regular_space X) (x y : X) :
    ∃ (U V : set X), is_open U ∧ is_open V ∧ x ∈ U ∧ y ∈ V ∧ closure U ∩ closure V = ∅ 

theorem exercise_31.2
 normal.disjoint_closed_of_disjoint_neighborhoods {X : Type*} 
    [topological_space X] [normal_space X] {A B : set X} 
    (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
    ∃ (U V : set X), is_open U ∧ is_open V ∧ A ⊆ U ∧ B ⊆ V ∧ disjoint (closure U) (closure V) 

theorem exercise_31.3
 regular_of_order_topology {α : Type*} [order_topology α] : regular_space α 

theorem exercise_32.1
 normal_of_closed_subset {X : Type*} [topological_space X] 
    (hX : normal_space X) (A : set X) (hA : is_closed A) : 
    normal_space (subtype.topological_space A) 

theorem exercise_32.2
 prod_topological_space.regular_of_regular 
    {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)] 
    (h : ∀ i, nonempty (X i)) (hreg : regular_space (Π i, X i)) :
    ∀ i, regular_space (X i) 

theorem exercise_32.2
 regular_of_locally_compact_hausdorff {X : Type*} [topological_space X]
    (hX : locally_compact_space X) (hX' : hausdorff_space X) :
    regular_space X 

theorem exercise_33.7
 is_completely_regular_of_is_locally_compact_hausdorff 
    (X : Type*) [topological_space X] (hX : is_locally_compact_hausdorff X) :
    is_completely_regular X 

theorem exercise_33.8
 exists_continuous_function_of_disjoint_compact_closed_sets 
    (X : Type*) [topological_space X] [t2_space X] [compact_space X] 
    (A B : set X) (hA : is_closed A) (hB : is_closed B) 
    (hAB : disjoint A B) :
    ∃ (f : X → ℝ), continuous f ∧ f '' A = {0} ∧ f '' B = {1} 

theorem exercise_34.9
 metrizable_of_compact_union_of_metrizable_closed_subsets 
    (X : Type*) [topological_space X] [compact_space X] 
    (X1 X2 : set X) (hX1 : is_closed X1) (hX2 : is_closed X2) 
    (hX : X = X1 ∪ X2) (hX1m : metrizable_space X1) 
    (hX2m : metrizable_space X2) : metrizable_space X 

theorem exercise_37.2
 lindelof_iff_countable_intersection_property {X : Type*} 
    [topological_space X] :
    lindelof X ↔ ∀ (A : set (set X)), countable_intersection_property A → 
    ∃ (x : X), ∀ (a : set X), a ∈ A → x ∈ closure a 

theorem exercise_38.4
 exists_surjective_closed_map_of_compactification {X : Type*} 
    [topological_space X] {Y : Type*} [compact_space Y] 
    (hY : is_compactification X Y) :
    ∃ (g : β X → Y), continuous g ∧ surjective g ∧ closed_map g ∧ g ∘ β.extend = id 

theorem exercise_38.6
 is_t_1_of_is_completely_regular (X : Type*) [topological_space X] 
    (hX : is_completely_regular X) : is_t_1 X 

theorem exercise_39.5
 countable_basis_of_countable_topology {X : Type*} [topological_space X]
    (hX : countable_topology X) :
    ∃ (B : set (set X)), is_topological_basis B ∧ countable B 

theorem exercise_43.2
 uniformly_continuous_extend_to_closure {X : Type*} [metric_space X]
    {Y : Type*} [metric_space Y] [complete_space Y] (A : set X) 
    (f : A → Y) (hf : uniform_continuous f) :
    ∃ (g : closure A → Y), continuous g ∧ uniform_continuous g ∧ 
    ∀ (x : A), g x = f x 

theorem exercise_43.7
 complete_l2_seq (X : Type*) [normed_group X] [normed_space ℝ X] :
  complete_space (l2_seq X) 