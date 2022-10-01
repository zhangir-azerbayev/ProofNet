import topology.basic
import topology.constructions
import topology.bases
import topology.stone_cech
import topology.path_connected
import topology.metric_space.basic
import topology.metric_space.metrizable
import data.real.basic
import data.set.countable
import data.real.irrational
import deprecated.subgroup

open_locale classical
open set

theorem exercise_13_1 {X : Type*} [topological_space X]
  [subset A X] [subset B X] [subset C X] (hA : A ⊆ X) (hB : B ⊆ X) (hC : C ⊆ X)
  (h : open B) (hU : open (B ∩ A)) : open C :=
sorry

theorem exercise_13_3a {X : Type*} [set X] [set (X - U)]
  (h : X - U ⊆ X) : topology.subset_of_countable_difference (X - U) :=
sorry

theorem exercise_13_3b {X : Type*} [set X] :
  not_topology_on_set_of_infinite_or_empty_or_all_of_X :=
sorry

theorem exercise_13_4a1 {X : Type*} [topology X]
  (h : ∀ (α : finset X), ∃ (β : finset X), α ⊆ β) :
  ∃ (α : finset X), ∀ (β : finset X), α ⊆ β :=
sorry

theorem exercise_13_4b1 {X : Type*} [topology X] [family (topology X)]
  (h : ∀ (α : finset X), ∃ (t : topology X), t.union (λ (α : finset X), t.topology α)) :
  ∃ (t : topology X), t.union (λ (α : finset X), t.topology α) :=
sorry

theorem exercise_13_4b2 {X : Type*} [topology X]
  [family_of_topologies X] (h : ∀ (α : finset X), ∃ (t : topology X), t ∈ α) :
  ∃ (t : topology X), ∀ (α : finset X), t ∈ α :=
sorry

theorem exercise_13_5a {X : Type*} 
  [topology X] [basis X] [basis_generated_by_basis X]
  (hX : basis X) (h : topology.intersection_of_topologies_containing_basis X hX) :
  topology.generated_by_basis X :=
sorry

theorem exercise_13_5b {X : Type*} [topology X]
  [subbasis_for_topology X] [subbasis_for_topology_intersection X]
  (h : subbasis_for_topology X) (hA : subbasis_for_topology X)
  (hB : subbasis_for_topology_intersection X) :
  topology_generated_by_subbasis_intersection X :=
sorry

theorem exercise_13_6 {K : Type*} [field K] 
  [add_comm_group K] [module K K] [fintype K] [finite_dimensional K K] 
  {t : finset K} (h : finite_dimensional.finrank K K + 1 < t.card) :
  lower_limit_topology K ≠ K.topology :=
sorry

theorem exercise_13_8a {R : Type*} [field R] [add_comm_group R]
  [module R R] [fintype R] [rational R] [rational_field R]
  (h : basis R) : basis R :=
sorry

theorem exercise_13_8b {R : Type*} [field R] [add_comm_group R]
  [module R R] [rational R] [finite_dimensional R] [finite_dimensional_finite_field R]
  (h : finite_dimensional.finite_field R) :
  basis_topology R :=
sorry

theorem exercise_16_1 {X Y : Type*} [subspace X Y]
  (hX : subspace X) (hY : subspace Y) (hA : subset A)
  (h : topology.subspace_topology A) : topology.subspace_topology (subspace X A) :=
sorry

theorem exercise_16_4 {X Y : Type*} [metric_space X] [metric_space Y]
  (hX : compact_space X) (hY : compact_space Y) (f : X → Y) (g : Y → X)
  (hg : continuous g) (hf : function.injective f)
  (h : open_map f) : open_map (g ∘ f) :=
sorry

theorem exercise_16_6 {a b c d : ℚ} (h : a < b ∧ c < d) :
  basis_of_R2 :=
sorry

theorem exercise_17_4 {X : Type*} [topological_space X]
  (hU : open U) (hA : closed A) : open (U-A) :=
sorry

theorem exercise_18_13 {X Y : Type*} [hausdorff_space Y] [continuous_map X]
  (hX : continuous f) (hY : continuous g) : continuous (g ∘ f) :=
sorry

theorem exercise_18_8a {X Y : Type*} 
  [ordered_set Y] [continuous_map X Y] [continuous_map Y X]
  (hY : compact_space Y) (f : X → Y) (g : Y → Y) (hgc : continuous g)
  (hgi : function.injective g)
  (h : closed_set (g ∘ f)) : closed_set f :=
sorry

theorem exercise_18_8b {X Y Z : Type*} 
  [ordered_set Y] [ordered_set Z] [continuous_map X Y] [continuous_map X Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : continuous (g ∘ f)) : continuous f :=
sorry

theorem exercise_20_2 {R : Type*} [metric_space R] :
  dictionary_order (ℝ × ℝ) :=
sorry

theorem exercise_21_6a {x : ℝ} :
  sequence (λ n, x ^ n) converges to x :=
sorry

theorem exercise_21_8  {X Y Z : Type*} [topological_space X] [metric_space Y] [metric_space Z]
  (hX : compact_space X) (f : X → Y) (hf : continuous f)
  (h : uniform_continuous f) (hx : sequence.uniform_convergence f hX)
  (hY : sequence.uniform_convergence f hX) :
  sequence.uniform_convergence f (hf ∘ hX) :=
sorry

theorem exercise_22_2a {X Y Z : Type*} 
  [metric_space X] [metric_space Y] [metric_space Z]
  (hX : compact_space X) (hY : compact_space Y) (hZ : compact_space Z)
  (f : Y → X) (g : Y → Z) (hg : continuous g) (hf : continuous f)
  (h : quotient_map (g ∘ f)) : quotient_map (p ∘ f) :=
sorry

theorem exercise_22_2b {X A : Type*} [continuous X] [continuous A]
  [retraction_onto A] (r : X → A) (h : retraction_onto r) :
  retraction_onto r :=
sorry

theorem exercise_22_5 {X Y : Type*} [open_map X] [open_map Y]
  (p : open_map X) (A : open_subset X) (q : open_map (p.restrict A)) :
  open_map (p.restrict A) :=
sorry

theorem exercise_23_11 {X Y : Type*} [group X] [group Y]
  [connected Y] (p : X → Y) (h : quotient.connected p) :
  connected X :=
sorry

theorem exercise_23_2 {X : Type*} [connected X]
  [sequence A : list X] (h : ∀ n, A n ∩ A n-1 ≠ ∅) :
  connected (union A) :=
sorry

theorem exercise_23_4 {X : Type*} [topology X] :
  connected (X.finite_complement_topology) :=
sorry

theorem exercise_23_6 {X : Type*} 
  [connected_space X] [connected_space_intersection X]
  (h : connected X) (hA : connected A) (hC : connected C)
  (hC : connected (X-A)) : connected C :=
sorry

theorem exercise_23_9 {X Y : Type*} [connected X] [connected Y]
  (hX : connected X) (hY : connected Y) (hA : proper subset.subset X) (hB : proper subset.subset Y)
  (h : connected (X \times Y) - (A \times B)) : connected (X \times Y) :=
sorry

theorem exercise_24_2 {S : Type*} [continuous S] [continuous_map S]
  (f : S →* ℝ) (hf : continuous f) :
  continuous f.symmetric_image :=
sorry

theorem exercise_24_3a {X : Type*} [continuous X]
  (f : X → X) (x : X) : f x = x :=
sorry

theorem exercise_25_4 {X : Type*} [locally_path_connected X] :
  connected_open X → path_connected X :=
sorry

theorem exercise_25_9 {G : Type*} [topological_group G]
  (h : component G.e = G) : normal_subgroup G :=
sorry

theorem exercise_26_11 {X : Type*} [compact_space X]
  [connected X] [hausdorff X] [compact_space Y] [connected Y]
  (h : connected X) (hA : closed A) (hC : closed (intersection A)) :
  connected Y :=
sorry

theorem exercise_26_12 {X Y : Type*} [topological_space X] [topological_space Y]
  (h : closed_continuous_surjective_perfect_map X Y)
  (hc : compact_space Y) : compact_space X :=
sorry

theorem exercise_27_4 {X : Type*} [metric_space X]
  (h : connected X) : uncountable :=
sorry

theorem exercise_28_4 {X : Type*} [topological_space X]
  (hX : limit_point_compact X) : countably_compact X :=
sorry

theorem exercise_28_5 {X : Type*} [compact_space X]
  [countable_space X] (h : countable_space X) :
  countably_compact X ↔ ∃ (C : set X), ∀ (n : ℕ), C ⊆ C n ↔ C ⊆ C (n + 1) :=
sorry

theorem exercise_28_6 {X : Type*} [metric_space X] 
  [is_isometry X] (f : X → X) (h : f.is_isometry) (hc : compact_space X)
  (hbi : function.injective f)
  (h : f.is_bijective) :
  is_bijective f :=
sorry

theorem exercise_29_1 {Q : Type*} [field Q] [add_comm_group Q] [module Q Q] :
  not_locally_compact Q :=
sorry

theorem exercise_29_10 {X : Type*} [hausdorff_space X]
  (hx : local_compact_at_point X) (hU : open_neighborhood X x)
  (hV : compact_neighborhood X x) :
  compact_neighborhood X x :=
sorry

theorem exercise_29_4 {X : Type*} [uniform_topology X]
  (h : X.is_compact) : X.is_not_locally_compact :=
sorry

theorem exercise_30_10 {X : Type*} [countable X] :
  countable (X × X × ⋯) :=
sorry

theorem exercise_30_13  {X : Type*} [metric_space X] [countable_space X]
  (hX : countable_space X) (hO : disjoint_open_sets X)
  (h : countable_dense_subset X) : countable_collection hO :=
sorry

theorem exercise_31_1 {X : Type*} [regular X]
  (hX : X.is_regular) (h : X.is_compact) (hN : X.is_open)
  (hN1 : X.closure_neighborhoods.is_open) (hN2 : X.closure_neighborhoods.is_closed)
  (hN3 : X.closure_neighborhoods.is_closed) (hN4 : X.closure_neighborhoods.is_compact)
  (hN5 : X.closure_neighborhoods.is_compact) (hN6 : X.closure_neighborhoods.is_regular)
  (hN7 : X.closure_neighborhoods.is_regular) (hN8 : X.closure_neighborhoods.is_open)
  (hN9 : X.closure_neighborhoods.is_open) (hN10 : X.closure_neighborhoods.is_closed)
  (hN11 : X.closure_neighborhoods.is_closed) (hN12 : X.closure_neighborhoods.is_compact)
  (hN13 : X.closure_neighborhoods.is_compact) (hN14 : X.closure_neighborhoods.is_regular)
  (hN15 : X.closure_neighborhoods.is_regular) (hN16 : X.closure_neighborhoods.is_open)
  (hN17 : X.closure_neighborhoods.is_open):=
sorry

theorem exercise_31_2 {X : Type*} [normal_space X]
  (hX : disjoint_closure X) (hA : disjoint A) (hB : disjoint B)
  (hA_neigh : A ⊆ hA.closure) (hB_neigh : B ⊆ hB.closure)
  (hA_neigh_neigh : A ⊆ hA.closure ∧ B ⊆ hB.closure) :
  A ∩ B = ∅ :=
sorry

theorem exercise_31_3 {X : Type*} [order_topology X] : regular :=
sorry

theorem exercise_32_1 {X : Type*} [normal_space X] [subspace X]
  (hX : closed_subspace X) : normal_space X :=
sorry

theorem exercise_32_2a {X : Type*} [hausdorff X]
  [nonempty X] (h : ∀ (α : ℕ), ∃ (x : X), x ∈ X) :
  ∃ (x : ∏ X α), x ∈ X :=
sorry

theorem exercise_32_2b {X : Type*} [regular X]
  (h : ∃ (x : X), x ∈ X) : ∃ (x : X), regular x :=
sorry

theorem exercise_32_2c {X : Type*} [group X] :
  ∃ (x : X), ∃ (y : X), ∃ (z : X),
    (∀ (α : finset X), x ∈ y ∧ y ∈ z ∧ z ∈ x) →
    x ∈ z ∧ y ∈ z ∧ z ∈ x :=
sorry

theorem exercise_32_3 {X : Type*} [locally_compact_space X] :
  regular (locally_compact_space X) :=
sorry

theorem exercise_33_7 {X : Type*} [locally_compact_space X] [hausdorff_space X]
  (hX : compact_space X) : complete_regular X :=
sorry

theorem exercise_33_8 {X : Type*} [completely_regular X] 
  [disjoint_closed_sets A B : closed_subset X] (hA : compact_space A) (hB : closed_subset B)
  (h : continuous (λ x, if x ∈ A then 0 else 1)) :
  continuous (λ x, if x ∈ A then 0 else 1) :=
sorry

theorem exercise_34_9 {X Y : Type*} 
  [compact_space X] [compact_space Y] [metrizable X] [metrizable Y]
  (hX : compact_space X) (hY : compact_space Y)
  (hX_1 : closed_subspace X_1) (hX_2 : closed_subspace X_2)
  (hY_1 : closed_subspace Y_1) (hY_2 : closed_subspace Y_2)
  (h : X = Y) : metrizable X :=
sorry

theorem exercise_38_4 {X Y : Type*} 
  [compact_space X] [compact_space Y] [continuous_map X Y]
  (hX : compact_space X) (hY : compact_space Y) (hg : continuous g)
  (h : identity_on_X (g ∘ hX)) : continuous g :=
sorry

theorem exercise_38_6 {X : Type*} [completely_regular X] [compact_space X]
  (hX : connected X) (hCX : compact_space X) : connected X ↔ compact_space X.is_connected :=
sorry

theorem exercise_43_2 {X Y : Type*} 
  [metric_space X] [metric_space Y] [complete_space Y]
  (hY : compact_space Y) (f : X → Y) (hgc : continuous f)
  (hgi : function.injective f)
  (h : uniform_continuous f) :
  extend_uniform_continuous f (hgi ∘ f) :=
sorry