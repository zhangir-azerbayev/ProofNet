import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_1 (X : Type*) [topological_space X]
  (A : set X) (hA : ∀ x ∈ A, ∃ U : set X, is_open U ∧ x ∈ U ∧ U ⊆ A): 
  is_open A :=
sorry

theorem exercise_13_4a1 {X : Type*} 
  {α : Type*} [fintype α] (T : α → topological_space X) :
  topological_space X :=
sorry

theorem exercise_13_4b1 {X : Type*} {α : Type*} 
  (T : α → topological_space X) :
  ∃! (t : topological_space X), ∀ (a : α), t.is_topology_of T a :=
sorry

theorem exercise_13_5a {X : Type*} {A : set (set X)}
  (hA : is_topological_basis A) :
  topology.generated A = ⋂₀ {T : set (set X) | is_topology T ∧ A ⊆ T} :=
sorry

theorem exercise_13_6 :
  ¬ (topological_space.is_topological_basis ℝ (lower_limit_topology ℝ) 
  ∨ topological_space.is_topological_basis ℝ (K_topology ℝ)) :=
sorry

theorem exercise_13_8b :
  is_topological_basis (set.range (λ (p : ℚ × ℚ), {a | p.1 < a ∧ a < p.2})) :=
sorry

theorem exercise_16_4 {X Y : Type*} [topological_space X] [topological_space Y] :
  is_open (pi_one : X × Y → X) :=
sorry

theorem exercise_17_4 {X : Type*} [topological_space X]
  (U : set X) (hU : is_open U) (A : set X) (hA : is_closed A) :
  is_open (U \ A) ∧ is_closed (A \ U) :=
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

theorem exercise_21_6a {x : ℝ} (hx : 0 ≤ x) 
  (hx1 : x ≤ 1) :
  ∀ (n : ℕ), tendsto (λ (n : ℕ), x ^ n) at_top (𝓝 (x ^ n)) :=
sorry

theorem exercise_21_8  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_22_2b {X Y : Type*} [topological_space X] 
  [topological_space Y] (f : X → Y) (hf : continuous f) (hf_eq : ∀ x : X, f x = x) :
  quotient_map f :=
sorry

theorem exercise_23_2 {X : Type*} [topological_space X]
  {n : ℕ} (h : ∀ n : ℕ, connected (A n)) (h₁ : ∀ n : ℕ, A n ∩ A (n + 1) ≠ ∅) :
  connected (⋃ n, A n) :=
sorry

theorem exercise_23_4 {X : Type*} [fintype X] (hX : ¬fintype.card X = 1) :
  connected_space (finite_compl_topology X) :=
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

theorem exercise_25_4  {X : Type*} [topological_space X] [locally_path_connected_space X]
  {U : set X} (hU : is_open U) (hcU : is_connected U) :
  path_connected_space U :=
sorry

theorem exercise_26_11  {X : Type*} [topological_space X] [compact_space X] [t2_space X]
  {Y : Type*} [topological_space Y] [t2_space Y]
  {f : X → Y} (hf : continuous f) (hf_conn : ∀ x, is_connected (f ⁻¹' {x}))
  (hf_closed : ∀ x, is_closed (f ⁻¹' {x}))
  (hf_simply_ordered : ∀ x y, x ≠ y → ∃ z, f ⁻¹' {z} ⊆ f ⁻¹' {x} ∧ f ⁻¹' {z} ⊆ f ⁻¹' {y}) :
  is_connected (f ⁻¹' univ) :=
sorry

theorem exercise_27_4 {X : Type*} [metric_space X]
  (hX : connected_space X) (hX_card : cardinal.mk X > 1) :
  cardinal.mk X = ⊤ :=
sorry

theorem exercise_28_5  {X : Type*} [topological_space X] (hX : countably_compact X) :
  ∀ (C : ℕ → set X), (∀ n, is_closed (C n)) → (∀ n, C n ≠ ∅) →
  ∃ (x : X), ∀ n, x ∈ C n :=
sorry

theorem exercise_29_1 : ¬ locally_compact_space ℚ :=
sorry

theorem exercise_29_10 {X : Type*} 
  [topological_space X] [t2_space X] [locally_compact_space X] (x : X) 
  (U : set X) (hU : is_open U) (hxU : x ∈ U) :
  ∃ (V : set X), is_open V ∧ compact_space (closure V) ∧ closure V ⊆ U :=
sorry

theorem exercise_30_13 {X : Type*} 
  [topological_space X] (hX : countable (dense_countable X)) 
  (h : ∀ (U : set X), is_open U → ∀ (V : set X), is_open V → U ≠ V → U ∩ V = ∅) :
  countable (set.range (λ (U : set X), U)) :=
sorry

theorem exercise_31_2 
  {X : Type*} [topological_space X] (hX : normal_space X) 
  {A B : set X} (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
  ∃ (U V : set X), is_open U ∧ is_open V ∧ A ⊆ U ∧ B ⊆ V ∧ disjoint (closure U) (closure V) :=
sorry

theorem exercise_32_1 {X : Type*} [topological_space X]
  [normal_space X] (Y : set X) (hY : is_closed Y) : normal_space Y :=
sorry

theorem exercise_32_2b {α : Type*} {X : α → Type*} [∀ a, regular_space (X a)]
  (h : regular_space (Π a, X a)) : ∀ a, regular_space (X a) :=
sorry

theorem exercise_32_3 {X : Type*} [topological_space X]
  (hX : locally_compact_space X) (hX' : hausdorff_space X) :
  regular_space X :=
sorry

theorem exercise_33_8 {X : Type*} [topological_space X]
  (hX : completely_regular_space X) (A B : set X) (hA : is_closed A) 
  (hB : is_closed B) (hAB : disjoint A B) (hA_comp : compact_space A) :
  ∃ (f : X → ℝ), continuous f ∧ f '' A = {0} ∧ f '' B = {1} :=
sorry

theorem exercise_38_6 {X : Type*} [topological_space X]
  [comp_reg_space X] (hX : connected_space X) :
  connected_space (compactification X) :=
sorry