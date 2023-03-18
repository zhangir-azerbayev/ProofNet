import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 




/--
theorem exercise_13_3b given collection $\mathcal{T}_\infty$ is not guaranteed to be a topology on the set $X$. To show this, we need to provide a counterexample where the collection does not satisfy the axioms of a topology.

Consider the set $X = \mathbb{Z}$, the set of integers. Let's check if $\mathcal{T}_\infty$ satisfies the axioms of a topology on $X$:

1. $X$ and $\emptyset$ are in $\mathcal{T}_\infty$: This is true by definition.

2. The intersection of any finite number of sets in $\mathcal{T}_\infty$ is also in $\mathcal{T}_\infty$: This is not necessarily true. Consider the sets $U = \{x \in \mathbb{Z} \mid x < 0\}$ and $V = \{x \in \mathbb{Z} \mid x > 0\}$. Both $U$ and $V$ are in $\mathcal{T}_\infty$ since $X - U = V$ and $X - V = U$ are infinite. However, their intersection $U \cap V = \emptyset$, and $X - \emptyset = X$ is not infinite. Thus, the intersection of $U$ and $V$ is not in $\mathcal{T}_\infty$.

Since the second axiom of a topology is not satisfied, $\mathcal{T}_\infty$ is not a topology on the set $X$.:=
sorry
-/
theorem exercise_13_4a2 : ∃ (X : Type) (T : set (set X)), ¬ is_topological_basis T :=
sorry

theorem exercise_13_4b2 {X : Type*} {ι : Type*}
  (T : ι → topological_space X) :
  ∃! (T_max : topological_space X), ∀ i, T_max ≤ T i :=
sorry

theorem exercise_13_5b
  {X : Type*} (A : set (set X)) :
  topological_space.generate_from A =
  Inf {τ : topological_space X | A ⊆ τ.is_open} :=
sorry

theorem exercise_13_8a :
  topological_space.is_topological_basis
    {U : set ℝ | ∃ (a b : ℚ), a < b ∧ U = set.Ioo a b} :=
sorry

theorem exercise_16_1 {X Y : Type*} [topological_space X]
  [topological_space Y] (hY : Y ≤ X) (A : set Y) :
  @topological_space.induced Y (subtype A) (subtype.val) (topological_space.induced Y subtype.val) =
  @topological_space.induced X (subtype A) (subtype.val ∘ @subtype.val Y hY) :=
sorry

theorem exercise_16_6 :
  topological_space.is_topological_basis
    {B : set (ℝ × ℝ) | ∃ a b c d : ℚ, a < b ∧ c < d ∧ B = set.prod (set.Ioo a b) (set.Ioo c d)} :=
sorry

theorem exercise_18_8a {X Y : Type*} [topological_space X]
  [partial_order Y] [topological_space Y] [order_topology Y]
  (f g : X → Y) (hf : continuous f) (hg : continuous g) :
  is_closed {x : X | f x ≤ g x} :=
sorry

theorem exercise_18_13 {X Y : Type*} [topological_space X]
  [topological_space Y] [t2_space Y] {A : set X} {f : A → Y}
  (hf : continuous_on f A) {g₁ g₂ : X → Y} (h₁ : continuous_on g₁ (closure A))
  (h₂ : continuous_on g₂ (closure A)) (hfg₁ : ∀ x ∈ A, g₁ x = f x)
  (hfg₂ : ∀ x ∈ A, g₂ x = f x) :
  g₁ = g₂ :=
sorry

theorem exercise_20_2 : metrizable (lex ℝ ℝ) :=
sorry

theorem exercise_21_6b  (n : ℕ) (f_n : ℝ → ℝ) (hf_n : ∀ x, f_n x = x ^ n) :
  ¬uniformly_convergent (λ n x, f_n x) :=
sorry

theorem exercise_22_2a {X Y : Type*}
  [topological_space X] [topological_space Y] (p : X → Y)
  (hp : continuous p) (f : Y → X) (hf : continuous f)
  (hpf : p ∘ f = id) : quotient_map p :=
sorry

theorem exercise_22_5 {X Y : Type*} [topological_space X] [topological_space Y]
  (p : X → Y) (hp : open_map p) (A : set X) (hA : is_open A) :
  open_map (set.restrict p A) :=
sorry

theorem exercise_23_3 {X : Type*} [topological_space X]
  {A : set X} (hA : is_connected A) {ι : Type} {s : ι → set X}
  (hs : ∀ i : ι, is_connected (s i)) (h_inter : ∀ i : ι, A ∩ s i ≠ ∅) :
  is_connected (A ∪ ⋃ (i : ι), s i) :=
sorry

theorem exercise_23_6 {X : Type*} [topological_space X]
  (A : set X) (C : set X) (hC : is_connected C)
  (hCA : C ∩ A ≠ ∅) (hCXA : C ∩ (X \ A) ≠ ∅) :
  C ∩ (boundary A) ≠ ∅ :=
sorry

theorem exercise_23_11 {X Y : Type*} [topological_space X]
  [topological_space Y] (p : X → Y) (hp : is_quotient_map p)
  (hY : connected_space Y) (hpreimage : ∀ y : Y, is_connected (p ⁻¹' {y})) :
  connected_space X :=
sorry

theorem exercise_24_3a {X : Type*} [topological_space X]
  [linear_order X] [order_closed_topology X] (f : X → X)
  (hf : continuous_on f (set.Icc (0 : X) 1)) :
  ∃ x ∈ set.Icc (0 : X) 1, f x = x :=
sorry

theorem exercise_25_9 {G : Type*} [topological_space G]
  [group G] [topological_group G] :
  normal_subgroup (connected_component G 1) :=
sorry

theorem exercise_26_12 {X Y : Type*}
  [topological_space X] [topological_space Y] (p : X → Y)
  (hp : continuous p ∧ closed_map p ∧ function.surjective p ∧
        ∀ y : Y, is_compact (p ⁻¹' {y}))
  (hY : is_compact Y) : is_compact X :=
sorry

theorem exercise_28_4 {X : Type*} [topological_space X]
  [t1_space X] : countably_compact_space X ↔ limit_point_compact_space X :=
sorry

theorem exercise_28_6 {X : Type*} [metric_space X]
  (f : X → X) (hf : isometry f) (hX : compact_space X) :
  function.bijective f ∧ homeomorphism X X :=
sorry

theorem exercise_29_4 :
  ¬locally_compact_space (uniform_space.of (ℕ → ℝ)) :=
sorry

theorem exercise_30_10 {ι : Type*} [encodable ι]
  {X : ι → Type*} [∀ i, topological_space (X i)] (Y : ι → set (X i))
  (hY : ∀ i, dense (Y i)) (hYcount : ∀ i, set.countable (Y i)) :
  dense (set.pi set.univ Y) :=
sorry

theorem exercise_31_1 {X : Type*} [topological_space X]
  [t1_space X] [regular_space X] {x y : X} (hxy : x ≠ y) :
  ∃ (U V : set X), is_open U ∧ is_open V ∧ x ∈ U ∧ y ∈ V ∧ closure U ∩ closure V = ∅ :=
sorry

theorem exercise_31_3 (α : Type*) [topological_space α] [partial_order α]
  [order_topology α] : regular_space α :=
sorry

theorem exercise_32_2a {ι : Type*} {X : ι → Type*}
  [∀ i, topological_space (X i)] [∀ i, nonempty (X i)]
  (h : t2_space (Π i, X i)) (i : ι) : t2_space (X i) :=
sorry

theorem exercise_32_2c {α : Type*} {X : α → Type*} [Π a, topological_space (X a)]
  [Π a, nonempty (X a)] (h : normal_space (Π a, X a)) (a : α) :
  normal_space (X a) :=
sorry

theorem exercise_33_7
  (X : Type*) [topological_space X] [locally_compact_space X] [t2_space X] :
  completely_regular_space X :=
sorry

theorem exercise_34_9
  {X : Type*} [topological_space X] [compact_space X] [t2_space X]
  (X₁ X₂ : set X) (hX : X = X₁ ∪ X₂) (hX₁ : is_closed X₁) (hX₂ : is_closed X₂)
  (hM₁ : metrizable_space (subtype X₁)) (hM₂ : metrizable_space (subtype X₂)) :
  metrizable_space X :=
sorry

theorem exercise_43_2 {X Y : Type*}
  [metric_space X] [metric_space Y] [complete_space Y]
  (A : set X) (f : X → Y) (hf : uniform_continuous_on f A) :
  ∃! (g : X → Y), uniform_continuous_on g A.closure ∧
    ∀ (x : X), x ∈ A → g x = f x :=
sorry