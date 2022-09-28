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

theorem exercise_13_1 (X : Type*) [topological_space X] (A : set X)
  (h1 : ∀ x ∈ A, ∃ U : set X, x ∈ U ∧ is_open U ∧ U ⊆ A) :
  is_open A :=
begin
  have : A = ⋃ x, ⋃ h : x ∈ A, (classical.some (h1 x h)),
  { ext x, simp, split,
  { intro xA,
    use [x, xA],
    exact (classical.some_spec (h1 x xA)).1},
  { rintros ⟨y, yA, yspec⟩,
    have h := classical.some_spec (h1 y yA),
    exact h.2.2 yspec }, },
  rw this,
  apply is_open_Union,
  intro x,
  apply is_open_Union,
  intro xA,
  have h := classical.some_spec (h1 x xA),
  exact h.2.1
end

def Tc (X : Type*) : set X → Prop :=
  λ U, U = ∅ ∨ countable Uᶜ

def exercise_13_3a (X : Type*) : topological_space X :=
{ is_open := Tc X,
  is_open_univ := by { right, simp },
  is_open_inter :=
  begin
    rintros s t (rfl | cs),
    { intro _, left, simp },
    rintros (rfl | ct),
    { left, simp [cs] },
    right,
    rw [set.compl_inter, set.countable_union],
    exact ⟨cs, ct⟩
  end,
  is_open_sUnion :=
  begin
    intros s hs,
    by_cases h : ∀ t ∈ s, t = ∅,
    { left,
    simp,
    exact h },
    push_neg at h,
    rcases h with ⟨t, ts, tne⟩,
    right,
    have := (hs t ts).resolve_left tne,
    apply countable.mono _ this,
    simp,
    exact subset_sUnion_of_mem ts
  end }

def Tinfty {X : Type*} : set X → Prop :=
λ U, set.infinite Uᶜ ∨ U = ∅ ∨ U = univ

theorem exercise_13_3b : ¬ ∀ X : Type, ∀s : set (set X),
  (∀t∈s, Tinfty t) → Tinfty (⋃₀ s) :=
begin
  simp,
  use [ℕ],
  use (image (λ i : nat, { i.succ }) univ),
  split; simp,
  { intro n,
  left,
  apply infinite_of_finite_compl,
  simp },
  simp [Tinfty], push_neg,
  have : range nat.succ = {0}ᶜ,
  { ext x,
  simp,
  split,
  { rintros ⟨y, rfl⟩, simp },
  intro h,
  use [nat.pred x],
  apply nat.succ_pred_eq_of_pos,
  exact nat.pos_of_ne_zero h },
  rw this,
  simp,
  apply nonempty.ne_empty,
  simp
end

def is_topology (X : Type*) (T : set (set X)) :=
  univ ∈ T ∧
  (∀ s t, s ∈ T → t ∈ T → s ∩ t ∈ T) ∧
  (∀s, (∀t ∈ s, t ∈ T) → ⋃₀ s ∈ T)

theorem exercise_13_4a1 (X I : Type*) (T : I → set (set X)) (h : ∀ i, is_topology X (T i)) :
  is_topology X (⋂ i : I, T i) :=
sorry

theorem exercise_13_4a2 :
  ∃ (X I : Type*) (T : I → set (set X)),
  (∀ i, is_topology X (T i)) ∧ ¬  is_topology X (⋂ i : I, T i) :=
sorry

theorem exercise_13_4b1 (X I : Type*) (T : I → set (set X)) (h : ∀ i, is_topology X (T i)) :
  ∃! T', is_topology X T' ∧ (∀ i, T i ⊆ T') ∧
  ∀ T'', is_topology X T'' → (∀ i, T i ⊆ T'') → T'' ⊆ T' :=
sorry

theorem exercise_13_4b2 (X I : Type*) (T : I → set (set X)) (h : ∀ i, is_topology X (T i)) :
  ∃! T', is_topology X T' ∧ (∀ i, T' ⊆ T i) ∧
  ∀ T'', is_topology X T'' → (∀ i, T'' ⊆ T i) → T' ⊆ T'' :=
sorry

inductive X4c | a | b | c

-- Find the unique smallest topology containing {∅, X, {a}, {a, b}}.
noncomputable theorem exercise_13_4c1 :
  { T // is_topology X4c T ∧ {∅, univ, {X4c.a}, {X4c.a, X4c.b}} ⊆ T ∧
  ∀ T', is_topology X4c T' → {∅, univ, {X4c.a}, {X4c.a, X4c.b}} ⊆ T' → T ⊆ T' } :=
sorry

noncomputable theorem exercise_13_4c2 :
  { T // is_topology X4c T ∧ {∅, univ, {X4c.a}, {X4c.b, X4c.c}} ⊆ T ∧
  ∀ T', is_topology X4c T' → {∅, univ, {X4c.a}, {X4c.b, X4c.c}} ⊆ T' → T ⊆ T' } :=
sorry

-- Find the unique largest topology contained in {∅, X, {a}, {a, b}}.
noncomputable theorem exercise_13_4c3 :
  { T // is_topology X4c T ∧ T ⊆ {∅, univ, {X4c.a}, {X4c.a, X4c.b}} ∧
  ∀ T', is_topology X4c T' → T' ⊆ {∅, univ, {X4c.a}, {X4c.a, X4c.b}} → T' ⊆ T } :=
sorry

noncomputable theorem exercise_13_4c4 :
  { T // is_topology X4c T ∧ T ⊆ {∅, univ, {X4c.a}, {X4c.b, X4c.c}} ∧
  ∀ T', is_topology X4c T' → T' ⊆ {∅, univ, {X4c.a}, {X4c.b, X4c.c}} → T' ⊆ T } :=
sorry

open topological_space

noncomputable theorem exercise_13_5a {X : Type*}
  [topological_space X] (A : set (set X)) (hA : is_topological_basis A) :
  generate_from A = generate_from (sInter {T | is_topology X T ∧ A ⊆ T}) :=
sorry

noncomputable theorem exercise_13_5b {X : Type*}
  [t : topological_space X] (A : set (set X)) (hA : t = generate_from A) :
  generate_from A = generate_from (sInter {T | is_topology X T ∧ A ⊆ T}) :=
sorry

def lower_limit_topology (X : Type) [preorder X] :=
  topological_space.generate_from {S : set X | ∃ a b, a < b ∧ S = Ico a b}

def Rl := lower_limit_topology ℝ

def K : set ℝ := {r | ∃ n : ℕ, r = 1 / n}

def K_topology := topological_space.generate_from
  ({S : set ℝ | ∃ a b, a < b ∧ S = Ioo a b} ∪ {S : set ℝ | ∃ a b, a < b ∧ S = Ioo a b \ K})

theorem exercise_13_6 :
  ¬ (∀ U, Rl.is_open U → K_topology.is_open U) ∧ ¬ (∀ U, K_topology.is_open U → Rl.is_open U) :=
sorry

theorem exercise_13_8a :
  topological_space.is_topological_basis {S : set ℝ | ∃ a b : ℚ, a < b ∧ S = Ioo a b} :=
sorry

theorem exercise_13_8b :
  (topological_space.generate_from {S : set ℝ | ∃ a b : ℚ, a < b ∧ S = Ico a b}).is_open ≠
  (lower_limit_topology ℝ).is_open :=
sorry

noncomputable
theorem exercise_16_1 {X : Type*} [topological_space X]
  (Y A : set X)
  (hA : A ⊂ Y)
  [tY : topological_space {x // x ∈ Y}]
  [tAX : topological_space {x : X // x ∈ A}]
  [tAY : topological_space {x : {x // x ∈ Y} // x ∈ A}]
  :
  tAY = tAX :=
sorry

theorem exercise_16_4 {X Y : Type*} [topological_space X] [topological_space Y]
  (π₁ : X × Y → X)
  (π₂ : X × Y → Y)
  (h₁ : π₁ = prod.fst)
  (h₂ : π₂ = prod.snd) :
  is_open_map π₁ ∧ is_open_map π₂ :=
sorry

def rational (x : ℝ) := x ∈ set.range (coe : ℚ → ℝ)

theorem exercise_16_6
  (S : set (set (ℝ × ℝ)))
  (hS : ∀ s, s ∈ S → ∃ a b c d, (rational a ∧ rational b ∧ rational c ∧ rational d
    ∧ s = {x | ∃ x₁ x₂, x = (x₁, x₂) ∧ a < x₁ ∧ x₁ < b ∧ c < x₂ ∧ x₂ < d})) :
  is_topological_basis S :=
sorry

theorem exercise_17_4 {X : Type*} [topological_space X]
    (U A : set X) (hU : is_open U) (hA : is_closed A) :
    is_open (U \ A) ∧ is_closed (A \ U) :=
sorry

theorem exercise_18_8a {X Y : Type*} [topological_space X] [topological_space Y]
  [linear_order Y] [order_topology Y] {f g : X → Y}
  (hf : continuous f) (hg : continuous g) :
  is_closed {x | f x ≤ g x} :=
sorry

theorem exercise_18_8b {X Y : Type*} [topological_space X] [topological_space Y]
  [linear_order Y] [order_topology Y] {f g : X → Y}
  (hf : continuous f) (hg : continuous g) :
  continuous (λ x, min (f x) (g x)) :=
sorry

theorem exercise_18_13
    {X : Type*} [topological_space X] {Y : Type*} [topological_space Y]
    [t2_space Y] {A : set X} {f : A → Y} (hf : continuous f)
    (g : closure A → Y)
    (g_con : continuous g) :
    ∀ (g' : closure A → Y), continuous g' →  (∀ (x : closure A), g x = g' x) :=
sorry

open filter
open_locale filter
open_locale topological_space

theorem exercise_19_6a
  {n : ℕ}
  {f : fin n → Type*} {x : ℕ → Πa, f a}
  (y : Πi, f i)
  [Πa, topological_space (f a)] :
  tendsto x at_top (𝓝 y) ↔ ∀ i, tendsto (λ j, (x j) i) at_top (𝓝 (y i)) :=
sorry

theorem exercise_20_2 -- TODO dictionary order on R x R
    [order_topology (ℝ × ℝ)]
    : metrizable_space (ℝ × ℝ) :=
sorry


abbreviation I : set ℝ := set.Icc 0 1

theorem exercise_21_6a
  (f : ℕ → I → ℝ )
  (h : ∀ x n, f n x = x ^ n) :
  ∀ x, ∃ y, tendsto (λ n, f n x) at_top (𝓝 y) :=
sorry

theorem exercise_21_6b
  (f : ℕ → I → ℝ )
  (h : ∀ x n, f n x = x ^ n) :
  ¬ ∃ f₀, tendsto_uniformly f f₀ at_top :=
sorry

theorem exercise_21_8
    {X : Type*} [topological_space X] {Y : Type*} [metric_space Y]
    {f : ℕ → X → Y} {x : ℕ → X}
    (hf : ∀ n, continuous (f n))
    (x₀ : X)
    (hx : tendsto x at_top (𝓝 x₀))
    (f₀ : X → Y)
    (hh : tendsto_uniformly f f₀ at_top) :
    tendsto (λ n, f n (x n)) at_top (𝓝 (f₀ x₀)) :=
sorry

theorem exercise_22_2a {X Y : Type*} [topological_space X]
    [topological_space Y] (p : X → Y) (h : continuous p) :
    quotient_map p ↔ ∃ (f : Y → X), continuous f ∧ p ∘ f = id :=
sorry

theorem exercise_22_2b {X : Type*} [topological_space X]
    {A : set X} (r : X → A) (hr : continuous r) (h : ∀ x : A, r x = x) :
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
    [topological_space X] {A : ℕ → set X}
    (hAn : ∀ n, is_connected (A n))
    (A₀ : set X)
    (hA : is_connected A₀)
    (h : ∀ n, A₀ ∩ A n ≠ ∅) :
    is_connected (A₀ ∪ (⋃ n, A n)) :=
sorry

theorem exercise_23_4 {X : Type*} [topological_space X] [cofinite_topology X]
  (s : set X) : set.infinite s → is_connected s :=
sorry

theorem exercise_23_6 {X : Type*}
    [topological_space X] {A C : set X} (hc : is_connected C)
    (hCA : C ∩ A ≠ ∅) (hCXA : C ∩ (A.compl) ≠ ∅) :
    C ∩ (frontier A) ≠ ∅ :=
sorry

theorem exercise_23_9 {X Y : Type*}
    [topological_space X] [topological_space Y]
    (A₁ A₂ : set X)
    (B₁ B₂ : set Y)
    (hA : A₁ ⊂ A₂)
    (hB : B₁ ⊂ B₂)
    (hA : is_connected A₂)
    (hB : is_connected B₂) :
    is_connected ({x | ∃ a b, x = (a, b) ∧ a ∈ A₂ ∧ b ∈ B₂} \
                  {x | ∃ a b, x = (a, b) ∧ a ∈ A₁ ∧ b ∈ B₁}) :=
sorry


theorem exercise_23_11 {X Y : Type*} [topological_space X] [topological_space Y]
  (p : X → Y) (hq : quotient_map p)
  (hY : connected_space Y) (hX : ∀ y : Y, is_connected (p ⁻¹' {y})) :
  connected_space X :=
sorry

theorem exercise_24_2 {f : (metric.sphere 0 1 : set ℝ) → ℝ}
    (hf : continuous f) : ∃ x, f x = f (-x) :=
sorry

theorem exercise_24_3a [topological_space I]
    (f : I → I) (hf : continuous f) :
    ∃ (x : I), f x = x :=
sorry

theorem exercise_25_4 {X : Type*} [topological_space X]
    [loc_path_connected_space X] (U : set X) (hU : is_open U)
    (hcU : is_connected U) : is_path_connected U :=
sorry

theorem exercise_25_9 {G : Type*} [topological_space G] [group G]
  [topological_group G] (C : set G) (h : C = connected_component 1) :
  is_normal_subgroup C :=
sorry

theorem exercise_26_11
    {X : Type*} [topological_space X] [compact_space X] [t2_space X]
    (A : set (set X)) (hA : ∀ (a b : set X), a ∈ A → b ∈ A → a ⊆ b ∨ b ⊆ a)
    (hA' : ∀ a ∈ A, is_closed a) (hA'' : ∀ a ∈ A, is_connected a) :
    is_connected (⋂₀ A) :=
sorry

theorem exercise_26_12 {X Y : Type*} [topological_space X] [topological_space Y]
  (p : X → Y) (h : function.surjective p) (hc : continuous p) (hp : ∀ y, is_compact (p ⁻¹' {y}))
  (hY : compact_space Y) : compact_space X :=
sorry

theorem exercise_27_4
    {X : Type*} [metric_space X] [connected_space X] (hX : ∃ x y : X, x ≠ y) :
    ¬ countable (univ : set X) :=
sorry

def countably_compact (X : Type*) [topological_space X] :=
  ∀ U : ℕ → set X,
    (∀ i, is_open (U i)) ∧ ((univ : set X) ⊆ ⋃ i, U i) →
    (∃ t : finset ℕ, (univ : set X) ⊆ ⋃ i ∈ t, U i)

def limit_point_compact (X : Type*) [topological_space X] :=
  ∀ U : set X, set.infinite U → ∃ x ∈ U, cluster_pt x (𝓟 U)

theorem exercise_28_4 {X : Type*}
    [topological_space X] (hT1 : t1_space X) :
    countably_compact X ↔ limit_point_compact X :=
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

theorem exercise_29_4 [topological_space (ℕ → I)] :
  ¬ locally_compact_space (ℕ → I) :=
sorry -- TODO check

theorem exercise_29_10 {X : Type*}
    [topological_space X] [t2_space X] (x : X)
    (hx : ∃ U : set X, x ∈ U ∧ is_open U ∧ (∃ K : set X, U ⊂ K ∧ is_compact K))
    (U : set X) (hU : is_open U) (hxU : x ∈ U) :
    ∃ (V : set X), is_open V ∧ x ∈ V ∧ is_compact (closure V) ∧ closure V ⊆ U :=
sorry

theorem exercise_30_10
    {X : ℕ → Type*} [∀ i, topological_space (X i)]
    (h : ∀ i, ∃ (s : set (X i)), countable s ∧ dense s) :
    ∃ (s : set (Π i, X i)), countable s ∧ dense s :=
sorry

theorem exercise_30_13 {X : Type*} [topological_space X]
    (h : ∃ (s : set X), countable s ∧ dense s) (U : set (set X))
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
    ∃ (U V : set X), is_open U ∧ is_open V ∧ A ⊆ U ∧ B ⊆ V ∧ closure U ∩ closure V = ∅ :=
sorry

theorem exercise_31_3 {α : Type*} [partial_order α]
    [topological_space α] (h : order_topology α) : regular_space α :=
sorry

theorem exercise_32_1 {X : Type*} [topological_space X]
    (hX : normal_space X) (A : set X) (hA : is_closed A) :
    normal_space {x // x ∈ A} :=
sorry

theorem exercise_32_2a
    {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)]
    (h : ∀ i, nonempty (X i)) (h2 : t2_space (Π i, X i)) :
    ∀ i, t2_space (X i) :=
sorry

theorem exercise_32_2b
    {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)]
    (h : ∀ i, nonempty (X i)) (h2 : regular_space (Π i, X i)) :
    ∀ i, regular_space (X i) :=
sorry

theorem exercise_32_2c
    {ι : Type*} {X : ι → Type*} [∀ i, topological_space (X i)]
    (h : ∀ i, nonempty (X i)) (h2 : normal_space (Π i, X i)) :
    ∀ i, normal_space (X i) :=
sorry

theorem exercise_32_3 {X : Type*} [topological_space X]
    (hX : locally_compact_space X) (hX' : t2_space X) :
    regular_space X :=
sorry

theorem exercise_32_7 {X : Type*} [topological_space X]
    (hX : locally_compact_space X) (hX' : t2_space X) :
    ∀ x A, is_closed A ∧ ¬ x ∈ A →
      ∃ (f : X → I), continuous f ∧ f x = 1 ∧ f '' A = {0}
    :=
sorry

theorem exercise_33_8
    (X : Type*) [topological_space X] [regular_space X]
    (h : ∀ x A, is_closed A ∧ ¬ x ∈ A →
      ∃ (f : X → I), continuous f ∧ f x = (1 : I) ∧ f '' A = {0})
    (A B : set X) (hA : is_closed A) (hB : is_closed B)
    (hAB : disjoint A B)
    (hAc : is_compact A) :
    ∃ (f : X → I), continuous f ∧ f '' A = {0} ∧ f '' B = {1} :=
sorry

theorem exercise_34_9
    (X : Type*) [topological_space X] [compact_space X]
    (X1 X2 : set X) (hX1 : is_closed X1) (hX2 : is_closed X2)
    (hX : X1 ∪ X2 = univ) (hX1m : metrizable_space X1)
    (hX2m : metrizable_space X2) : metrizable_space X :=
sorry

theorem exercise_38_4 {X Y S : Type*}
    [topological_space Y] [compact_space Y] [t2_space Y]
    (X : set Y)
    [topological_space {x // x ∈ X}]
    (hc : closure X = (univ : set Y))
    (hs : S = stone_cech {x // x ∈ X}) :
    ∃ (g : S → Y), continuous g ∧ function.surjective g ∧ is_closed_map g ∧
      ∀ x ∈ X, g x = x :=
sorry -- TODO fix

theorem exercise_38_6 {X : Type*}
    (X : Type*) [topological_space X] [regular_space X]
    (h : ∀ x A, is_closed A ∧ ¬ x ∈ A →
      ∃ (f : X → I), continuous f ∧ f x = (1 : I) ∧ f '' A = {0}) :
    is_connected (univ : set X) ↔ is_connected (univ : set (stone_cech X)) :=
sorry

theorem exercise_43_2 {X : Type*} [metric_space X]
    {Y : Type*} [metric_space Y] [complete_space Y] (A : set X)
    (f : A → Y) (hf : uniform_continuous f) :
    ∃! (g : closure A → Y), continuous g ∧ uniform_continuous g ∧
    ∀ (x : A), g x = f x :=
sorry
