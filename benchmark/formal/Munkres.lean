import topology.basic
import topology.constructions
import topology.bases
import topology.metric_space.basic
import topology.metric_space.metrizable
import data.real.basic
import data.set.countable
import data.real.irrational

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
  [topological_space {x // x ∈ Y}]
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



