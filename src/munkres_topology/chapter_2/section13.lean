import topology.basic
import topology.constructions
import topology.bases
import topology.metric_space.basic
import data.real.basic
import data.set.countable

open_locale classical
open set

theorem exercise1 (X : Type*) [topological_space X] (A : set X)
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

def exercise3a (X : Type*) : topological_space X :=
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

theorem exercise3b : ¬ ∀ X : Type, ∀s : set (set X),
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

theorem exercise4a1 (X I : Type*) (T : I → set (set X)) (h : ∀ i, is_topology X (T i)) :
  is_topology X (⋂ i : I, T i) :=
sorry

theorem exercise4a2 :
  ∃ (X I : Type*) (T : I → set (set X)),
    (∀ i, is_topology X (T i)) ∧ ¬  is_topology X (⋂ i : I, T i) :=
sorry

theorem exercise4b1 (X I : Type*) (T : I → set (set X)) (h : ∀ i, is_topology X (T i)) :
  ∃! T', is_topology X T' ∧ (∀ i, T i ⊆ T') ∧
    ∀ T'', is_topology X T'' → (∀ i, T i ⊆ T'') → T'' ⊆ T' :=
sorry

theorem exercise4b2 (X I : Type*) (T : I → set (set X)) (h : ∀ i, is_topology X (T i)) :
  ∃! T', is_topology X T' ∧ (∀ i, T' ⊆ T i) ∧
    ∀ T'', is_topology X T'' → (∀ i, T'' ⊆ T i) → T' ⊆ T'' :=
sorry

inductive X4c | a | b | c

-- Find the unique smallest topology containing {∅, X, {a}, {a, b}}.
noncomputable theorem exercise4c1 :
  { T // is_topology X4c T ∧ {∅, univ, {X4c.a}, {X4c.a, X4c.b}} ⊆ T ∧
    ∀ T', is_topology X4c T' → {∅, univ, {X4c.a}, {X4c.a, X4c.b}} ⊆ T' → T ⊆ T' } :=
sorry

noncomputable theorem exercise4c2 :
  { T // is_topology X4c T ∧ {∅, univ, {X4c.a}, {X4c.b, X4c.c}} ⊆ T ∧
    ∀ T', is_topology X4c T' → {∅, univ, {X4c.a}, {X4c.b, X4c.c}} ⊆ T' → T ⊆ T' } :=
sorry

-- Find the unique largest topology contained in {∅, X, {a}, {a, b}}.
noncomputable theorem exercise4c3 :
  { T // is_topology X4c T ∧ T ⊆ {∅, univ, {X4c.a}, {X4c.a, X4c.b}} ∧
    ∀ T', is_topology X4c T' → T' ⊆ {∅, univ, {X4c.a}, {X4c.a, X4c.b}} → T' ⊆ T } :=
sorry

noncomputable theorem exercise4c4 :
  { T // is_topology X4c T ∧ T ⊆ {∅, univ, {X4c.a}, {X4c.b, X4c.c}} ∧
    ∀ T', is_topology X4c T' → T' ⊆ {∅, univ, {X4c.a}, {X4c.b, X4c.c}} → T' ⊆ T } :=
sorry

def lower_limit_topology (X : Type) [preorder X] :=
  topological_space.generate_from {S : set X | ∃ a b, a < b ∧ S = Ico a b}

def Rl := lower_limit_topology ℝ

def K : set ℝ := {r | ∃ n : ℕ, r = 1 / n}

def K_topology := topological_space.generate_from
  ({S : set ℝ | ∃ a b, a < b ∧ S = Ioo a b} ∪ {S : set ℝ | ∃ a b, a < b ∧ S = Ioo a b \ K})

theorem exercise6 :
  ¬ (∀ U, Rl.is_open U → K_topology.is_open U) ∧ ¬ (∀ U, K_topology.is_open U → Rl.is_open U) :=
sorry

theorem exercise8a :
  topological_space.is_topological_basis {S : set ℝ | ∃ a b : ℚ, a < b ∧ S = Ioo a b} :=
sorry

theorem exercise8b :
  (topological_space.generate_from {S : set ℝ | ∃ a b : ℚ, a < b ∧ S = Ico a b}).is_open ≠
    Rl.is_open :=
sorry














