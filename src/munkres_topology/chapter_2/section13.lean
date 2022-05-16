import topology.basic
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




