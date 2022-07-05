import tactic
import data.real.sqrt
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import data.set.intervals.basic
import topology.metric_space.basic
import topology.instances.real
import dynamics.ergodic.measure_preserving

open filter real

open_locale topological_space


theorem exercise_1
  : ∃ (f : ℝ → ℝ), (∀ (x : ℝ), tendsto (λ y, f(x + y) - f(x - y)) (𝓝 0) (𝓝 0)) ∧ ¬ continuous f :=
begin
  use λ x, if x = 0 then (1 : ℝ) else 0,
  split,
  {
    intro x,
    by_cases h : x = 0,
    {
      rw h, simp,
      exact tendsto_const_nhds,
    },
    {
      intros a b,
      simp,
      refine mem_nhds_iff.mpr _,
      simp,
      use {z | -|x| < z ∧ z < |x|},
      simp,
      split,
      sorry,
      sorry,
    },
  },
  {
    intro h,
    let f : (ℝ → ℝ) := λ x, if x = 0 then (1 : ℝ) else 0,
    have g : f 0 = 1 := sorry,
    have : continuous_at f 0 := continuous.continuous_at h,
    have := continuous_at.tendsto this,
    rw g at this,
    unfold tendsto at this,
    have := le_def.1 this,
    simp at this,
    have := this (set.Ioo (0.5 : ℝ) (1.5 : ℝ)),
    have i : set.Ioo (0.5 : ℝ) (1.5 : ℝ) ∈ 𝓝 (1 : ℝ) := sorry,
    have j : f ⁻¹' set.Ioo (1 / 2) (3 / 2) = {0} := sorry,
    have := this i,
    rw j at this,
    have := mem_nhds_iff.1 this,
    cases this with s h,
    cases h with k g,
    cases g,
    by_cases a : s = {0},
    {
      have : is_closed {(0 : ℝ)} := is_closed_singleton,
      rw a at g_left,
      have := (is_open_singleton_iff_nhds_eq_pure (0 : ℝ)).1 g_left,
      unfold pure at this,
      unfold nhds at this,
      simp at this,
      sorry,
    },
    {
      have l : {(0 : ℝ)} ⊆ s := set.zero_subset.mpr g_right,
      have l : s = {(0 : ℝ)} := set.subset.antisymm k l,
      contradiction,
    },
  },
end

#printinstances t2_space
theorem aa : t1_space real := by apply_instance
#print aa

theorem exercise_2
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f : α → β)
  (h₁ : continuous f)
  : ∀ (x : set α), f '' (closure x) ⊆ closure (f '' x) :=
begin
  intros x_a x h₂ x_b h₃,
  unfold closure at *,
  simp at *,
  cases h₃,
  cases h₂ with w h₄,
  cases h₄,
  have h₅ := h₄_left x_a,
  sorry,
end

theorem exercise_3
  {α : Type} [metric_space α]
  (f : α → ℝ) (h : continuous f) (z : set α) (g : z = f⁻¹' {0})
  : is_closed z :=
begin
  rw g,
  apply is_closed.preimage h,
  exact is_closed_singleton,
end

theorem exercise_4_a
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f : α → β)
  (s : set α)
  (h₁ : continuous f)
  (h₂ : dense s)
  : f '' set.univ ⊆ closure (f '' s) :=
begin
  simp,
  exact continuous.range_subset_closure_image_dense h₁ h₂,
end

theorem exercise_4_b
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f g : α → β)
  (s : set α)
  (h₁ : continuous f)
  (h₂ : continuous g)
  (h₃ : dense s)
  (h₄ : ∀ x ∈ s, f x = g x)
  : f = g :=
begin
  have h₅ : is_closed {x | f x = g x} := is_closed_eq h₁ h₂,
  unfold dense at h₃,
  set t := {x : α | f x = g x} with h,
  have h₆ : s ⊆ t := h₄,
  have h₇ : closure s ⊆ closure t := closure_mono h₆,
  --have h₁₀ : closure s = set.univ := by { ext, simp, apply h₃,},
  --exact h₃, -- does not work ...
  have h₈ : ∀ x, x ∈ closure t := by { intro, apply h₇ (h₃ x), },
  have h₉ : closure t = t := closure_eq_iff_is_closed.2 h₅,
  rw h₉ at h₈,
  ext,
  exact h₈ x,
end

theorem exercise_5_a
  (f : ℝ → ℝ)
  (E : set ℝ)
  (h₁ : is_closed E)
  (h₂ : continuous_on f E)
  : ∃ (g : ℝ → ℝ), continuous g ∧ ∀ x ∈ E, f x = g x :=
begin
  sorry,
end

theorem exercise_5_b
  (f : ℝ → ℝ)
  (E : set ℝ)
  (h₂ : continuous_on f E)
  : ∃ (E : set ℝ), ¬ ∃ (g : ℝ → ℝ), continuous g ∧ ∀ x ∈ E, f x = g x :=
begin
  sorry,
end

theorem exercise_6
  (f : ℝ → ℝ)
  (E : set ℝ)
  (h₁ : is_compact E)
  : continuous_on f E ↔ is_compact {1 : ℝ} :=
begin
  sorry,
end
