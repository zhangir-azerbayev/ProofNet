import tactic
import data.real.sqrt
import analysis.specific_limits.basic
import analysis.specific_limits.normed

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
      apply tendsto_const_nhds,
    },
    {
      intros a b,
      simp,
      --suggest,
      refine mem_nhds_iff.mpr _,
      simp,
      use {z | -|x| < z ∧ z < |x|},
      simp,
      split,
    },
  },
  {
  intro h,
  have : continuous_at (λ (x : ℝ), ite (x = 0) (1 : ℝ) 0) 0 := continuous.continuous_at h,
  have := continuous_at.tendsto this,
  simp at this,
  unfold tendsto at this,
  unfold map at this,
  sorry,
  },
end

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
  simp at *,
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
  (h₁ : continuous f)
  (s : set α)
  (h₂ : dense s)
  --: closure (f '' s) = f '' α :=
  : closure (f '' s) = set.range f :=
begin
  sorry,
end

theorem exercise_4_b
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f : α → β)
  (g : α → β)
  (h₁ : continuous f)
  (h₂ : continuous g)
  (s : set α)
  (h₃ : dense s)
  (h₄ : ∀ x ∈ s, f x = g x)
  : f = g :=
begin
  ext,
  by_cases h₅ : x ∈ s,
  {exact h₄ x h₅,},
  {
    --by_contradiction h₆,
    sorry,
  },
end
