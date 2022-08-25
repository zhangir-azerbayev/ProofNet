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
  use λ x, if x = 0 then (1 : ℝ) else (0 : ℝ),
  split,
  {
    intro x,
    by_cases h : x = 0,
    {
      rw h, simp,
      exact tendsto_const_nhds,
    },
    {
      intros X hX,
      refine mem_nhds_iff.2 _,
      use {z | -|x| < z ∧ z < |x|},
      simp,
      split,
      {
        set f := (λ (y : ℝ), ite (x + y = 0) (1 : ℝ) 0 - ite (x - y = 0) 1 0),
        set f₁ := (λ (y : ℝ), ite (x + y = 0) (1 : ℝ) 0),
        set f₂ := (λ (y : ℝ), - ite (x - y = 0) (1 : ℝ) 0),
        set Y := {z : ℝ | - | x | < z ∧ z < | x |},
        have : (0 : ℝ) ∈ X := mem_of_mem_nhds hX,
        have h₁: {(0 : ℝ)} ⊆ X := set.singleton_subset_iff.mpr this,
        have h₂ : f = f₁ + f₂ := rfl,
        have g₁ : ∀ y ∈ Y, ¬x + y = 0 := by {
          simp,
          intros y hy₁ hy₂ hy₃,
          by_cases hx : 0 < x,
          rw abs_of_pos hx at *,
          linarith,
          simp at hx,
          have hx : x < 0 := lt_of_le_of_ne hx h,
          rw abs_of_neg hx at *,
          linarith,
        },
        have g₂ : ∀ y ∈ Y, ¬x - y = 0 := by {
          simp,
          intros y hy₁ hy₂ hy₃,
          by_cases hx : 0 < x,
          rw abs_of_pos hx at *,
          linarith,
          simp at hx,
          have hxx : x < 0 := lt_of_le_of_ne hx h,
          rw abs_of_neg hxx at *,
          linarith,
        },
        have gg₁ : ∀ y ∈ Y, f₁ y = (0 : ℝ) := by {
          intros a b,
          simp,
          intro c,
          exact g₁ a b c,
        },
        have gg₂ : ∀ y ∈ Y, f₂ y = (0 : ℝ) := by {
          intros a b,
          simp,
          intro c,
          exact g₂ a b c,
        },
        have gg : ∀ z ∈ Y, f z = (0 : ℝ) := by {
          intros a b,
          simp [h₂],
          rw [gg₁ a b, gg₂ a b],
          norm_num,
        },
        have : f ⁻¹' {(0 : ℝ)} ⊆ f ⁻¹' X := set.preimage_mono h₁,
        exact set.subset.trans gg this,
      },
      {
        split,
        {
          rw set.set_of_and,
          apply is_open.inter _ _,
          apply is_open_lt' (-|x|),
          apply is_open_gt' (|x|),
        },
        {
          exact h,
        }
      },
    },
  },
  {
    intro h,
    let f : (ℝ → ℝ) := λ x, if x = 0 then (1 : ℝ) else 0,
    have g : f 0 = 1 := if_pos rfl,
    have g₁ : f 1 = 0 := by {refine if_neg _, norm_num,},
    have : continuous_at f 0 := continuous.continuous_at h,
    have := continuous_at.tendsto this,
    rw g at this,
    unfold tendsto at this,
    have := le_def.1 this,
    simp at this,
    have := this (set.Ioo (0.5 : ℝ) (1.5 : ℝ)),
    have i : set.Ioo (0.5 : ℝ) (1.5 : ℝ) ∈ 𝓝 (1 : ℝ) := by {
      apply is_open.mem_nhds,
      exact is_open_Ioo,
      norm_num,
    },
    have h₁ : set.range f  = {(0 : ℝ), 1} := by {
      ext, split,
      {
        intro h,
        simp,
        cases h,
        by_cases r : h_w = 0,
        rw r at h_h,
        rw g at h_h,
        right,
        exact eq.symm h_h,
        have ii : f h_w = 0 := if_neg r,
        rw ii at h_h,
        left,
        symmetry,
        exact h_h,
      },
      intro h,
      apply set.mem_range.2,
      by_cases r : x = 0,
      {
        use 1,
        rw g₁,
        apply eq.symm _,
        exact r,
      },
      {
        have i : x ∈ {(1 : ℝ)} := by {
          apply set.mem_of_mem_insert_of_ne _ r,
          exact h,
        },
        use 0,
        rw g,
        exact eq.symm i,
      },
    },
    have h₂ : set.Ioo ((1 / 2 : ℝ)) (3 / 2) ∩ {(0 : ℝ), 1} = {(1 : ℝ)} := by {
      unfold set.Ioo,
      ext, split,
      {
        intro h,
        simp at h,
        cases h,
        cases h_right,
        rw h_right at h_left,
        norm_num at h_left,
        exact h_right,
      },
      {
        intro h,
        have : x = 1 := h,
        rw this,
        norm_num,
      },
    },
    have h₃ : {0} ⊆ f ⁻¹' {(1 : ℝ)} := set.singleton_subset_iff.mpr g,
    have j : f ⁻¹' set.Ioo (1 / 2) (3 / 2) = {0} := by {
      rw [← set.preimage_inter_range, h₁, h₂],
      ext,
      split,
      {
        intro hx,
        by_contradiction h₄,
        have : ¬ x = 0 := h₄,
        have h₅ : f x = 0 := if_neg this,
        have : f x = 1 := hx,
        rw h₅ at this,
        norm_num at this,
      },
      intro x, exact h₃ x,
    },
    have := this i,
    rw j at this,
    have := mem_nhds_iff.1 this,
    cases this with s h,
    cases h with k g,
    cases g,
    by_cases a : s = {0},
    {
      rw a at g_left,
      have := dense_compl_singleton (0 : ℝ),
      have := dense_compl_singleton_iff_not_open.1 this,
      contradiction,
    },
    {
      have : {(0 : ℝ)} ⊆ s := set.zero_subset.mpr g_right,
      have : s = {(0 : ℝ)} := set.subset.antisymm k this,
      contradiction,
    },
  },
end

theorem exercise_1_shorter
  : ∃ (f : ℝ → ℝ), (∀ (x : ℝ), tendsto (λ y, f(x + y) - f(x - y)) (𝓝 0) (𝓝 0)) ∧ ¬ continuous f :=
begin
  let f := λ x : ℝ, if x = 0 then (1 : ℝ) else (0 : ℝ),
  use f, split,
  { intro x,
    suffices : (λ y, f (x + y) - f(x - y)) =ᶠ[𝓝 0] (λ y, 0),
    { simp [filter.tendsto_congr' this,  tendsto_const_nhds_iff] },
    by_cases h : x = 0,
    { dsimp [f], simp [h] },
    have : set.Ioo (-abs x) (abs x) ∈ 𝓝 (0 : ℝ),
    { apply Ioo_mem_nhds; simp [h] },
    apply eventually_of_mem this,
    intro y, simp, dsimp [f],
    intros h1 h2,
    rw [if_neg, if_neg]; simp [lt_abs, neg_lt] at *; cases h1; cases h2; linarith },
  simp [continuous_iff_continuous_at, continuous_at, tendsto_nhds],
  use [0, set.Ioo 0 2, is_open_Ioo], split,
  { dsimp [f], simp, norm_num },
  simp [mem_nhds_iff_exists_Ioo_subset],
  intros a b aneg bpos h,
  have : b / 2 ∈ set.Ioo a b,
  { simp, split; linarith },
  have := h this,
  simpa [f, (ne_of_lt bpos).symm] using this
end

theorem exercise_2
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f : α → β)
  (h₁ : continuous f)
  : ∀ (x : set α), f '' (closure x) ⊆ closure (f '' x) :=
begin
  intros X x h₂ Y h₃,
  simp at *,
  cases h₃ with h₃ h₄,
  cases h₂ with w h₅,
  cases h₅ with h₅ h₆,
  have h₈ : is_closed (f ⁻¹' Y) := is_closed.preimage h₁ h₃,
  have h₉ : closure X ⊆ f ⁻¹' Y := closure_minimal h₄ h₈,
  rw ←h₆,
  exact h₉ h₅,
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
  : ∃ (E : set ℝ) (f : ℝ → ℝ), (continuous_on f E) ∧
    (¬ ∃ (g : ℝ → ℝ), continuous g ∧ ∀ x ∈ E, f x = g x) :=
begin
  set E : set ℝ := (set.Iio 0) ∪ (set.Ioi 0) with hE,
  let f : ℝ → ℝ := λ x, if x < 0 then 0 else 1,
  use E, use f,
  split,
  {
    refine continuous_on_iff.mpr _,
    intros x h₁ X h₂ h₃,
    by_cases h₄ : x < 0,
    {
      use set.Ioo (x - 1) 0,
      have h₅ : f x = 0 := if_pos h₄,
      split, exact is_open_Ioo,
      split,
      {
        have h₆ : x - 1 < x := by linarith,
        exact set.mem_sep h₆ h₄,
      },
      have h₆ : set.Ioo (x - 1) 0 ⊆ set.Iio 0 := set.Ioo_subset_Iio_self,
      have h₇ : set.Ioo (x - 1) 0 ∩ E = set.Ioo (x - 1) 0 := by {
        rw hE, simp, exact set.subset_union_of_subset_left h₆ (set.Ioi 0),
      },
      rw h₇,
      have h₈ : (0 : ℝ) ∈ X := by {rw h₅ at h₃, exact h₃,},
      have h₉ : {(0 : ℝ)} ⊆ X := set.singleton_subset_iff.mpr h₈,
      have h₁₀ : set.Iio 0 ⊆ f ⁻¹' {0} := by {
        intros y hy,
        apply set.mem_preimage.2,
        have : f y = 0 := if_pos hy,
        rw this, simp,
      },
      have h₁₁ : f ⁻¹' {0} ⊆ f ⁻¹' X := set.preimage_mono h₉,
      have h₁₂ : set.Iio 0 ⊆ f ⁻¹' X := set.subset.trans h₁₀ h₁₁,
      exact set.subset.trans h₆ h₁₂,
    },
    {
      use set.Ioo 0 (x + 1),
      have h₄' : x > 0  := by {
        have : x ≠ 0 := by {rw hE at h₁, simp at h₁, exact h₁,},
        refine lt_of_le_of_ne _ this.symm,
        exact not_lt.mp h₄,
      },
      have h₅ : f x = 1 := if_neg h₄,
      split, exact is_open_Ioo,
      split,
      {
        have h₆ : x < x + 1:= by linarith,
        exact set.mem_sep h₄' h₆,
      },
      have h₆ : set.Ioo 0 (x + 1) ⊆ set.Ioi 0 := set.Ioo_subset_Ioi_self,
      have h₇ : set.Ioo 0 (x + 1) ∩ E = set.Ioo 0 (x + 1) := by {
        rw hE, simp, exact set.subset_union_of_subset_right h₆ (set.Iio 0),
      },
      rw h₇,
      have h₈ : (1 : ℝ) ∈ X := by {rw h₅ at h₃, exact h₃,},
      have h₉ : {(1 : ℝ)} ⊆ X := set.singleton_subset_iff.mpr h₈,
      have h₁₀ : set.Ioi 0 ⊆ f ⁻¹' {1} := by {
        intros y hy,
        have : y ∈ set.Ici (0 : ℝ) := set.mem_Ici_of_Ioi hy,
        have : ¬ y < 0 := asymm hy,
        apply set.mem_preimage.2,
        have : f y = 1 := if_neg this,
        rw this, simp,
      },
      have h₁₁ : f ⁻¹' {1} ⊆ f ⁻¹' X := set.preimage_mono h₉,
      have h₁₂ : set.Ioi 0 ⊆ f ⁻¹' X := set.subset.trans h₁₀ h₁₁,
      exact set.subset.trans h₆ h₁₂,
    },
  },
  {
    by_contradiction h₁,
    cases h₁ with g h₁,
    cases h₁ with h₁ h₂,
    have h₃ : continuous_at g 0 := continuous.continuous_at h₁,
    have h₄ := continuous_at.tendsto h₃,
    unfold tendsto at h₄,
    have h₅ := le_def.1 h₄,
    simp at h₅,
    by_cases h₆ : g 0 > 0.5,
    {
      have h₇ : set.Ioi (0 : ℝ) ∈ 𝓝 (g 0) := by { refine Ioi_mem_nhds _, linarith,},
      have h₈ := h₅ (set.Ioi (0 : ℝ)) h₇,
      have h₉ : g ⁻¹' set.Ioi 0 = set.Ici 0 := by {
        ext,
        split,
        {
          intro h,
          simp at h,
          by_cases hw : x = 0,
          {rw hw, exact set.left_mem_Ici,},
          {
            have : x ∈ E := by {rw hE, simp, exact hw,},
            rw ←(h₂ x this) at h,
            by_contradiction hh,
            simp at hh,
            have : f x = 0 := if_pos hh,
            linarith,
          },
        },
        {
          intro h,
          simp,
          by_cases hw : x = 0,
          {rw hw, linarith,},
          {
            have h₉ : x > 0 := (ne.symm hw).le_iff_lt.mp h,
            have : x ∈ E := (set.Iio 0).mem_union_right h₉,
            rw ←(h₂ x this),
            have : ¬ x < 0 := asymm h₉,
            have : f x = 1 := if_neg this,
            linarith,
          },
        },
      },
      rw h₉ at h₈,
      have h₁₀ := interior_mem_nhds.2 h₈,
      simp at h₁₀,
      have := mem_of_mem_nhds h₁₀,
      simp at this,
      exact this,
    },
    {
      have h₇ : set.Iio (1 : ℝ) ∈ 𝓝 (g 0) := by { refine Iio_mem_nhds _, linarith, },
      have h₈ := h₅ (set.Iio (1 : ℝ)) h₇,
      have h₉ : g ⁻¹' set.Iio 1 = set.Iic 0 := by {
        ext,
        split,
        {
          intro h,
          simp at h,
          by_cases hw : x = 0,
          {simp [hw],},
          {
            have : x ∈ E := by {rw hE, simp, exact hw,},
            rw ←(h₂ x this) at h,
            by_contradiction hh,
            simp at hh,
            have : f x = 1 := if_neg ((by linarith) : ¬x < 0),
            linarith,
          },
        },
        {
          intro h,
          simp,
          by_cases hw : x = 0,
          {rw hw, linarith,},
          {
            have h₉ : x < 0 := (ne.le_iff_lt hw).mp h,
            have : x ∈ E := (set.Ioi 0).mem_union_left h₉,
            rw ←(h₂ x this),
            have : f x = 0 := if_pos h₉,
            linarith,
          },
        },
      },
      rw h₉ at h₈,
      have h₁₀ := interior_mem_nhds.2 h₈,
      simp at h₁₀,
      have := mem_of_mem_nhds h₁₀,
      simp at this,
      exact this,
    }
  }
end

theorem exercise_6
  (f : ℝ → ℝ)
  (E : set ℝ)
  (G : set (ℝ × ℝ))
  (h₁ : is_compact E)
  (h₂ : G = {(x, f x) | x ∈ E})
  : continuous_on f E ↔ is_compact G :=
begin
  sorry,
end
