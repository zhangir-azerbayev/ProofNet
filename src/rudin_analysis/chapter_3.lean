import tactic
import data.real.sqrt
import analysis.specific_limits.basic
import analysis.specific_limits.normed

open filter real

open_locale topological_space
noncomputable theory


theorem Rudin_3_1
  (f : ℕ → ℝ)
  (h : ∃ (a : ℝ), tendsto (λ (n : ℕ), f n) at_top (𝓝 a))
  : ∃ (a : ℝ), tendsto (λ (n : ℕ), |f n|) at_top (𝓝 a) :=
begin
  cases h with a h,
  use |a|,
  apply filter.tendsto.abs h,
end

theorem Rudin_3_2
  : tendsto (λ (n : ℝ), (sqrt (n^2 + n) - n)) at_top (𝓝 (1/2)) :=
begin
  have h : ∀ (n : ℝ), n > 0 → sqrt (n^2 + n) - n = 1 / (sqrt (1 + 1 / n) + 1) :=
  begin
    intro n,
    intro h,
    have h₁ : sqrt (n^2 + n) + n ≠ 0 := by {intro h₁, simp at *, rw ←h₁ at h, simp at h,
              have : sqrt (n ^ 2 + n) ≥ 0 := sqrt_nonneg (n ^ 2 + n), linarith,},
    have h₂ : sqrt (n^2 + n) + n = sqrt (n^2 + n) + n := by refl,
    have h₃ : n ≥ 0 := by linarith,
    have h₄ : n ≠ 0 := by linarith,
    have h₅ : n^2 + n ≥ 0 := by {simp, transitivity, apply h₃, simp, apply sq_nonneg},
    calc  _ = (sqrt (n^2 + n) - n) * 1 : by rw mul_one _
        ... = (sqrt (n^2 + n) - n) * ((sqrt (n^2 + n) + n) /
                                      (sqrt (n^2 + n) + n)) : by rw ←((div_eq_one_iff_eq h₁).2 h₂)
        ... = n / (sqrt (n^2 + n) + n) : by {field_simp, ring, simp [sq_sqrt h₅]}
        ... = 1 / (sqrt (n^2 + n) / sqrt (n^2) + n / sqrt (n^2)) : by {field_simp, simp [sqrt_sq h₃]}
        ... = 1 / (sqrt (n^2 + n) / sqrt (n^2) + 1) : by simp [sqrt_sq h₃, (div_eq_one_iff_eq h₄).2]
        ... = 1 / (sqrt (1 + n / (n ^ 2)) + 1): by {rw ←(sqrt_div h₅ (n^2)), field_simp}
        ... = 1 / (sqrt (1 + 1 / n) + 1): by simp [pow_succ]
  end,
  refine (tendsto_congr' _).mp _,
  exact λ n, 1 / (sqrt (1 + 1 / n) + 1),
  refine eventually_at_top.mpr _,
  use 1,
  intros b bgt1, symmetry, apply h, linarith,
  have g : tendsto (λ (n : ℝ), 1 / n) at_top (𝓝 0) :=
  begin
    simp,
    apply tendsto_inv_at_top_zero,
  end,
  have h : tendsto (λ (n : ℝ), 1 / (sqrt (1 + n) + 1)) (𝓝 0) (𝓝 (1/2)) :=
  begin
    have : (1/2 : ℝ) = (λ (n : ℝ), 1 / (sqrt (1 + n) + 1)) 0 := by {simp, norm_num}, rw this,
    apply continuous_at.tendsto, simp,
    refine continuous_at.comp _ _, simp,
    refine continuous_at.add _ _,
    refine continuous_at.sqrt _, simp,
    refine continuous_at.add _ _,
    exact continuous_at_const,
    exact continuous_at_id,
    exact continuous_at_const,
  end,
  apply tendsto.comp h g,
end

noncomputable def f : ℕ → ℝ
| 0 := sqrt 2
| (n + 1) := sqrt (2 + sqrt (f n))

theorem Rudin_3_3
  : ∃ (x : ℝ), tendsto f at_top (𝓝 x) ∧ ∀ n, f n < 2
  :=
begin
  sorry,
end
