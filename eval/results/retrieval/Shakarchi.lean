import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b {f : ℂ → ℂ} (hf : holomorphic f)
  (h : ∀ (z : ℂ), f z = f 0) :
  ∀ (z : ℂ), f z = 0 :=
sorry

theorem exercise_1_19a (z : ℂ) (hz : complex.abs z = 1) :
  ¬converges_to_fun (λ (n : ℕ), n * z ^ n) z :=
sorry

theorem exercise_1_19c (z : ℂ) :
	is_cau_seq has_abs.abs (λ (n : ℕ), (finset.range n).sum (λ (m : ℕ), ⇑complex.abs (z ^ m / ↑(m.factorial)))) :=
sorry

theorem exercise_2_2 :
  ∫ (0 : ℝ) ∞ (λ x, sin x / x) = real.pi / 2 :=
sorry

theorem exercise_2_13 {𝕜 : Type*}
	[nontrivially_normed_field 𝕜] {E : Type*} [normed_add_comm_group E]
	[normed_space 𝕜 E] {f : 𝕜 → E} {z₀ : 𝕜} {U : set 𝕜} (hf : analytic_on 𝕜 f U)
	(hU : is_preconnected U) (h₀ : z₀ ∈ U)
	(hfz₀ : z₀ ∈ closure ({z : 𝕜 | f z = 0} \ {z₀})) :
	set.eq_on f 0 U :=
sorry

theorem exercise_3_4 {a : ℝ} (ha : 0 < a) :
  ∫ (x : ℝ) in -∞..∞, x * sin x / (x ^ 2 + a ^ 2) = π * exp (-a) :=
sorry

theorem exercise_3_14 {f : ℂ → ℂ} (hf : entire f) (hfi : function.injective f) :
  ∃ (a b : ℂ), a ≠ 0 ∧ f = λ z, a * z + b :=
sorry

theorem exercise_5_1  {f : ℂ → ℂ} (hf : holomorphic f) (hf0 : f ≠ 0) (hf1 : ∀ z, f z = 0 → abs z < 1)
  (hf2 : ∀ z, abs z < 1 → f z ≠ 0) (hf3 : ∀ z, abs z < 1 → abs (f z) ≤ 1) :
  ∑' (z : ℂ), 1 - abs z < ⊤ :=
sorry