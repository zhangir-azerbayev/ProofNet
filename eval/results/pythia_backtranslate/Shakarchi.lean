import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b {f : ℂ → ℝ} {s : set ℂ}
	(hs : is_open s) (hf : ∀ (z : ℂ), z ∈ s → z.im = f z) :
	f = function.const ℂ s (f z) :=
sorry

theorem exercise_1_19a {R : Type*} [comm_ring R]
	[is_domain R] (z : R) :
	¬summable (λ (n : ℕ), ↑n * z ^ n) :=
sorry

theorem exercise_1_19c (z : ℂ) (hz : z ≠ 1) :
	summable (λ (n : ℕ), (↑n)⁻¹ * z ^ n) :=
sorry

theorem exercise_2_2 :
	∫ (x : ℝ) in 0..⊤, real.sin x / x * complex.I = real.pi / 2 :=
sorry

theorem exercise_2_13 : ℂ → ℂ} (hf : analytic f) ( : ℂ)
	(h : ∀ (z₀ : ℂ), z₀ ∈ metric.sphere c |R| → (∃ (r : ℝ) (H : r > 0), f =O[filter.comap complex.abs filter.at_top ⊓ filter.principal {z : ℂ | z₀ ^ complex.abs z - z₀} r)) :
	has_sum (λ (z : ℂ), (z - z₀) ^ z.re * complex.abs (z - z₀)) f :=
sorry

theorem exercise_3_4 (a : ℝ) (h : 0 < a) :
	(∫ (x : ℝ) in ℝ, real.sin x / (x ^ 2 + a ^ 2) = real.pi * real.exp (-a)) :=
sorry

theorem exercise_3_14 (z : ℂ) :
	complex.abs (z - {re :=
sorry

theorem exercise_5_1 {f : holor ℂ unit_ds} {z : ℂ → ℂ}
	(hz : ∀ (i : ℕ), z i = z ↑i) (h_nn_abs : ∀ (i : ℕ), z i.nat_abs = 1)
	(h_mble : ∃ (i : ℕ), z i = 0) (h_nble : ∃ (i : ℕ), z i.re ≠ 0) :
	(finset.range n).sum (λ (k : ℕ), 1 - complex.abs (z k)) < ⊤ :=
sorry