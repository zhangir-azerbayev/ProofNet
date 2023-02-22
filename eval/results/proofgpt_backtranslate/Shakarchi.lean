import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b {α : Type u} {β : Type v}
	[topological_space α] [normed_group β] {f : α → β} {s : set α}
	(hf : is_holomorphic f s) (h : set.im f = function.const α (⇑real.nnabs (f 0))) :
	f = function.const α (⇑real.nnabs (f 0)) :=
sorry

theorem exercise_1_19a (z : ℂ) (h : z ∈ metric.sphere 0 1) :
	¬summable (λ (n : ℕ), z ^ n) :=
sorry

theorem exercise_1_19c (z : ↥(metric.sphere 1 0)) :
	has_sum ↑z (λ (n : ℕ), ↑z ^ n) :=
sorry

theorem exercise_2_2 {x : ℝ} :
	∫ (a : ℝ) in 0..⊤, real.sin x / x = real.pi / 2 :=
sorry

theorem exercise_2_13 {𝕜 : Type*}
	[nondiscrete_normed_field 𝕜] [complete_space 𝕜] (f : 𝕜 → ℂ) (z0 : 𝕜)
	(hf : ∀ (z : 𝕜), z ≠ z0 → f z = 0)
	(hfd : ∀ (z : 𝕜), z ≠ z0 → f z = ⇑(fderiv 𝕜 f z0) z) :
	f.is_poly :=
sorry

theorem exercise_3_4 {a : ℝ} (h : 0 < a) :
	∫ (x : ℝ) in set.Ioi (-a), (x * real.sin x) / (x ^ 2 + a ^ 2) = real.pi * real.exp (-a) :=
sorry

theorem exercise_3_14 {α : Type*}
	[add_comm_group α] {a b : α} (ha : a ≠ 0) :
	function.injective (λ (z : α), a * z + b) ∧ function.injective (λ (z : α), a * z + b) :=
sorry

theorem exercise_5_1 {α : Type} {ds : list ℕ}
	[metric_space α] {β : Type*} [add_comm_group β] {f : ℂ → α}
	(h : holor.bounded f (unit_disc.radius) ds)
	(h' : ¬∃ (z : ℂ) (H : z ∈ unit_disc.radius), f z = 0) {z : ℂ}
	(hz : ∥z∥ < 1) :
	summable (λ (n : ℕ), 1 - ∥z n∥) :=
sorry