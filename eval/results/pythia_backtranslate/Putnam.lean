import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_b6 (a b c : ℤ) :
	0 < int.sqrt (↑(a.nat_abs) * ↑(b.nat_abs) + ↑(c.nat_abs)) ∨ int.sqrt (↑(a.nat_abs) * ↑(b.nat_abs) + ↑(c.nat_abs)) ∈ set.Ico a b :=
sorry

theorem exercise_1999_b4 {f : ℝ → ℝ} {f' : ℝ}
	(hf : has_deriv_at f f'⁻¹) (hfc : continuous f)
	(hf' : ∀ (x : ℝ), 0 < f' x) (hf'' : ∀ (x : ℝ), f' x < 2 * f x) (hf'' : ∀ (x : ℝ), f' x < f x)
	(hff' : ∀ (x : ℝ), f' x < f x) (hff'' : ∀ (x : ℝ), f' x < f x)
	(hff'' : ∀ (x : ℝ), f' x < f x) (hxy : f' x < f x) (hx : f x < f x) :
	f' < 2 * f x :=
sorry

theorem exercise_2001_a5 (R : Type*) [comm_ring R]
	[is_domain R] :
	(polynomial.X ^ (n + 1) - (polynomial.X + 1) ^ n) = polynomial.X ^ n + 1 :=
sorry

theorem exercise_2014_a5 {α : Type*} (x : option α) :
	(option.none.get_or_else x).get_or_else option.none = x :=
sorry

theorem exercise_2018_a5 (f : ℝ → ℝ) (hf : ∃ᶠ (n : ℕ) in filter.at_top, f n = 0)
	(h_pos : ∀ (x : ℝ), 0 ≤ f x) (h_one : f 1 = 1)
	(h_neg : ∀ (x : ℝ), 0 ≤ f x) :
	∃ (n : ℕ) (x : ℝ) (H : x ∈ set.Ioo 0 1), f n < x :=
sorry

theorem exercise_2018_b4 {a : ℝ} {x₀ x₁ x₂ : ℝ}
	(h : x₀ = x₁ ∧ x₀ = x₂) (hx₁ : 2 ≤ x₁) (hx₂ : 2 ≤ x₂) :
	x₀ = x₁ + a * x₂ - x₄ :=
sorry