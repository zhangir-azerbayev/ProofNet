import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_b6 {a b c : ℤ}
	(h : ¬(a * b * c).is_integer) :
	¬(∃ (n : ℕ), 0 < n ∧ pell.sqrt_add_nonnegg (n ^ 3 + a * n ^ 2 + b * n + c) (n * (n + 1)) :=
sorry

theorem exercise_1999_b4 {f f' f'' : ℝ → ℝ}
	(hf : has_deriv_at f f' x) (hf' : has_deriv_at f' f'' x)
	(hf'' : ∀ (x : ℝ), 0 < f'' x) (h : ∀ (x : ℝ), f'' x < f x) (x : ℝ) :
	deriv f'' x < 2 * f x :=
sorry

theorem exercise_2001_a5 (a : ℕ+) (n : ℕ) (h₁ : a ≠ 1)
	(h₂ : a ^ (n + 1) - (a + 1) ^ n =2001) :
	a.xgcd_aux n = (a, n) :=
sorry

theorem exercise_2014_a5 {α : Type*} {l : filter α} {p : α → Prop} :
	quotient.mk = ↑p :=
sorry

theorem exercise_2018_a5 {f : ℝ → ℝ} (hf : infinitely_differentiable ℝ f)
	(h0 : f 0 = 0) (h1 : f 1 = 1) (h2 : ∀ (x : ℝ), 0 ≤ f x) :
	∃ (n : ℕ) (x : ℝ), deriv^[n] f x < 0 :=
sorry

theorem exercise_2018_b4 {a : ℝ} (h : a ≠ 0)
	(n : ℕ) :
	(2 * a).periodic n.succ → a.periodic n.succ :=
sorry