theorem exercise_2021.b4
(m : ℕ) (hm : m > 2) : (∏ k in finset.range (fib m - 1), k ^ k) % fib m = fib m :=
sorry

theorem exercise_2020.b5
{z : ℂ} (hz : ∣z∣ = 1) (hz1 : z ≠ 1) : 3 - z - z^2 - z^3 - z^4 + z^5 ≠ 0 :=
sorry

theorem exercise_2018.a5
{f : ℝ → ℝ} (hf : ∀ x, f x ≥ 0) (hf0 : f 0 = 0) (hf1 : f 1 = 1) (hf_inf_diff : ∀ n, differentiable_at ℝ f n) : ∃ (n : ℕ) (x : ℝ), f n x < 0 :=
sorry

theorem exercise_2018.b2
(n : ℕ) (hn : 0 < n) : ∀ z : ℂ, abs z ≤ 1 → polynomial.eval (polynomial.X ^ n + (n - 1) * polynomial.X ^ (n - 1) + (n - 2) * polynomial.X ^ (n - 2) + (n - 3) * polynomial.X ^ (n - 3) + (n - 4) * polynomial.X ^ (n - 4) + (n - 5) * polynomial.X ^ (n - 5) + (n - 6) :=
sorry

theorem exercise_2018.b4
{a : ℝ} (h : ∃ n, x n = 0) : ∃ (p : ℕ), ∀ (n : ℕ), x (n + p) = x n :=
sorry

theorem exercise_2018.b6
: card (S : set (fin 2018 → ℕ)) ≤ 2 ^ 3860 * (2018 / 2048) ^ 2018 :=
sorry

theorem exercise_2017.b3
{f : ℚ → ℚ} (hf : is_power_series f) (hc : ∀ i, f.coeff i = 0 ∨ f.coeff i = 1) (h23 : f (2 / 3) = 3 / 2) : ∃ (x : ℚ), ¬ x.is_rational :=
sorry

theorem exercise_2016.a6
{G : Type*} [group G] (g h : G) (hg : g.order = 2) (hgh : group_generated G [g, h]) : ∀ (x : G), ∃ (m n : ℕ), x = g ^ m * h ^ n :=
sorry

theorem exercise_2015.b1
{f : ℝ → ℝ} (hf : differentiable ℝ f) (hf' : differentiable ℝ (derivative f)) (hf'' : differentiable ℝ (derivative (derivative f))) (hf''' : differentiable ℝ (derivative (derivative (derivative f)))) (hf'''' : differentiable ℝ (derivative (der :=
sorry