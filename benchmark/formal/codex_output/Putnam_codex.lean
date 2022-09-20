theorem exercise_2021_b4 (m : ℕ) (hm : m > 2) :
    (∏ k in finset.range (fib m - 1), k ^ k) % fib m ∈ fibset :=
sorry

theorem exercise_2020_b5 {z1 z2 z3 z4 : ℂ} 
    (h1 : abs z1 = 1) (h2 : abs z2 = 1) (h3 : abs z3 = 1) (h4 : abs z4 = 1) 
    (h5 : z1 ≠ 1) (h6 : z2 ≠ 1) (h7 : z3 ≠ 1) (h8 : z4 ≠ 1) : 
    3 - z1 - z2 - z3 - z4 + z1 * z2 * z3 * z4 ≠ 0 :=
sorry

theorem exercise_2018_a5 {f : ℝ → ℝ} (hf : ∀ x, f x = 0) 
    (hf1 : f 1 = 1) (hf2 : ∀ x, f x ≥ 0) :
    ∃ (n : ℕ) (x : ℝ), (fderiv n f x) < 0 :=
sorry

theorem exercise_2018_b2 (n : ℕ) (hn : 0 < n) :
    ∀ (z : ℂ), abs z ≤ 1 → polynomial.eval (polynomial.X ^ n + (n - 1) * polynomial.X ^ (n - 1) + (n - 2) * polynomial.X ^ (n - 2) + (n - 3) * polynomial.X ^ (n - 3) + (n - 4) * polynomial.X ^ (n - 4) + (n - 5) * polynomial.X ^ (n - 5) + (n - 6 :=
sorry

theorem exercise_2018_b4 {a : ℝ} (h : ∃ n, x n = 0) : 
    ∃ (p : ℕ), ∀ (n : ℕ), x (n + p) = x n :=
sorry

theorem exercise_2018_b6 :
    card (S : set (fin 2018 → ℕ)) ≤ 2 ^ 3860 * (2018 / 2048) ^ 2018 :=
sorry

theorem exercise_2017_b3 {f : ℚ → ℚ} (hf : is_power_series f) 
    (hc : ∀ i, f.coeff i = 0 ∨ f.coeff i = 1) (h23 : f (2 / 3) = 3 / 2) :
    ∃ (x : ℚ), ¬ x.is_rational :=
sorry

theorem exercise_2016_a6 
    {G : Type*} [group G] (g h : G) (hg : g.order.odd) 
    (hgh : is_group_generated G [g, h]) :
    ∃ (r : ℕ) (m n : fin r → ℤ), r ≤ G.card ∧ 
    ∀ (i : fin r), m i ∈ {-1, 1} ∧ n i ∈ {-1, 1} ∧ 
    g ^ m i :=
sorry

theorem exercise_2015_b1 
    {f : ℝ → ℝ} (hf : three_times_differentiable_on ℝ f) 
    (hfz : has_five_distinct_zeros f) :
    has_two_distinct_zeros (λ x, f x + 6 * f' x + 12 * f'' x + 8 * f''' x) :=
sorry