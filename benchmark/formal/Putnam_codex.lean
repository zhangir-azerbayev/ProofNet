theorem exercise_2021.b4
 fib_prod_mod_fib (m : ℕ) (hm : m > 2) :
    (∏ k in finset.range (fib m - 1), k ^ k) % fib m = fib m 

theorem exercise_2020.b5
 ne_zero_of_norm_eq_one {z : ℂ} (hz : ∣z∣ = 1) (hz1 : z ≠ 1) :
    3 - z - z^2 - z^3 - z^4 + z^5 ≠ 0 

theorem exercise_2018.a5
 exists_n_x_of_inf_diff_f_ge_0 {f : ℝ → ℝ} (hf : ∀ x, f x ≥ 0) 
    (hf0 : f 0 = 0) (hf1 : f 1 = 1) (hf_inf_diff : ∀ n, differentiable_at ℝ f n) :
    ∃ (n : ℕ) (x : ℝ), f n x < 0 

theorem exercise_2018.b2
 no_roots_in_closed_unit_disk (n : ℕ) (hn : 0 < n) :
  ∀ z : ℂ, abs z ≤ 1 → polynomial.eval (polynomial.X ^ n + (n - 1) * polynomial.X ^ (n - 1) + (n - 2) * polynomial.X ^ (n - 2) + (n - 3) * polynomial.X ^ (n - 3) + (n - 4) * polynomial.X ^ (n - 4) + (n - 5) * polynomial.X ^ (n - 5) + (n - 6)

theorem exercise_2018.b4
 periodic_of_zero {a : ℝ} (h : ∃ n, x n = 0) : 
    ∃ (p : ℕ), ∀ (n : ℕ), x (n + p) = x n 

theorem exercise_2018.b6
 card_S_le_2_pow_3860_cdot_2018_pow_2018_div_2048_pow_2018 :
    card (S : set (fin 2018 → ℕ)) ≤ 2 ^ 3860 * (2018 / 2048) ^ 2018 

theorem exercise_2017.b3
 exists_irrational_of_coeff_01 {f : ℚ → ℚ} (hf : is_power_series f) 
    (hc : ∀ i, f.coeff i = 0 ∨ f.coeff i = 1) (h23 : f (2 / 3) = 3 / 2) :
    ∃ (x : ℚ), ¬ x.is_rational 

theorem exercise_2016.a6
 exists_eq_prod_of_odd_order {G : Type*} [group G] 
    (g h : G) (hg : g.order = 2) (hgh : group_generated G [g, h]) :
    ∀ (x : G), ∃ (m n : ℕ), x = g ^ m * h ^ n 

theorem exercise_2015.b1
 has_at_least_two_real_roots_of_add_six_derivative_twelve_second_derivative_eight_third_derivative 
    {f : ℝ → ℝ} (hf : differentiable ℝ f) (hf' : differentiable ℝ (derivative f)) 
    (hf'' : differentiable ℝ (derivative (derivative f))) 
    (hf''' : differentiable ℝ (derivative (derivative (derivative f)))) 
    (hf'''' : differentiable ℝ (derivative (der