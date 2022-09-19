theorem exercise_1.13a
 is_const_of_real_const {f : ℂ → ℂ} (hf : holomorphic f) 
    (hc : is_const (λ z, (f z).re)) : is_const f 

theorem exercise_1.13b
 is_const_of_holomorphic_of_const_im {f : ℂ → ℂ} 
    (hf : holomorphic f) (hc : ∀ z, f z ∈ set.Icc 0 0) :
    ∃ (c : ℂ), f = function.const ℂ c 

theorem exercise_1.13c
 is_constant_of_constant_abs {f : ℂ → ℂ} (hf : holomorphic f) 
    (h : ∀ z, abs (f z) = abs (f 0)) :
    ∀ z, f z = f 0 

theorem exercise_1.18
 exists_power_series_around_point_of_convergence {R : Type*} 
    [comm_ring R] {f : ℕ → R} (hf : power_series.is_power_series f) 
    (z : R) (hz : power_series.converges_at f z) :
    ∃ (g : ℕ → R), power_series.is_power_series g ∧ 
    power_series.converges_at g 0 ∧ 
    ∀ (n : ℕ), g n = f n * z ^ n 

theorem exercise_1.19a
 power_series_not_converges_on_unit_circle (z : ℂ) (hz : abs z = 1) :
    ¬ (∃ (r : ℝ), abs (∑ n, n * z ^ n) < r) 

theorem exercise_1.19b
 power_series_converges_at_unit_circle (z : ℂ) : 
    abs z = 1 → power_series.converges_at (λ n, z / n ^ 2) z 

theorem exercise_1.19c
 power_series_converges_at_unit_circle (z : ℂ) (hz : abs z = 1) 
    (h1 : z ≠ 1) :
    power_series.converges (λ n, z ^ n / n) 

theorem exercise_1.22
 not_finite_partition_of_nat_into_arithmetic_progressions 
    (S : finset ℕ) (hS : ∀ s ∈ S, ∃ a d : ℕ, s = {a, a + d, a + 2 * d, a + 3 * d, a + 4 * d, a + 5 * d, a + 6 * d, a + 7 * d, a + 8 * d, a + 9 * d}) :
    false 

theorem exercise_1.26
 exists_eq_const_of_bounded {E : Type u} [normed_group E]
    [normed_space ℂ E] {F : Type v} [normed_group F] [normed_space ℂ F]
    {f : E → F} (hf : differentiable ℂ f) (hb : metric.bounded (set.range f)) :
    ∃ (c : F), f = function.const E c 

theorem exercise_2.2
 integral_sin_x_div_x_eq_pi_div_two :
  ∫ (0 : ℝ) ∞ (λ x, sin x / x) = π / 2 

theorem exercise_2.5
 complex.triangle_integral_eq_zero {f : ℂ → ℂ} {Ω : set ℂ}
    (hf : ∀ z ∈ Ω, f.has_fderiv_at ℂ z) (T : set ℂ) (hT : is_triangle T)
    (hT_subset_Ω : T ⊆ Ω) (hT_interior_subset_Ω : interior T ⊆ Ω) :
    complex.integral T f = 0 

theorem exercise_2.6
 holomorphic_triangle_integral_eq_zero {Ω : Type*} [complex_normed_vector_space Ω]
    [normed_group Ω] [normed_space ℂ Ω] [complete_space Ω]
    (hΩ : is_open Ω) (T : set Ω) (hT : is_open (interior T))
    (hT' : interior T ⊆ Ω) (f : Ω → ℂ) (hf : holomorphic_on Ω f)
    (w : Ω) (hw : w ∈ interior T) (hb : metric

theorem exercise_2.9
 holomorphic_linear_of_fixed_point_and_derivative_eq_one 
    {Ω : Type*} [nondiscrete_normed_field Ω] [normed_group Ω] 
    [normed_space ℂ Ω] [complete_space Ω] [open_mapping_theorem Ω] 
    [holomorphic_on_open_subset Ω] (Ω_bounded : bounded_subset Ω) 
    (Ω_open : is_open Ω) (φ : Ω → Ω) (hφ : holomorphic_on Ω

theorem exercise_2.13
 is_analytic_of_coeff_eq_zero_of_analytic {f : ℂ → ℂ} 
    (hf : ∀ z₀ : ℂ, ∃ n : ℕ, f.is_analytic_at z₀ ∧ 
    (∀ m : ℕ, m < n → f.taylor_nth_coeff z₀ m = 0)) :
    ∃ (p : polynomial ℂ), f = p 

theorem exercise_3.2
 integral_of_inv_one_plus_x_four :
    ∫⁻∞ ∞ (λ x, 1 / (1 + x ^ 4)) = π / 2 

theorem exercise_3.3
 integral_cos_over_x_squared_plus_a_squared (a : ℝ) (ha : 0 < a) :
    ∫⁻ a⁺ (λ x, cos x / (x ^ 2 + a ^ 2)) = π * (exp (-a) / a) 

theorem exercise_3.4
 integral_sin_over_x_squared_plus_a_squared (a : ℝ) (ha : 0 < a) :
    ∫⁻∞ ∞ (λ x, x * sin x / (x ^ 2 + a ^ 2)) = π * exp (-a) 

theorem exercise_3.9
 integral_log_sin_pi_x : ∫ (0 : ℝ) 1 (λ x, log (sin (π * x))) = -log 2 

theorem exercise_3.14
 entire_injective_iff_linear {f : ℂ → ℂ} (hf : entire f) 
    (hf_inj : function.injective f) :
    ∃ (a b : ℂ), f = λ z, a * z + b ∧ a ≠ 0 

theorem exercise_3.22
 no_holomorphic_function_extends_to_1_over_z (D : set ℂ) 
    (hD : is_open D) (hD_unit_disc : D = set.ball 0 1) 
    (hD_contains_0 : 0 ∈ D) (hD_contains_1 : 1 ∈ D) 
    (hD_contains_neg_1 : -1 ∈ D) (hD_contains_i : I ∈ D) 
    (hD_contains_neg_i : -I ∈ D) :
    ∀ (f : ℂ →

theorem exercise_4.4a
 integral_of_polynomial_over_reals {α : Type*} [comm_ring α] 
    [decidable_eq α] (Q : polynomial α) (hQ : Q.degree ≥ 2) 
    (hQ_roots : ∀ (x : α), Q.eval x = 0 → x ≠ 0) (hQ_real : ∀ (x : α), Q.eval x = 0 → x.re ≠ 0) :
    ∫ (x : ℝ) (hx : x ≠ 0), (exp (-2 * π * x * I * ξ) / Q.eval x) =

theorem exercise_5.1
 sum_of_inverse_dist_to_boundary_of_zeros_is_finite {f : ℂ → ℂ} 
    (hf : holomorphic_on ℂ f (set.ball 0 1)) (hb : metric.bounded (set.range f)) 
    (h0 : f ≠ 0) (hz : ∀ z, z ∈ set.ball 0 1 → f z = 0 → z ≠ 0) :
    ∑ z in set.ball 0 1, 1 - abs z < ⊤ 

theorem exercise_5.3
 entire_of_order_1_div_alpha (α : ℝ) (hα : 0 < α) :
  entire_function ℂ (λ z, ∑ n : ℕ, z ^ n / (fact n) ^ α) 