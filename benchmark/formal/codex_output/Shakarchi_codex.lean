theorem exercise_1_13a {f : ℂ → ℂ} (hf : holomorphic f) 
    (hc : is_const (λ z, (f z).re)) : is_const f :=
sorry

theorem exercise_1_13b {f : ℂ → ℂ} 
    (hf : holomorphic f) (hc : ∀ z, f z ∈ set.Icc 0 0) :
    ∃ (c : ℂ), f = function.const ℂ c :=
sorry

theorem exercise_1_13c {f : ℂ → ℂ} (hf : holomorphic f) 
    (hf_abs : is_constant (λ z, abs (f z))) : is_constant f :=
sorry

theorem exercise_1_18 
    {R : Type*} [comm_ring R] {f : power_series R} (hf : f.has_expansion_at 0) 
    (z : R) (hz : f.has_expansion_at z) :
    ∃ (g : power_series R), g.has_expansion_at 0 ∧ g.has_expansion_at z :=
sorry

theorem exercise_1_19a (z : ℂ) (hz : abs z = 1) :
    ¬ (∃ (r : ℝ), abs (∑ n, n * z ^ n) < r) :=
sorry

theorem exercise_1_19b (z : ℂ) : 
    abs z = 1 → power_series.converges_at (λ n, z / n ^ 2) z :=
sorry

theorem exercise_1_19c (z : ℂ) (hz : abs z = 1) 
    (h1 : z ≠ 1) :
    power_series.converges (λ n, z ^ n / n) :=
sorry

theorem exercise_1_22 
    (S : finset ℕ) (hS : ∀ s ∈ S, ∃ a d : ℕ, s = {a, a + d, a + 2 * d, a + 3 * d, a + 4 * d, a + 5 * d, a + 6 * d, a + 7 * d, a + 8 * d, a + 9 * d}) :
    false :=
sorry

theorem exercise_1_26 {E : Type u} [normed_group E]
    [normed_space ℂ E] {F : Type v} [normed_group F] [normed_space ℂ F]
    {f : E → F} (hf : differentiable ℂ f) (hb : metric.bounded (set.range f)) :
    ∃ (c : F), f = function.const E c :=
sorry

theorem exercise_2_2 :
  ∫ (0 : ℝ) ∞ (λ x, sin x / x) = π / 2 :=
sorry

theorem exercise_2_5 {f : ℂ → ℂ} {Ω : set ℂ}
    (hf : ∀ z ∈ Ω, f.has_fderiv_at ℂ z) (T : set ℂ) (hT : is_triangle T)
    (hT_subset_Ω : T ⊆ Ω) (hT_interior_subset_Ω : interior T ⊆ Ω) :
    complex.integral T f = 0 :=
sorry

theorem exercise_2_6 {Ω : Type*} [complex_normed_vector_space Ω]
    [normed_group Ω] [normed_space ℂ Ω] [complete_space Ω]
    (hΩ : is_open Ω) (T : set Ω) (hT : is_open (interior T))
    (hT' : interior T ⊆ Ω) (f : Ω → ℂ) (hf : holomorphic_on Ω f)
    (w : Ω) (hw : w ∈ interior T) (hb : metric :=
sorry

theorem exercise_2_9 
    {Ω : Type*} [nondiscrete_normed_field Ω] [normed_group Ω] 
    [normed_space ℂ Ω] [complete_space Ω] [open_mapping_theorem Ω] 
    [holomorphic_on_open_subset Ω] (Ω_bounded : bounded_subset Ω) 
    (Ω_open : is_open Ω) (φ : Ω → Ω) (hφ : holomorphic_on Ω :=
sorry

theorem exercise_2_13 {f : ℂ → ℂ} 
    (hf : ∀ z₀ : ℂ, ∃ n : ℕ, f.is_analytic_at z₀ ∧ 
    (∀ m : ℕ, m < n → f.taylor_nth_coeff z₀ m = 0)) :
    ∃ (p : polynomial ℂ), f = p :=
sorry

theorem exercise_3_2 :
    ∫⁻∞ ∞ (λ x, 1 / (1 + x ^ 4)) = π / 2 :=
sorry

theorem exercise_3_3 (a : ℝ) (ha : 0 < a) :
    ∫⁻ a⁺ (λ x, cos x / (x ^ 2 + a ^ 2)) = π * (exp (-a) / a) :=
sorry

theorem exercise_3_4 (a : ℝ) (ha : 0 < a) :
    ∫⁻∞ ∞ (λ x, x * sin x / (x ^ 2 + a ^ 2)) = π * exp (-a) :=
sorry

theorem exercise_3_9 : ∫ (0 : ℝ) 1 (λ x, log (sin (π * x))) = -log 2 :=
sorry

theorem exercise_3_14 {f : ℂ → ℂ} (hf : entire f) 
    (hf_inj : function.injective f) :
    ∃ (a b : ℂ), f = λ z, a * z + b ∧ a ≠ 0 :=
sorry

theorem exercise_3_22 (D : set ℂ) 
    (hD : is_open D) (hD_unit_disc : D = set.ball 0 1) 
    (hD_contains_0 : 0 ∈ D) (hD_contains_1 : 1 ∈ D) 
    (hD_contains_neg_1 : -1 ∈ D) (hD_contains_i : I ∈ D) 
    (hD_contains_neg_i : -I ∈ D) :
    ∀ (f : ℂ → :=
sorry

theorem exercise_4_4a {α : Type*} [comm_ring α] 
    [decidable_eq α] (Q : polynomial α) (hQ : Q.degree ≥ 2) 
    (hQ_roots : ∀ (x : α), Q.eval x = 0 → x ≠ 0) (hQ_real : ∀ (x : α), Q.eval x = 0 → x.re ≠ 0) :
    ∫ (x : ℝ) (hx : x ≠ 0), (exp (-2 * π * x * I * ξ) / Q.eval x) = :=
sorry

theorem exercise_5_1 {f : ℂ → ℂ} 
    (hf : holomorphic_on ℂ f (set.ball 0 1)) (hb : metric.bounded (set.range f)) 
    (h0 : f ≠ 0) (hz : ∀ z, z ∈ set.ball 0 1 → f z = 0 → z ≠ 0) :
    ∑ z in set.ball 0 1, 1 - abs z < ⊤ :=
sorry

theorem exercise_5_3 (α : ℝ) (hα : 0 < α) :
  entire_function ℂ (λ z, ∑ n : ℕ, z ^ n / (n! : ℝ) ^ α) :=
sorry