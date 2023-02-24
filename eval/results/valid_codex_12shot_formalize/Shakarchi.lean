import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13a {f : ℂ → ℂ} (hf : holomorphic f)
  (h : ∀ z, f z = f z.re) :
  ∀ z, f z = f 0 :=
sorry

theorem exercise_1_13c {f : ℂ → ℂ} {s : set ℂ} (hf : holomorphic f s)
  (h : ∀ z ∈ s, abs (f z) = abs (f 0)) :
  ∀ z ∈ s, f z = f 0 :=
sorry

theorem exercise_1_19b (z : ℂ) :
  is_cau_seq (λ n, z ^ n / n ^ 2) :=
sorry

theorem exercise_1_26 {α : Type*} [has_deriv α] [topological_space α]
  [has_integral α] [has_continuous_deriv α] {f : α → ℝ} {Ω : set α}
  (hf : continuous_on Ω f) (hf' : ∀ x ∈ Ω, has_deriv_at f (f' x))
  (hf'_cont : continuous_on Ω f') (hf_prim : ∀ x ∈ Ω, ∃ g : α → ℝ,
  (∀ y ∈ Ω, has_deriv_at g (f' y)) ∧ (∀ y ∈ Ω, g y = g x + ∫ x y f')) :
  ∀ x y ∈ Ω, ∃ c : ℝ, ∀ z ∈ Ω, ∃ g : α → ℝ, (∀ y ∈ Ω, has_deriv_at g (f' y)) ∧
  (∀ y ∈ Ω, g y = g z + ∫ z y f') ∧ (g x = c) :=
sorry

theorem exercise_2_9 
  {Ω : Type*} [nondiscrete_normed_field Ω] [normed_group Ω] 
  [normed_space ℂ Ω] [complete_space Ω] [open_mapping Ω] 
  [open_mapping ℂ Ω] [open_mapping Ω ℂ] [complete_space ℂ] 
  [normed_group ℂ] [normed_space ℝ ℂ] [normed_field ℂ] 
  [complete_space ℝ] [normed_group ℝ] [normed_space ℝ ℝ] 
  [normed_field ℝ] [complete_space ℝ] [normed_group ℝ] 
  [normed_space ℝ ℝ] [normed_field ℝ] [complete_space ℝ] 
  [normed_group ℝ] [normed_space ℝ ℝ] [normed_field ℝ] 
  [complete_space ℝ] [normed_group ℝ] [normed_space ℝ ℝ] 
  [normed_field ℝ] [complete_space ℝ] [normed_group ℝ] 
  [normed_space ℝ ℝ] [normed_field ℝ] [complete_space ℝ] 
  [normed_group ℝ] [normed_space ℝ ℝ] [normed_field ℝ]:=
sorry

theorem exercise_3_3 
  (a : ℝ) (ha : 0 < a) :
  ∫⁻ a⁺ (λ x, cos x / (x ^ 2 + a ^ 2)) = π * e⁻ a / a :=
sorry

theorem exercise_3_9 :
  ∫⁻ a b, log (sin (π * x)) = -log 2 :=
sorry

theorem exercise_3_22  (f : ℂ → ℂ) (hf : holomorphic f) (hf_ext : continuous_on f (set.univ ∖ {0}))
  (hf_bd : ∀ z : ℂ, z ≠ 0 → f z = 1 / z) : false :=
sorry