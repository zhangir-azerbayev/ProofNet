import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b  {X Y : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (h : function.injective g) (h : constant_image (g ∘ f)) : constant_image f :=
sorry

theorem exercise_1_19a  {z : ℂ} (hz : |z| < 1) : uniform_continuous (λ z, z ^ n) :=
sorry

theorem exercise_1_19c {K : Type*} [field K]
  [add_comm_group V] [module K V] [finite_dimensional K V]
  {v : K → V} (hv₁ : continuous_on (unit_circle K) v)
  (hv₂ : ∀ z : unit_circle K, v z ≠ 0) :
  uniform_continuous (λ z, z ^ (v z)) :=
sorry

theorem exercise_2_2 (x : ℝ) :
  ∫ x, sin x ∂ x = ∫ x, cos x ∂ x :=
sorry

theorem exercise_2_13 {f : ℂ → ℂ}
  (hf : analytic f) (z0 : ℂ) (n : ℕ) :
  f z0 = 0 ∧ ∀ z ∈ ℂ, f z = 0 → z = z0 ∨ z = 0 ∨ f z = 0 :=
sorry

theorem exercise_3_4 {a : ℝ} (h : 0 < a) :
  ∫ x in ℝ, sin x / x^2 = π e^{-a} :=
sorry

theorem exercise_3_14  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : entire_of_injective_entire_of_entire_comp_entire_comp hY f g hgc hgi) :
  entire_of_injective_entire_of_entire_comp_entire_comp hY f g hgc :=
sorry

theorem exercise_5_1  {n : ℕ} (hn : n ≠ 0) (hn_inf : ∀ k : ℕ, k ≠ n → k ≠ 0) :
  ∑ k in (n.divisors), 1 / k = 0 :=
sorry