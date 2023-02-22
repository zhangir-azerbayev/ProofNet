import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : holomorphic (g ∘ f)) : holomorphic f :=
sorry

theorem exercise_1_19a {X : Type*} [field X] 
  [add_comm_group X] [module X X] [finite_dimensional X] 
  {z : ℝ} (h : ∀ n, z ^ n ≠ 0) :
  ∃ (f : X → ℝ), ∃ (x : X), ∃ (y : X), ∃ (z : ℝ), ∃ (n : ℕ),
  ∃ (m : ℤ), ∃ (k : ℤ), ∃ (l : ℤ), ∃ (r : ℤ),
  ∃ (s : ℤ), ∃ (t : ℤ), ∃ (u : ℤ), ∃ (v : ℤ),
  ∃ (w : ℤ), ∃ (x : ℝ), ∃ (y : ℝ), ∃ (z : ℝ),
  ∃ (n : ℕ), ∃ (m : ℤ), ∃ (k : ℤ), ∃ (l : ℤ), ∃ (r : ℤ),
  ∃ (s : ℤ), ∃ (t : ℤ), ∃ (u : ℤ), ∃ (v : ℤ),
  ∃ (w : ℤ), ∃ (x : ℝ), ∃ (y : ℝ), ∃ (z : ℝ),
  ∃ (n : ℕ), ∃ (m : ℤ), ∃ (k : ℤ), ∃ (l : ℤ), ∃ (r : ℤ),:=
sorry

theorem exercise_1_19c  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_2_2 {x : ℤ} [interval_of_0_to_infinity]
  [interval_of_0_to_infinity] (h : ∀ x, x < 0 → ∃ y, x < y → x ∈ interval_of_0_to_infinity) :
  ∀ x, x < 0 → ∃ y, x < y → x ∈ interval_of_0_to_infinity :=
sorry

theorem exercise_2_13 {C : Type*} [field C] 
  [add_comm_group C] [module C C] [finite_dimensional C] 
  (f : C → C) (f0 : C → C) (f1 : C → C) (f2 : C → C) (f3 : C → C) (f4 : C → C) (f5 : C → C) (f6 : C → C) (f7 : C → C) (f8 : C → C) (f9 : C → C) (f10 : C → C) (f11 : C → C) (f12 : C → C) (f13 : C → C) (f14 : C → C) (f15 : C → C) (f16 : C → C) (f17 : C → C) (f18 : C → C) (f19 : C → C) (f20 : C → C) (f21 : C → C) (f22 : C → C) (f23 : C → C) (f24 : C → C) (f25 : C → C) (f26 : C → C) (f27 : C → C) (f28 : C → C) (f29 : C → C) (f30 : C → C) (f31 : C → C) (f32 : C → C) (f33 : C → C) (f34 : C → C) (f35 : C → C) (f36 : C → C) (f37 : C → C) (f38 : C → C) (f39 : C → C) (f40 : C → C) (f41 : C → C) (f42 : C → C) (f43:=
sorry

theorem exercise_3_4 {a : ℝ} [interval_interval]
  [interval_interval] (x : a) :
  ∫ x sin x dx = ∫ x cos x dx :=
sorry

theorem exercise_3_14  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : injective (g ∘ f)) : injective f :=
sorry

theorem exercise_5_1holomorphic_in_disc_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_zeros_of_sum_:=
sorry