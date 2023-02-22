import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_26 {M : Type*} [topological_space M]
  {U : set M} (hU : U.nonempty) :
  U.is_open ↔ ∀ x ∈ U, ∃ y ∈ M, y ∉ U ∧ x = y :=
sorry

theorem exercise_2_32a {X Y : Type*} [metric_space X] [metric_space Y]
  (hY : compact_space Y) (f : X → Y) (g : Y → ℕ) (hgc : continuous g)
  (hgi : function.injective g) :
  is_clopen (g ∘ f) :=
sorry

theorem exercise_2_46 {M : Type*} [metric_space M]
  {A B : set M} (hA : A.nonempty) (hB : B.nonempty) (hAB : A.disjoint B) :
  ∃ (a : M) (b : M), a ∈ A ∧ b ∈ B ∧ d(a, b) = d(a_0, b_0) :=
sorry

theorem exercise_2_92 {X : Type*} [metric_space X]
  (K : set X) (K_cover : K.cover) (K_nonempty : K.nonempty) :
  ∃ (U : set X) (U_nonempty : U.nonempty), K ⊆ U ∧ K_cover ⊆ U_nonempty :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ} {a b : ℝ}
  (h : ∀ x ∈ (a, b), continuous_on (f ∘ x) (a, b)) :
  constant_on (a, b) f ↔ continuous_on (a, b) f :=
sorry

theorem exercise_3_63asum_p_gt_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_div_p_pow_one_:=
sorry

theorem exercise_4_15a  {X Y : Type*} [metric_space X] [metric_space Y] [compact_space Y]
  (hY : uniform_continuous (λ x, x)) (f : X → Y) :
  uniform_continuous f :=
sorry