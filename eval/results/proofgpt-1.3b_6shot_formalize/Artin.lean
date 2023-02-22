import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 {G : Type*} [group G] [group_with_zero G]
  (a b : G) : conjugate a b ↔ a = b :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H]
  [is_abelian G] [is_abelian H] (f : G →* H) (hf : function.injective f) :
  (center G).product (center H) = center (G × H) :=
sorry

theorem exercise_3_2_7 {K L : Type*} [field K] [field L]
  (f : K →+* L) (hf : function.injective f) : function.injective f :=
sorry

theorem exercise_3_7_2 {V : Type*} [field V]
  [add_comm_group V] [module V V] [finite_dimensional V V] :
  ¬(∃ (W : submodule V), W.is_proper_subspace) :=
sorry

theorem exercise_6_4_2 {p q : ℕ} [hp : fact (p.prime)] [hq : fact (q.prime)] :
  simple_group (p, q) :=
sorry

theorem exercise_6_4_12 :
  simple_group (224 : ℕ) :=
sorry

theorem exercise_10_1_13 {R : Type*} [ring R] (x : R) :
  nilpotent x → x.unit :=
sorry

theorem exercise_10_4_7a {R : Type*} [ring R]
  (I : ideal R) (J : ideal R) (hI : I.is_ideal) (hJ : J.is_ideal) :
  (I ∩ J).is_ideal :=
sorry

theorem exercise_10_6_7ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal_of_nonzero_ideal:=
sorry

theorem exercise_11_2_13gcd_div_eq_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd_div_one_div_gcd:=
sorry

theorem exercise_11_4_6a {K : Type*} [field K]
  [add_comm_group K] [module K K] [finite_dimensional K K]
  (h : irreducible (x^2+x+1)) : irreducible x :=
sorry

theorem exercise_11_4_6c [F : field F] [char_F F = 3] :
  irreducible (x^3 - 9) :=
sorry

theorem exercise_11_13_3 {p : ℕ} [hp : fact (nat.prime p)] :
  ∃ (n : ℕ), p ≡ -1 [MOD 4] ∧ n ≡ 0 [MOD p] :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] [char_zero K]
  (x : K) (hx : x ≠ 0) :
  x * x = -1 :=
sorry