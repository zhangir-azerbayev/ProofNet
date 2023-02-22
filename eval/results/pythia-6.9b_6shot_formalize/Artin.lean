import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 {G : Type*} [group G] (a : G) (b : G) :
  a.conjugate b = b.conjugate a :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H]
  [is_cyclic H] (f : G →* H) (hf : f.ker ≤ center G) :
  product_of_center G H = product_of_center G H :=
sorry

theorem exercise_3_2_7 {F G : Type*} [field F] [field G]
  [is_field F] [is_field G] (f : F →* G) (hf : f.ker ≤ field.ker) :
  f.is_injective :=
sorry

theorem exercise_3_7_2 {V : Type*} [field F] [vector_space V]
  [finite_dimensional V] (h : finite_dimensional.finrank V + 1 < ∞) :
  ∃ (f : V → F), ∃ (x : V), f x ≠ 0 ∧ ∃ (y : V), f y ≠ 0 :=
sorry

theorem exercise_6_4_2 {p q : ℕ} [group p q] :
  ∃ (g : group p q), ∃ (h : group.is_simple g) :=
sorry

theorem exercise_6_4_12 : no_simple_group 224 :=
sorry

theorem exercise_10_1_13  {R : Type*} [ring R] (x : R) (h : x nilpotent) :
  1 + x ≡ 1 : R :=
sorry

theorem exercise_10_4_7a {R : Type*} [ring R] [ring R]
  [add_comm_ring R] [module R R] [finite_dimensional R]
  (i : ideal R) (j : ideal R) (h : i.intersection j = i ∩ j) :
  i.intersection j = i ∩ j :=
sorry

theorem exercise_10_6_7 {R : Type*} [ring R]
  [field R] [finite_field R] [finite_field_ring R] [finite_field_field R]
  (h : nonzero_ideal R) : nonzero_ideal R → nonzero_integer R :=
sorry

theorem exercise_11_2_13 {a b : ℤ} (a : a divides b) (b : b divides a) : a divides b :=
sorry

theorem exercise_11_4_6a {F2 : Type*} [field F2]
  [add_comm_group F2] [module F2 F2] [finite_dimensional F2]
  (x : F2) (h : x^2 + x + 1 ≠ 0) :
  irreducible F2 x :=
sorry

theorem exercise_11_4_6c {F31 : Type*} [field F31]
  [finite_field F31] [finite_field_extension F31] [finite_field_degree F31]
  (x : F31) (h : x^3 - 9 ≠ 0) :
  irreducible F31 x :=
sorry

theorem exercise_11_13_3 {p : ℕ} :
  ∃ (n : ℕ), p ≡ -1 (mod 4) :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K]
  [finite_field K] [field_homomorphism K K] [field_homomorphism K K]
  (h : nonzero_product K) :
  ∀ x y : K, x * y = -1 * y * x :=
sorry