import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 conjugate_of_product {G : Type*} [group G] (a b : G) :
  is_conjugate G (a * b) (b * a) :=
sorry

theorem exercise_2_8_6 center_prod_of_groups {G H : Type*} [group G] [group H] :
  center (G × H) = (center G).prod (center H) :=
sorry

theorem exercise_3_2_7 field_hom_injective {F K : Type*} [field F] [field K]
  (f : F →+* K) : function.injective f :=
sorry

theorem exercise_3_7_2 not_union_finite_proper_subspaces (V : Type*) (F : Type*)
  [field F] [infinite F] [add_comm_group V] [module F V] :
  ¬(∃ (s : finset (submodule F V)), V = ⋃₀ (s.subset_to_set)) :=
sorry

theorem exercise_6_4_2 not_simple_of_order_pq {G : Type*} [group G] [fintype G]
  {p q : ℕ} (hp : fact (nat.prime p)) (hq : fact (nat.prime q)) (hG : fintype.card G = p * q) :
  ¬ is_simple_group G :=
sorry

theorem exercise_6_4_12 not_simple_group_of_order_224 (G : Type*) [group G] [fintype G]
  (hG : fintype.card G = 224) : ¬is_simple_group G :=
sorry

theorem exercise_10_1_13 unit_of_nilpotent {R : Type*} [ring R] (x : R) (hx : is_nilpotent x) :
  is_unit (1 + x) :=
sorry

theorem exercise_10_4_7a ideal_mul_eq_inf {R : Type*} [comm_ring R] (I J : ideal R)
  (h : I + J = ⊤) : I * J = I ⊓ J :=
sorry

theorem exercise_10_6_7 nonzero_ideal_contains_nonzero_integer (I : ideal ℤ[i]) (hI : I ≠ ⊥) :
  ∃ (n : ℤ), n ≠ 0 ∧ (n : ℤ[i]) ∈ I :=
sorry

theorem exercise_11_2_13 divides_in_gauss_int_implies_divides_in_int {a b : ℤ}
  (h : ∃ (z : ℤ[i]), ↑b = ↑a * z) : a ∣ b :=
sorry

theorem exercise_11_4_6a irreducible_x_squared_plus_x_plus_one {F : Type*} [field F] (hF : char_p F 2) :
  irreducible (X ^ 2 + X + 1 : polynomial F) :=
sorry

theorem exercise_11_4_6c x_cubed_minus_nine_irreducible_in_F31 :
  irreducible (polynomial.X ^ 3 - 9 : polynomial (zmod 31)) :=
sorry

theorem exercise_11_13_3 infinitely_many_primes_mod_neg_one_four : ∃ (s : set ℕ), set.infinite s ∧
  (∀ (p : ℕ) (hp : p ∈ s), nat.prime p ∧ p % 4 = 3) :=
sorry

theorem exercise_13_6_10 prod_nonzero_elements_eq_neg_one {K : Type*} [field K] [fintype K] :
  finset.prod (finset.erase (finset.univ : finset K) 0) id = -1 :=
sorry