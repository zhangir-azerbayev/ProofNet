import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 {G : Type*} [group G] 
  [fintype G] {a : ℕ} [a_prime : a ≠ e] {a_prime_prime : a ≠ a_prime} 
  (a_prime_prime_prime : a_prime ≠ a_prime_prime) :
  element_of_a_group G :=
sorry

theorem exercise_2_1_26finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_of_finite_group_:=
sorry

theorem exercise_2_2_3abelian_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_of_group_:=
sorry

theorem exercise_2_2_6c {G H : Type*} [group G] [group H]
  [group H] (f : G →* H) (hf : f.ker ≤ center G) :
  group (H.normalizer ⧸ subgroup.comap H.normalizer.subtype H) :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G] [group G]
  [is_cyclic G] (p : ℤ) (h : G.order p) :
  cyclic G :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G] [group G]
  [is_normal G] (f : G →* G) (hf : f.ker ≤ normal G) :
  normal_subgroup G :=
sorry

theorem exercise_2_5_31 {G : Type*} [group G] [group H]
  [is_abelian H] (f : G → H) (hf : f.ker ≤ H) :
  characteristic_subgroup G :=
sorry

theorem exercise_2_5_43 {G : Type*} [group G] [group G]
  [is_abelian G] (h : G.order 9) :
  ∃ (x : G), h.order 9 = 1 :=
sorry

theorem exercise_2_5_52  {G : Type*} [group G] [finite_group G] [finite_group G]
  (h : finite_group G) (h : automorphism G) (h : automorphism G) (h : automorphism G)
  (h : automorphism G) (h : automorphism G) (h : automorphism G) :
  abelian_group G :=
sorry

theorem exercise_2_7_7homomorphism_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normalizer_of_normal:=
sorry

theorem exercise_2_8_15 {G H : Type*} [group G] [group H]
  (f : G →* H) (hf : f.ker ≤ center G) :
  is_isomorphic G H :=
sorry

theorem exercise_2_10_1 {G : Type*} [group G] 
  [group G] (b : G) (p : ℤ) (h : G.normal_subgroup G.prime_order p) :
  normal_subgroup G.prime_order p :=
sorry

theorem exercise_2_11_7 {G : Type*} [group G] [group G]
  [is_p_subgroup G] (p : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (p' : ℕ) (:=
sorry

theorem exercise_3_2_21 {G : Type*} [group G] [group G]
  [is_cyclic G] (f : G →* G) (hf : f.ker ≤ center G) :
  permutation G :=
sorry

theorem exercise_4_1_34 {T : Type*} [group T] [group S_3]
  (h : T.is_isomorphic_to_S_3 T) : T.is_isomorphic_to_S_3 T :=
sorry

theorem exercise_4_2_6comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_:=
sorry

theorem exercise_4_3_1 {R : Type*} [ring R] [ideal R]
  (a : R) (L : ideal R) (L : L(a)) : ideal R :=
sorry

theorem exercise_4_4_9 {p : ℕ} [p] (q : p - 1) :
  quadratic_residue p :=
sorry

theorem exercise_4_5_23 {F : Type*} [field F] 
  [field F] [is_isomorphic F] [is_isomorphic F] [is_isomorphic F] [is_isomorphic F]
  (p : F → F) (q : F → F) (f : F → F) (g : F → F) (h : F → F) (gq : g.is_isomorphic F) (hq : h.is_isomorphic F)
  (h : F → F) (gq : g.is_isomorphic F) (hq : h.is_isomorphic F) :
  irreducible_in_field_quotient_quotient_field_quotient F :=
sorry

theorem exercise_4_6_2 {f : Q[x]} (h : f.is_irreducible) : f.is_irreducible :=
sorry

theorem exercise_5_1_8field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not_zero_of_field_of_characteristic_p_not:=
sorry

theorem exercise_5_3_7 {K F : Type*} 
  [field K] [subfield F] [subfield F] [subfield F] [subfield F] [subfield F]
  (a : K.subfield F) (a_F : F.subfield F) (a_F_F : F.subfield F) (a_F_F_F : F.subfield F_F_F)
  (a_F_F_F_F : F.subfield F_F_F_F) (a_F_F_F_F_F : F.subfield F_F_F_F_F) :
  a^2 ≡ a_F^2_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_F_:=
sorry

theorem exercise_5_4_3algebraic_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_of_degree_:=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] [group F]
  [finite_field F] [finite_field F] [finite_field F]
  (h : finite_field F) (hF : finite_field F) (hF : finite_field F) (h : finite_field F)
  (hF : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (h : finite_field F) (h : finite_field F) (h : finite_field F)
  (h : finite_field F) (:=
sorry