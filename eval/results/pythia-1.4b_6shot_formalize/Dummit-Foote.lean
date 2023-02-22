import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_2a {Z : Type*} [group Z] [group Z]
  [fintype Z] {a : Z → Z} (h : Z.is_commutative a) :
  non_commutative_operation_star Z a :=
sorry

theorem exercise_1_1_4 {n : ℕ} [group Z] [group Z]
  (h : Z.is_residue_class_mod n) :
  Z.is_residue_class_mod n :=
sorry

theorem exercise_1_1_15inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_inverse_of_:=
sorry

theorem exercise_1_1_17 {G : Type*} [group G] 
  [group G] (x : G) (hx : ∀ n : ℕ, x ^ n ≠ 1) :
  ∀ m n : ℤ, m ≠ n → x ^ m ≠ x ^ n :=
sorry

theorem exercise_1_1_20 {G : Type*} [group G] 
  [add_comm_group G] [module G] [finite_dimensional G] 
  (h : order_of_elements G) :
  order_of_elements G :=
sorry

theorem exercise_1_1_22b  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_1_1_29 {A B : Type*} [group A] [group B]
  [is_abelian A] [is_abelian B] (f : A → B) (g : B → A) :
  abelian_group A B :=
sorry

theorem exercise_1_3_8infinite_group_of_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_cardinal_card:=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B]
  [group A] [group B] (h : group A × group B) :
  group A × group B :=
sorry

theorem exercise_1_6_23abelian_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_of_automorphism_of_finite_group_:=
sorry

theorem exercise_2_1_13 {G : Type*} [group G] [group G]
  [is_cyclic G] (f : G →* G) (hf : f.ker ≤ G) :
  zero (G.normalizer ⧸ G.comap G.normalizer.subtype G) :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G] 
  [finite_group G] [finite_group G]
  (h : finite_group G) :
  maximal_subgroup G :=
sorry

theorem exercise_2_4_16c {G : Type*} [group G] [group G]
  [is_cyclic G] (f : G →* G) (hf : f.ker ≤ center G) :
  maximal_subgroup G :=
sorry

theorem exercise_3_1_22a {G H K : Type*} [group G] [group H] [group K]
  [is_normalizer H] [is_normalizer K] (f : G →* H) (hf : f.ker ≤ center G) :
  intersection (H.normalizer ⧸ subgroup.comap H.normalizer.subtype H) (K.normalizer ⧸ subgroup.comap K.normalizer.subtype K) :=
sorry

theorem exercise_3_2_8 {G H K : Type*} 
  [group G] [group H] [group K] [finite_group G] [finite_group H] [finite_group K]
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group H) (hKH : finite_group K H)
  (h : finite_group G) (hK : finite_group K) (hH : finite_group:=
sorry

theorem exercise_3_2_16 {G : Type*} [group G] 
  [group G] (p : ℕ) (a : G) (b : G) (c : G) (d : G) (e : G) (f : G) (g : G) (h : G) (i : G) (j : G) (k : G) (l : G) (m : G) (n : G) (o : G) (p : ℕ) (q : ℕ) (r : ℕ) (s : ℕ) (t : ℕ) (u : ℕ) (v : ℕ) (w : ℕ) (x : ℕ) (y : ℕ) (z : ℕ) (a_p : G.p) (a_q : G.q) (a_r : G.r) (a_s : G.s) (a_t : G.t) (a_u : G.u) (a_v : G.v) (a_w : G.w) (a_x : G.x) (a_y : G.y) (a_z : G.z) (a_p_q : G.p.q) (a_r_s : G.r.s) (a_t_u : G.t.u) (a_u_v : G.u.v) (a_v_w : G.v.w) (a_w_x : G.w.x) (a_y_z : G.y.z) (a_z_p : G.z.p) (a_p_q_:=
sorry

theorem exercise_3_3_3 {G H : Type*} [group G] [group H]
  [is_normal H] (f : G →* H) (hf : f.ker ≤ H) :
  normal_subgroup G :=
sorry

theorem exercise_3_4_4 {G : Type*} [group G] 
  [fintype G] {n : ℕ} [fintype G] {p : ℕ} [fintype G] {H : subgroup G} 
  (h : finite_abelian_group G) :
  ∃ (f : G →* H), n.card (H.normalizer ⧸ subgroup.comap H.normalizer.subtype H) = n :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] 
  [solvable G] (h : solvable G) :
  solvable (G ∘ h) :=
sorry

theorem exercise_4_2_8 {G H : Type*} [group G] [group H]
  [finite_index H] (f : G →* H) (hf : f.ker ≤ H) :
  finite_index H :=
sorry

theorem exercise_4_2_9a {G : Type*} [group G] [group G]
  [is_normal G] (f : G →* G) (hf : f.ker ≤ G) :
  normal_index G :=
sorry

theorem exercise_4_4_2 {G : Type*} [group G] [group G]
  [is_cyclic G] (f : G →* G) (hf : f.ker ≤ G) :
  cyclic G :=
sorry

theorem exercise_4_4_6b {G : Type*} [group G] 
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : card H = p ^ n) :
  not (H.normalizer ⧸ subgroup.comap H.normalizer.subtype H) :=
sorry

theorem exercise_4_4_8a {G H K : Type*} [group G] [group H]
  [group K] (h : characteristic H ∩ characteristic K) :
  ∃ (H' : subgroup H ∩ characteristic K), H' ∩ characteristic K ≠ 0 :=
sorry

theorem exercise_4_5_13 {G : Type*} [group G] [group G]
  [is_cyclic G] (p : ℕ) (n : ℕ) (p_n : p ^ n = p) :
  Sylow_subgroup G :=
sorry

theorem exercise_4_5_15 {G : Type*} [group G] 
  [group G] (p : ℤ) (n : ℤ) (h : G.order p n) :
  normal_Sylow_p_subgroup G :=
sorry

theorem exercise_4_5_17 {G : Type*} [group G] [group G]
  [normal_Sylow_subgroup G] (f : G →* G) (hf : f.ker ≤ normal_Sylow_subgroup G) :
  normal_Sylow_subgroup G :=
sorry

theorem exercise_4_5_19 {G : Type*} [group G] [group G]
  [is_simple G] (h : G.is_simple) : G.is_simple :=
sorry

theorem exercise_4_5_21 {G : Type*} [group G] [group G]
  [is_simple G] (h : G.order ≤ 2907) :
  simple G :=
sorry

theorem exercise_4_5_23 {G : Type*} [group G] [group G]
  [is_simple G] (h : G.order ≤ 462) :
  simple G :=
sorry

theorem exercise_4_5_33 {G H : Type*} [group G] [group H]
  [normal_Sylow_p_subgroup H] (p : ℕ) (p' : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ) (p : ℕ) (p' : ℕ):=
sorry

theorem exercise_7_1_2 {R : Type*} [ring R] [add_comm_group R] [module R R]
  (h : unit R) : unit R :=
sorry

theorem exercise_7_1_12 {R : Type*} [field R] [subring R]
  (h : R.is_integral) : integral_domain R :=
sorry

theorem exercise_7_2_2zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial_ring_zero_divisor_of_polynomial:=
sorry

theorem exercise_7_3_16 {R S : Type*} [ring R] [ring S]
  [surjective_homomorphism R S] (h : R → S) (hS : S → R) (hR : R → S) (hR : R → R) (hS : S → S)
  (h : surjective_homomorphism (hR ∘ hS)) :
  center_image R S :=
sorry

theorem exercise_7_4_27 {R : Type*} [ring R] [nilpotent_element R]
  (a : R.nilpotent) (b : R.nilpotent) (c : R.nilpotent) :
  R.nilpotent.element a ≡ R.nilpotent.element b ≡ R.nilpotent.element c :=
sorry

theorem exercise_8_2_4 {R : Type*} [integral_domain R] 
  [finite_field R] (a : R) (r : a ≠ 0) :
  principal_ideal_domain R :=
sorry

theorem exercise_8_3_5a {R : Type*} [ring R] [group R]
  [finite_group R] [finite_group R] [finite_group R]
  (h : finite_group R) (h : irreducibles_of_squarefree_integer_in_R (R.irreducible_element)) :
  irreducible_element (R.irreducible_element) :=
sorry

theorem exercise_8_3_6bfield_quotient_of_ring_quotient_of_field_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring_quotient_of_ring:=
sorry

theorem exercise_9_1_10 {R : Type*} [ring R] 
  [finite_dimensional R] [finite_dimensional R]
  (h : finite_dimensional.finrank R + 1 < ∞) :
  ∃ (I : finite_dimensional.finrank R + 1 < ∞), I.minimal_prime_ideal R :=
sorry

theorem exercise_9_4_2airreducible_in_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_:=
sorry

theorem exercise_9_4_2cirreducible_in_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_Z_:=
sorry

theorem exercise_9_4_9irreducible_polynomial_over_Z_square_of_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_of_Z_square_:=
sorry

theorem exercise_11_1_13is_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_to_isomorphic_:=
sorry