import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_2a {n : ℕ} :
  n ≠ 0 → ∀ a b : ℤ, a ≠ b → a * b ≠ 0 :=
sorry

theorem exercise_1_1_4 {n : ℕ} (n_prime : n ≠ 0) :
  (n : ℕ) * (n : ℕ) = n * n :=
sorry

theorem exercise_1_1_15inverse_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_comm_group_of_:=
sorry

theorem exercise_1_1_17 {G : Type*} 
  [group G] [group_with_zero G] [nontrivial_group G] (x : G) (hx_nontrivial : x ≠ 1) :
  ∃ (f : G →* ℤ), x.sum (λ (e : G), f e • e) = 0 ∧ x.sum (λ (e : G), f e) = 0 ∧
    ∃ (n : ℕ), x ^ n ≠ 1 :=
sorry

theorem exercise_1_1_20order_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_inv_of_:=
sorry

theorem exercise_1_1_22b {G : Type*} [group G] [group_with_zero G]
  (hG : commutative G) :
  comm_group G ↔ G.is_commutative :=
sorry

theorem exercise_1_1_29 :
  abelian G ↔ abelian (G × G) :=
sorry

theorem exercise_1_3_8 [group G] (x : G) :
  ∃ (n : ℕ), x ^ n = 1 :=
sorry

theorem exercise_1_6_11 :
  (A × B) × (A × B) ≃ (A × B) × (A × B) :=
sorry

theorem exercise_1_6_23  {G : Type*} [group G] [order G] [is_order_two G] :
  ∃ (f : G →* G) (h : ∀ g, f(g) = g) (h2 : ∀ g, f(g) = g⁻¹) (h2' : ∀ g, f(g) = g⁻¹) :
  abelian G :=
sorry

theorem exercise_2_1_13 {H : subgroup of ℚ} (hH : H.nonempty) :
  H = 0 ∨ H = ℚ :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G] [fintype G]
  (h : finite_group G) :
  ∃ (M : subgroup G), M.maximal :=
sorry

theorem exercise_2_4_16c {G : Type*} [group G] [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] :
  maximal G ↔ ∃ (p : ℕ), p ^ n = p ∧ p ∣ n :=
sorry

theorem exercise_3_1_22a {G H K : Type*} [group G] [group H] [group K]
  [is_normal H] [is_normal K] (hH : H.normal) (hK : K.normal) :
  H ∩ K.normal :=
sorry

theorem exercise_3_2_8  {G : Type*} [group G] [fintype G] {p : ℕ} [hp : fact (nat.prime p)] :
  (∀ (H : subgroup G) (K : subgroup G), H.card = p ^ (fintype.card K)) →
  (∀ (H : subgroup G) (K : subgroup G), K.card = p ^ (fintype.card H)) →
  (∀ (H : subgroup G) (K : subgroup G), H.card.card = p ^ (fintype.card K.card))

end

end group
:=
sorry

theorem exercise_3_2_16 {p : ℕ} {a : ℤ} (hp : prime p) : a ^ p ≡ a (\bmod p) :=
sorry

theorem exercise_3_3_3  {G H : Type*} [group G] [group H] [is_normal H] [is_prime G] [is_prime H]
  (hG : is_prime G) (hH : is_prime H) (hG_normal : is_normal G) (hH_normal : is_normal H) :
  G = H ⨁ (G : H) :=
sorry

theorem exercise_3_4_4 {n : ℕ} {G : Type*} [group G] [fintype G]
  (hG : finite_abelian_group G) (dvd_card G) :
  ∃ (H : subgroup G), dvd_card H ∧ H.order = n :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] [solvable G] :
  solvable (G ⧸ center G) :=
sorry

theorem exercise_4_2_8 {G : Type*} [group G] [fintype G] {H : subgroup G}
  (hH : H.normalizer = G) :
  card (G ⧸ H) ≡ card H [MOD |G|] :=
sorry

theorem exercise_4_2_9a {G : Type*} [group G] [fintype G]
  {p : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} (hH : card H = p ^ n) :
  card (H.normalizer ⧸ subgroup.comap H.normalizer.subtype H) ≡  card (G ⧸ H) [MOD p] :=
sorry

theorem exercise_4_4_2 {G : Type*} [group G] [order G]
  (hG : cyclic G) : cyclic G :=
sorry

theorem exercise_4_4_6b {G : Type*} [group G] [group_with_zero G]
  (H : subgroup G) (hH : H.normalizer.subtype H ≠ G) :
  ¬(H.char G) :=
sorry

theorem exercise_4_4_8a  {G H K : Type*} [group G] [group H] [group K] [group K] [group H] [group K]
  [is_group H] [is_group K] [is_group K] [is_group H] [is_group K]
  (hH : H.normal_subgroup_char_normal_subgroup_normal_comap_normal_comap K)
  (hK : K.normal_subgroup_char_normal_subgroup_normal_comap_normal_comap H) :
  normal_subgroup_char_normal_subgroup_normal_comap_normal_comap K :=
sorry

theorem exercise_4_5_13 {G : Type*} [group G] [order G]
  (hG : order G = 56) :
  ∃ (p : ℕ) (P : Sylow p G), p.prime ∧ P.normal :=
sorry

theorem exercise_4_5_15normal_Sylow_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_subgroup_card_p_sub:=
sorry

theorem exercise_4_5_17  {G : Type*} [group G] [order G] [is_prime_order G] :
  normal_sylow_5 G normal_sylow_7 G normal_sylow G :=
sorry

theorem exercise_4_5_19 {G : Type*} [group G] [fintype G] [fintype G]
  (hG : card G = 1) : ¬simple G :=
sorry

theorem exercise_4_5_21 {G : Type*} [group G] [fintype G] :
  ∀ (H : subgroup G), H.card = 2907 → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1} → H ≠ {1}:=
sorry

theorem exercise_4_5_23 [group G] [fintype G]
  (hG : fintype G) (hG_ne : G ≠ 1) : ¬simple G :=
sorry

theorem exercise_4_5_33 {G : Type*} [group G]
  [fintype G] {p : ℕ} [hp : fact (nat.prime p)] {H : subgroup G}
  (hH : is_Sylow_p_subgroup H) :
  is_Sylow_p_subgroup (H.normalizer ⧸ subgroup.comap H.normalizer.subtype H) :=
sorry

theorem exercise_7_1_2 [ring R] (u : R) :
  u.unit.of_infinite_order_element :=
sorry

theorem exercise_7_1_12 {K : Type*} [field K]
  [add_comm_group K] [module K K] [finite_dimensional K K]
  (h : field K) (hK : K ≠ 0) :
  integral_domain K :=
sorry

theorem exercise_7_2_2 {R : Type*} [ring R]
  {p : R[x]} (hp : p.is_zero_divisor) :
  is_zero_divisor (p : R[x]) :=
sorry

theorem exercise_7_3_16 {R S : Type*} [ring R] [ring S]
  (h : function.surjective (ring_hom.comp R S)) :
  (center R).map (ring_hom.comp R S) = center S :=
sorry

theorem exercise_7_4_27 {R : Type*} [ring R] [is_domain R]
  (a : R) (b : R) (h : nilpotent a) : nilpotent b :=
sorry

theorem exercise_8_2_4 {R : Type*} [ring R]
  [is_domain R] {a b c : R} (ha : a ≠ 0) (hb : b ≠ 0) (hc : c ≠ 0) :
  is_principal_ideal R ↔ ∃ (r : R) (s : R), r * a + s * b = c :=
sorry

theorem exercise_8_3_5a :
  irreducible (2, √n) ∧ irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n) ∧ irreducible (1 - √n) ∧
  irreducible (1 + √n:=
sorry

theorem exercise_8_3_6b  {q : ℕ} (hq : prime q) (n : ℕ) :
  field.quotient.card (q ^ n) = q ^ (n + 1) :=
sorry

theorem exercise_9_1_10  {K V : Type*} [field K] [add_comm_group V] [module K V] [finite_dimensional K V]
  {n : ℕ} (hV : finite_dimensional.finrank K V + 1 < n) :
  ∃ (p : ℕ) (h : p.prime), ∃ (x : V) (H : x ∈ p.prime), x ∈ p.ideal ∧
    (p.ideal : ideal K V) = ⊥ ∧ (p.prime : ideal K V) = ⊤ ∧
    (p.prime : ideal K V) = ⊤ ∧ (p.prime : ideal K V) = ⊥ ∧
    (p.prime : ideal K V) = ⊥ ∧ (p.prime : ideal K V) = ⊤ :=
sorry

theorem exercise_9_4_2a [ring R]
  (p : R[X]) (h : irreducible p) : irreducible (p.map_ring_hom : R[X] → R[X]) :=
sorry

theorem exercise_9_4_2c  (p : ℕ) [hp : fact (nat.prime p)] : irreducible (p ^ 4 + 4 * p ^ 3 + 6 * p ^ 2 + 2 * p + 1) :=
sorry

theorem exercise_9_4_9  {K : Type*} [field K] [algebra K K] [char_zero K] [char_zero K] [char_zero K]
  {a b : K} (ha : a ≠ 0) (hb : b ≠ 0) (h : a * b = 0) :
  irreducible (a * b) :=
sorry

theorem exercise_11_1_13  {V : Type*} [inner_product_space V] [normed_space V] [finite_dimensional V]
  {v : V} (hv : isometry v) : isometry v :=
sorry