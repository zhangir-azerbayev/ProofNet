import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 [fintype G] [fintype G]
  (hG : fintype.card G = 2 ^ (fintype.card G)) :
  ∃ a : G, a ≠ e ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] [fintype G]
  (a : G) (n : ℕ) (h : ∀ x, x ^ n = a) : ∃ (k : ℕ), k > 0 ∧ k ≤ n ∧ a ^ k = e :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] 
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : card H = p ^ n) :
  abelian G :=
sorry

theorem exercise_2_2_6cabelian_group_of_order_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd_pow_dvd:=
sorry

theorem exercise_2_3_16 {G : Type*} [group G]
  [fintype G] [no_proper_subgroups G] :
  prime :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G] [fintype G]
  (H : subgroup G) (hH : normalizer H = H) :
  ∃ (a b : G), a ∈ H ∧ b ∈ H ∧ a * b = b * a :=
sorry

theorem exercise_2_5_31  {G : Type*} [group G] [fintype G] [fintype.card G = p ^ n * m]
  (H : subgroup G) (hH : H.is_characteristic) :
  H.is_characteristic :=
sorry

theorem exercise_2_5_43 {G : Type*} [group G] [fintype G]
  [is_cyclic G] (f : G →* G) (hf : f.ker ≤ center G) : abelian G :=
sorry

theorem exercise_2_5_52 {G : Type*} [group G] [fintype G]
  [fintype.card G] [fintype.card (G ⧸ subgroup.comap G.normalizer.subtype)]
  (hG : fintype.card G = 3 ∨ fintype.card G = 4) :
  abelian G :=
sorry

theorem exercise_2_7_7  {G G' : Type*} [group G] [group G'] [uniform_space G] [uniform_space G']
  (hG : compact_space G) (hG' : compact_space G') (h : uniform_continuous (λ x, x))
  (hG'N : uniform_continuous (λ x, x ∘ N)) : uniform_continuous (λ x, x ∘ N) :=
sorry

theorem exercise_2_8_15 {p q : ℕ} [hp : fact (prime p)] [hpq : fact (prime q)]
  (h : p ≠ q) (hq : q ∣ p - 1) :
  is_cyclic (ℤ / p * ℤ / q) :=
sorry

theorem exercise_2_10_1  {G : Type*} [group G] [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : card H = p ^ n) (hA : A ≤ H) (hA' : A' ≤ H)
  (hA_prime : A' ≤ H.normalizer) :
  A ∩ (H.normalizer.subtype H) = (e) :=
sorry

theorem exercise_2_11_7automorphism_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_subgroup_of_Sylow_sub:=
sorry

theorem exercise_3_2_21  {G : Type*} [group G] [fintype G] {p n : ℕ} [hp : fact (nat.prime p)]
  {H : subgroup G} (hH : card H = p ^ n) (h : H.normalizer = G) :
  ∃ (σ : G), σ ∈ H.cycle_center_eq_id ∧ σ ∈ H.cycle_center_eq_id ∧ σ ^ p = e :=
sorry

theorem exercise_4_1_34  {A : Type*} [field A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A] [algebra A A]
  [algebra A A] [algebra A A] [algebra A A:=
sorry

theorem exercise_4_2_6comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_comm_group_of_square_zero_:=
sorry

theorem exercise_4_3_1ideal_of_comm_ring_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_of_ideal_:=
sorry

theorem exercise_4_4_9 {p : ℕ} [hp : fact (prime p)]
  {H : subgroup G} (hH : card H = p ^ (p - 1) / 2) :
  quadratic_residues G H :=
sorry

theorem exercise_4_5_23irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_irreducible_of_ir:=
sorry

theorem exercise_4_6_2 [nontrivial ℕ] [char_p ℚ] [char_p ℚ]
  (f : ℚ[x] → ℚ[x]) (hf : irreducible f) (hdeg : degree f = 3) : irreducible f :=
sorry

theorem exercise_5_1_8power_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_pow_add_eq_sum_:=
sorry

theorem exercise_5_3_7algebraic_of_square_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_of_two_is_root_:=
sorry

theorem exercise_5_4_3 {a : ℚ} (h : p(a) = 0) :
  algebraic a :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] [char_p F] [char_zero F]
  {m : ℕ} (hm : m ≠ 0) (x : F) :
  x ∈ (x ^ m - x) ↔ x ^ m ≠ x :=
sorry