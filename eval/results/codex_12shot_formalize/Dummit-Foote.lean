

theorem exercise_1_1_15 {G : Type*} [group G] {n : ℕ} (a : fin n → G) :
  (∏ i in finset.range n, a i)⁻¹ = ∏ i in finset.range n, (a i)⁻¹ :=
sorry

theorem exercise_1_1_16 {G : Type*} [group G] (x : G)
  (hx : x ^ 2 = 1) : order x = 1 ∨ order x = 2 :=
sorry

theorem exercise_1_1_17 {G : Type*} [group G] (x : G)
  (hx : x.order = ⟨nat.succ_pos _, λ n hn, nat.succ_inj hn⟩) :
  x⁻¹ = x ^ (x.order.val - 1) :=
sorry

theorem exercise_1_1_18 {G : Type*} [group G]
  (x y : G) : x * y = y * x ↔ y⁻¹ * x * y = x ↔ x⁻¹ * y⁻¹ * x * y = 1 :=
sorry

theorem exercise_1_1_20 {G : Type*} [group G] (x : G) :
  order x = order x⁻¹ :=
sorry

theorem exercise_1_1_22a (G : Type*) [group G] (x g : G) :
  order x = order (g⁻¹ * x * g) :=
sorry

theorem exercise_1_1_22b {G : Type*} [comm_group G] 
  [normed_group G] (a b : G) :
  ∥a * b∥ = ∥b * a∥ :=
sorry

theorem exercise_1_1_25 {G : Type*} [group G] (h : ∀ x : G, x ^ 2 = 1) :
  abelian G :=
sorry

theorem exercise_1_1_29 {A B : Type*} [group A] [group B] :
  abelian (A × B) ↔ abelian A ∧ abelian B :=
sorry

theorem exercise_1_1_2a : ¬commutative (λ a b : ℤ, a - b) :=
sorry

theorem exercise_1_1_3 (n : ℕ) (a b c : ℤ) :
  (a + b) % n + c % n = (a + c) % n + b % n :=
sorry

theorem exercise_1_1_34 (G : Type*) [group G] (x : G)
  (hx_inf : ∀ n : ℕ, x ^ n ≠ 1) :
  ∀ m n : ℤ, m ≠ n → x ^ m ≠ x ^ n :=
sorry

theorem exercise_1_1_4 (n : ℕ) (a b c : ℤ) :
  (a % n) * (b % n) * (c % n) = (a * b * c) % n :=
sorry

theorem exercise_1_1_5 {n : ℕ} (h : 1 < n) :
  ¬group (add_comm_group.quotient (nat.mod_add_sub_group n)) :=
sorry

theorem exercise_1_3_8 {Ω : Type*} [fintype Ω] :
  infinite (perm Ω) :=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B] :
  is_group_isomorphism (prod.comm : A × B → B × A) :=
sorry

theorem exercise_1_6_17 {G : Type*} [group G] :
  function.is_group_hom (λ g : G, g⁻¹) ↔ comm_group G :=
sorry

theorem exercise_1_6_23 {G : Type*} [group G] 
  [fintype G] (s : G →* G) (hs : ∀ (g : G), s g = g ↔ g = 1) 
  (hs2 : s ∘ s = function.id) :
  comm_group G :=
sorry

theorem exercise_1_6_4 :
  ¬(multiplicative ℝ ≃* multiplicative ℂ) :=
sorry

theorem exercise_2_1_13 {H : Type*} [add_group H]
  [decidable_eq H] [has_inv H] [has_one H] (h : ∀ x : H, x ≠ 0 → x⁻¹ ∈ H) :
  H = {0} ∨ H = univ :=
sorry

theorem exercise_3_1_22a {G : Type*} [group G] {H K : subgroup G}
  (hH : H.normal) (hK : K.normal) : (H ∩ K).normal :=
sorry

theorem exercise_3_1_22b {G : Type*} [group G] 
  {I : Type*} (hI : nonempty I) (H : I → subgroup G) (hH : ∀ i, is_normal (H i)) :
  is_normal (⋂ i, H i) :=
sorry

theorem exercise_3_1_3a {A B : Type*} [group A] [group B]
  [abelian A] (hB : is_subgroup B) : abelian (A / B) :=
sorry

theorem exercise_3_2_16 (p : ℕ) (hp : nat.prime p) (a : ℕ) :
  a ^ p ≡ a [MOD p] :=
sorry

theorem exercise_3_2_21a :
  ∀ (G : Type*) [group G] [fintype G] [decidable_eq G] (H : subgroup G) 
  (hH : H ≠ ⊥) (hH_fin : fintype.card H < fintype.card G),
  H = ⊤ :=
sorry

theorem exercise_3_2_8 {G : Type*} 
  [group G] {H K : subgroup G} [fintype H] [fintype K] 
  (hH : H.card.prime_factors.pairwise_coprime) 
  (hK : K.card.prime_factors.pairwise_coprime) :
  H ∩ K = 1 :=
sorry

theorem exercise_3_4_1 {G : Type*} [group G] 
  [fintype G] [fact (nat.prime (card G))] (hG : is_cyclic G) :
  is_abelian_simple G :=
sorry

theorem exercise_3_4_4 {G : Type*} 
  [group G] [fintype G] [abelian_group G] (n : ℕ) (h : n ∣ fintype.card G) :
  ∃ (H : subgroup G), fintype.card H = n :=
sorry

theorem exercise_3_4_5a {G : Type*} [group G] [solvable G] 
  {H : subgroup G} : solvable H :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] [solvable G] 
  {N : subgroup G} (hN : N.normal) : solvable (G / N) :=
sorry

theorem exercise_4_3_26 {G : Type*} 
  [fintype G] [group G] [perm_group G] (A : Type*) [fintype A] (hA : fintype.card A > 1) 
  (hG : transitive_perm_group G A) :
  ∃ (σ : G), ∀ (a : A), σ a ≠ a :=
sorry

theorem exercise_4_4_6a {G : Type*} [group G] {p : ℕ} [hp : fact (nat.prime p)]
  (H : subgroup G) (hH : H.is_char p) :
  normal_subgroup H :=
sorry

theorem exercise_4_5_13 {G : Type*} [group G] [fintype G]
  (hG : card G = 56) :
  ∃ (p : ℕ) (hp : nat.prime p) (H : sylow p G), H.normal :=
sorry

theorem exercise_4_5_14 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 312) :
  ∃ (p : ℕ) (hp : nat.prime p) (H : sylow p G), H.normal :=
sorry

theorem exercise_4_5_1a {G : Type*} [group G] {p : ℕ}
  (hG : is_p_group p G) (H : subgroup G) (P : sylow p G) (hP : P ≤ H) :
  sylow p H :=
sorry