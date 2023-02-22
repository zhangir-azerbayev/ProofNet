import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_2a : ¬commutative (λ a b : ℤ, a - b) :=
sorry

theorem exercise_1_1_4 (n : ℕ) (a b c : ℤ) :
  (a % n) * (b % n) * (c % n) = (a * b * c) % n :=
sorry

theorem exercise_1_1_15 {G : Type*} [group G] {n : ℕ} (a : fin n → G) :
  (∏ i in finset.range n, a i)⁻¹ = ∏ i in finset.range n, (a i)⁻¹ :=
sorry

theorem exercise_1_1_17 {G : Type*} [group G] (x : G)
  (hx : ∃ n : ℕ, x ^ n = 1) : x⁻¹ = x ^ (nat.find hx - 1) :=
sorry

theorem exercise_1_1_20 {G : Type*} [group G] (x : G) :
  order x = order x⁻¹ :=
sorry

theorem exercise_1_1_22b {G : Type*} [comm_group G] 
  [normed_group G] (a b : G) :
  ∥a * b∥ = ∥b * a∥ :=
sorry

theorem exercise_1_1_29 {A B : Type*} [group A] [group B] :
  abelian (A × B) ↔ abelian A ∧ abelian B :=
sorry

theorem exercise_1_3_8 {Ω : Type*} [fintype Ω] :
  infinite (perm Ω) :=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B] :
  is_group_isomorphism (prod.comm : A × B → B × A) :=
sorry

theorem exercise_1_6_23 {G : Type*} [group G] 
  [fintype G] (s : G →* G) (hs : ∀ (g : G), s g = g ↔ g = 1) 
  (hs2 : s ∘ s = function.id) :
  comm_group G :=
sorry

theorem exercise_2_1_13 {H : Type*} [add_group H] [decidable_eq H]
  [fintype H] (h : ∀ x : H, x ≠ 0 → 1 / x ∈ H) :
  H = (0 : H) ∨ H = univ :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G] 
  [fintype G] {H : subgroup G} (hH : H ≠ ⊤) :
  ∃ (M : subgroup G), M.is_maximal ∧ H ≤ M :=
sorry

theorem exercise_2_4_16c {G : Type*} [group G] 
  (x : G) (hx : x ≠ 1) (hx_ord : ∃ n : ℕ, x ^ n = 1) 
  (H : subgroup G) (hH : H.is_maximal) :
  ∃ (p : ℕ) (hp : nat.prime p), p ∣ (x.order hx_ord) ∧ H = ⟨x ^ p⟩ :=
sorry

theorem exercise_3_1_22a {G : Type*} [group G] {H K : subgroup G}
  (hH : H.normal) (hK : K.normal) : (H ∩ K).normal :=
sorry

theorem exercise_3_2_8 {G : Type*} 
  [group G] {H K : subgroup G} [fintype H] [fintype K] 
  (hH : H.card.prime_factors.pairwise_coprime) 
  (hK : K.card.prime_factors.pairwise_coprime) :
  H ∩ K = 1 :=
sorry

theorem exercise_3_2_16 (p : ℕ) (hp : nat.prime p) (a : ℕ) :
  nat.coprime a p → a ^ p ≡ a [MOD p] :=
sorry

theorem exercise_3_3_3 {G : Type*} 
  [group G] {H K : subgroup G} (hH : H.normal) (hK : K ≤ G) 
  (h : card (G ⧸ H) = nat.prime p) (hp : nat.prime p) :
  K ≤ H ∨ (G = H * K ∧ card (K ⧸ K ∩ H) = p) :=
sorry

theorem exercise_3_4_4 {G : Type*} 
  [group G] [fintype G] [abelian_group G] (n : ℕ) (h : n ∣ fintype.card G) :
  ∃ (H : subgroup G), fintype.card H = n :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] [solvable G] 
  {N : subgroup G} (hN : N.normal) : solvable (G / N) :=
sorry

theorem exercise_4_2_8 {G : Type*} [group G]
  {H : subgroup G} (hH : fintype (G / H)) :
  ∃ (K : subgroup G), K ≤ H ∧ fintype (G / K) ∧ card (G / K) ≤ fact (card (G / H)) :=
sorry

theorem exercise_4_2_9a {G : Type*} [group G] 
  [fintype G] {p : ℕ} [hp : fact (nat.prime p)] {α : ℕ} (hG : fintype.card G = p ^ α) 
  {H : subgroup G} (hH : H.card = p ^ (α - 1)) :
  H.normal :=
sorry

theorem exercise_4_4_2 {G : Type*} [group G] [fintype G]
  {p q : ℕ} (hp : nat.prime p) (hq : nat.prime q) (hpq : p ≠ q)
  (hG : fintype.card G = p * q) (hG_abelian : abelian_group G) :
  is_cyclic G :=
sorry

theorem exercise_4_4_6b {G : Type*} [group G] :
  ∃ (H : subgroup G), H.normal ∧ ¬H.char :=
sorry

theorem exercise_4_4_8a {G : Type*} [group G] {H K : subgroup G}
  (hH : H ≤ K) (hK : K ≤ G) (hK_normal : normal K G)
  (hH_char : characteristic K H) : normal H G :=
sorry

theorem exercise_4_5_13 {G : Type*} [group G] [fintype G]
  (hG : card G = 56) :
  ∃ (p : ℕ) (hp : nat.prime p) (H : sylow p G), H.normal :=
sorry

theorem exercise_4_5_15 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 351) :
  ∃ (p : ℕ) (hp : nat.prime p) (P : sylow p G), P.normal :=
sorry

theorem exercise_4_5_17 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 105) :
  ∃ (P : sylow 5 G) (Q : sylow 7 G), P.normalizer = Q.normalizer :=
sorry

theorem exercise_4_5_19 {G : Type*} [group G] [fintype G] 
  (hG : fintype.card G = 6545) : ¬simple_group G :=
sorry

theorem exercise_4_5_21 {G : Type*} [group G] [fintype G] 
  (hG : fintype.card G = 2907) : ¬simple_group G :=
sorry

theorem exercise_4_5_23 {G : Type*} [group G] [fintype G] 
  (hG : fintype.card G = 462) : ¬ simple_group G :=
sorry

theorem exercise_4_5_33 {G : Type*} [group G]
  {p : ℕ} [hp : fact (nat.prime p)] {P : subgroup G} (hP : is_p_group p P)
  (hP_normal : P ≤ normalizer G P) {H : subgroup G} :
  is_p_group p (P ∩ H) ∧ ∀ (Q : subgroup G), is_p_group p Q → Q ≤ P ∩ H :=
sorry

theorem exercise_7_1_2 {R : Type*} [comm_ring R] (u : R) (hu : is_unit u) :
  is_unit (-u) :=
sorry

theorem exercise_7_1_12 {R : Type*} [field R] {S : Type*} 
  [ring S] [subring R S] (h : (1 : S) ≠ 0) : integral_domain S :=
sorry

theorem exercise_7_2_2 {R : Type*} [comm_ring R]
  {p : polynomial R} :
  p.is_zero_divisor ↔ ∃ (b : R), b ≠ 0 ∧ b * p = 0 :=
sorry

theorem exercise_7_3_16 {R S : Type*} [ring R] [ring S]
  (f : R →+* S) (hf : function.surjective f) :
  f '' (center R) ⊆ center S :=
sorry

theorem exercise_7_4_27 {R : Type*} [comm_ring R] 
  (hR : ∃ (a : R), a ≠ 0 ∧ a ≠ 1) (a : R) (ha : a ≠ 0) (hna : a.is_nilpotent) 
  (b : R) : is_unit (1 - a * b) :=
sorry

theorem exercise_8_2_4 {R : Type*} 
  [integral_domain R] (hgcd : ∀ a b : R, a ≠ 0 ∨ b ≠ 0 → ∃ r s : R, gcd a b = r * a + s * b) 
  (hdiv : ∀ (a : ℕ → R) (h : ∀ i, a (i + 1) ∣ a i), ∃ N : ℕ, ∀ n ≥ N, a n = a N * (a n / a N)) :
  PID R :=
sorry

theorem exercise_8_3_5a {n : ℕ} 
  (hn : n > 3) (h : nat.prime 2) (h_sqrt_neg_n : ∃ (x : ℤ), x ^ 2 = -n) :
  irreducible (2 : ℤ[sqrt (-n)]) ∧ irreducible (sqrt (-n)) ∧ 
  irreducible (1 + sqrt (-n)) :=
sorry

theorem exercise_8_3_6b {q : ℤ} (hq : nat.prime q)
  (hq₁ : q % 4 = 3) :
  is_field (quotient_ring ℤ (ideal.span {q})) :=
sorry

theorem exercise_9_1_10 
  (R : Type*) [comm_ring R] :
  ∃ (I : ideal R), I.is_prime ∧ I.is_minimal ∧ ∀ (J : ideal R), J.is_prime → J.is_minimal → J = I :=
sorry

theorem exercise_9_4_2a  (p : polynomial ℤ) (hp : p.degree = 4) (hp₁ : p.coeff 4 = 1) (hp₂ : p.coeff 6 = -4) :
  irreducible p :=
sorry

theorem exercise_9_4_2c (x : polynomial ℤ) :
  irreducible (x^4 + 4*x^3 + 6*x^2 + 2*x + 1) :=
sorry

theorem exercise_9_4_9 {R : Type*} [comm_ring R]
  [is_ufd R] (x : R) :
  irreducible (x^2 - 2) :=
sorry

theorem exercise_11_1_13 (n : ℕ) :
  vector_space ℚ (euclidean_space ℝ (fin n)) ≃ₗ[ℚ] euclidean_space ℝ (fin 1) :=
sorry