import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory


theorem exercise_1_1_2a : ¬commutative (λ a b : ℤ, a - b) :=
sorry

theorem exercise_1_1_4 (n : ℕ) (a b c : ℕ) :
  (((a % n) * (b % n)) % n * (c % n)) % n = ((a % n) * ((b % n) * (c % n)) % n) % n :=
sorry

theorem exercise_1_1_15 {G : Type*} [group G] (n : ℕ) (a : ℕ → G) :
  (finset.prod (finset.range n) a)⁻¹ = finset.prod (finset.range n) (λ i, (a i)⁻¹) :=
sorry

theorem exercise_1_1_17 {G : Type*} [group G] (x : G) (n : ℕ)
  (hx : order_of x = n) : x⁻¹ = x^(n - 1) :=
sorry

theorem exercise_1_1_20 {G : Type*} [group G] (x : G) :
  order_of x⁻¹ = order_of x :=
sorry

theorem exercise_1_1_22b {G : Type*} [group G] [fintype G]
  (a b : G) : card (a * b • subgroup.center G) = card (b * a • subgroup.center G) :=
sorry

theorem exercise_1_1_29 {A B : Type*} [group A] [group B] :
  is_abelian (A × B) ↔ is_abelian A ∧ is_abelian B :=
sorry

theorem exercise_1_3_8 (ℕ : Type*) [infinite ℕ] :
  infinite (perm ℕ) :=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B] :
  (A × B) ≃* (B × A) :=
sorry

theorem exercise_1_6_23 {G : Type*} [group G] [fintype G]
  (σ : G ≃* G) (hσ : ∀ g : G, σ g = g ↔ g = 1) (hσ2 : σ * σ = 1) :
  is_abelian G :=
sorry

theorem exercise_2_1_13 {H : add_subgroup ℚ} :
  (∀ x ∈ H, x ≠ 0 → x⁻¹ ∈ H) → H = ⊥ ∨ H = ⊤ :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G] [fintype G]
  (H : subgroup G) (hH : H ≠ ⊤) :
  ∃ (M : subgroup G), M.is_maximal ∧ H ≤ M :=
sorry

theorem exercise_2_4_16c {G : Type*} [group G]
  {x : G} {n : ℕ} (hn : n ≥ 1) (hx : order_of x = n) (H : subgroup G)
  (hH : H ≤ subgroup.gpowers x) :
  H.is_maximal ↔ ∃ (p : ℕ) (hp : nat.prime p), H = subgroup.gpowers (x ^ p) ∧ p ∣ n :=
sorry

theorem exercise_3_1_22a {G : Type*} [group G]
  (H K : subgroup G) (hH : H.normal) (hK : K.normal) :
  (H ⊓ K).normal :=
sorry

theorem exercise_3_2_8 {G : Type*} [group G]
  {H K : subgroup G} (hH : fintype.card H) (hK : fintype.card K)
  (h_coprime : nat.coprime (fintype.card H) (fintype.card K)) :
  H ⊓ K = ⊥ :=
sorry

theorem exercise_3_2_16 (p : ℕ) [fact p.prime] (a : ℕ) :
  a ^ p ≡ a [MOD p] :=
sorry

theorem exercise_3_3_3 {G H K : Type*} [group G] [group H] [group K]
  (hG : fintype G) (hH : fintype H) (hK : fintype K)
  (hH_normal : normal_subgroup H G) (hp : fact (nat.prime (card (G ⧸ H))))
  (hK_leq_G : K ≤ G) :
  K ≤ H ∨ (G = H * K ∧ card (K ⧸ (K ⊓ H)) = hp.1) :=
sorry

theorem exercise_3_4_4
  {G : Type*} [group G] [fintype G] (hG : is_abelian G) (n : ℕ)
  (hn : n ∣ fintype.card G) : ∃ (H : subgroup G), fintype.card H = n :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] (H : subgroup G)
  (hG : is_solvable G) : is_solvable (G ⧸ H) :=
sorry

theorem exercise_4_2_8 {G : Type*} [group G]
  {H : subgroup G} (hH : H.index < cardinal.omega) :
  ∃ (K : subgroup G), K ≤ H ∧ K.normal ∧ cardinal.mk (quotient_group.quotient K) ≤ hH.fact :=
sorry

theorem exercise_4_2_9a {G : Type*} [group G] [fintype G]
  {p α : ℕ} (hp : fact (nat.prime p)) (hG : card G = p ^ α)
  (H : subgroup G) (hH : card (G ⧸ H) = p) :
  H.normal :=
sorry

theorem exercise_4_4_2 {G : Type*} [group G] [fintype G]
  (hpq : fact (fintype.card G = p * q)) (hab : comm_group G) :
  is_cyclic G :=
sorry

theorem exercise_4_4_6b {G : Type*} [group G] [fintype G] :
  ∃ (N : subgroup G), N.normal ∧ ¬N.characteristic :=
sorry

theorem exercise_4_4_8a {G H K : Type*} [group G] [group H] [group K]
  (hHK : H ≤ K) (hKH : is_char_subgroup H K) (hKG : is_normal_subgroup K G) :
  is_normal_subgroup H G :=
sorry

theorem exercise_4_5_13 (G : Type*) [group G] [fintype G]
  (hG : fintype.card G = 56) : ∃ (p : ℕ) (H : sylow p G), nat.prime p ∧ H.normal :=
sorry

theorem exercise_4_5_15 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 351) : ∃ p [fact (nat.prime p)], ∃ (P : sylow p G), is_normal P :=
sorry

theorem exercise_4_5_17 {G : Type*} [group G] [fintype G]
  (hG : card G = 105) :
  (∃ (P5 : sylow 5 G), is_normal P5) ∧ (∃ (P7 : sylow 7 G), is_normal P7) :=
sorry

theorem exercise_4_5_19 (G : Type*) [group G] [fintype G] (hG : card G = 6545) :
  ¬ is_simple_group G :=
sorry

theorem exercise_4_5_21 (G : Type*) [group G] [fintype G] (hG : fintype.card G = 2907) :
  ¬ is_simple_group G :=
sorry

theorem exercise_4_5_23 (G : Type*) [group G] [fintype G] (hG : card G = 462) :
  ¬ is_simple_group G :=
sorry

theorem exercise_4_5_33 {G : Type*} [group G] [fintype G]
  {p : ℕ} [fact (nat.prime p)] {P : sylow p G} (hP : is_normal P)
  {H : subgroup G} :
  ∃! (Q : sylow p H), P.to_subgroup ⊓ H = Q.to_subgroup :=
sorry

theorem exercise_7_1_2 {R : Type*} [ring R] (u : R) (hu : is_unit u) :
  is_unit (-u) :=
sorry

theorem exercise_7_1_12 {K : Type*} [field K]
  (R : Type*) [ring R] [is_subring R K] : integral_domain R :=
sorry

theorem exercise_7_2_2 {R : Type*} [comm_ring R]
  (p : polynomial R) :
  is_zero_divisor p ↔ ∃ (b : R), b ≠ 0 ∧ b • p = 0 :=
sorry

theorem exercise_7_3_16 {R S : Type*} [comm_ring R] [comm_ring S]
  (ϕ : R →+* S) (hϕ : function.surjective ϕ) :
  ϕ '' (center R) ⊆ center S :=
sorry

theorem exercise_7_4_27 {R : Type*} [comm_ring R] (a : R)
  (ha : is_nilpotent a) (b : R) :
  is_unit (1 - a * b) :=
sorry

theorem exercise_8_2_4
  (R : Type*) [integral_domain R] [nontrivial R]
  (h_gcd : ∀ (a b : R), a ≠ 0 → b ≠ 0 →
    ∃ (d : R) (r s : R), d = gcd a b ∧ d = a * r + b * s)
  (h_chain : ∀ (a : ℕ → R), (∀ i, a (i + 1) ∣ a i) →
    ∃ (N : ℕ), ∀ (n : ℕ), n ≥ N → ∃ (u : R), is_unit u ∧ a n = u * a N) :
  is_principal_ideal_domain R :=
sorry

theorem exercise_8_3_5a (n : ℕ) (hn : squarefree n) (hn_gt_3 : n > 3) :
  irreducible (2 : ℤ[sqrt (-n)]) ∧
  irreducible (sqrt (-n) : ℤ[sqrt (-n)]) ∧
  irreducible (1 + sqrt (-n) : ℤ[sqrt (-n)]) :=
sorry

theorem exercise_8_3_6b 
  (q : ℕ) [fact (nat.prime q)] (hq : q % 4 = 3) :
  fintype.card (units (zmod q)[X] / (X ^ 2 + 1)) = q ^ 2 :=
sorry

theorem exercise_9_1_10 
  (R : Type*) [comm_ring R] (x : ℕ → R) :
  infinite {P : ideal (R ⧸ ideal.span (set.range (λ n, x (2 * n) * x (2 * n + 1)))) | P.is_prime ∧ P.is_minimal} :=
sorry

theorem exercise_9_4_2a  :
  irreducible (polynomial.X ^ 4 - 4 * polynomial.X ^ 3 + 6 : polynomial ℤ) :=
sorry

theorem exercise_9_4_2c {R : Type*} [comm_ring R] :
  irreducible (polynomial.X ^ 4 + 4 * polynomial.X ^ 3 + 6 * polynomial.X ^ 2 + 2 * polynomial.X + 1 : polynomial R) :=
sorry

theorem exercise_9_4_9 {R : Type*} [integral_domain R]
  [algebra ℚ[R] ℝ] (hR : ∀ (x : ℝ), algebra_map ℚ[R] ℝ (real.sqrt 2 * x) = x * real.sqrt 2)
  (hR' : ∀ (x : ℝ), algebra_map ℚ[R] ℝ (x * real.sqrt 2) = x * real.sqrt 2) :
  irreducible (X ^ 2 - C (real.sqrt 2)) :=
sorry

theorem exercise_11_1_13 (n : ℕ) :
  nonempty (module_equiv ℚ (euclidean_space ℝ (fin n)) ℝ) :=
sorry