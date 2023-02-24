import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_21 {G : Type*} [group G] (hG : fintype G) 
  (hG_card : fintype.card G = 5) :
  abelian_group G :=
sorry

theorem exercise_2_1_27 {G : Type*} [group G] [fintype G] :
  ∃ (m : ℕ), ∀ (a : G), a ^ m = 1 :=
sorry

theorem exercise_2_2_5 {G : Type*} 
  [group G] (h : ∀ (a b : G), (a * b) ^ 3 = a ^ 3 * b ^ 3 ∧ (a * b) ^ 5 = a ^ 5 * b ^ 5) :
  abelian_group G :=
sorry

theorem exercise_2_3_17 {G : Type*} [group G] 
  (a x : G) :
  commutator x⁻¹ * a * x = x⁻¹ * commutator a * x :=
sorry

theorem exercise_2_4_36 {n : ℕ} (h : n > 1) (hn : n.prime_pow) :
  ∀ (m : ℕ), m ∣ nat.phi (n ^ m - 1) :=
sorry

theorem exercise_2_5_30 {G : Type*} [group G] 
  [fintype G] {p m : ℕ} [hp : fact (nat.prime p)] (hG : card G = p * m) 
  (H : subgroup G) (hH : card H = p) (hHn : H ≤ normalizer G H) :
  char_subgroup G H :=
sorry

theorem exercise_2_5_37 {G : Type*} [group G]
  (hG : ¬ abelian_group G) (hG₁ : G.card = 6) :
  G ≃ symmetric_group 3 :=
sorry

theorem exercise_2_5_44 {G : Type*} 
  [group G] [fintype G] {p : ℕ} [hp : fact (nat.prime p)] 
  (hG : fintype.card G = p ^ 2) :
  ∃ (H : subgroup G), H.normal ∧ fintype.card H = p :=
sorry

theorem exercise_2_6_15 {G : Type*} [group G]
  [abelian G] {m n : ℕ} (hm : ∃ (x : G), x ^ m = 1) (hn : ∃ (x : G), x ^ n = 1)
  (hmn : nat.coprime m n) :
  ∃ (x : G), x ^ (m * n) = 1 :=
sorry

theorem exercise_2_8_12 {G H : Type*} [group G] 
  [group H] (hG : ¬ abelian G) (hH : ¬ abelian H) (hG_21 : card G = 21) 
  (hH_21 : card H = 21) :
  G ≃ H :=
sorry

theorem exercise_2_9_2 {G₁ G₂ : Type*} [group G₁] 
  [group G₂] [is_cyclic G₁] [is_cyclic G₂] (h : nat.gcd (card G₁) (card G₂) = 1) :
  is_cyclic (G₁ × G₂) :=
sorry

theorem exercise_2_11_6 {G : Type*} [group G] {p : ℕ} [hp : fact (nat.prime p)]
  {P : sylow p G} (hP : P.normal) :
  ∀ (Q : sylow p G), P = Q :=
sorry

theorem exercise_2_11_22 {G : Type*} [group G] [fintype G]
  {p n : ℕ} [hp : fact (nat.prime p)] (hG : card G = p ^ n)
  {H : subgroup G} (hH : card H = p ^ (n - 1)) :
  H ≤ normalizer G :=
sorry

theorem exercise_4_1_19 :
  ∞ = cardinal.mk (quaternion.I : quaternion ℚ) :=
sorry

theorem exercise_4_2_5 {R : Type*} [ring R] 
  (h : ∀ x : R, x ^ 3 = x) : comm_ring R :=
sorry

theorem exercise_4_2_9 {p : ℕ} (hp : nat.prime p)
  (h : ∑ i in finset.range (p - 1), (1 : ℚ) / (i + 1) = (a : ℚ) / b) :
  p ∣ a :=
sorry

theorem exercise_4_3_25 {R : Type*} [ring R] 
  [fintype R] [decidable_eq R] (I : ideal R) :
  I = ⊥ ∨ I = ⊤ :=
sorry

theorem exercise_4_5_16 {p : ℕ} [fact (nat.prime p)] 
  {n : ℕ} {q : polynomial ℤ} (hq : irreducible q) (hqn : q.nat_degree = n) :
  fintype.card (fintype.of_equiv (polynomial.quotient_ring_equiv_of_irreducible q)) = p ^ n :=
sorry

theorem exercise_4_5_25 {p : ℕ} (hp : nat.prime p) :
  irreducible (polynomial.C 1 + polynomial.sum (λ i, polynomial.X ^ i) (finset.range (p - 1))) :=
sorry

theorem exercise_4_6_3 (f : polynomial ℚ)
  (hf : irreducible f) (hf_deg : f.nat_degree = 7) :
  ∃ (a : ℤ), irreducible (f + C a) :=
sorry

theorem exercise_5_2_20 {V : Type*} [field F] [add_comm_group V]
  [vector_space F V] [fintype V] (h : ∃ (s : finset (submodule F V)), 
  ∀ (x : V), x ∈ ⨆ (H : submodule F V) (H_1 : H ∈ s), H → x = 0) :
  false :=
sorry

theorem exercise_5_3_10 : algebraic ℚ (cos (1 : ℝ)) :=
sorry

theorem exercise_5_5_2  (x : polynomial ℚ) (hx : x.degree = 3) (hx₁ : x.coeff 1 = 1) 
  (hx₃ : x.coeff 3 = -3) : irreducible x :=
sorry