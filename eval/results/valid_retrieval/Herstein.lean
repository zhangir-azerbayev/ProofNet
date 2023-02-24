import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_21 {G : Type*} [group G] [fintype G]
  (h : fintype.card G = 5) : abelian G :=
sorry

theorem exercise_2_1_27 {G : Type*} [group G] [finite G] (x : G) :
  ∃ (i : ℤ) (H : i ≠ 0), x ^ i = 1 :=
sorry

theorem exercise_2_2_5 {G : Type*} [group G]
  (h : ∀ (a b : G), (a * b) ^ 3 = a ^ 3 * b ^ 3 ∧ (a * b) ^ 5 = a ^ 5 * b ^ 5) :
  abelian G :=
sorry

theorem exercise_2_3_17 {G : Type*} [group G] (a x : G) :
  commute (x⁻¹ * a * x) (x⁻¹ * x) :=
sorry

theorem exercise_2_4_36 {a n : ℕ} (ha : a > 1) (hn : n > 0) :
  n ∣ nat.phi (a ^ n - 1) :=
sorry

theorem exercise_2_5_30 {G : Type*} [group G]
  (p : ℕ) [hp : fact (nat.prime p)] (m : ℕ) (h : p ∣ m)
  (H : subgroup G) (hH : fintype.card ↥H = p ^ m) (hHn : H.normal) :
  H.characteristic :=
sorry

theorem exercise_2_5_37 {G : Type*} [hG : group G]
  (h : ∀ (a b : G), a * b = b * a) :
  is_solvable G :=
sorry

theorem exercise_2_5_44 {p : ℕ} {G : Type*} [group G]
  {H K : subgroup G} (hH : is_p_group p H) (hK : is_p_group p K)
  (hHK : H ≤ K.normalizer) :
  is_p_group p (H ⊔ K) :=
sorry

theorem exercise_2_6_15 {G : Type u} {x : G} {n : ℕ} [monoid G]
  (h : n.coprime (order_of x)) :
  ∃ (m : ℕ), (x ^ n) ^ m = x :=
sorry

theorem exercise_2_8_12 {G : Type*} [group G]
  (hG : G.order = 21) (hGn : ¬ abelian_group G) :
  ∃ (H : Type*) [group H], H.order = 21 ∧ ¬ abelian_group H ∧ G ≃* H :=
sorry

theorem exercise_2_9_2 {G₁ G₂ : Type*} [group G₁] [group G₂]
  (hG₁ : cyclic G₁) (hG₂ : cyclic G₂) :
  (∃ (g : G₁ × G₂), ∀ (n : ℕ), (g ^ n).1 = 1) ↔
  nat.coprime (order G₁) (order G₂) :=
sorry

theorem exercise_2_11_6 {G : Type u} [group G] {p : ℕ}
  [fact (nat.prime p)] [finite (sylow p G)] (P : sylow p G) (h : ↑P.normal) :
  subsingleton (sylow p G) :=
sorry

theorem exercise_2_11_22 {G : Type u} [group G]
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G}
  (hH : fintype.card ↥H = p ^ (n - 1))
  (h : fintype.card ↥(H.normalizer) ≡ fintype.card G [MOD p ^ n]) :
  ↑H.normalizer ≤ H :=
sorry

theorem exercise_4_1_19  {R : Type*} [comm_ring R] [has_one R] [has_neg R] [infinite R] :
  infinite {x : quaternion R | x ^ 2 = -1} :=
sorry

theorem exercise_4_2_5 {R : Type*} [ring R]
  (h : ∀ (x : R), x ^ 3 = x) : commutative R :=
sorry

theorem exercise_4_2_9 {p : ℕ} (hp : odd_prime p) :
  p ∣ (finset.range p).sum (λ (i : ℕ), (1 : ℚ) / (i + 1)) :=
sorry

theorem exercise_4_3_25 {R : Type u} [comm_ring R]
  (I : ideal (matrix_ring 2 2 R)) :
  I = ⊥ ∨ I = ⊤ :=
sorry

theorem exercise_4_5_16 {Fq : Type*}
  [field Fq] [fintype Fq] (p : ℕ) [fact (nat.prime p)] (n : ℕ) (h : n ≠ 0)
  (q : polynomial Fq) (hq : irreducible q) :
  fintype.card (quotient_ring.quotient q) = p ^ n :=
sorry

theorem exercise_4_5_25 {R : Type*} [comm_ring R]
  {p n : ℕ} (hp : nat.prime p) :
  polynomial.cyclotomic (p ^ (n + 1)) R = (finset.range p).sum (λ (i : ℕ), (polynomial.X ^ p ^ n) ^ i) :=
sorry

theorem exercise_4_6_3 {a : ℕ}
  (h : ∀ (a : ℕ), irreducible (polynomial.C a * polynomial.X ^ 7 + 15 * polynomial.C a * polynomial.X ^ 2 - 30 * polynomial.C a * polynomial.X + polynomial.C a)) :
  ∃ (a : ℕ), ∀ (a : ℕ), irreducible (polynomial.C a * polynomial.X ^ 7 + 15 * polynomial.C a * polynomial.X ^ 2 - 30 * polynomial.C a * polynomial.X + polynomial.C a) :=
sorry

theorem exercise_5_2_20 {K : Type u} {V : Type v}
  [division_ring K] [add_comm_group V] [module K V]
  (h : ¬∃ (s : set V) (b : basis ↥s K V), s.finite) :
  finite_dimensional.finrank K V = 0 :=
sorry

theorem exercise_5_3_10 :
	is_algebraic ℚ (real.cos (1 * real.pi / 180)) :=
sorry

theorem exercise_5_5_2 {R : Type*} [field R]
  (p : polynomial R) (h : p.degree = 1) (x : R) (hx : p.is_root x) :
  irreducible p :=
sorry