import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_2a {G : Type*} [subtraction_monoid G]
  {a b : G} (h : add_commute a b) : ¬commute a b :=
sorry

theorem exercise_1_1_4 (a b c : ℕ) :
	(a * b) % c = (a % c) * (b % c) % c :=
sorry

theorem exercise_1_1_15 {G : Type*} {α : Sort u_4} [division_comm_monoid G]
  (f : α → G) :
  finprod (λ (x : α), (f x)⁻¹) = (finprod (λ (x : α), f x))⁻¹ :=
sorry

theorem exercise_1_1_17 {G : Type u} [monoid G] (x : G) (n : ℕ)
  (hx : x ^ n = 1) (hn : n ≠ 0) :
  x⁻¹ = x ^ (n - 1) :=
sorry

theorem exercise_1_1_20 {G : Type u} [group G] (x : G) :
	order_of x⁻¹ = order_of x :=
sorry

theorem exercise_1_1_22b {α : Type*} [normed_division_ring α] (a b : α) :
	‖a * b‖ = ‖b * a‖ :=
sorry

theorem exercise_1_1_29 {A B : Type*}
  [abelian_group A] [abelian_group B] :
  abelian_group (A × B) ↔ abelian_group A ∧ abelian_group B :=
sorry

theorem exercise_1_3_8 {α : Type*} [decidable_eq α] [fintype α]
  (h : fintype (equiv.perm α) → false) :
  infinite (equiv.perm α) :=
sorry

theorem exercise_1_6_11 (A B : Top) :
	(fundamental_groupoid_functor.prod_iso A B).hom = (fundamental_groupoid_functor.prod_iso B A).hom :=
sorry

theorem exercise_1_6_23  {G : Type*} [fintype G] [group G] (sigma : G → G)
  (hsigma : ∀ (g : G), sigma g = g ↔ g = 1)
  (hsigma_squared : ∀ (g : G), sigma (sigma g) = g) :
  is_abelian G :=
sorry

theorem exercise_2_1_13 {G : Type*} [add_group G]
  (H : add_subgroup G) (h : ∀ (x : G), x ≠ 0 → 1 / x ∈ H) :
  H = ⊥ ∨ H = univ :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G]
  (H : subgroup G) (hH : H ≠ ⊤) :
  ∃ (M : subgroup G), M.is_maximal_of_proper_subgroup H :=
sorry

theorem exercise_2_4_16c {G : Type u} {x : G} [monoid G]
  {p : ℕ} [hp : fact (nat.prime p)] (hg : x ^ p = 1) (hg1 : x ≠ 1) :
  maximal (subgroup.closure (λ (i : ℕ), x ^ i)) ↔ order_of x = p :=
sorry

theorem exercise_3_1_22a {G : Type*} [group G] (H K : subgroup G)
  [hH : H.normal] [hK : K.normal] :
  (H ⊓ K).normal :=
sorry

theorem exercise_3_2_8 {G : Type*} [group G] {H K : subgroup G}
  [fintype G] [fintype ↥H] [fintype ↥K] (h1 : fintype.card ↥H.finite)
  (h2 : fintype.card ↥K.finite) (h3 : (fintype.card ↥H).coprime (fintype.card ↥K)) :
  H ∩ K = 1 :=
sorry

theorem exercise_3_2_16 {p a : ℕ} (hp : nat.prime p) (a1 : a ≠ 1) :
  a ^ p ≡ a [ZMOD p] :=
sorry

theorem exercise_3_3_3 {p : ℕ} {G : Type*} [group G]
  {H K : subgroup G} (hH : is_p_group p H) (hK : is_p_group p K)
  (hHK : K ≤ H.normalizer) :
  is_p_group p (H ⊔ K) :=
sorry

theorem exercise_3_4_4 {G : Type*} [group G]
  [fintype G] [fintype.abelian G] (n : ℕ) (hdvd : n ∣ fintype.card G) :
  ∃ (H : subgroup G), fintype.card ↥H = n :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] (H : subgroup G)
  [H.normal] [h : is_solvable G] :
  is_solvable (G ⧸ H) :=
sorry

theorem exercise_4_2_8 {G : Type*} [group G]
  {H : subgroup G} [H.finite_index] :
  ∃ (K : subgroup G) [K.normal], K ≤ H ∧ (nat.card ↥K).finite ∧
  (nat.card ↥K).fact ≤ H.index :=
sorry

theorem exercise_4_2_9a {G : Type u} [group G] {p : ℕ}
  [fact (nat.prime p)] (hG : ∃ (n : ℕ), G.card = p ^ n) (H : subgroup G)
  [H.finite_index] (hH : H.index = p) : H.normal :=
sorry

theorem exercise_4_4_2 {α : Type u} [group α] [fintype α] {p : ℕ}
  [hp : fact (nat.prime p)] (h : fintype.card α = p) :
  is_cyclic α :=
sorry

theorem exercise_4_4_6b {G : Type*} [group G] :
	∃ (H : subgroup G), H.normal ∧ ¬H.characteristic :=
sorry

theorem exercise_4_4_8a {G : Type*} [group G]
  {H : subgroup G} [hH : H.normal] {K : subgroup G} [hK : K.characteristic]
  (HK : H ≤ K) : H.normal :=
sorry

theorem exercise_4_5_13 {G : Type*} [group G] [fintype G]
  (hG : fintype.card G = 56) :
  ∃ (p : ℕ) (h : nat.prime p) (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_15 {G : Type u} [group G] [finite G]
  (hG : G.card = 351) :
  ∃ (p : ℕ) (h : nat.prime p) (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_17 {G : Type*} [group G]
  (hG : fintype.card G = 105) :
  ∃ (P : sylow 5 G) (Q : sylow 7 G), P.normal ∧ Q.normal :=
sorry

theorem exercise_4_5_19 {G : Type*} [group G] [fintype G]
  (h : fintype.card G = 6545) : ¬ simple_group G :=
sorry

theorem exercise_4_5_21 {G : Type*} [group G] [fintype G]
  (h : fintype.card G = 2907) : ¬ simple_group G :=
sorry

theorem exercise_4_5_23 {G : Type*} [group G] [fintype G]
  (h : fintype.card G = 462) : ¬ simple_group G :=
sorry

theorem exercise_4_5_33 {G : Type u} [group G] {p : ℕ}
  [fact (nat.prime p)] [finite (sylow p G)] (P : sylow p G) (h : ↑P.normal) :
  subsingleton (sylow p G) :=
sorry

theorem exercise_7_1_2 {R : Type*} [ring R] (u : units R) :
  units (-u) :=
sorry

theorem exercise_7_1_12 {R S : Type*} [comm_ring R]
  [comm_ring S] [is_field S] (hRS : is_subring R S) : is_domain R :=
sorry

theorem exercise_7_2_2 {R : Type u} [semiring R]
  [no_zero_divisors R] {p : polynomial R} :
  is_zero_divisor p ↔ ∃ (b : R), b ≠ 0 ∧ b * p = 0 :=
sorry

theorem exercise_7_3_16 {R : Type u} {S : Type v}
  [ring R] [ring S] (f : R →+* S) (hf : function.surjective f) :
  f '' (set.center R) ⊆ set.center S :=
sorry

theorem exercise_7_4_27 {R : Type u}
  [comm_ring R] [local_ring R] (a : R) (h : a ∈ nonunits R) :
  is_unit (1 - a) :=
sorry

theorem exercise_8_2_4  {R : Type*} [integral_domain R] (h : ∀ (a b : R), a ≠ 0 → b ≠ 0 →
    ∃ (r s : R), gcd_monoid.gcd a b = r * a + s * b)
  (h' : ∀ (a : ℕ → R), (∀ (i : ℕ), a (i + 1) ∣ a i) → ∃ (N : ℕ),
    ∀ (n : ℕ), n ≥ N → a n = units.mk0 (a N)) :
  is_principal_ideal_ring R :=
sorry

theorem exercise_8_3_5a {n : ℕ}
  (hn : n > 3) (hns : squarefree n) :
  irreducible (2 : ℤ[sqrt (-n)]) ∧ irreducible (sqrt (-n)) ∧
  irreducible (1 + sqrt (-n)) :=
sorry

theorem exercise_8_3_6b {p q : ℕ}
  [fact (nat.prime p)] [fact (nat.prime q)] (hp3 : p % 4 = 3) (hq3 : q % 4 = 3)
  (hpq : p ≠ q) :
  is_square ↑q ↔ ¬is_square ↑p :=
sorry

theorem exercise_9_1_10 {R : Type*}
  [comm_ring R] (I : ideal R) (hI : I.is_infinite) :
  ∃ (p : ideal R), p.is_prime ∧ p.is_minimal ∧ p.is_infinite :=
sorry

theorem exercise_9_4_2a {p : polynomial ℤ} (hp : p.is_unit_trinomial)
  (h : ∀ (q : polynomial ℤ), q ∣ p → q ∣ p.mirror → is_unit q) :
  irreducible p :=
sorry

theorem exercise_9_4_2c (x : ℤ) :
  irreducible (x ^ 4 + 4 * x ^ 3 + 6 * x ^ 2 + 2 * x + 1) :=
sorry

theorem exercise_9_4_9 {R : Type*} [comm_ring R]
  [is_domain R] [unique_factorization_domain R] {x : R} (hx : x ≠ 0)
  {ϖ : R} (hirr : irreducible ϖ) :
  squarefree (x ^ 2 - ϖ) → irreducible (x ^ 2 - ϖ) :=
sorry

theorem exercise_11_1_13 {K : Type u} {V : Type v} [field K]
  [add_comm_group V] [module K V] (h : module.rank K V = 1) :
  V ≃ₗ[K] K :=
sorry