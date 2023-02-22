import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 {G : Type*} [group G] (hG : fintype G)
  (hG2 : 2 ∣ fintype.card G) :
  ∃ (a : G), a ≠ 1 ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] [finite G]
  (x : G) : ∃ (n : ℕ), 0 < n ∧ x ^ n = 1 :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] {a b : G}
  (h : (a * b) ^ 1 = a ^ 1 * b ^ 1 ∧ (a * b) ^ 2 = a ^ 2 * b ^ 2 ∧
    (a * b) ^ 3 = a ^ 3 * b ^ 3) :
  ∀ (x y : G), x * y = y * x :=
sorry

theorem exercise_2_2_6c {n : ℕ} {G : Type*} [group G]
  (h : (nat.card G).coprime n) :
  ⇑(pow_coprime h) 1 = 1 :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G]
  (hG : ∀ (H : subgroup G), H = ⊥ ∨ H = ⊤) [nontrivial G] [finite G] :
  ∃ (p : ℕ), nat.prime p ∧ is_p_group p G :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G] {H : subgroup G}
  (hH : ∀ (H : subgroup G), H.normal) {a b : G} (h : a * b ∈ H) :
  b * a ∈ H :=
sorry

theorem exercise_2_5_31  {G : Type*} [group G] (p : ℕ) (n m : ℕ) (hG : G.card = p ^ n * m)
  (hG' : nat.prime p) (hG'' : p ∣ m) (H : subgroup G)
  (hH : H.card = p ^ n) : H.characteristic :=
sorry

theorem exercise_2_5_43 (G : Type*) [group G] [fintype G]
  (hG : fintype.card G = 9) : comm_group G :=
sorry

theorem exercise_2_5_52  {G : Type*} [fintype G] [group G] (φ : G →* G) (hφ : function.bijective φ)
  (h : ∃ (x : G), (3 : ℕ) / 4 * fintype.card G ≤
    (finset.univ.filter (λ (y : G), φ y = y⁻¹)).card) :
  ∀ (y : G), φ y = y⁻¹ :=
sorry

theorem exercise_2_7_7 {G : Type*} [group G] {N : Type*} [group N] {H : subgroup G}
  (h : H.normal) (f : G →* N) (hf : function.surjective ⇑f) :
  (subgroup.map f H).normal :=
sorry

theorem exercise_2_8_15 {p : ℕ} {G : Type*} [group G]
  (hG : is_p_group p G) [hp : fact (nat.prime p)] [fintype G] :
  nontrivial G ↔ ∃ (n : ℕ) (H : n > 0), fintype.card G = p ^ n :=
sorry

theorem exercise_2_10_1 {G : Type*} [group G]
  (A B' B : subgroup G) (hB : B' ≤ B) [hN : (B'.subgroup_of B).normal] :
  ((A ⊓ B').subgroup_of (A ⊓ B)).normal :=
sorry

theorem exercise_2_11_7 {G : Type u} [group G] {p : ℕ}
  [fact (nat.prime p)] [finite (sylow p G)] (P : sylow p G) (h : ↑P.normal) :
  ↑P.characteristic :=
sorry

theorem exercise_3_2_21 {α : Type*} {σ τ : equiv.perm α}
  (hστ : σ.disjoint τ) :
  σ * τ = 1 ↔ σ = 1 ∧ τ = 1 :=
sorry

theorem exercise_4_1_34 (T : Type*) [group T]
  (hT : ∀ (A : T), ∃ (a b c d : ℤ), A = ⟨⟨a, b⟩, ⟨c, d⟩⟩ ∧ a * d ≠ b * c) :
  is_isomorphic T (alternating_group 3) :=
sorry

theorem exercise_4_2_6 {R : Type*} [comm_ring R] (a : R) (h : a ^ 2 = 0) :
  ∀ (x : R), a * x + x * a = x * a + a * x :=
sorry

theorem exercise_4_3_1 {R : Type u} [comm_ring R] {a : R}
  {I : ideal R} :
  ⇑(ideal.quotient.mk I) a = 0 ↔ a ∈ I :=
sorry

theorem exercise_4_4_9 (p : ℕ) [fact (nat.prime p)] :
  fintype.card (zmod p)ˣ = (p - 1) / 2 :=
sorry

theorem exercise_4_5_23  {F : Type*} [field F] (p : polynomial F) (q : polynomial F)
  (h : p.degree = 1 ∧ q.degree = 3 ∧ p.degree = 3) :
  is_domain F :=
sorry

theorem exercise_4_6_2 {F : Type*} [field F] {P : polynomial F}
  (h : P.roots = ∅) : irreducible P :=
sorry

theorem exercise_5_1_8 (R : Type u) [comm_semiring R] {p : ℕ}
  [fact (nat.prime p)] [char_p R p] {n : ℕ} (x y : R) :
  (x + y) ^ p ^ n = x ^ p ^ n + y ^ p ^ n :=
sorry

theorem exercise_5_3_7 {F K : Type*} [field F] [field K]
  [algebra F K] (h : normal F K) (x : K) (hx : is_algebraic F (x ^ 2)) :
  is_algebraic F x :=
sorry

theorem exercise_5_4_3  {R : Type u} {a b c d e : R} [semiring R] (ha : a ≠ 0) :
  (⇑polynomial.C b * polynomial.X ^ 2 + ⇑polynomial.C c * polynomial.X + ⇑polynomial.C d).degree < (⇑polynomial.C a * polynomial.X ^ 5).degree :=
sorry

theorem exercise_5_6_14 {R : Type*} [comm_ring R]
  [is_domain R] [decidable_rel has_dvd.dvd] {p : R} (hp : prime p) {x y : R}
  (hxy : p ∣ x - y) (hx : ¬p ∣ x) {n : ℕ} (hn : ¬p ∣ ↑n) :
  multiplicity p (x ^ n - y ^ n) = multiplicity p (x - y) :=
sorry