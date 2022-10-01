import algebra.group.basic
import group_theory.order_of_element
import data.nat.basic
import data.real.basic 
import data.complex.basic
import data.fintype.basic
import data.zmod.basic 
import data.countable.basic
import data.set.countable
import data.polynomial.basic
import group_theory.subgroup.basic
import group_theory.quotient_group
import group_theory.index 
import group_theory.specific_groups.cyclic
import group_theory.solvable 
import group_theory.free_group
import group_theory.presented_group
import group_theory.group_action.conj_act
import group_theory.sylow
import number_theory.zsqrtd.gaussian_int
import ring_theory.ideal.operations
import algebra.char_p.basic
import algebra.quaternion
import linear_algebra.general_linear_group
import field_theory.finite.galois_field

open set function nat fintype real 
open subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory


theorem exercise_2_10_1 {G : Type*} 
  [group G] {A : subgroup G} (hA : A.normal) {b : G} (hb : b.order = nat.prime.prime) 
  (hbA : b ∉ A) :
  A ∩ (b) = (1) :=
sorry

theorem exercise_2_11_22 {G : Type*} [group G] [fintype G]
  {p n : ℕ} [hp : fact (nat.prime p)] (hG : card G = p ^ n)
  {H : subgroup G} (hH : card H = p ^ (n - 1)) :
  H ≤ normalizer G :=
sorry

theorem exercise_2_11_6 {G : Type*} [group G] {p : ℕ} [hp : fact (nat.prime p)]
  {P : sylow p G} (hP : P.normal) :
  ∀ (Q : sylow p G), P = Q :=
sorry

theorem exercise_2_11_7 {G : Type*} [group G] 
  {p : ℕ} [hp : fact (nat.prime p)] {P : subgroup G} (hP : is_p_group p P) 
  (hP_normal : P ≤ G.normalizer P) (hP_sylow : is_sylow p G P) 
  (φ : G →* G) (hφ : is_group_hom φ) : φ P = P :=
sorry

theorem exercise_2_1_18 {G : Type*} [group G] 
  [fintype G] (hG : 2 ∣ fintype.card G) :
  ∃ (a : G), a ≠ 1 ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_21 {G : Type*} [group G] (hG : fintype G) 
  (hG_card : fintype.card G = 5) :
  abelian_group G :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] [fintype G] 
  (a : G) : ∃ (n : ℕ), a ^ n = 1 :=
sorry

theorem exercise_2_1_27 {G : Type*} [group G] [fintype G] :
  ∃ (m : ℕ), ∀ (a : G), a ^ m = 1 :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] 
  (a b : G) (i j k : ℕ) (h : (a * b) ^ i = a ^ i * b ^ i) 
  (h' : (a * b) ^ j = a ^ j * b ^ j) (h'' : (a * b) ^ k = a ^ k * b ^ k) :
  comm_group G :=
sorry

theorem exercise_2_2_5 {G : Type*} 
  [group G] (h : ∀ (a b : G), (a * b) ^ 3 = a ^ 3 * b ^ 3 ∧ (a * b) ^ 5 = a ^ 5 * b ^ 5) :
  abelian_group G :=
sorry

theorem exercise_2_2_6c {G : Type*} [group G]
  (n : ℕ) (hn : n > 1) (h : ∀ a b : G, (a * b) ^ n = a ^ n * b ^ n) :
  ∀ a b : G, (a * b * a⁻¹ * b⁻¹) ^ (n * (n - 1)) = 1 :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G] 
  (hG : ∀ (H : subgroup G), H = ⊥ ∨ H = ⊤) :
  ∃ (p : ℕ) (hp : nat.prime p), is_cyclic G p :=
sorry

theorem exercise_2_3_17 {G : Type*} [group G] 
  (a x : G) :
  commutator x⁻¹ * a * x = x⁻¹ * commutator a * x :=
sorry

theorem exercise_2_3_19 {G : Type*} [group G] {M : subgroup G}
  (hM : ∀ (x : G), x⁻¹ * M * x ⊆ M) :
  ∀ (x : G), x⁻¹ * M * x = M :=
sorry

theorem exercise_2_4_36 {n : ℕ} (h : n > 1) (hn : n.prime_pow) :
  ∀ (m : ℕ), m ∣ nat.phi (n ^ m - 1) :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G]
  (hG : ∀ (H : subgroup G), H ≤ normalizer G H) (a b : G) :
  ∃ (j : ℤ), b * a = a ^ j * b :=
sorry

theorem exercise_2_5_30 {G : Type*} [group G] 
  [fintype G] {p m : ℕ} [hp : fact (nat.prime p)] (hG : card G = p * m) 
  (H : subgroup G) (hH : card H = p) (hHn : H ≤ normalizer G H) :
  char_subgroup G H :=
sorry

theorem exercise_2_5_31 {G : Type*} [group G] [fintype G]
  {p n m : ℕ} [hp : fact (nat.prime p)] (hG : card G = p ^ n * m)
  (H : subgroup G) (hH : card H = p ^ n) :
  char_subgroup G H :=
sorry

theorem exercise_2_5_37 {G : Type*} [group G]
  (hG : ¬ abelian_group G) (hG₁ : G.card = 6) :
  G ≃ symmetric_group 3 :=
sorry

theorem exercise_2_5_43 {G : Type*} [group G] (hG : fintype.card G = 9) :
  abelian_group G :=
sorry

theorem exercise_2_5_44 {G : Type*} 
  [group G] [fintype G] {p : ℕ} [hp : fact (nat.prime p)] 
  (hG : fintype.card G = p ^ 2) :
  ∃ (H : subgroup G), H.normal ∧ fintype.card H = p :=
sorry

theorem exercise_2_5_52 {G : Type*} [group G]
  [fintype G] (φ : G →* G) (hφ : ∀ x : G, φ x = x⁻¹)
  (h : (3 : ℕ) / 4 < fintype.card G) :
  ∀ y : G, φ y = y⁻¹ :=
sorry

theorem exercise_2_6_15 {G : Type*} [group G]
  [abelian G] {m n : ℕ} (hm : ∃ (x : G), x ^ m = 1) (hn : ∃ (x : G), x ^ n = 1)
  (hmn : nat.coprime m n) :
  ∃ (x : G), x ^ (m * n) = 1 :=
sorry

theorem exercise_2_7_7 {G G' : Type*} [group G] [group G']
  (f : G →* G') (hf : function.surjective f) (N : subgroup G)
  (hN : N.normal) :
  (f '' N).normal :=
sorry

theorem exercise_2_8_12 {G H : Type*} [group G] 
  [group H] (hG : ¬ abelian G) (hH : ¬ abelian H) (hG_21 : card G = 21) 
  (hH_21 : card H = 21) :
  G ≃ H :=
sorry

theorem exercise_2_8_15 {p q : ℕ} (hp : nat.prime p)
  (hq : nat.prime q) (h : q ∣ p - 1) (hqp : q < p) :
  ∀ (G₁ G₂ : Type*) [group G₁] [group G₂] [fintype G₁] [fintype G₂]
  (hG₁ : nonabelian_group G₁) (hG₂ : nonabelian_group G₂)
  (hG₁_card : fintype.card G₁ = p * q) (hG₂_card : fintype.card G₂ = p * q),
  G₁ ≃* G₂ :=
sorry

theorem exercise_2_9_2 {G₁ G₂ : Type*} [group G₁] 
  [group G₂] [is_cyclic G₁] [is_cyclic G₂] (h : nat.coprime (G₁.card) (G₂.card)) :
  is_cyclic (G₁ × G₂) :=
sorry

theorem exercise_3_2_21 {α : Type*} [fintype α]
  (s t : perm α) (hst : s * t = 1) (hdisj : ∀ a : α, s a ≠ t a) :
  s = 1 ∧ t = 1 :=
sorry

theorem exercise_4_1_19 :
  ∃ (x : quaternion ℚ), x ^ 2 = -1 :=
sorry

theorem exercise_4_1_34 (T : Type*) [group T] 
  [fintype T] [decidable_eq T] (hT : ∀ (A : T), A ≠ 1) :
  T ≃ symmetric_group 3 :=
sorry

theorem exercise_4_2_5 {R : Type*} [ring R] 
  (h : ∀ x : R, x ^ 3 = x) : comm_ring R :=
sorry

theorem exercise_4_2_6 {R : Type*} [ring R] (a x : R) (h : a ^ 2 = 0) :
  a * x + x * a = a * (x + x) :=
sorry

theorem exercise_4_2_9 {p : ℕ} (hp : nat.prime p)
  (h : ∑ i in finset.range (p - 1), (1 : ℚ) / (i + 1) = (a : ℚ) / b) :
  p ∣ a :=
sorry

theorem exercise_4_3_1 {R : Type*} [comm_ring R] (a : R) :
  ideal R (annihilator a) :=
sorry

theorem exercise_4_3_25 {R : Type*} [ring R] 
  [fintype R] [decidable_eq R] (I : ideal R) (hI : I ≠ (0)) :
  I = ⊤ :=
sorry

theorem exercise_4_4_9 {p : ℕ} 
  (hp : nat.prime p) :
  card {x : units ℤ | x.val ^ 2 = 1 [ZMOD p]} ≡ (p - 1) / 2 [MOD 2] :=
sorry

theorem exercise_4_5_16 {p : ℕ} [fact (nat.prime p)] 
  {n : ℕ} {q : polynomial ℤ} (hq : irreducible q) (hqn : q.nat_degree = n) :
  fintype.card (fintype.of_equiv (polynomial.quotient_ring_equiv_of_irreducible q)) = p ^ n :=
sorry

theorem exercise_4_5_23  {F : Type*} [field F] {p q : polynomial F} (hp : irreducible p) (hq : irreducible q)
  (h : p.eval₂ (λ x y, x + y) q = 0) :
  is_field_hom (polynomial.eval₂ F (λ x y, x + y) p q) :=
sorry

theorem exercise_4_5_25 {p : ℕ} (hp : nat.prime p) :
  irreducible (polynomial.C 1 + polynomial.sum (λ i, polynomial.X ^ i) (p - 1)) :=
sorry

theorem exercise_4_6_2 (x : polynomial ℚ) :
  irreducible (x^3 + 3*x + 2) :=
sorry

theorem exercise_4_6_3 (f : polynomial ℚ)
  (hf : irreducible f) (hf_deg : f.nat_degree = 7) :
  ∃ (a : ℤ), irreducible (f + C a) :=
sorry

theorem exercise_5_1_8 {F : Type*} [field F] 
  (p : ℕ) (hp : fact (nat.prime p)) (hF : char_p F = p) (n : ℕ) 
  (a b : F) :
  (a + b) ^ (p ^ n) = a ^ (p ^ n) + b ^ (p ^ n) :=
sorry

theorem exercise_5_2_20 {V : Type*} [field F] [add_comm_group V]
  [vector_space F V] [fintype V] (h : ∃ (s : finset (submodule F V)), 
  ∀ (U : submodule F V), U ∈ s → U ≠ ⊥ ∧ U ≠ ⊤ ∧ ∀ (U' : submodule F V), U' ∈ s → U ≠ U') :
  false :=
sorry

theorem exercise_5_3_10 : algebraic ℚ (cos (1 : ℝ)) :=
sorry

theorem exercise_5_3_7 {K : Type*} [field K]
  {F : Type*} [field F] (hF : F ⊆ K) (a : K) (h : is_algebraic F (a ^ 2)) :
  is_algebraic F a :=
sorry

theorem exercise_5_4_3 {a : ℂ} (ha : polynomial.eval ℂ (X^5 + √2*X^3 + √5*X^2 + √7*X + √11) a = 0) :
  algebraic.degree ℚ a ≤ 80 :=
sorry

theorem exercise_5_5_2  (x : polynomial ℚ) (hx : x.degree = 3) (hx₁ : x.coeff 1 = 1) 
  (hx₃ : x.coeff 3 = -3) : irreducible x :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] 
  (p : ℕ) (hp : fact (nat.prime p)) (n : ℕ) (h : char_p F = p) :
  ∀ (x : F), x ^ (p ^ n) - x ≠ 0 → 
  ∀ (y : F), y ^ (p ^ n) - y ≠ 0 → x ≠ y :=
sorry