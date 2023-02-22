import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 {G : Type*} [group G] 
  [fintype G] (hG : 2 ∣ fintype.card G) :
  ∃ (a : G), a ≠ 1 ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] [fintype G] 
  (a : G) : ∃ (n : ℕ), a ^ n = 1 :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] 
  (a b : G) (i j k : ℕ) (h : (a * b) ^ i = a ^ i * b ^ i) 
  (h' : (a * b) ^ j = a ^ j * b ^ j) (h'' : (a * b) ^ k = a ^ k * b ^ k) :
  comm_group G :=
sorry

theorem exercise_2_2_6c {G : Type*} [group G]
  (n : ℕ) (hn : n > 1) (h : ∀ a b : G, (a * b) ^ n = a ^ n * b ^ n) :
  ∀ a b : G, (a * b * a⁻¹ * b⁻¹) ^ (n * (n - 1)) = 1 :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G] 
  (hG : ∀ (H : subgroup G), H = ⊥ ∨ H = ⊤) :
  ∃ (p : ℕ) (hp : nat.prime p), is_cyclic G p :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G]
  (hG : ∀ (H : subgroup G), H ≤ normalizer G H) (a b : G) :
  ∃ (j : ℤ), b * a = a ^ j * b :=
sorry

theorem exercise_2_5_31 {G : Type*} [group G] [fintype G]
  {p n m : ℕ} [hp : fact (nat.prime p)] (hG : card G = p ^ n * m)
  (H : subgroup G) (hH : card H = p ^ n) :
  char_subgroup G H :=
sorry

theorem exercise_2_5_43 {G : Type*} [group G] (hG : fintype.card G = 9) :
  abelian_group G :=
sorry

theorem exercise_2_5_52 {G : Type*} [group G]
  [fintype G] (φ : G →* G) (hφ : ∀ x : G, φ x = x⁻¹)
  (h : (3 : ℕ) / 4 < fintype.card G) :
  ∀ y : G, φ y = y⁻¹ :=
sorry

theorem exercise_2_7_7 {G G' : Type*} [group G] [group G']
  (f : G →* G') (hf : function.surjective f) (N : subgroup G)
  (hN : N.normal) :
  (f '' N).normal :=
sorry

theorem exercise_2_8_15 {p q : ℕ} (hp : nat.prime p)
  (hq : nat.prime q) (hqp : q ∣ p - 1) (hqp' : q < p) :
  ∀ (G₁ G₂ : Type*) [group G₁] [group G₂] [fintype G₁] [fintype G₂]
  (hG₁ : nonabelian_group G₁) (hG₂ : nonabelian_group G₂)
  (hG₁_card : fintype.card G₁ = p * q) (hG₂_card : fintype.card G₂ = p * q),
  G₁ ≃* G₂ :=
sorry

theorem exercise_2_10_1 {G : Type*} 
  [group G] {A : subgroup G} (hA : A.normal) {b : G} (hb : b.order = nat.prime.prime) 
  (hbA : b ∉ A) :
  A ∩ (b) = (1) :=
sorry

theorem exercise_2_11_7 {G : Type*} [group G] 
  {p : ℕ} [hp : fact (nat.prime p)] {P : subgroup G} (hP : is_p_group p P) 
  (hP_normal : P ≤ G.normalizer P) (hP_sylow : is_sylow p G P) 
  (φ : G →* G) (hφ : is_group_hom φ) : φ P = P :=
sorry

theorem exercise_3_2_21 {α : Type*} [fintype α]
  (σ τ : perm α) (hστ : σ * τ = 1) (hdisj : ∀ a : α, σ a ≠ τ a) :
  σ = 1 ∧ τ = 1 :=
sorry

theorem exercise_4_1_34 (T : Type*) [group T] 
  [fintype T] [decidable_eq T] (hT : ∀ (A : T), A ≠ 1) :
  T ≃ symmetric_group 3 :=
sorry

theorem exercise_4_2_6 {R : Type*} [ring R] (a x : R) (h : a ^ 2 = 0) :
  a * x + x * a = a * (x + x) :=
sorry

theorem exercise_4_3_1 {R : Type*} [comm_ring R] (a : R) :
  ideal R (annihilator a) :=
sorry

theorem exercise_4_4_9 {p : ℕ} 
  (hp : nat.prime p) :
  card {x : units ℤ | x.val ^ 2 = 1 [ZMOD p]} ≡ (p - 1) / 2 [MOD 2] :=
sorry

theorem exercise_4_5_23is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_of_is_field_:=
sorry

theorem exercise_4_6_2 (x : polynomial ℚ) :
  irreducible (x^3 + 3*x + 2) :=
sorry

theorem exercise_5_1_8 {F : Type*} [field F] 
  (p : ℕ) (hp : fact (nat.prime p)) (hF : char_p F ≠ 0) (a b : F) (m : ℕ) :
  (a + b) ^ m = a ^ m + b ^ m :=
sorry

theorem exercise_5_3_7 {K : Type*} [field K]
  {F : Type*} [field F] (a : K) (h : is_algebraic F (a ^ 2)) :
  is_algebraic F a :=
sorry

theorem exercise_5_4_3 {a : ℂ} (ha : polynomial.eval ℂ (X^5 + √2*X^3 + √5*X^2 + √7*X + √11) a = 0) :
  algebraic_closure.degree ℚ a ≤ 80 :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] 
  (p : ℕ) (hp : fact (nat.prime p)) (n : ℕ) (h : char_p F = p) :
  ∀ (x y : F), x ^ p ^ n - x = 0 → y ^ p ^ n - y = 0 → x ≠ y :=
sorry