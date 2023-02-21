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

theorem exercise_2_5_31 
  {G : Type*} [group G] [fintype G] {p n m : ℕ} [hp : fact (nat.prime p)] 
  (hG : card G = p ^ n * m) (H : subgroup G) (hH : card H = p ^ n) 
  (hG_comm : comm_group G) :
  characteristic G H :=
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