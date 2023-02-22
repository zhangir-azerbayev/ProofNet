import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 :
  odd_order_group G → exists a : G, a ≠ e ∧ a = a^-1 ∧ a ≠ e :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] (a : G) :
  ∀ n : ℕ, a ^ n = e ↔ a = e :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] 
  [group G] (a b c : G) (h : a ^ b ^ c = a ^ c ^ b) :
  a ^ b ^ c = a ^ c ^ b :=
sorry

theorem exercise_2_2_6c  (G : Type*) [group G] (a : G) (b : G) (n : ℕ) (h : a ^ n ≠ b ^ n) :
  a ^ n ≠ b ^ n :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G] 
  [is_cyclic G] (p : ℕ) (h : ∃ (g : G), g.ker ≤ p) :
  is_cyclic G :=
sorry

theorem exercise_2_5_23  {G H : Type*} [group G] [group H] [group K] [group L] [group M]
  (a : G) (b : H) (c : K) (d : L) (e : M) (f : H ⊗ K ⊗ L ⊗ M)
  (g : H ⊗ K ⊗ L ⊗ M) (h : normal_subgroup G) (i : normal_subgroup H)
  (j : normal_subgroup K) (k : normal_subgroup L) (l : normal_subgroup M)
  (m : normal_subgroup (H ⊗ K ⊗ L ⊗ M)) :
  normal_subgroup (H ⊗ K ⊗ L ⊗ M) :=
sorry

theorem exercise_2_5_31 {G : Type*} [group G] 
  [fintype G] {p n m : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : card H = p ^ n) :
  H.subgroup.characteristic G :=
sorry

theorem exercise_2_5_43 [group G] :
  ∃ (x : G), x^9 = 1 :=
sorry

theorem exercise_2_5_52 {G : Type*} [group G] 
  [finite_group G] (h : ∃ x : G, ∃ y : G, x ^ 3 = y ^ 3 ∧ x ^ 4 = y ^ 4) :
  ∀ x y : G, x ^ 4 = y ^ 4 → x = y :=
sorry

theorem exercise_2_7_7 {G : Type*} [group G] [group G'] 
  [is_normal G] [is_normal G'] (f : G →* G') (hf : f.ker ≤ normal G) :
  hom_group G :=
sorry

theorem exercise_2_8_15 {p q : ℕ}
  [prime p] [prime q] [is_prime p] [is_prime q] [is_prime (p - 1)]
  (h : p - 1 = q * q) : p divides q → p divides (p - 1) :=
sorry

theorem exercise_2_10_1  {G : Type*} [group G] (b : G) (p : ℕ) (A : normal G) (e : G) :
  (A ∩ (b)) = (e) :=
sorry

theorem exercise_2_11_7 {G : Type*} [group G] 
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {P : subgroup G} 
  (hP : p_Sylow P) : p_Sylow (normalizer P) = P :=
sorry

theorem exercise_3_2_21 {G : Type*} [group G] 
  [group G] (g : G → G) (h : ∀ x, g.inv x = g.inv x) (h : ∀ x, g.inv x = e) :
  ∀ x, g.inv x = e :=
sorry

theorem exercise_4_1_34 {T : Type*} [group T] [field T]
  [add_comm_group T] [module T T] [finite_dimensional T T]
  (h : finite_dimensional.finrank T + 1 < 3) :
  is_S3 T :=
sorry

theorem exercise_4_2_6comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_comm_of_:=
sorry

theorem exercise_4_3_1 {R : Type*} [ring R] [ring R]
  (a : R) (h : ∀ x : R, xa = 0 → x = 0) :
  ideal_of_zero a :=
sorry

theorem exercise_4_4_9 {p : ℕ} [hp : fact (nat.prime p)] :
  ∀ n : ℕ, (p - 1) mod (p - 1) = (p - 1) mod (p - 1) + n ∧
  ∀ n : ℕ, (p - 1) mod (p - 1) ≠ (p - 1) mod (p - 1) + n :=
sorry

theorem exercise_4_5_23 :=
sorry

theorem exercise_4_6_2 {Q : Type*} [field Q] [ring Q] [polynomial Q]
  (f : Q → Q) (g : Q → Q) (h : polynomial.irreducible f) :
  irreducible f :=
sorry

theorem exercise_5_1_8 {F : Type*} [field F] [field F]
  [add_comm_group F] [module F F] [finite_degree F] (a : F) (b : F)
  (h : a + b = a ^ p + b ^ p) : a ^ p = b ^ p :=
sorry

theorem exercise_5_3_7 {K F : Type*} [field K] [field F]
  [algebraic_field K F] (a : K) (b : F) (h : a^2 ∈ F) :
  a ∈ F :=
sorry

theorem exercise_5_4_3 {a : ℤ} [field ℚ] (p : a^5 + a^3 + a^2 + a + 1 = 0) :
  degree (p.quotient ℚ) ≤ 80 :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] [char F p] [F_p]
  (m : ℕ) (n : ℕ) (x : F) (hx : x ^ m ≠ x ^ m) :
  ∀ i j : ℤ, i ≠ j → x ^ i ≠ x ^ j :=
sorry