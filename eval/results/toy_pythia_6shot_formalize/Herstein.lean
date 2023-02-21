import .common
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