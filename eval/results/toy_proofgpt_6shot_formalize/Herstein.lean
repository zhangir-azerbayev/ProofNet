import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 [fintype G] [fintype G]
  (hG : fintype.card G = 2 ^ (fintype.card G)) :
  ∃ a : G, a ≠ e ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] [fintype G]
  (a : G) (n : ℕ) (h : ∀ x, x ^ n = a) : ∃ (k : ℕ), k > 0 ∧ k ≤ n ∧ a ^ k = e :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] 
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : card H = p ^ n) :
  abelian G :=
sorry