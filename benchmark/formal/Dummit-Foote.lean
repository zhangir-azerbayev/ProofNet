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

theorem exercise_1_1_2a : ∃ a b : ℤ, a - b ≠ b - a :=
begin
  use [0, 1]
end

theorem exercise_1_1_3 (n : ℕ) : 
  ∀ (a b c : ℕ), (a+b)+c ≡ a+(b+c) [MOD n] :=
begin 
  intros a b c, 
  ring_nf
end

theorem exercise_1_1_4 (n : ℕ) : 
  ∀ (a b c : ℕ), (a * b) * c ≡ a * (b * c) [MOD n] :=
begin 
  intros a b c, 
  ring_nf, 
end

theorem exercise_1_1_5 (n : ℕ) (hn : 1 < n) : 
  is_empty (group (zmod n)) :=
sorry

theorem exercise_1_1_15 {G : Type*} [group G] (as : list G) :
  as.prod⁻¹ = (as.reverse.map (λ x, x⁻¹)).prod :=
sorry

theorem exercise_1_1_16 {G : Type*} [group G] 
  (x : G) (hx : x ^ 2 = 1) :
  order_of x = 1 ∨ order_of x = 2 :=
sorry

theorem exercise_1_1_17 {G : Type*} [group G] {x : G} {n : ℕ}
  (hxn: order_of x = n) :
  x⁻¹ = x ^ (n-1) :=
sorry

theorem exercise_1_1_18 {G : Type*} [group G] {x y : G} : 
    x * y = y * x ↔ y⁻¹ * x * y = x ∧ 
    x * y = y * x ↔ x⁻¹ * y⁻¹ * x * y = 1 :=
sorry 

theorem exercise_1_1_20 {G : Type*} [group G] {x : G} :
  order_of x = order_of x⁻¹ :=
sorry

theorem exercise_1_1_22a {G : Type*} [group G] (x g : G) :
    order_of x = order_of (g⁻¹ * x * g) :=
sorry

theorem exercise_1_1_22b {G: Type*} [group G] (a b : G) : 
  order_of (a * b) = order_of (b * a) :=
sorry

theorem exercise_1_1_25 {G : Type*} [group G] 
  (h : ∀ x : G, x ^ 2 = 1) : ∀ a b : G, a*b = b*a :=
sorry

theorem exercise_1_1_29 {A B : Type*} [group A] [group B] :
  ∀ x y : A × B, x*y = y*x ↔ (∀ x y : A, x*y = y*x) ∧ 
  (∀ x y : B, x*y = y*x) :=
sorry

theorem exercise_1_1_34 {G : Type*} [group G] {x : G} 
  (hx_inf : order_of x = 0) (n m : ℤ) :
  x ^ n ≠ x ^ m :=
sorry

theorem exercise_1_3_8 : infinite (equiv.perm ℕ) :=
sorry

theorem exercise_1_6_4 : 
  is_empty (multiplicative ℝ ≃* multiplicative ℂ) :=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B] : 
  A × B ≃* B × A :=
sorry 

theorem exercise_1_6_17 {G : Type*} [group G] (f : G → G) 
  (hf : f = λ g, g⁻¹) :
  ∀ x y : G, f x * f y = f (x*y) ↔ ∀ x y : G, x*y = y*x :=   
sorry

theorem exercise_1_6_23 {G : Type*} 
  [group G] (σ : mul_aut G) (hs : ∀ g : G, σ g = 1 → g = 1) 
  (hs2 : ∀ g : G, σ (σ g) = g) :
  ∀ x y : G, x*y = y*x :=
sorry

theorem exercise_2_1_13 (H : add_subgroup ℚ) {x : ℚ} 
  (hH : x ∈ H → (1 / x) ∈ H):
  H = ⊥ ∨ H = ⊤ :=
sorry

theorem exercise_3_1_3a {A : Type*} [comm_group A] (B : subgroup A) :
    ∀ a b : A ⧸ B, a*b = b*a   :=
sorry

theorem exercise_3_1_22a (G : Type*) [group G] (H K : subgroup G) 
    [subgroup.normal H] [subgroup.normal K] :
    subgroup.normal (H ⊓ K) :=
sorry

theorem exercise_3_1_22b {G : Type*} [group G] (I : Type*)
    (H : I → subgroup G) (hH : ∀ i : I, subgroup.normal (H i)) : 
    subgroup.normal (⨅ (i : I), H i):=
sorry

theorem exercise_3_2_8 {G : Type*} [group G] (H K : subgroup G)
    [fintype H] [fintype K] 
    (hHK : nat.coprime (fintype.card H) (fintype.card K)) : 
    H ⊓ K = ⊥ :=
sorry

theorem exercise_3_2_16 (p : ℕ) (hp : nat.prime p) (a : ℕ) :
  nat.coprime a p → a ^ p ≡ a [MOD p] :=
sorry

theorem exercise_3_2_21a (G : Type*) [group G] 
    [fintype G] [decidable_eq G] (H : subgroup G) (hH : H ≠ ⊥) :
    H = ⊤ :=
sorry

theorem exercise_3_4_1 (G : Type*) [comm_group G] [is_simple_group G] :
    is_cyclic G ∧ ∃ G_fin : fintype G, nat.prime (@card G G_fin) :=
sorry

theorem exercise_3_4_4 {G : Type*} [comm_group G] [fintype G] {n : ℕ}
    (hn : n ∣ (fintype.card G)) :
    ∃ (H : subgroup G) (H_fin : fintype H), @card H H_fin = n  :=
sorry

theorem exercise_3_4_5a {G : Type*} [group G] 
    (H : subgroup G) [is_solvable G] : is_solvable H :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] [is_solvable G] 
    (H : subgroup G) [subgroup.normal H] : 
    is_solvable (G ⧸ H) :=
sorry

theorem exercise_4_3_26 {α : Type*} [fintype α] (ha : fintype.card α > 1)
    (h_tran : ∀ a b: α, ∃ σ : equiv.perm α, σ a = b) : 
    ∃ σ : equiv.perm α, ∀ a : α, σ a ≠ a := 
sorry

theorem exercise_4_4_6a {G : Type*} [group G] (H : subgroup G)
    [subgroup.characteristic H] : subgroup.normal H  :=
sorry

theorem exercise_4_5_1a {p : ℕ} {G : Type*} [group G] 
    {P : subgroup G} (hP : is_p_group p P) (H : subgroup G) 
    (hH : P ≤ H) : is_p_group p H :=
sorry


theorem exercise_4_5_13 {G : Type*} [group G] [fintype G]
    (hG : card G = 56) :
    ∃ (p : ℕ) (P : sylow p G), P.normal :=
sorry

theorem exercise_4_5_14 {G : Type*} [group G] [fintype G]
    (hG : card G = 312) :
    ∃ (p : ℕ) (P : sylow p G), P.normal :=
sorry