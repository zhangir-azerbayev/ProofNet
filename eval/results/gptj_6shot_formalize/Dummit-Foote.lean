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


theorem exercise_1_1_15 {G : Type*} [group G] :
  (a_1a_2\dots a_n)^{-1} = a_n^{-1}a_{n-1}^{-1}\dots a_1^{-1}
  → (a_1a_2\dots a_n) = a_n a_{n-1}\dots a_1
  :=
sorry

theorem exercise_1_1_16 {G : Type*} [group G] (x : G) :
  x^2 = 1 ↔ (x = 1 ∨ x = 2) :=
sorry

theorem exercise_1_1_17 {G : Type*} [group G] (x : G)
  (hx_n : x ^ n = 1) : x ^ {-1} = x ^ {n-1} :=
sorry

theorem exercise_1_1_18 {G : Type*} [group G] 
  (h : commutative G) (hxy : xy = yx) (hyx : yx = xy) :
  xy = yx ↔ y^{-1}xy = x ↔ x^{-1}y^{-1}xy = 1 :=
sorry

theorem exercise_1_1_20 {G : Type*} [group G] (x : G)
  (hx : order x = order x.inverse) : order x = order x.inverse :=
sorry

theorem exercise_1_1_22a {G : Type*} [group G] (x : G) (hx : x ≠ 1) :
  |x| = |x^{-1}| :=
sorry

theorem exercise_1_1_22b {G : Type*} [group G] : comm_group G :=
sorry

theorem exercise_1_1_25 {G : Type*} [group G] :
  ∀ x : G, x^{2} = 1 → G.abelian :=
sorry

theorem exercise_1_1_29 {A B : Type*} [group A] [group B]
  [abelian A] [abelian B] : abelian (A × B) ↔ abelian A ∧ abelian B :=
sorry

theorem exercise_1_1_2a {a b : ℤ} : a ⋅ b ≠ b ⋅ a :=
sorry

theorem exercise_1_1_3 {n m : ℕ} :
  (ℤ/nℤ) + (ℤ/mℤ) = (ℤ/nℤ) + (ℤ/mℤ) :=
sorry

theorem exercise_1_1_34 (G : Type*) [group G] (x : G)
  (hx_inf : ∀ n : ℕ, x ^ n ≠ 1) :
  ∀ m n : ℤ, m ≠ n → x ^ m ≠ x ^ n :=
sorry

theorem exercise_1_1_4 {n m : ℕ} : _ := 
sorry

theorem exercise_1_1_5 {n : ℕ} :
  not (group.is_group (ℤ/nℤ)) :=
sorry

theorem exercise_1_3_8 {n : ℕ} :
  infinite_group (set.finite n) :=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B] :
  A × B ≅ B × A :=
sorry

theorem exercise_1_6_17 {G : Type*} [group G] :
  homomorphism G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →* G →:=
sorry

theorem exercise_1_6_23 {G : Type*} [group G]
  [automorphism_group G] (h : ∀ g, g = 1 ↔ g = 1) :
  abelian G :=
sorry

theorem exercise_1_6_4 {G : Type*} [group G] :
  not (is_isomorphism G.real_group G.complex_group) :=
sorry

theorem exercise_2_1_13 {H : Type*} [group H]
  [rational_group H] (h : H.is_subgroup H) :
  H = 0 ∨ H = ℚ :=
sorry

theorem exercise_3_1_22a {G H K : Type*} [group G] [group H] [group K]
  [normal_subgroup H] [normal_subgroup K] (h : H ∩ K = H) :
  normal_subgroup (H ∩ K) :=
sorry

theorem exercise_3_1_22b  {G : Type*} [group G] [nonempty_collection G] [subgroup G]
  (h : ∃ (H : subgroup G), ∃ (N : nonempty_collection G), H ⊆ N) :
  subgroup (intersection N) :=
sorry

theorem exercise_3_1_3a {A B : Type*} [group A] [group B]
  [abelian A] [abelian B] (h : B ⊆ A) : abelian (A / B) :=
sorry

theorem exercise_3_2_16lagrange_theorem_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_p_group_times_mod_:=
sorry

theorem exercise_3_2_21a {G : Type*} [group G] :
  no_subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite_index G.subgroup_of_finite:=
sorry

theorem exercise_3_2_8 {G H K : Type*} 
  [group G] [group H] [group K] [fintype G] [fintype H] [fintype K]
  (hG : finite_group G) (hH : finite_group H) (hK : finite_group K)
  (h : H ∩ K = 1) : hG.comm_group :=
sorry

theorem exercise_3_4_1 {G : Type*} [group G] :
  abelian_simple_group G → cyclic G :=
sorry

theorem exercise_3_4_4 {G : Type*} [group G] 
  [fintype G] (h : finite_group G) (hdiv : divisor.divides h.order) :
  ∃ (n : ℕ), n.divides h.order :=
sorry

theorem exercise_3_4_5a {G : Type*} [group G] 
  [solvable_group G] (h : subgroup G) : solvable_group G :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] [solvable G]
  (h : solvable.quotient G) : solvable G :=
sorry

theorem exercise_4_3_26 {G : Type*} [group G] 
  [transitive_permutation_group G] (h : finite_set G) (hA : ∀ a, a ∈ h) :
  ∃ a, a ∈ h ∧ a ≠ h.trans :=
sorry

theorem exercise_4_4_6a {G : Type*} [group G] 
  [subgroup G] (h : subgroup.characteristic G) :
  normal G :=
sorry

theorem exercise_4_5_13 {G : Type*} [group G]
  [p : nat.prime (order G)] [p_divides_order G] [p_is_prime p]
  (h : Sylow_p_subgroup G) :
  normal_Sylow_p_subgroup G :=
sorry

theorem exercise_4_5_14 {G : Type*} [group G] :
  has_normal_sylow_p G ∧ order G = 312 →
  ∃ p : nat, p divides order G :=
sorry

theorem exercise_4_5_1a {G : Type*} [group G] [p : nat]
  [p_sylow_subgroup G p] (H : subgroup G) :
  p_sylow_subgroup H :=
sorry