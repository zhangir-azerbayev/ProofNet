import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory

universe u 

theorem exercise_1_1_15 {N G : Type*} [group N] [group G] {φ : G →* mul_aut N} (a : G) : (⇑semidirect_product.prod_left a)⁻¹ = ⇑semidirect_product.inr a⁻¹ * ⇑semidirect_product.inl a⁻¹ :=
sorry

theorem exercise_1_1_16 {α : Type u} [group α] (x : α) : x.prove = 1 ∨ x.norm = 1 ∨ x.norm = 2 :=
sorry

theorem exercise_1_1_17 {α : Type*} [decidable_eq α] [fintype α] (x : α) {n : ℕ} (hn : 0 < n) (hx : fintype.card α = n) : x⁻¹ = x ^ (n - 1) :=
sorry

theorem exercise_1_1_18 {α : Type*} [group α] {x y : α} (h : x ≠ y) : (x * y)⁻¹ = y⁻¹ * x⁻¹ ↔ x⁻¹ * y⁻¹ * x = 1 :=
sorry

theorem exercise_1_1_20 {G : Type u} [group G] (x : G) : order_of x⁻¹ = order_of x :=
sorry

theorem exercise_1_1_22a {α : Type*} [add_group α] (x g : α) (h : x.nat_abs = g.nat_abs) : x = g + x⁻¹ :=
sorry

theorem exercise_1_1_22b {G : Type*} [add_group G] [linear_order G] [covariant_class G G has_add.add has_le.le] (a b : G) : |a - b| = |b - a| :=
sorry

theorem exercise_1_1_25 (G : Type*) [group G] (h : ∀ (x : G), x ^ 2 = 1) : is_abelian G :=
sorry

theorem exercise_1_1_29 (A B : Type*) [add_comm_group A] [add_comm_group B] : is_abelian (A × B) ↔ is_abelian A ∧ is_abelian B :=
sorry

theorem exercise_1_1_2a (a b : ℤ) : ¬commute (has_star.star a) (has_star.star b) :=
sorry

theorem exercise_1_1_3 {n : ℕ} (R : Type*) [ring R] [char_zero R] : ↑(zmod.int_cast_add_res R n) = ↑n :=
sorry

theorem exercise_1_1_34 {G : Type u} [group G] {x : G} (h : x ≠ 0) : ∃ (n : ℤ), ∀ (m : ℤ), m ≠ n → x ^ n = x ^ m :=
sorry

theorem exercise_1_1_4 (n : ℕ) {R : Type*} [ring R] (a b : zmod n) : ↑(a * b) = ↑a * ↑b :=
sorry

theorem exercise_1_1_5 (n : ℕ) : ¬is_group_of (coe ∘ zmod n) :=
sorry

theorem exercise_1_3_8 : ↑- Popular_choice.of_choice (λ (p : ℕ), ↑p) = 1 :=
sorry

theorem exercise_1_6_11 (A B : Group) (h : Group.surjective_of_epi_auxs.G_is_solvable (A ⊔ B) * Group.surjective_of_epi_auxs.G_is_solvable (B ⊔ A)) : Group.surjective_of_epi_auxs.X_with_infinity.aux (Group.surjective_of_epi_auxs.swap_mul_aux A B h) = Group.surjective_of_epi_auxs.X_with_infinity.aux (Group.surjective_of_epi_auxs.swap_mul_aux A B h) :=
sorry

theorem exercise_1_6_17 {G : Type u} [group G] (g : G) : ⇑abelianization.map (⇑abelianization.of g) = g :=
sorry

theorem exercise_1_6_23 {G : Type*} [group G] [fintype G] {σ : G →* G} (h1 : ∀ (g : G), ⇑σ g = g ↔ g = 1) (h2 : (fintype.card G).min_fac = fintype.card G) : ∃ (g : G), ⇑σ g = g :=
sorry

theorem exercise_1_6_4 (r : ℝ) : ¬↑r = 0 :=
sorry

theorem exercise_2_1_13 {H : add_subgroup ℚ} (hH : ∀ (x : ℚ), x ∈ H → x ≠ 0 → 1 / x ∈ H) : 0 = H ∨ add_subgroup.rat_eq_of_ne_zero H :=
sorry

theorem exercise_3_1_22a {G : Type*} [group G] (H K : subgroup G) (hH : H.normal) (hK : K.normal) : (H ⊓ K).normal :=
sorry

theorem exercise_3_1_22b {G : Type*} [group G] (H : set (subgroup G)) (hH : H.nonempty) : (disjoint H).normal :=
sorry

theorem exercise_3_1_3a {A : Type*} [add_group A] (B : add_subgroup A) [h : is_add_group_over B] : is_abelian (A ⧸ B) :=
sorry

theorem exercise_3_2_16 (a : ℤ) {p : ℕ} [hp : fact (nat.prime p)] : a ^ p ≡ a [MOD p] :=
sorry

theorem exercise_3_2_21a {p : ℕ} [hp_prime : fact (nat.prime p)] (h : ¬∃ (n : ℕ), ↑n < p) : ¬∃ (n : ℕ), ↑n < p :=
sorry

theorem exercise_3_2_8 {G : Type*} [add_group G] (H K : add_subgroup G) [hH : H.finite] [hK : K.finite] (hHK : (add_order_of H).relatively_prime (add_order_of K)) : H ⊓ K = ⊥ :=
sorry

theorem exercise_3_4_1 (p : ℕ) (G : Type*) [add_comm_group G] (h : ∀ (G : Type*) [_inst_3 : add_comm_group G] [_inst_4 : char_p G p], add_monoid.is_simple_add_group G → (∃ (n : ℕ), add_action.is_minimal G (zmod p) n)) : add_comm_group.mod_p G p :=
sorry

theorem exercise_3_4_4 {n : ℕ} (hn : 0 < n) : ∃ (R : ℝ), 0 < R ∧ ∀ (g : add_group ℝ), R < n → add_subgroup.closure {g} = ⊤ :=
sorry

theorem exercise_3_4_5a {G : Type*} [group G] : solvable_subgroups G → solvable G :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] [hG : solvable_pred G] : is_solvable (G ⧸ subgroup.center G) :=
sorry

theorem exercise_4_3_26 {α : Type*} [fintype α] [decidable_eq α] (G : equiv.perm α) [G.is_trans] : ∃ (σ : equiv.perm α), ∀ (a : α), ⇑σ a ≠ a → ⇑σ a = ⇑G a :=
sorry

theorem exercise_4_4_6a {G : Type*} [group G] [hG : char_zero G] : (subgroup.char_zero G).normal :=
sorry

theorem exercise_4_5_13 (p : ℕ) {G : Type*} [group G] [hp : fact (nat.prime p)] [hG : is_P_group p G] : ∃ (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_14 (p : ℕ) {G : Type*} [group G] [hp : fact (nat.prime p)] [h : is_P_group p G] : ∃ (N : sylow p G), ↑N = ⊤ :=
sorry

theorem exercise_4_5_1a {G : Type*} [add_group G] {p : ℕ} (P : G) (H : add_subgroup G) (hP : P ∈ add_subgroup.sublists_of_prime p H) : P ∈ add_subgroup.sublists_of_prime p H :=
sorry