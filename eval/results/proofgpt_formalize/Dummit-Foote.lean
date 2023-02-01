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
=======
inv_mul_pow_of_le_inv_mul_pow {G : Type*} [comm_group G] {n : ℕ}
	{a : G} :
	a ^ n ≤ a⁻¹ * a ^ (n - 1)⁻¹ :=
sorry

is_R_or_C.abs_sq_eq_one_iff_of_sq_eq_one {G : Type*} [normed_group G]
	{x : G} (hx : is_R_or_C.abs x) :
	is_R_or_C.abs x ^ 2 = 1 ↔ is_R_or_C.abs x = 1 ∨ is_R_or_C.abs x ^ 2 = 2 :=
sorry

zpow_neg_of_nat_abs_eq {G : Type*} [div_inv_monoid G] (x : G) (n : ℕ)
	(hpos : 0 < n) (h : ↑(x.nat_abs) = x ^ n) :
	x ^ -n = x ^ (n - 1) :=
sorry

conj_inv_mul_eq_iff {α : Type u} [comm_semiring α] {x y : α} :
	⇑conj_inv (x * y) = y * ⇑conj_inv x ↔ ⇑conj_inv y * x * y = 1 ↔ y⁻¹ * x * y = 1 :=
sorry

group.order_of_eq_of_inv {G : Type u} [group G] (x : G) :
	group.order_of x = order_of x⁻¹ :=
sorry

abs_eq {α : Type u} [group α] [linear_order α] (x g : α) :
	|x| = |g⁻¹ * x * g| :=
sorry

abs_comm {α : Type u} [add_group α] [linear_order α]
	[covariant_class α α has_add.add has_le.le] (a b : α) :
	|a| * |b| = |b * a| :=
sorry

is_add_cyclic_of_exists_sq_eq_one {α : Type u} [add_group α]
	[is_add_cyclic α] :
	(∃ (x : α), x ^ 2 = 1) → is_add_cyclic α :=
sorry

add_comm_group.add_comm_group_of_prod_eq_add_comm_group_of_prod_eq_add_comm_group
	{A B : Type*} [add_comm_group A] [add_comm_group B] (h : (A × B) = + A + B) :
	add_comm_group.add_comm_group_of (prod.map (add_monoid_hom.fst A B) h) = add_comm_group.add_comm_group_of_prod_eq_add_comm_group h :=
sorry

int.neg_of_mul_eq_sub (a b : ℤ) :
	-a * b = a - b :=
sorry

zmod.val_mul_assoc {n : ℕ} (a b : zmod n) :
	(a * b).val = a.val * b.val :=
sorry

infinite_order.of_ne_zero_zpow {G : Type u} [group G] {x : G}
	(hx : x ≠ 0) (n : ℤ) :
	infinite_order.of x ≠ 0 → n ≠ 0 :=
sorry

zmod.mul_assoc {n : ℕ} (a b c : zmod n) :
	(a * b) * c = a * c * b :=
sorry

zmod.val_ne_group_of_not_group (n : ℕ) :
	¬zmod.val n = group_of_not_group n :=
sorry

set.infinite_group_of_infinite_of_fintype {α : Type*} [group α]
	[fintype α] :
	(set.of (λ (n : ℕ), ↑n) (λ (n : ℕ), ↑n)).infinite :=
sorry

group.mul_induction_on₂ {A B : Type*} [group A] [group B]
	{p : group.of (A × B) → Prop} (h₁ : group.of A)
	(h₂ : group.of B)
	(h₃ : ∀ (a : A) (b : B), p (group.of a) (group.of b)) :
	p z :=
sorry

abelian_of_mul_equiv_apply {G : Type*} [group G] (ᾰ : G) :
	⇑abelian_of_mul_equiv ᾰ = abelian_of_mul ᾰ :=
sorry

equiv.perm.is_three_cycle_eq_one_iff {α : Type*} [fintype α]
	[decidable_eq α] {σ : equiv.perm α} (hσ : σ.is_three_cycle) :
	σ.is_three_cycle = 1 ↔ σ = 1 :=
sorry

sym_alg.sym_mul_of_ne_zero {α : Type*} [has_mul α] [has_add α]
	[has_neg α] [invertible 2] (a : α) :
	sym_alg.sym a * sym_alg.of_ne_zero a = 1 :=
sorry

add_subgroup_of_rat_zero_or_bot {H : add_subgroup ℚ}
	(hH : ∀ (x : ℚ), x ≠ 0 → 1 / x ∈ H) :
	H = 0 ∨ ℚ :=
sorry

is_normal_subgroup.of_inf_of_normal {G : Type*} [group G]
	{H K : subgroup G} (hH : is_normal_subgroup ↑H)
	(hK : is_normal_subgroup ↑K) :
	is_normal_subgroup (H ⊓ K) :=
sorry

subgroup.countable_of_inf_normal {G : Type*} [group G]
	{H : subgroup G} [h : H.normal] (hne : H.nonempty) :
	(subgroup.of h).countable :=
sorry

quotient_add_group.mk_eq_neg_add_group_of_mk {A : Type u}
	[add_comm_group A] (B : add_subgroup A) (hB : is_add_subgroup B) :
	quotient_add_group.mk' B = -quotient_add_group.mk B :=
sorry

zmod.pow_card_sub_mul_self_eq_mul {p : ℕ} [fact (nat.prime p)]
	{a : ℤ} (ha : ↑a ≠ 0) :
	(∀ (a : ℤ), a ^ p * a = a * ↑a) → (∀ (a b : ℤ), a ^ p * b = a * b) ∧ ∀ (a b : ℤ), a ^ p * b = a * b → a ^ p = a * b :=
sorry

rat.not_nonempty_of_not_inf_dvd_card {α : Type*}
	[linear_ordered_field α] [archimedean α] {ι : Type*} [fintype ι]
	(h : ¬(⊤ ∩ ι).nonempty) :
	¬(rat.of_finset ι).nonempty :=
sorry

subgroup.inf_eq_top_of_relindex_eq_one {G : Type*} [group G]
	{H K : subgroup G} [fintype ↥H] [fintype ↥K]
	(hH : fintype.card ↥H = fintype.card ↥K) :
	H ⊓ K = ⊤ :=
sorry

is_add_fundamental_group.exists_neg_of_prime_ne {p : ℕ} {G : Type*}
	[add_group G] [hp : fact (nat.prime p)] [fintype G]
	(hG : is_add_fundamental_group p G) :
	∃ (z : G), z ≠ 0 ∧ is_add_fundamental_group.neg z :=
sorry

add_subgroup.card_pos_add_one_eq_card_pos_of_pos {G : Type*}
	[add_group G] {n : ℕ} (hpos : 0 < n) (h : ∀ (g : G), 0 < g → n ≤ g) :
	fintype.card ↥(add_subgroup.pos_add_one G n) = fintype.card ↥(pos_subgroup.of G n) :=
sorry

is_solvable_of_is_solvable {G : Type*} [group G] [is_solvable G] :
	is_solvable G :=
sorry

is_solvable_quotient_center (G : Type*) [group G] [h : is_solvable G] :
	is_solvable (quotient_group.mk' G).carrier :=
sorry

equiv.perm.exists_inv_mul_of_one_lt_card {α : Type u} [fintype α]
	[decidable_eq α] [is_trans α has_one.one] {σ : equiv.perm α}
	(hσ : σ.is_swap) :
	∃ (a : α), ⇑σ a ≠ a ∧ ∀ (b : α), ⇑σ b ≠ b → b ≠ a :=
sorry

add_subgroup.is_normal_iff_char_le {G : Type*} [add_group G]
	{N : add_subgroup G} :
	N.normal ↔ add_subgroup.char N ≤ N :=
sorry

is_p_group.exists_normal {p : ℕ} {G : Type*} [group G]
	[hp : fact (nat.prime p)] [fintype (sylow p G)] (P : sylow p G) :
	∃ (Q : sylow p G), ↑Q.normal :=
sorry

is_p_group.exists_normal {p : ℕ} {G : Type*} [group G]
	[hp : fact (nat.prime p)] [fintype (sylow p G)] (hG : is_p_group p G) :
	∃ (H : subgroup G), is_p_group p ↥H ∧ H.normal :=
sorry

subgroup.mem_comm_of_mem_zpowers {G : Type*} [group G] {p : G}
	{P : G} [hp : fact (nat.prime p)] (hP : P ∈ subgroup.zpowers p)
	{H : subgroup G} (hH : P ≤ H) :
	P ∈ subgroup.zpowers p :=
