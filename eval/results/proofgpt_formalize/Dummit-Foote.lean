

semidirect_product.inv_prod_left {N G : Type*} [group N] [group G]
	{φ : G →* mul_aut N} (a : G) :
	(⇑semidirect_product.prod_left a)⁻¹ = ⇑semidirect_product.inr a⁻¹ * ⇑semidirect_product.inl a⁻¹ :=
sorry

prove_eq_one_or_two {α : Type u} [group α] (x : α) :
	x.prove = 1 ∨ x.norm = 1 ∨ x.norm = 2 :=
sorry

equiv.perm.inv_eq_of_card_eq {α : Type*} [decidable_eq α] [fintype α]
	(x : α) {n : ℕ} (hn : 0 < n) (hx : fintype.card α = n) :
	x⁻¹ = x ^ (n - 1) :=
sorry

equiv.perm.disjoint.inv_smul_eq_iff {α : Type*} [group α] {x y : α}
	(h : x ≠ y) :
	(x * y)⁻¹ = y⁻¹ * x⁻¹ ↔ x⁻¹ * y⁻¹ * x = 1 :=
sorry

order_of_inv_eq_order_of {G : Type u} [group G] (x : G) :
	order_of x⁻¹ = order_of x :=
sorry

tactic.norm_num.sum_congr_left_swap {α : Type*} [add_group α]
	(x g : α) (h : x.nat_abs = g.nat_abs) :
	x = g + x⁻¹ :=
sorry

abs_comm {G : Type*} [add_group G] [linear_order G]
	[covariant_class G G has_add.add has_le.le] (a b : G) :
	|a - b| = |b - a| :=
sorry

abelian_of_pow_two_eq_one (G : Type*) [group G]
	(h : ∀ (x : G), x ^ 2 = 1) :
	is_abelian G :=
sorry

abelian_prod_iff (A B : Type*) [add_comm_group A] [add_comm_group B] :
	is_abelian (A × B) ↔ is_abelian A ∧ is_abelian B :=
sorry

int.not_comm_of_star (a b : ℤ) :
	¬commute (has_star.star a) (has_star.star b) :=
sorry

zmod.int_cast_add_res {n : ℕ} (R : Type*) [ring R] [char_zero R] :
	↑(zmod.int_cast_add_res R n) = ↑n :=
sorry

infinite.exact_of_zpow_ne {G : Type u} [group G] {x : G} (h : x ≠ 0) :
	∃ (n : ℤ), ∀ (m : ℤ), m ≠ n → x ^ n = x ^ m :=
sorry

zmod.int_cast_mul (n : ℕ) {R : Type*} [ring R] (a b : zmod n) :
	↑(a * b) = ↑a * ↑b :=
sorry

int.coe_nat_not_group_of_one_lt_nth_residual (n : ℕ) :
	¬is_group_of (coe ∘ zmod n) :=
sorry

gaussian_int.to_complex_neg_of_Omega_eq_one :
	↑- Popular_choice.of_choice (λ (p : ℕ), ↑p) = 1 :=
sorry

Group.surjective_of_epi_auxs.swap_mul_aux (A B : Group)
	(h : Group.surjective_of_epi_auxs.G_is_solvable (A ⊔ B) * Group.surjective_of_epi_auxs.G_is_solvable (B ⊔ A)) :
	Group.surjective_of_epi_auxs.X_with_infinity.aux (Group.surjective_of_epi_auxs.swap_mul_aux A B h) = Group.surjective_of_epi_auxs.X_with_infinity.aux (Group.surjective_of_epi_auxs.swap_mul_aux A B h) :=
sorry

abelianization.map_of {G : Type u} [group G] (g : G) :
	⇑abelianization.map (⇑abelianization.of g) = g :=
sorry

eq_one_of_aut_eq_one_of_fintype {G : Type*} [group G] [fintype G]
	{σ : G →* G} (h1 : ∀ (g : G), ⇑σ g = g ↔ g = 1)
	(h2 : (fintype.card G).min_fac = fintype.card G) :
	∃ (g : G), ⇑σ g = g :=
sorry

complex.not_equiv_real_of_not_near_zero (r : ℝ) :
	¬↑r = 0 :=
sorry

add_subgroup.zero_or_rat_eq_of_forall_ne_zero {H : add_subgroup ℚ}
	(hH : ∀ (x : ℚ), x ∈ H → x ≠ 0 → 1 / x ∈ H) :
	0 = H ∨ add_subgroup.rat_eq_of_ne_zero H :=
sorry

subgroup.normal.inf {G : Type*} [group G] (H K : subgroup G)
	(hH : H.normal) (hK : K.normal) :
	(H ⊓ K).normal :=
sorry

subgroup.countable_normal_of_disjoint {G : Type*} [group G]
	(H : set (subgroup G)) (hH : H.nonempty) :
	(disjoint H).normal :=
sorry

add_subgroup.abelian_over_to_over {A : Type*} [add_group A]
	(B : add_subgroup A) [h : is_add_group_over B] :
	is_abelian (A ⧸ B) :=
sorry

int.modeq_zpow_of_prime (a : ℤ) {p : ℕ} [hp : fact (nat.prime p)] :
	a ^ p ≡ a [MOD p] :=
sorry

padic_int.not_exists_rat_nat_subgroup_rat {p : ℕ}
	[hp_prime : fact (nat.prime p)] (h : ¬∃ (n : ℕ), ↑n < p) :
	¬∃ (n : ℕ), ↑n < p :=
sorry

add_subgroup.inf_eq_bot_of_rel_prime_order {G : Type*} [add_group G]
	(H K : add_subgroup G) [hH : H.finite] [hK : K.finite]
	(hHK : (add_order_of H).relatively_prime (add_order_of K)) :
	H ⊓ K = ⊥ :=
sorry

add_comm_group.mod_p_induction (p : ℕ) (G : Type*) [add_comm_group G]
	(h : ∀ (G : Type*) [_inst_3 : add_comm_group G] [_inst_4 : char_p G p], add_monoid.is_simple_add_group G → (∃ (n : ℕ), add_action.is_minimal G (zmod p) n)) :
	add_comm_group.mod_p G p :=
sorry

real.add_haar_sphere {n : ℕ} (hn : 0 < n) :
	∃ (R : ℝ), 0 < R ∧ ∀ (g : add_group ℝ), R < n → add_subgroup.closure {g} = ⊤ :=
sorry

solvable_subgroups.solvable {G : Type*} [group G] :
	solvable_subgroups G → solvable G :=
sorry

solvable_pred.solvable_quotient_group_is_solvable {G : Type*} [group G]
	[hG : solvable_pred G] :
	is_solvable (G ⧸ subgroup.center G) :=
sorry

equiv.perm.exists_swap_mul_eq_self {α : Type*} [fintype α]
	[decidable_eq α] (G : equiv.perm α) [G.is_trans] :
	∃ (σ : equiv.perm α), ∀ (a : α), ⇑σ a ≠ a → ⇑σ a = ⇑G a :=
sorry

subgroup.normal_subgroups_of_char_zero {G : Type*} [group G]
	[hG : char_zero G] :
	(subgroup.char_zero G).normal :=
sorry

is_P_group.exists_normal_sylow (p : ℕ) {G : Type*} [group G]
	[hp : fact (nat.prime p)] [hG : is_P_group p G] :
	∃ (P : sylow p G), ↑P.normal :=
sorry

is_P_group.exists_normal_sylow (p : ℕ) {G : Type*} [group G]
	[hp : fact (nat.prime p)] [h : is_P_group p G] :
	∃ (N : sylow p G), ↑N = ⊤ :=
sorry

add_subgroup.mem_sublists_of_mem_add_centralizer {G : Type*} [add_group G]
	{p : ℕ} (P : G) (H : add_subgroup G) (hP : P ∈ add_subgroup.sublists_of_prime p H) :
	P ∈ add_subgroup.sublists_of_prime p H :=
sorry