

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
sorry