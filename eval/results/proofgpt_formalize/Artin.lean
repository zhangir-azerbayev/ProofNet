

is_unit_one_add_self_of_power_eq_zero {R : Type*} [add_monoid R]
	[has_one R] [is_add_unit 1] (x : R) :
	is_unit (1 + x) :=
sorry

zmod.int_cast_ring_hom_apply (n : ℕ) (x : ↥(non_assoc_ring (zmod n))) :
	⇑(zmod.int_cast_ring_hom n) x = ↑x :=
sorry

ideal.quotient_inf_self_is_nilpotent {α : Type*} [ring α] {I J : ideal α}
	(h : ∀ (x : α), x ∈ I ⊓ J → is_nilpotent (⇑(ideal.quotient.mk I) x) := _) :
	is_nilpotent (I ⊓ J) :=
sorry

ideal.quotient_mk_eq_of_sup_eq_self {α : Type u} [ring α] {I J : ideal α}
	(h : I ⊔ J = ⊤) :
	⇑(ideal.quotient.mk I) = ⇑(ideal.quotient.mk J) :=
sorry

char.quadratic_char_eq_of_ring_equiv_apply {F : Type*} [field F]
	[fintype F] [decidable_eq F] (hF : ring_equiv F)
	(hF' : ring_equiv.ker (hF.mul_self))
	(hF'' : ring_equiv.ker (hF'.mul_self)) :
	⇑(char.quadratic_char_eq_of_ring_equiv hF hF' hF'') = ⇑(char.quadratic_char F) :=
sorry

mem_non_zero_divisors_of_mem_non_zero_divisors {R : Type*}
	[comm_ring R] [no_zero_divisors R] {a b : R} (ha : a ∈ non_zero_divisors R)
	(hb : b ∈ non_zero_divisors R) :
	a * b ∈ non_zero_divisors R :=
sorry

ideal.quotient_mk_eq_top_of_is_maximal_of_is_unit {R : Type u}
	[ring R] {M : ideal R} (hm : ∀ (x : R), x ∉ M → is_unit x)
	(hM : is_maximal M) :
	ideal.quotient.mk M = ⊤ :=
sorry

uniq_sum_four_mul_add_one (α : Type*) [field α] (x : α) :
	(finset.range (x + 1)).sum (λ (m : ℕ), x ^ m * (x ^ 2 + 1)) = (x ^ 2 + x + 1) * (finset.range (x + 1)).sum (λ (m : ℕ), x ^ m * x ^ 2) :=
sorry

dioph.sq_of_le_of_lt {α : Type u} [has_pow α ℕ] [preorder α]
	[decidable_rel has_le.le] {p : ℕ} (x y : α) :
	dioph.of_le x y → (∃ (z : ℤ), x ^ 2 = y) ∨ 2 * x ^ 2 = p ∨ 2 * x ^ 2 + 2 * y + 3 * y ∧ y ^ 2 = p) :=
sorry

zmod.exists_finset_card_eq (n : ℕ) [fact (0 < n)] :
	∃ (i : finset (zmod n)), i.card = (-1) ^ n :=
sorry

int.dvd_of_dvd_of_nat_of_dvd {a b : ℤ} (w₁ : a ∣ b) (w₂ : a.nat_abs ∣ b.nat_abs) :
	a ∣ b :=
sorry

polynomial.irreducible_C_mul_iff {α : Type u} {a b : α} [field α]
	{F : Type*} [field F] [algebra α F] (ha : a ≠ 0) (hF : polynomial.splits (algebra_map α F) (minpoly α b)) :
	irreducible (⇑polynomial.C a * F) ↔ irreducible (⇑polynomial.C b * F) :=
sorry

rat.int_coe_eq_zero {x : ℤ} {n : ℕ} :
	↑n = 0 ↔ n.nat_degree ≤ 0 ∧ ↑n.nat_degree ≤ 1 :=
sorry

rat.irreducible_of_pow_three_add_three {α : Type*} [division_ring α]
	[char_zero α] [is_domain α] [normalized_gcd_monoid α] {x : α} (z : α)
	(hx : x ^ 3 + 4 * x + (z * x) ^ 2) (hz : z ≠ 0) :
	irreducible (rat.mk (x ^ 3) * z + (x ^ 2) * z) :=
sorry

is_irreducible_sq_add_self {α : Type u} [field α] [is_irreducible_sq_field α]
	{x : α} :
	is_irreducible (x ^ 2) :=
sorry

zmod.exists_irreducible_pow_two (x : zmod 0) :
	irreducible (has_pow.pow x) :=
sorry

is_irreducible_of_is_irreducible_pow_three {α : Type u} [field α]
	[is_alg_closed α] (x : α) :
	is_irreducible (x ^ 3) :=
sorry

polynomial.irreducible_pow_sub_C {p : ℕ} [hp : fact (nat.prime p)]
	(hirr : irreducible (polynomial.cyclotomic p ℚ)) (n : ℕ) :
	irreducible (polynomial.cyclotomic p ℚ ^ n - p) :=
sorry

tactic.ring_exp.pow_overlap_pf_succ {α : Type u} [comm_semiring α]
	{pps p : α} {r : ℕ} :
	pps = p * 2 ^ r + 1 → p ^ (2 ^ k) = 2 ^ (r + 1) :=
sorry

finite_field.Prove_prod_nonzero_units_eq_neg_one {K : Type*} [field K]
	[fintype K] [algebra K ℚ] [h : finite_field K]
	(h_int : ∀ (x : K), x ≠ 0 → ↑x = -1) :
	↑(finite_field.Prove_prod_nonzero_units h_int) = -1 :=
sorry

is_of_fin_two_mul_self {α : Type u} [monoid α] [has_le α]
	[covariant_class α α has_mul.mul has_le.le] :
	is_of_fin_two_mul_self α :=
sorry

subgroup.is_add_subgroup_of_comm {G : Type*} [add_group G]
	{H : add_subgroup G} {a b : G} (h : a * b = b * a) :
	H.is_add_subgroup :=
sorry

real.mk_of_add_of_mul_of_add_of_pos {P : Type*} [add_monoid P]
	(h : ∀ (x y : P), 0 < x → 0 < y → x + y = 0) :
	⟨real.of_add_of_mul_of_add_of_pos h, _⟩ = h :=
sorry

is_conj_swap {α : Type u} [group α] [has_le α]
	[covariant_class α α has_mul.mul has_le.le]
	[covariant_class α α (function.swap has_mul.mul) has_le.le] (a b : α) :
	is_conj (function.swap has_mul.mul a) (function.swap has_mul.mul b) :=
sorry

subgroup.mem_center_of_one_le {G : Type*} [group G]
	[is_domain G] [covariant_class G G has_mul.mul has_le.le]
	[covariant_class G G (function.swap has_mul.mul) has_le.le] :
	1 ≤ subgroup.center G :=
sorry

subgroup.center_prod_eq {G : Type*} [group G] {N : Type*} [group N]
	(H : subgroup G) (K : subgroup N) :
	(H.prod K).center = H.center * K.center :=
sorry

alg_hom.coe_field_with_zero_hom_injective {R : Type u} {A : Type v}
	{B : Type w} [comm_semiring R] [semiring A] [semiring B] [algebra R A]
	[algebra R B] [algebra A B] [is_scalar_tower R A B] :
	function.injective coe :=
sorry

finite_dimensional.finrank_pos_part_of_linear_independent_aux {V : Type u}
	[category_theory.category V] [category_theory.limits.has_images V]
	[category_theory.limits.has_zero_object V]
	[category_theory.limits.has_zero_morphisms V]
	[category_theory.limits.has_kernels V] [category_theory.limits.has_images V] {n : ℕ}
	(h_dim : finite_dimensional.finrank V = n) (f : (fin n → V) →ₗ[ℕ] V)
	(hf : ∀ (i : fin n), f i = 0) :
	0 < n :=
sorry

projectivization.exists_eq_of_not_finite_subspaces {α : Type*}
	[infinite α] (V : set (projectivization α))
	(hV : ¬(supr V).finite) :
	∃ (s : set (projectivization α)), s = {⟨V, _⟩} :=
sorry

is_cyclic.of_quotient_center {α : Type u} [group α] {Z : subgroup α}
	(hZ : is_cyclic ↑Z) :
	is_commutative α ∧ ∀ (g : α), g ∈ Z → is_cyclic α → is_cyclic α :=
sorry

is_simple_add_group.of_nat_succ {G : Type*} [add_group G] {n : ℕ} :
	is_simple_add_group G → is_simple_add_group.of ↑n + 1 = 0 :=
sorry

is_simple_add_prime_pow {α : Type*} [add_comm_monoid α] {p q : ℕ}
	[hp : fact (nat.prime p)] [hq : fact (nat.prime q)] (h : p + q = p + q) :
	is_simple_add_prime (λ (x : α), x ^ p) :=
sorry

is_simple_order.of_prime_sq {α : Type*} [comm_monoid α] {p q : ℕ}
	[hp : fact (nat.prime p)] [hq : fact (nat.prime q)] (h : p ^ 2 = q) :
	is_simple_order α :=
sorry

group.mk_mul_mk {G : Type*} [group G] (a b : G) :
	group.mk a b * group.mk b a b = group.mk (b * a) (b * a) :=
sorry

is_free_group.of_mul_of_mul_swap {α : Type u} [monoid α] (x y z : α) :
	is_free_group (x * y * z⁻¹) :=
sorry

group.to_subgroup_of_normal {G : Type*} [group G] (N : subgroup G)
	[group.normal] [hN : (G ⧸ N).normal] (hN : (G ⧸ N).normal) :
	N.normal :=
sorry