

is_subgroup.not_mem_of_mem_prime_disjoint_subgroup {α : Type*}
	[group α] {s : set α} {p : α} {b : α} (h : is_normal_subgroup s)
	(hb : b ∈ s) [hs : fact (nat.prime p)] (hbp : p ∣ b) :
	¬b ∈ s :=
sorry

is_normal_of_subgroup_of_order_of {G : Type*} [group G] (p : G → Prop)
	[decidable_pred p] [is_normal_subgroup p] (H : subgroup G) :
	is_normal ↑(subgroup.of_subgroup p) :=
sorry

is_p_group.of_is_empty_of_is_empty_of_is_p_group {p : ℕ}
	{G : Type*} [group G] (hG : is_p_group p G) (hP : is_p_group p G)
	(h : is_empty G) :
	is_p_group p G :=
sorry

is_p_group.to_subgroup_of_of_is_p_group {p : ℕ} {G : Type*} [group G]
	(h : is_p_group p G) (P : subgroup G) (hP : is_p_group p ↥P)
	(hP : ∀ (ϕ : G ≃* G), is_p_group p ↥(P.to_subgroup)) :
	is_p_group p ↥(P.to_subgroup_of P hP) :=
sorry

is_of_fin_odd_of_exists_inv {G : Type*} [group G] [fintype G] :
	∃ (a : G), a ≠ 1 ∧ is_of_fin_odd G a :=
sorry

add_subgroup.top_eq_top {G : Type*} [add_group G] :
	⊤ = ⊤ :=
sorry

pow_eq_exponent {G : Type u} [fintype G] [group G] (a : G)
	[decidable_eq G] :
	a ^ fintype.card G = e :=
sorry

pow_eq_exp_of_exists_zpow_eq_exp {G : Type*} [div_inv_monoid G]
	[fintype G] (a : G) :
	∃ (m : ℤ) (H : m > 0), a ^ m = e :=
sorry

is_add_group.of_pow_eq {G : Type*} [add_group G] {i : ℕ}
	(h : is_add_group (a ^ i) (b ^ i)) :
	is_add_group G :=
sorry

is_add_cyclic_of_is_add_cyclic_of_is_add_cyclic {α : Type u}
	[add_group α] [is_add_cyclic α] (a b : α) :
	is_add_cyclic α :=
sorry

mul_pow_mul_inv_mul_pow_of_one_lt_pow {G : Type u} [group G] {n : ℕ}
	[fact (1 < n)] (a b : G) :
	1 < n → (a * b * (a⁻¹ * b⁻¹) ^ n) :=
sorry

is_cyclic_of_prime_card {α : Type u} [group α] [is_prime_card α]
	(G : Type u) [group G] [fintype G] :
	monoid.is_cyclic G → fintype.card G = p :=
sorry

inv_mul_cancel_left {G : Type*} [group G] (a x : G) (C : G → Prop) :
	(x⁻¹ * a)⁻¹ * C x = x⁻¹ * C a * x :=
sorry

is_subgroup.of_inv_mul_of_mem {G : Type*} [group G] {x : G}
	{M : set G} (h : is_subgroup M) (hx : x⁻¹ * M x ⊆ M) :
	is_subgroup.of x⁻¹ * M x :=
sorry

nat.of_mul_pow_sub_one_dvd_totient_aux (a : ℕ) (n : ℕ) (a1 : 1 < a) :
	↑n ∣ (a ^ n - 1).totient :=
sorry

is_subgroup.mk_mul_of_mul_eq_mul {G : Type*} [group G]
	(hG : ∀ (a b : G), is_subgroup (a * b)) (a b : G) (hb : b * a = a * b) :
	is_subgroup.mk (b * a) _ = is_subgroup.mk b hb * is_subgroup.of a hb :=
sorry

is_normal_subgroup_of_prime_of_normal {G : Type*} [group G] {m p : ℕ}
	[hp : fact (nat.prime p)] [fintype (G ⧸ p)] [fintype (H ⧸ p)]
	(h : is_normal_subgroup_of_prime p) (H : subgroup G) :
	is_char ↥H :=
sorry

add_subgroup.is_characteristic.add_subgroup_of_prime_le {G : Type*}
	[add_group G] {p n : ℕ} [hp : fact (nat.prime p)] (hle : p ≤ n)
	{H : add_subgroup G} (hH : is_characteristic ↥H) :
	h H.add_subgroup_of (G ⧸ add_subgroup.of p G) :=
sorry

nonarchimedean_add_group.of_add_equiv_apply {G : Type*} [add_group G]
	{S₃ : Type*} [add_group S₃] (h : S₃ ≃+ S₁) (ᾰ : G) :
	⇑(nonarchimedean_add_group.of_add_equiv h) ᾰ = ↑(h.to_add_equiv.to_fun ᾰ) :=
sorry

add_units.neg_add_equiv_apply {α : Type u} [add_group α]
	(ᾰ : add_units α) :
	⇑add_units.neg_add_equiv ᾰ = ↑ᾰ :=
sorry

is_p_group.of_prime_of_normal {p : ℕ} [hp : fact (nat.prime p)]
	(h : is_p_group p) (hprime : nat.prime p) (hnormal : normal ℕ) :
	is_normal_subgroup (is_p_group.of_prime p h) :=
sorry

sylow.inv_of_card_eq_finrank_aux {G : Type u} [group G] [fintype G]
	{φ : G →* G} (hφ : φ.is_three_of_card = fintype.card G)
	(hφ' : ∀ (x : G), ⇑φ x = x⁻¹) (y : G) :
	(finset.filter (λ (x : G), ⇑φ x = y) finset.univ).card = finite_dimensional.finrank ↥(φ.inv_of_card) y :=
sorry

is_add_of_is_add_of_order_of_eq_mul_one {G : Type*} [add_group G]
	{m n : ℕ} (h : is_add_of_order_of m = is_add_of_order_of n) :
	is_add_of_order_of (m * n) = m * n :=
sorry

semidirect_product.mem_right_coset {G : Type*} [group G] {N : Type*}
	[group N] {G' : Type*} [group G'] {φ : G →* G'} {N' : Type*} [group N']
	{φ' : G' →* G} (hφ : φ.comp N = φ'.comp N') (h : N ⊆ φ.range) :
	φ' ∈ semidirect_product.right_coset φ N :=
sorry

nonarchimedean_add_group.mk_coe {α : Type*} [add_group α]
	[topological_space α] [nonarchimedean_add_group α] (a : α) (b : α)
	(h : a + b = 0) :
	↑{to_add_equiv := a, nonarchimedean_add_group.to_add_equiv := b, nonarchimedean_add_group.to_add_equiv := h} = a :=
sorry

zmod.nonarchimedean_equiv_of_dvd_sub_one {p q : ℕ}
	[hp : fact (nat.prime p)] (hq1 : q ≠ 1) (hq2 : q ∣ p - 1) :
	⇑zmod.nonarchimedean_equiv p q = ⇑zmod.nonarchimedean_equiv p q :=
sorry

is_cyclic.of_prod_eq_one {α : Type u} {β : Type v} [group α] [group β]
	{G₁ G₂ : Type*} [group G₁] [group G₂] {m : α} {n : β}
	(h : is_cyclic (G₁ ×ˢ G₂)) (hn : is_cyclic (G₁ ×ˢ G₂)) :
	is_cyclic (G₁ ×ˢ G₂) :=
sorry

equiv.perm.eq_on_one_of_mul_eq_one {α : Type*} [fintype α]
	{σ τ : equiv.perm α} (h1 : σ * τ = 1) (h2 : σ * τ = 1) :
	σ = τ :=
sorry

quaternion.exists_sq_eq_neg_one {R : Type*} [comm_ring R]
	{c₁ c₂ : R} (h : c₁ ≠ 0) (x : R) :
	∃ (y : R), y * x = -1 :=
sorry

modular_group.coe_eq_S_three_of_det_ne_zero {T : matrix.special_linear_group (fin 2) ℤ}
	(hT : ↑T = matrix.special_linear_group.S 3)
	(hT' : modular_group.det ≠ 0) :
	↑T = modular_group.S 3 :=
sorry

ring.of_three_eq_x {R : Type u} [ring R] (x : R)
	(h : ∀ (x : R), x ^ 3 = x) :
	ring.of_three x h = x :=
sorry

commute.quadratic {R : Type*} [mul_zero_one_class R] {a x : R}
	(h : commute a x) :
	commute (a * x) x :=
sorry

zmod.χ₈_int_eq_pow_of_odd_dvd_dvd_of_mod_eq_pow_sub_one
	{p : ℕ} [fact (nat.prime p)] (hp : p ≠ 2) (a b : ℤ) :
	↑(zmod.χ₈ ↑a) ^ 2 + ↑(zmod.χ₈ ↑b) ^ 2 = ↑a ^ (↑b - 1) :=
sorry

is_ideal.to_ideal_of_mul_self_eq_bot {R : Type*} [comm_ring R]
	{a : R} {L : ideal R} (h : is_ideal (has_mul.mul a '' L)) :
	is_ideal ↑(ideal.of_mul_self L) :=
sorry

is_ideal_of_is_ideal_of_is_empty_of_is_compl {R : Type*}
	[comm_ring R] [is_domain R] [is_principal_ideal_ring R] {a b : R} (h : is_ideal a b)
	(hI : is_ideal ↑I) :
	is_ideal a b :=
sorry

zmod.legendre_sym_eq_pow (p : ℕ) [fact (nat.prime p)] :
	zmod.legendre_sym p = ↑p ^ (p / 2) :=
sorry

padic_int.zmod_pow_irreducible_aux {p : ℕ} [hp_prime : fact (nat.prime p)]
	{F : Type*} [field F] [fintype F] [algebra F ℤ_[x] λ (x : ℤ_[x]), F⟮x⟯)
	(hF : ∀ (n : ℕ), irreducible (polynomial.cyclotomic ↑n ℤ))
	(hF' : ∀ (x : ℤ), x ≠ 0 → (polynomial.cyclotomic ↑n ℤ).degree ≠ 0 → (polynomial.cyclotomic ↑n ℤ).separable)
	(q : polynomial F) (hq : irreducible q) (n : ℕ) (hn : n < q.nat_degree) :
	∃ (a : ℤ), (∀ (n : ℕ), n < q.nat_degree → irreducible (q.coeff n)) ∧ (∀ (n : ℕ), ↑n < q.coeff n) ∧ polynomial.eval₂ (zmod.cast_hom _ (zmod (q.coeff n)) (q.coeff n)) (p ^ n) = ↑n :=
sorry

zmod.χ₈_inv_sq_mul {F : Type*} [field F] (p : fin 2 → F) (q : fin 2 → F)
	(hF : ∀ (x : F), is_cyclotomic_extension {n : ℕ} (z : fin n → F) (p x) (q x))
	(h : irreducible (polynomial.cyclotomic ↑n ℤ)) :
	function.inv_sq (⇑(zmod.χ₈ p) (⇑(zmod.χ₈ q) (⇑(algebra_map (zmod p) (zmod q)))) * ⇑(algebra_map (zmod p) (zmod q)) = ⇑(algebra_map (zmod p) (zmod q)) :=
sorry

witt_vector.is_irreducible_aux {p : ℕ} {R : Type*}
	[hp : fact (nat.prime p)] [comm_ring R] [char_p R p] (q : witt_vector p R) :
	is_irreducible ↑q → irreducible (witt_vector.mk p q) :=
sorry

polynomial.irreducible_of_bodd_of_irreducible_mul_X {R : Type u}
	[comm_semiring R] {f : polynomial R} (hf : f.nat_degree ≠ 1) :
	irreducible (polynomial.X.comp f) :=
sorry

polynomial.exists_irreducible_of_exists_infinite_of_infinite_of_irreducible
	{K : Type*} [field K] {f : polynomial K} (hf : ∃ (x : K), f ≠ 0 ∧ f.degree ≠ 0 ∧ irreducible (polynomial.cyclotomic x K)) :
	irreducible f :=
sorry

char_p.add_pow_eq_pow_add {p : ℕ} [fact (nat.prime p)] (F : Type*)
	[field F] [fintype (zmod p)] [char_p F p] (a b : F) (h : p ≠ 0) :
	⇑(char_p.add_pow F a) b = ⇑(char_p.pow F a) b + ⇑(char_p.pow F b) a :=
sorry

projectivization.not_nonempty_of_finite_dimensional (α : Type*)
	[infinite α] (V : Type*) [has_mem V] [finite_dimensional α V] :
	¬nonempty (projectivization α V) :=
sorry

is_algebraic_rat_cos_one_pow_of_one_le_left {α : Type*} [comm_ring α]
	[is_domain α] {x : ℝ} (hx : 1 ≤ x) :
	is_algebraic ℚ (real.cos x) :=
sorry

is_algebraic_of_is_algebraic_sq {K : Type*} [field K] (a : K)
	(h : is_algebraic K (a ^ 2)) :
	is_algebraic K a :=
sorry

algebraic_closure.mk_of_step_zero (C : Type u)
	[category_theory.category C] [category_theory.preadditive C]
	[category_theory.limits.has_binary_products C] (a : C) (p : ℚ) (h : p a = 0) :
	algebraic_closure.mk_of_step C a 0 = 0 :=
sorry

is_irreducible_pow_three_sub_one {α : Type u} [comm_ring α] [is_domain α]
	{x : α} :
	is_irreducible (x ^ 3 - 3x - 1) :=
sorry

char_p.pow_sub_one_char_ne_zero {F : Type*} [field F] (p : ℕ)
	[HF : char_p F p] (hF : p ≠ 0) (x : F) :
	⇑(p ^ sub_one_char F) x ≠ 0 :=
sorry