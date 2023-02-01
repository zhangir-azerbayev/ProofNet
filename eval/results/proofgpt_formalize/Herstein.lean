import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory

universe u

theorem exercise_2_10_1 {G : Type*} [group G] {A : subgroup G} [hA : A.normal] {b : G} {p : ℕ} [hp : fact (nat.prime p)] (hb : b ∈ subgroup.zpowers p) (hA' : ¬p ∣ A) (h'A : A.normal) (h'b : b ∉ A) : subgroup.is_complement' A {b} :=
sorry

theorem exercise_2_11_22 {G : Type u} [group G] (p n : ℕ) [h : fact (nat.prime p)] (H : subgroup G) [fintype (sylow p ↥H)] : H.normal :=
sorry

theorem exercise_2_11_6 {p : ℕ} {G : Type*} [group G] (P : sylow p G) (h : ↑P.relindex ↑P.alternating_subgroup = ⊥) : P = ⊥ :=
sorry

theorem exercise_2_11_7 {p : ℕ} {G : Type*} [group G] [fact (nat.prime p)] [mul_distrib_mul_action G G] (P : sylow p G) (h : ∀ (ϕ : G ≃* G), ↑ϕ.comp ↑P = ↑P) : P.to_subgroup = P :=
sorry

theorem exercise_2_1_18 {G : Type*} [group G] [fintype G] [h : ∀ (a : G), even (order_of a)] : ∃ (a : G) (H : a ≠ 0), a = a⁻¹ :=
sorry

theorem exercise_2_1_21 (G : Type*) [group G] [fintype G] : is_add_comm_group (G → G) :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] [fintype G] [h : is_of_fintype G] (a : G) : ∃ (n : ℕ), 0 < n ∧ a ^ n = group.exp a :=
sorry

theorem exercise_2_1_27 {G : Type*} [fintype G] [group G] [decidable_pred (λ (m : ℤ), 0 < m)] (a : G) : ∃ (m : ℤ) (H : m > 0), a ^ m = e :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] (h : ∀ (i j k : ℤ), i ≤ j → k ≤ l → a * b ^ i = a ^ i * b ^ k) : is_ abelian G :=
sorry

theorem exercise_2_2_5 {G : Type*} [group G] (a b : G) (h1 : (a * b) ^ 3 = a ^ 3 * b ^ 3) (h2 : (a * b) ^ 5 = a ^ 5 * b ^ 5) : is_ abelian G :=
sorry

theorem exercise_2_2_6c {G : Type w} [group G] (n : ℤ) [fact (1 < n)] (a b : G) : (a * b) ^ (n - 1) = e :=
sorry

theorem exercise_2_3_16 {α : Type u} [group α] [fintype α] (h : ∀ (p : ℕ), nat.prime p → ¬p < fintype.card α) {p : ℕ} (hp : nat.prime p) : is_cyclic α :=
sorry

theorem exercise_2_3_17 {G : Type*} [div_inv_monoid G] (a x : G) : ⇑conj_act.of_conj_act (a * x) = ⇑conj_act.of_conj_act a * ⇑conj_act.of_conj_act x :=
sorry

theorem exercise_2_3_19 {G : Type*} [group G] (M : subgroup G) (h : ∀ (x : G), x⁻¹ ∈ M * ↑M) : M.centralizer ≤ ⊤ :=
sorry

theorem exercise_2_4_36 {a : ℕ} (ha : 1 < a) (n : ℕ) : n * (a ^ n - 1).totient = (a ^ n - 1).totient :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G] (hnc : ∀ (H : subgroup G), H.normal) {a b : G} (h : b * a = a * b) : b = a :=
sorry

theorem exercise_2_5_30 {G : Type u} [group G] {p : ℕ} [hp : fact (nat.prime p)] (hpm : pm.card = p) {H : subgroup G} [H.normal] (hH : ∃ (g : G), ↑g ∈ H) : char_p G p :=
sorry

theorem exercise_2_5_31 {G : Type*} [add_group G] {p n : ℕ} (h : nat.prime p) (hG : add_subgroup G) (hH : ∀ (G : Type*) [add_group G] [_inst_3 : fintype G], fintype.card G = p ^ n → (∃ (H : add_subgroup G), H.is_add_unit)) (h1 : p ≠ 2) : H ≤ add_subgroup.centralizer G p ^ n :=
sorry

theorem exercise_2_5_37 {G : Type*} [group G] [h : nonabelian G] [fintype G] : 3 ≤ fintype.card G :=
sorry

theorem exercise_2_5_43 (G : Type*) [group G] : (∃ (a b c : G), a * b * c = a * c * b) → group.is_three_right_inv G :=
sorry

theorem exercise_2_5_44 (p : ℕ) (w : 0 < p) : (lucas_lehmer.X w).subgroup.normal :=
sorry

theorem exercise_2_5_52 {G : Type*} [add_group G] [fintype G] {φ : equiv.perm G} (hφ : φ.is_add_group) (hφ' : ∀ (x : G), x ∈ add_subgroup.zmultiples φ → ⇑φ x = x⁻¹) (hG : is_add_group G) : is_add_group G :=
sorry

theorem exercise_2_6_15 {G : Type*} [add_group G] (h : ∃ (m n : ℕ), m.coprime n ∧ add_order_of m = add_order_of n) : ∃ (m n : ℕ), m.coprime n ∧ add_order_of m = add_order_of n :=
sorry

theorem exercise_2_7_7 {G : Type*} [group G] {G' : Type*} [group G'] (φ : G →* G') (N : G) (h : N ∈ subgroup.map φ G) : ⇑φ N ∣G' :=
sorry

theorem exercise_2_8_12 {V : Type u} [category_theory.category V] [category_theory.limits.has_zero_morphisms V] {W : Type u} [category_theory.category W] [category_theory.limits.has_zero_morphisms W] (f : V → W) [category_theory.limits.has_image f] [category_theory.limits.has_cokernel (image_to_kernel f)] [category_theory.limits.has_cokernel f] (i : V ≅ V) : category_theory.limits.is_limit (nonabelian_group.direct_limit_of_iso f i) :=
sorry

theorem exercise_2_8_15 (p q : ℕ) (hp : nat.prime p) (hq : q ≠ 0) (hdiv : q ∣ p - 1) : ∃ (g₁ g₂ : ℕ), g₁ ≠ 0 ∧ g₁ ∣ p * g₂ ∧ g₂ ≠ 0 ∧ g₁ ≠ g₂ :=
sorry

theorem exercise_2_9_2 {G₁ G₂ : Type*} [group G₁] [group G₂] [is_cyclic G₁] [is_cyclic G₂] {m n : ℕ} (hm : nat.prime m) (hn : nat.prime n) (h : (G₁ * G₂).cyclic) : is_cyclic (m, n) ↔ m.coprime n :=
sorry

theorem exercise_3_2_21 {α : Type*} {σ τ : equiv.perm α} (hστ : σ.disjoint τ) (h : σ = τ) : σ.sigma_congr τ = e :=
sorry

theorem exercise_4_1_19 : (∃ (x : ℝ), x * x = -1) :=
sorry

theorem exercise_4_1_34 {T : matrix.special_linear_group (fin 2) ℤ} {S₃ : Type*} [field S₃] [module ℤ S₃] (h : T.det ≠ 0) (A : ↥(matrix.special_linear_group (fin 2) ℤ)) : ⇑(modular_group.equiv_of_det_ne_zero h) A = ↑A.mul (↑A)⁻¹ :=
sorry

theorem exercise_4_2_5 that_mul_self_eq {R : Type*} [ring R] (hR : ∀ (x : R), x * x = x) : ring.Prove_ that_mul_self_eq :=
sorry

theorem exercise_4_2_6 {R : Type x} [mul_zero_class R] {a : R} (h : commute (a * a) a) : commute a (a * x + x * a) :=
sorry

theorem exercise_4_2_9 (p : ℕ) [hp : fact (nat.prime p)] (hp1 : p ≠ 2) (hdiv : ∀ (a b : ℤ), p ∣ a → p ∣ b → a ∣ b) (a b : ℤ) : p ∣ a → p ∣ b → a ∣ b :=
sorry

theorem exercise_4_3_1 {R : Type u} {L : Type v} [comm_ring R] [lie_ring L] [lie_algebra R L] {a : L} : lie_algebra.root_set R L a = ⊥ → a ∈ ⊥ :=
sorry

theorem exercise_4_3_25 {R : Type*} [ring R] [no_zero_divisors R] (I : ideal R) : ⇑(matrix.to_lin I) 0 = ⊤ ∨ ⇑(matrix.to_lin I) 0 = ⊤ :=
sorry

theorem exercise_4_4_9 (p : ℕ) [fact (nat.prime p)] : (p - 1) / 2 = zmod.legendre_sym p - zmod.legendre_sym p.num / 2 :=
sorry

theorem exercise_4_5_16 {F : Type*} [field F] {p n : ℕ} [hp : fact (nat.prime p)] (q : polynomial F) (hq : irreducible (polynomial.map (algebra_map (zmod p) F)) → is_field (zmod p) F) (hn : n ≠ 0) : is_field (↑p ^ n) (mod_p F q) :=
sorry

theorem exercise_4_5_23 : Type*} [field F] [fintype F] {p q : polynomial F} (hp : irreducible p) (hq : irreducible q) (F_Z : fintype.card F = 7) (x : F) : ⇑(polynomial.aeval x) p = ⇑(polynomial.aeval x) q :=
sorry

theorem exercise_4_5_25 {Q : Type*} [comm_ring Q] {p : ℕ} (hp : p ≠ 1) (hQ : irreducible (polynomial.map (algebra_map Q (polynomial ℕ)) p)) : irreducible p :=
sorry

theorem exercise_4_6_2 {Q : Type*} [field Q] (f : polynomial Q) : irreducible (⇑(polynomial.aeval f) f) :=
sorry

theorem exercise_4_6_3 {Q : polynomial ℚ} (f : ℚ → ℂ) (x : ℝ) : (∃ (a : ℤ), a ≠ 0 ∧ a ≠ ↑(⇑polynomial.aeval x)) ∧ irreducible (polynomial.aeval x) :=
sorry

theorem exercise_5_1_8 (F : Type*) [field F] (p : ℕ) [char_p F p] (n : ℕ) (a b : F) (h : p ≠ 0) : (a + b) ^ p = a ^ p + b ^ p :=
sorry

theorem exercise_5_2_20 (F : Type*) {V : Type*} [field F] [add_comm_group V] [module F V] [infinite_space F] [h : ∀ (s : set_theory.submodule F V), s.finite → s ≠ ⊤] : ∃ (n : ℕ) (s : set_theory.submodule F V), s.finite ∧ s ≠ ⊤ ∧ set_theory.vector_space.restrict_scalars F (n • s) = ⊤ :=
sorry

theorem exercise_5_3_10 (x : ℂ) : is_algebraic ℚ (complex.cos (1 ^ x)) :=
sorry

theorem exercise_5_3_7 {K : Type*} [field K] {F : Type*} [field F] [algebra K F] (a : K) (h : is_algebraic K (a * a)) : is_algebraic F a :=
sorry

theorem exercise_5_4_3 {a : ℕ} {p : ℕ → ℤ} (h : is_root (p a) (algebraic_norm ℚ a)) (hp : p 1 = 0) : is_cau_seq has_abs.abs (λ (n : ℕ), (↑n ^ 5 + ↑(real.sqrt_two_add_series p n)) * ↑(real.sqrt_five_series p a) + ↑(real.sqrt_six_series p a) * ↑(real.sqrt_11_series p a) :=
sorry

theorem exercise_5_5_2 {x : ℝ} : irreducible (⇑(polynomial.aeval x) (polynomial.X ^ 3 - 3 * polynomial.X - 1)) :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] [char_p F p] (m p : ℕ) (hF : char_p F p) (h : ∀ (x : F), x ^ m - x ≠ 0) : (polynomial.map (int.cast_ring_hom F) (polynomial.X ^ m - polynomial.X)).roots.nodup :=
=======
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
