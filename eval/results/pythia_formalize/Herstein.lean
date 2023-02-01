

subgroup.is_complement'_singleton_of_prime_order {G : Type*} [group G]
	{A : subgroup G} [hA : A.normal] {b : G} {p : ℕ} [hp : fact (nat.prime p)]
	(hb : b ∈ subgroup.zpowers p) (hA' : ¬p ∣ A) (h'A : A.normal)
	(h'b : b ∉ A) :
	subgroup.is_complement' A {b} :=
sorry

sylow.normal_of_order_pow_pred {G : Type u} [group G] (p n : ℕ)
	[h : fact (nat.prime p)] (H : subgroup G) [fintype (sylow p ↥H)] :
	H.normal :=
sorry

is_p_sylow.eq_bot_of_triangle {p : ℕ} {G : Type*} [group G]
	(P : sylow p G) (h : ↑P.relindex ↑P.alternating_subgroup = ⊥) :
	P = ⊥ :=
sorry

sylow.eq_of_forall_smul_eq {p : ℕ} {G : Type*} [group G]
	[fact (nat.prime p)] [mul_distrib_mul_action G G] (P : sylow p G)
	(h : ∀ (ϕ : G ≃* G), ↑ϕ.comp ↑P = ↑P) :
	P.to_subgroup = P :=
sorry

exists_inv_eq_self {G : Type*} [group G] [fintype G]
	[h : ∀ (a : G), even (order_of a)] :
	∃ (a : G) (H : a ≠ 0), a = a⁻¹ :=
sorry

abelian_of_group_of_fintype (G : Type*) [group G] [fintype G] :
	is_add_comm_group (G → G) :=
sorry

is_of_fintype.exists_pow_eq_exp {G : Type*} [group G] [fintype G]
	[h : is_of_fintype G] (a : G) :
	∃ (n : ℕ), 0 < n ∧ a ^ n = group.exp a :=
sorry

exists_zpow_eq_exp {G : Type*} [fintype G] [group G]
	[decidable_pred (λ (m : ℤ), 0 < m)] (a : G) :
	∃ (m : ℤ) (H : m > 0), a ^ m = e :=
sorry

abelian_of_three_consecutive_int {G : Type*} [group G]
	(h : ∀ (i j k : ℤ), i ≤ j → k ≤ l → a * b ^ i = a ^ i * b ^ k) :
	is_ abelian G :=
sorry

abelian_of_three_mul_self_pow_eq {G : Type*} [group G]
	(a b : G) (h1 : (a * b) ^ 3 = a ^ 3 * b ^ 3)
	(h2 : (a * b) ^ 5 = a ^ 5 * b ^ 5) :
	is_ abelian G :=
sorry

zpow_mul_inv_mul_pow_inv {G : Type w} [group G] (n : ℤ) [fact (1 < n)]
	(a b : G) :
	(a * b) ^ (n - 1) = e :=
sorry

is_cyclic_of_prime_card {α : Type u} [group α] [fintype α]
	(h : ∀ (p : ℕ), nat.prime p → ¬p < fintype.card α) {p : ℕ} (hp : nat.prime p) :
	is_cyclic α :=
sorry

conj_act.of_conj_act_mul {G : Type*} [div_inv_monoid G] (a x : G) :
	⇑conj_act.of_conj_act (a * x) = ⇑conj_act.of_conj_act a * ⇑conj_act.of_conj_act x :=
sorry

subgroup.mul_centralizer_top {G : Type*} [group G] (M : subgroup G)
	(h : ∀ (x : G), x⁻¹ ∈ M * ↑M) :
	M.centralizer ≤ ⊤ :=
sorry

nat.totient_mul_sub_one_of_one_lt {a : ℕ} (ha : 1 < a) (n : ℕ) :
	n * (a ^ n - 1).totient = (a ^ n - 1).totient :=
sorry

normal_of_comm {G : Type*} [group G] (hnc : ∀ (H : subgroup G), H.normal)
	{a b : G} (h : b * a = a * b) :
	b = a :=
sorry

sylow.normal_of_prime_dvd_of_char_p {G : Type u} [group G] {p : ℕ}
	[hp : fact (nat.prime p)] (hpm : pm.card = p) {H : subgroup G}
	[H.normal] (hH : ∃ (g : G), ↑g ∈ H) :
	char_p G p :=
sorry

add_subgroup.prime_nsmul_induction {G : Type*} [add_group G] {p n : ℕ}
	(h : nat.prime p) (hG : add_subgroup G)
	(hH : ∀ (G : Type*) [add_group G] [_inst_3 : fintype G], fintype.card G = p ^ n → (∃ (H : add_subgroup G), H.is_add_unit))
	(h1 : p ≠ 2) :
	H ≤ add_subgroup.centralizer G p ^ n :=
sorry

three_le_card_of_nonabelian {G : Type*} [group G] [h : nonabelian G]
	[fintype G] :
	3 ≤ fintype.card G :=
sorry

group.is_three_right_inv_three_mul_inv_aux (G : Type*) [group G] :
	(∃ (a b c : G), a * b * c = a * c * b) → group.is_three_right_inv G :=
sorry

lucas_lehmer.subgroup_is_normal (p : ℕ) (w : 0 < p) :
	(lucas_lehmer.X w).subgroup.normal :=
sorry

equiv.perm.is_add_group.of_subgroup_of_equiv {G : Type*} [add_group G]
	[fintype G] {φ : equiv.perm G} (hφ : φ.is_add_group)
	(hφ' : ∀ (x : G), x ∈ add_subgroup.zmultiples φ → ⇑φ x = x⁻¹)
	(hG : is_add_group G) :
	is_add_group G :=
sorry

exists_add_of_exponent_gt_one {G : Type*} [add_group G]
	(h : ∃ (m n : ℕ), m.coprime n ∧ add_order_of m = add_order_of n) :
	∃ (m n : ℕ), m.coprime n ∧ add_order_of m = add_order_of n :=
sorry

monoid.hom.map_triangleleft {G : Type*} [group G] {G' : Type*} [group G']
	(φ : G →* G') (N : G) (h : N ∈ subgroup.map φ G) :
	⇑φ N ∣G' :=
sorry

nonabelian_group.direct_limit_of_iso {V : Type u} [category_theory.category V]
	[category_theory.limits.has_zero_morphisms V] {W : Type u}
	[category_theory.category W] [category_theory.limits.has_zero_morphisms W]
	(f : V → W) [category_theory.limits.has_image f]
	[category_theory.limits.has_cokernel (image_to_kernel f)]
	[category_theory.limits.has_cokernel f] (i : V ≅ V) :
	category_theory.limits.is_limit (nonabelian_group.direct_limit_of_iso f i) :=
sorry

nat.prime.dvd_sub_one_of_nonabelian_of_dvd_sub_one (p q : ℕ)
	(hp : nat.prime p) (hq : q ≠ 0) (hdiv : q ∣ p - 1) :
	∃ (g₁ g₂ : ℕ), g₁ ≠ 0 ∧ g₁ ∣ p * g₂ ∧ g₂ ≠ 0 ∧ g₁ ≠ g₂ :=
sorry

is_cyclic.prod_of_prime_iff {G₁ G₂ : Type*} [group G₁] [group G₂]
	[is_cyclic G₁] [is_cyclic G₂] {m n : ℕ} (hm : nat.prime m) (hn : nat.prime n)
	(h : (G₁ * G₂).cyclic) :
	is_cyclic (m, n) ↔ m.coprime n :=
sorry

equiv.perm.sigma_congr_perm_eq_of_disjoint {α : Type*}
	{σ τ : equiv.perm α} (hστ : σ.disjoint τ) (h : σ = τ) :
	σ.sigma_congr τ = e :=
sorry

real.mul_self_solutions_eq_neg_one :
	(∃ (x : ℝ), x * x = -1) :=
sorry

modular_group.equiv_of_det_ne_zero_apply
	{T : matrix.special_linear_group (fin 2) ℤ} {S₃ : Type*} [field S₃]
	[module ℤ S₃] (h : T.det ≠ 0) (A : ↥(matrix.special_linear_group (fin 2) ℤ)) :
	⇑(modular_group.equiv_of_det_ne_zero h) A = ↑A.mul (↑A)⁻¹ :=
sorry

ring.Prove_ that_mul_self_eq {R : Type*} [ring R]
	(hR : ∀ (x : R), x * x = x) :
	ring.Prove_ that_mul_self_eq :=
sorry

commute.self_mul_left {R : Type x} [mul_zero_class R] {a : R}
	(h : commute (a * a) a) :
	commute a (a * x + x * a) :=
sorry

legendre_symbol.dvd_prime_of_odd_prime_of_dvd (p : ℕ)
	[hp : fact (nat.prime p)] (hp1 : p ≠ 2) (hdiv : ∀ (a b : ℤ), p ∣ a → p ∣ b → a ∣ b)
	(a b : ℤ) :
	p ∣ a → p ∣ b → a ∣ b :=
sorry

lie_algebra.adjoin_root_set_of_eq_bot {R : Type u} {L : Type v}
	[comm_ring R] [lie_ring L] [lie_algebra R L] {a : L} :
	lie_algebra.root_set R L a = ⊥ → a ∈ ⊥ :=
sorry

matrix.to_lin_zero_or_eq_top {R : Type*} [ring R]
	[no_zero_divisors R] (I : ideal R) :
	⇑(matrix.to_lin I) 0 = ⊤ ∨ ⇑(matrix.to_lin I) 0 = ⊤ :=
sorry

zmod.is_quadratic_residue_sub_one_div_two (p : ℕ) [fact (nat.prime p)] :
	(p - 1) / 2 = zmod.legendre_sym p - zmod.legendre_sym p.num / 2 :=
sorry

mod_p.inf_card_pow_is_field_of_irreducible {F : Type*} [field F]
	{p n : ℕ} [hp : fact (nat.prime p)] (q : polynomial F)
	(hq : irreducible (polynomial.map (algebra_map (zmod p) F)) → is_field (zmod p) F)
	(hn : n ≠ 0) :
	is_field (↑p ^ n) (mod_p F q) :=
sorry

{F : Type*} [field F] [fintype F]
	{p q : polynomial F} (hp : irreducible p) (hq : irreducible q)
	(F_Z : fintype.card F = 7) (x : F) :
	⇑(polynomial.aeval x) p = ⇑(polynomial.aeval x) q :=
sorry

polynomial.irreducible_of_irreducible_sum {Q : Type*} [comm_ring Q]
	{p : ℕ} (hp : p ≠ 1) (hQ : irreducible (polynomial.map (algebra_map Q (polynomial ℕ)) p)) :
	irreducible p :=
sorry

polynomial.irreducible_aeval {Q : Type*} [field Q] (f : polynomial Q) :
	irreducible (⇑(polynomial.aeval f) f) :=
sorry

polynomial.irreducible_of_exists_infinite_int_aeval {Q : polynomial ℚ}
	(f : ℚ → ℂ) (x : ℝ) :
	(∃ (a : ℤ), a ≠ 0 ∧ a ≠ ↑(⇑polynomial.aeval x)) ∧ irreducible (polynomial.aeval x) :=
sorry

char_p.add_pow_p_of_ne_zero (F : Type*) [field F] (p : ℕ) [char_p F p]
	(n : ℕ) (a b : F) (h : p ≠ 0) :
	(a + b) ^ p = a ^ p + b ^ p :=
sorry

set_theory.vector_space.exists_of_infinite_space (F : Type*)
	{V : Type*} [field F] [add_comm_group V] [module F V] [infinite_space F]
	[h : ∀ (s : set_theory.submodule F V), s.finite → s ≠ ⊤] :
	∃ (n : ℕ) (s : set_theory.submodule F V), s.finite ∧ s ≠ ⊤ ∧ set_theory.vector_space.restrict_scalars F (n • s) = ⊤ :=
sorry

complex.is_algebraic_cos_one_pow_comp (x : ℂ) :
	is_algebraic ℚ (complex.cos (1 ^ x)) :=
sorry

is_algebraic_of_is_algebraic_sq {K : Type*} [field K] {F : Type*}
	[field F] [algebra K F] (a : K) (h : is_algebraic K (a * a)) :
	is_algebraic F a :=
sorry

algebraic_of_is_root_of_is_cau_gen {a : ℕ} {p : ℕ → ℤ}
	(h : is_root (p a) (algebraic_norm ℚ a)) (hp : p 1 = 0) :
	is_cau_seq has_abs.abs (λ (n : ℕ), (↑n ^ 5 + ↑(real.sqrt_two_add_series p n)) * ↑(real.sqrt_five_series p a) + ↑(real.sqrt_six_series p a) * ↑(real.sqrt_11_series p a) :=
sorry

polynomial.irreducible_aeval_three_sub_three_sub_one {x : ℝ} :
	irreducible (⇑(polynomial.aeval x) (polynomial.X ^ 3 - 3 * polynomial.X - 1)) :=
sorry

char_p.iterate_roots_nodup_of_char_ne_zero {F : Type*} [field F]
	[char_p F p] (m p : ℕ) (hF : char_p F p) (h : ∀ (x : F), x ^ m - x ≠ 0) :
	(polynomial.map (int.cast_ring_hom F) (polynomial.X ^ m - polynomial.X)).roots.nodup :=
sorry