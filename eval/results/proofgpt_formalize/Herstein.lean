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
sorry