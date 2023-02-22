import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_2a :
	¬commute (has_star.star ℕ) :=
sorry

theorem exercise_1_1_4 (n : ℕ) [fact (0 < n)] (a b : zmod n) :
	(a * b).val = a.val * b.val :=
sorry

theorem exercise_1_1_15 {G : Type*} [group G] (a : G) (n : ℕ) :
	(group.mk a * a ^ n)⁻¹ = group.mk a⁻¹ * (a ^ n)⁻¹ :=
sorry

theorem exercise_1_1_17 {α : Type*} [fintype α] [group α]
	(x : α) {n : ℕ} (hn : 0 < n) (h : fintype.card α = n) :
	x⁻¹ = (x ^ (n - 1))⁻¹ :=
sorry

theorem exercise_1_1_20 {G : Type u} {x : G} [group G] :
	order_of x = order_of x⁻¹ :=
sorry

theorem exercise_1_1_22b {α : Type u} [add_group α] [linear_order α]
	[covariant_class α α has_add.add has_le.le] (a b : α) :
	|a b| = |b a| :=
sorry

theorem exercise_1_1_29 (A B : Type*) [add_comm_group A] [add_comm_group B] :
	is_add_group.add (A × B) ↔ is_add_group.add A ∧ is_add_group.add B :=
sorry

theorem exercise_1_3_8 {α : Type*}
	[measurable_space α] [topological_space α] [opens_measurable_space α]
	(μ : measure_theory.probability_measure α) [μ.is_inv_invariant] :
	is_greatest ((λ (i : ℕ), (↑i)⁻¹) '' {1, 2, 3, ∀ (n : ℕ), (↑n)⁻¹ < (↑i)⁻¹) μ :=
sorry

theorem exercise_1_6_11 (A B : Group)
	(i : A ⟶ B) :
	(Group.surjective_of_epi_auxs.X_with_infinity.from_coset ⟨A, i⟩).inv ≫ (Group.surjective_of_epi_auxs.X_with_infinity.snd A B).inv = (Group.surjective_of_epi_auxs.X_with_infinity.snd B A).inv ≫ (Group.surjective_of_epi_auxs.X_with_infinity.snd B A).inv :=
sorry

theorem exercise_1_6_23 {α : Type*}
	[fintype α] [add_group α] {σ : equiv.perm α}
	(h1 : fintype.card α = 1) (h2 : σ.is_add_group_of α)
	(h3 : σ ^ 2 = 1) (h4 : σ.symm = σ) :
	is_add_group α :=
sorry

theorem exercise_2_1_13 {H : add_subgroup ℚ}
	(hH : ∀ (x : ℚ), x ∈ H → x ≠ 0 → 1 / ↑x ∈ H) :
	0 / ↑x = H :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G] (M : subgroup G) :
	M < subgroup.closure ↑M ∧ (∀ (H : subgroup G), M ≤ H → H = M) ∧ ∀ (H : subgroup G), M ≤ H → H = M :=
sorry

theorem exercise_2_4_16c {n : ℕ} {G : Type*} [group G]
	(hnc : is_cyclic G) (hn : 1 ≤ n) :
	∃ (H : subgroup G), H.is_maximal :=
sorry

theorem exercise_3_1_22a {G : Type*} [group G] (H K : subgroup G)
	[H.normal] [K.normal] :
	(H ⊓ K).normal :=
sorry

theorem exercise_3_2_8 {G : Type*}
	[add_group G] (H K : add_subgroup G)
	(h : ∀ (h' : add_subgroup G), (add_subgroup.add_order H).prime → (add_subgroup.add_order K).prime)
	[fintype (H ⊓ K)] :
	H ⊓ K = ⊥ :=
sorry

theorem exercise_3_2_16 (p : ℕ) (a : ℤ) :
	⇑(⇑(zmod.zmod_equiv_zpowers p) a) = ↑a ^ p :=
sorry

theorem exercise_3_3_3 : Type*} [group G] [is_dedekind_domain G]
	(v : is_dedekind_domain.height_one_spectrum G) (K : subgroup G) (hK : K ≤ ⊤)
	(p : ℕ) [hp : fact (nat.prime p)] (h : v.int_valuation_def K = p) :
	↑(v.int_valuation_def K) ∨ G = H K ∧ nat.card ↥K ⊣ p :=
sorry

theorem exercise_3_4_4 (n : ℕ+) (h : ∀ (m : ℕ), 0 < m → n ≤ m → n • m = m) :
	n • Cauchy's_theorem :=
sorry

theorem exercise_3_4_5b (G : Type*) [group G]
	[h : is_solvable G] :
	is_solvable (G ⧸ subgroup.top G) :=
sorry

theorem exercise_4_2_8 {G : Type*}
	[group G] (H : subgroup G) (n : ℕ) (hn : H.index ≠ 0) :
	∃ (K : subgroup G) (hK : K ≤ H) (hK' : K.normal), hK'.card ≤ n.factorial :=
sorry

theorem exercise_4_2_9a {p : ℕ+} {G : Type*}
	[group G] (h : ∃ (α : ℕ+), is_prime ↑α ∧ subgroup.zpowers α = ⊤) :
	subgroup.normal G :=
sorry

theorem exercise_4_4_2 (G : Type*) [add_group G]
	{p q : ℕ} (h : nat.prime p) (hne : p ≠ 0) (hq : nat.prime q)
	(hG : ∀ (g : G), g ≠ 0 → g ≠ 0 ∧ p ∣ g + q) :
	is_add_cyclic G :=
sorry

theorem exercise_4_4_6b (k : ℤ) :
	∃ (H : subgroup ℤ), is_normal_subgroup k ∧ ¬is_char_subgroup k :=
sorry

theorem exercise_4_4_8a {G : Type*} [group G]
	{H K : subgroup G} (hH : H ≤ K) [hN : (subgroup.comap K.subtype H).normal]
	[hK : (subgroup.comap K.subtype H).normal] (h : H.characteristic) :
	K.normal :=
sorry

theorem exercise_4_5_13 {G : Type u} [group G]
	[fintype G] {p : ℕ} [hp : fact (nat.prime p)] (hdvd : p ∣ fintype.card G)
	(P : sylow p G) :
	∃ (g : G), ↑P = g :=
sorry

theorem exercise_4_5_15 {p : ℕ} {G : Type*} [group G]
	(hG : nat.prime p) (hG' : p ∣ group.card G) :
	∃ (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_17 {G : Type*}
	[group G] (h : is_galois G 105) :
	is_normal_sylow (subgroup.normal_sylow G) ∧ is_normal_sylow ↑(subgroup.normal_sylow G) :=
sorry

theorem exercise_4_5_19 (G : Type*) [group G] [fintype G] :
	(fintype.card G).factorization.support = ∅ :=
sorry

theorem exercise_4_5_21 (G : Type*) [group G]
	[fintype G] :
	(fintype.card G).gcd (λ (g : G), 1) = 1 ∨ ∃ (m : ℕ), fintype.card G = m ∧ g = 1 ∧ m < fintype.card G :=
sorry

theorem exercise_4_5_23 462 {G : Type*} [group G] [fintype G] :
	(finset.filter (λ (g : G), ∃ (m : ℕ), m ∈ finset.Ico 0 (fintype.card G)) finset.univ).card = 462 → ¬simple G :=
sorry

theorem exercise_4_5_33 {p : ℕ} {G : Type*} [group G]
	(P : sylow p G) (H : subgroup G) (hP : ↑P.normal)
	(h : (↑P ∩ H).to_subgroup = ⊤) :
	P = subgroup.comap (sylow.subtype H) ⊤ :=
sorry

theorem exercise_7_1_2 {α : Type*} [ring α] {u : αˣ} :
	is_unit ↑u → is_unit (-u) :=
sorry

theorem exercise_7_1_12 (F : Type*) [field F] {x : F} :
	is_integral_subring ⊤ x :=
sorry

theorem exercise_7_2_2 {R : Type u} [comm_semiring R]
	{p : polynomial R} {a : ℕ → R} :
	(∃ (b : R), b ≠ 0 ∧ polynomial.eval₂ (algebra_map R (polynomial R)) b p = 0) ↔ p.div_by_monic a = 0 :=
sorry

theorem exercise_7_3_16 {R : Type u} {S : Type v} [ring R]
	[ring S] (φ : R →+* S) (hφ : function.surjective ⇑φ)
	(h : subring.center R ≤ subring.comap φ (subring.center S)) :
	subring.center S ≤ subring.comap φ (subring.center R) :=
sorry

theorem exercise_7_4_27 {R : Type u} [comm_ring R]
	[nontrivial R] (h : is_nilpotent (is_localization.mk' R 1)) (b : R) :
	is_unit (1 - ⇑(algebra_map R (localization.away 1)) b) :=
sorry

theorem exercise_8_2_4 {R : Type*}
	[comm_ring R] [id : is_domain R] [iic : is_principal_ideal_ring R]
	(hR : is_integral R) (hR1 : ∀ {a b : R}, a ≠ 0 → b ≠ 0 → is_least_greatest (gcd_monoid.gcd a b) 1 → a * b ∣ a) :
	∃ (N : ℕ), ∀ (n : ℕ), n ≥ N → is_unit (a ^ n) * a ^ N :=
sorry

theorem exercise_8_3_5a {R : Type*} [comm_ring R] [is_domain R]
	[is_principal_ideal_ring R] {n : ℕ} (hn3 : 3 ≤ n) (hR : is_irreducible (zsqrtd.sqrt ↑n)) :
	is_irreducible (zsqrtd.sq_irreducible n hR) :=
sorry

theorem exercise_8_3_6b (q : ℕ) (h : lucas_lehmer.int.prime q)
	(hq : ↑q ≠ 2) :
	↑(q ^ 2 * (lucas_lehmer.int.fst q h)) = ↑(q ^ 2) * ↑(q ^ 2) :=
sorry

theorem exercise_9_1_10 (x : ℕ → ℤ) :
	(∀ (n : ℕ), (zmod.int_cast x n).is_prime) → (∃ (n : ℕ), (zmod.int_cast x n).is_prime) :=
sorry

theorem exercise_9_4_2a (x : ℕ) :
	irreducible (zmod.c4 x) :=
sorry

theorem exercise_9_4_2c (x : ℤ) :
	irreducible (zmod.χ₄ ↑x) :=
sorry

theorem exercise_9_4_9 {F : Type*}
	[field F] [fintype F] :
	(polynomial.X ^ 2 - ⇑polynomial.C (real.sqrt 2)).is_U_F_D :=
sorry

theorem exercise_11_1_13 (α : Type*)
	{m : measurable_space α} {M : Type*} [add_comm_monoid M] [topological_space M]
	[t2_space M] {n : ℕ} (hn : n ≠ 0) :
	⇑(measure_theory.vector_measure.of_nat_eq α M hn) = ⇑(measure_theory.vector_measure.of_real_nat_cast hn) :=
sorry