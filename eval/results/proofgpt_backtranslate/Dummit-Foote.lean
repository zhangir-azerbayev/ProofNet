import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory

universes u v w 



theorem exercise_1_1_2a (a b : ℤ) :
	has_star.star a ≠ -b :=
sorry

theorem exercise_1_1_4 (n : ℕ) (a b : zmod n) :
	⇑(zmod.mul_hom (zmod n)) (a * b) = a * ⇑(zmod.mul_hom (zmod n)) b :=
sorry

theorem exercise_1_1_15 {G : Type*} [comm_group G] (n : ℕ)
	(a : G) :
	(a * a * a * a)⁻¹ = a⁻¹ * (a * a)⁻¹ * (a * a)⁻¹ :=
sorry

theorem exercise_1_1_17 {G : Type*} [div_inv_monoid G] (x : G)
	{n : ℕ} (hn : 0 < n) (h : x.nat_abs = n) :
	x⁻¹ = x ^ (n - 1) :=
sorry

theorem exercise_1_1_20 {G : Type u} [group G] (x : G) :
	order_of x = order_of x⁻¹ :=
sorry

theorem exercise_1_1_22b {G : Type*} [add_group G] (a b : G) :
	add_commute |a| |b| :=
sorry

theorem exercise_1_1_29 {α β : Type*} [add_comm_group α]
	[add_comm_group β] {a : α} {b : β} :
	a +ᵥ b = b + a :=
sorry

theorem exercise_1_3_8 (Γ : Type*)
	[linear_ordered_comm_group Γ] [infinite Γ] :
	infinite_group (part_enat.S Γ) :=
sorry

theorem exercise_1_6_11 {α β : Type*} (e : α ≃ β) :
	e.prod_comm.symm = e.symm.prod_comm :=
sorry

theorem exercise_1_6_23 {α : Type u} [add_group α] [fintype α]
	{σ : α → α} (hσ : ∀ (g : α), ⇑σ g = g ↔ g = 1)
	(hσ_two : function.inv_fun σ ^ 2 = id) :
	is_add_cyclic α :=
sorry

theorem exercise_2_1_13 {H : add_subgroup (rat.with_top ℚ)}
	(hH : ∀ (x : ℚ), x ∈ H → x ≠ 0 → 1 / x ∈ H) :
	H = 0 ∨ add_subgroup.rat ℚ :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G] (M : subgroup G) :
	M.is_maximal ↔ M ≠ ⊤ ∧ ∀ (H : subgroup G), M ≤ H → H = M ⊔ H :=
sorry

theorem exercise_2_4_16c {α : Type u} [group α]
	[is_cyclic α] [fintype α] {x : α} (hx : x ≠ 1) :
	is_maximal (subgroup.zpowers x) ↔ ∃ (p : ℕ), nat.prime p ∧ p ∣ fintype.card α ∧ subgroup.zpowers x = ⊤ :=
sorry

theorem exercise_3_1_22a {G : Type*} [group G] {H K : subgroup G}
	(hH : H.normal) (hK : K.normal) :
	(H ⊓ K).normal :=
sorry

theorem exercise_3_2_8 {G : Type*} [group G]
	{H K : subgroup G} [fintype ↥H] [fintype ↥K]
	(h : (fintype.card ↥H).rel (fintype.card ↥K) = 1) :
	H ⊔ K = ⊥ :=
sorry

theorem exercise_3_2_16 (p : ℕ) [fact (nat.prime p)]
	(a : (zmod p)ˣ) :
	↑a ^ p = ↑a % ↑p :=
sorry

theorem exercise_3_3_3 {G : Type*} [group G]
	{H : subgroup G} [H.normal] {p : ℕ} [hp : fact (nat.prime p)]
	(hH : H.index ≠ 0) :
	(∃ (K : subgroup G), K.normal ∧ K = H ∨ K.card = p) ↔ H.normal :=
sorry

theorem exercise_3_4_4 {G : Type u}
	[add_comm_group G] [fintype G] {n : ℕ} (h0 : 0 < n)
	(h1 : ∀ (m : ℕ), 0 < m → m ∣ n → m ≠ 0 → m = n) :
	∃ (H : add_subgroup G), H.finrank = n ∧ ∀ (n : ℕ), 0 < n → n ∣ fintype.card ↥H :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] :
	is_solvable (quotient_group G) :=
sorry

theorem exercise_4_2_8 {G : Type*} [group G] {H : subgroup G}
	(hH : H.index ≠ 0) :
	∃ (n : ℕ), H.normal ∧ fintype.card ↥H ≤ n ∧ n.factorial ≤ fintype.card G :=
sorry

theorem exercise_4_2_9a {G : Type u} [group G] {p : ℕ} [hp : fact (nat.prime p)]
	{α : ℕ} (hα : p ^ α ∣ fintype.card G) :
	(subgroup.of p G).normal :=
sorry

theorem exercise_4_4_2 {α : Type u} [add_group α] {p q : ℕ}
	(hpq : p ≠ 2) (h : p ≠ ⊤) (x : α) (hx : x ≠ 0) :
	is_add_cyclic α :=
sorry

theorem exercise_4_4_6b (G : Type*) [group G] :
	∃ (H : subgroup G), ¬H.characteristic :=
sorry

theorem exercise_4_4_8a {G : Type*} [group G]
	{H K : subgroup G} [hH : H.characteristic] [hK : K.normal] (hHK : H ≤ K) :
	K.normal :=
sorry

theorem exercise_4_5_13 {G : Type u} [group G]
	[fintype G] (h2 : 2 < fintype.card G) :
	∃ (p : ℕ), p ∣ fintype.card G ∧ ∃ (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_15 {G : Type u} [group G]
	[fintype G] (hG : fintype.card G ∣ fintype.card G) {p : ℕ} [hp : fact (nat.prime p)]
	(hdvd : p ∣ fintype.card G) :
	∃ (P : sylow p G), ↑P.normal :=
sorry

theorem exercise_4_5_17 {G : Type u} [group G]
	(h : fintype.card G = 3) :
	∃ (P : sylow 3 G), P.normal ∧ P.sylow_subgroup = ⊥ :=
sorry

theorem exercise_4_5_19 {α : Type*} [group α]
	(h : fintype.card α = 6545) :
	¬simple_group α :=
sorry

theorem exercise_4_5_21 {α : Type u}
	[group α] [fintype α] (h : fintype.card α = 3) :
	¬simple_group α :=
sorry

theorem exercise_4_5_23 {α : Type*} [group α]
	[fintype α] (h : fintype.card α = M * M) :
	¬simple_group α :=
sorry

theorem exercise_4_5_33 {p : ℕ} {G : Type*} [group G] {P : sylow p G}
	{H : subgroup G} (hP : ↑P.normal) :
	↑(P ⊓ H) = ↑P ∩ ↑H :=
sorry

theorem exercise_7_1_2 {α : Type u} [ring α] {u : αˣ} :
	is_unit ↑u → is_unit (-↑u) :=
sorry

theorem exercise_7_1_12 {R : Type*} [field R] [subring R] :
	is_integral R ↥⊥ :=
sorry

theorem exercise_7_2_2 {R : Type u} [comm_semiring R]
	{p : polynomial R} {a : ℕ} (hp : p.degree ≠ 0) :
	(∃ (b : R), b ≠ 0 ∧ polynomial.eval b p = 0) ↔ p.is_root a :=
sorry

theorem exercise_7_3_16 {R : Type u} {S : Type v}
	[ring R] [ring S] (φ : R →+* S) (hφ : function.surjective ⇑φ)
	(h : subring.center R ≤ subring.center S) :
	subring.map φ (subring.center R) ≤ subring.center S :=
sorry

theorem exercise_7_4_27 {R : Type u} [comm_ring R]
	[nontrivial R] {a : R} (h : is_nilpotent a) (b : R) :
	is_unit (1 - a * b) :=
sorry

theorem exercise_8_2_4 {R : Type u}
	[comm_ring R] [is_domain R] [is_principal_ideal_ring R] (hR : algebra.is_integral R)
	(h : ∀ (a b : R), a ≠ 0 → b ≠ 0 → (∃ (r s : R), r * a + s * b = 1 ∧ ∀ (i : ℕ), i ≤ r * a.val + s * b.val → is_unit (a.val * b.val)) :
	∃ (N : ℕ), ∀ (n : ℕ), n ≥ N → is_unit (a.val * b.val) :=
sorry

theorem exercise_8_3_5a {R : Type} [comm_ring R] [is_domain R]
	(n : ℕ) (hn : 3 < n) :
	irreducible (2 * ⟨-n, _⟩) ∧ irreducible (⟨-n, _⟩ + ⟨-n, _⟩) :=
sorry

theorem exercise_8_3_6b (q : ℕ)
	[fact (nat.prime q)] (hq3 : q % 4 = 3) :
	fintype.card (zmod q) ^ 2 ≠ 3 :=
sorry

theorem exercise_9_1_10 {R : Type u} [comm_ring R]
	(s : multiset R) [is_domain R] :
	∃ (p : prime_spectrum R), p.fst ∈ s ∧ p.snd ∈ s ∧ ∀ (p' : prime_spectrum R), p' ∈ s → p'.fst * p'.snd = p.fst * p'.snd → p'.fst = p.fst ∧ p'.snd = p.snd :=
sorry

theorem exercise_9_4_2a : ℤ} (hx : x ≠ 0) :
	irreducible (x ^ 4 - 4 * x ^ 3 + 6) :=
sorry

theorem exercise_9_4_2c (x : ℤ) :
	irreducible (x ^ 4 + 4 * x ^ 3 + 6 * x ^ 2 + 2 * x + 1) :=
sorry

theorem exercise_9_4_9 {d : ℕ} (hd : d ≠ 0) :
	irreducible (zsqrtd.sqrtd * zsqrtd.sqrtd - ↑d) :=
sorry

theorem exercise_11_1_13 (n : ℕ) :
	vector_ex_rat (n + 1) = vector_ex (n + 1) ⊤ :=
sorry