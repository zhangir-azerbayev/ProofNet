import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 {G : Type*} [fintype G] [add_group G]
	(h : ∃ (a : G), a ≠ 0 ∧ a = a⁻¹) :
	∃ (a : G), a ≠ 0 ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] [fintype G]
	(hG : group.is_of_fintype G) (a : G) :
	∃ (n : ℕ) (hn : 0 < n), a ^ n = group.exp a :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G]
	(i : ℕ) (h : (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i * (i *:=
sorry

theorem exercise_2_2_6c {G : Type*} [group G] (n : ℕ) (a b : G)
	(h : (a * b) ^ (n - 1) = a ^ n * b ^ (n - 1))
	(h' : ∀ (a b : G), a ^ n * b ^ (n - 1) = a ^ (n - 1) * b ^ (n - 1)) :
	(a * b) ^ n * a ^ (n - 1) = a ^ n * b ^ (n - 1) :=
sorry

theorem exercise_2_3_16 {α : Type u} [group α] [fintype α]
	(h : ∀ (p : ℕ), nat.prime p → ¬is_subgroup α p) (hp : nat.prime p) :
	is_cyclic α :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G]
	(hG : ∀ (H : subgroup G), H.normal) {a b : G} (h : b * a = a ^ b) :
	b = a ^ b :=
sorry

theorem exercise_2_5_31 {G : Type*} [add_group G]
	{p n : ℕ} (m : ℕ) [hp : fact (nat.prime p)] (h : (p ^ n • m).add_subgroup G)
	(H : add_subgroup G) (hH : H.is_add_subgroup) :
	H.is_add_subgroup :=
sorry

theorem exercise_2_5_43 (G : Type*) [add_group G]
	[h : group.is_lie_abelian G] :
	∃ (g : G), ∀ (m n : ℕ), m ≤ n → g + m = g + n :=
sorry

theorem exercise_2_5_52 {G : Type u} [fintype G]
	[add_group G] {φ : G ≃+ G} (hφ : function.semiconj ⇑φ has_add.add has_inv.inv)
	(hφ' : ∀ (x : G), ⇑φ x = x⁻¹)
	(hG : ∀ (y : G), y ∈ add_subgroup.map φ (add_subgroup.top G)) :
	is_add_group G :=
sorry

theorem exercise_2_7_7 {G : Type*} [group G] {G' : Type*} [group G']
	(φ : G →* G') (h1 : ∀ (n : G), ⇑φ n = 1) (h2 : ∀ (m n : G), m * n = m * n)
	(h3 : ∀ (m n : G), m * n = m * n) (h4 : ∀ (m n : G), n * m = n * m) :
	⇑φ (inf_aux n m) = inf_aux (⇑φ n) (⇑φ m) :=
sorry

theorem exercise_2_8_15 {p q : ℕ}
	(hp : nat.prime p) (hq : q ≠ 0) (h : q ∣ p - 1) :
	category_theory.is_iso (p * q) :=
sorry

theorem exercise_2_10_1 {G : Type*} [group G]
	{A : subgroup G} (hA : A.normal) {b : G} (hb : b ∈ A.prime_compl)
	(hnb : b ∉ A) (h : subgroup.is_pwo A) :
	↑A = ⊤ :=
sorry

theorem exercise_2_11_7 {p : ℕ} {G : Type*} [group G]
	(hG : sylow p G) {P : sylow p G} (h : ∀ (ϕ : G ≃* G), ↑P ≤ ϕ) :
	↑P = P :=
sorry

theorem exercise_3_2_21 {α : Type*} {σ τ : equiv.perm α}
	(hστ : σ.disjoint τ) (hτ : τ = σ) :
	σ = τ :=
sorry

theorem exercise_4_1_34 : matrix.special_linear_group (fin 2) ℤ} {S : matrix.special_linear_group (fin 2) ℤ}
	(hT : T.det ≠ 0) (A : matrix.special_linear_group (fin 2) ℤ) :
	⇑(modular_group.equiv_of_det_ne_zero hT) A = ↑A :=
sorry

theorem exercise_4_2_6 {R : Type x} [mul_zero_class R] {a : R}
	(h : commute (has_mul.mul a) a) :
	commute (has_mul.mul a) (has_mul.mul a) :=
sorry

theorem exercise_4_3_1 {R : Type u} {L : Type v} [comm_ring R]
	[lie_ring L] [lie_algebra R L] (a : R) (h : ⁅⇑(algebra_map R L) a⁆ = ⊥) :
	lie_algebra.adjoin_lm R L a h = ⊥ :=
sorry

theorem exercise_4_4_9 (p : ℕ) (w : 0 < p) :
	(p - 1) / 2 = (p - 1) / 2 :=
sorry

theorem exercise_4_5_23 (Fq : Type) [field Fq]
	(p : polynomial Fq) (hp : irreducible p) (q : polynomial Fq)
	(h : ∀ (x : Fq), x ∈ field.ring_sep (p * p.mirror).roots → (⇑(algebra_map (polynomial Fq) (ratfunc Fq)) x).val = q) :
	is_separable (ratfunc Fq) (p * p.mirror).roots :=
sorry

theorem exercise_4_6_2 (Q : polynomial Q) :
	irreducible (polynomial.X ^ 3 + 3 * polynomial.X + 2) :=
sorry

theorem exercise_5_1_8 {F : Type*} [field F] (p : ℕ) [char_p F p]
	(h : p ≠ 0) {m : ℕ} (hm : p ^ m ≠ 0) :
	(λ (a b : F), a + b) ^ m = a ^ m + b ^ m :=
sorry

theorem exercise_5_3_7 {K : Type u} {A : Type v} [field K]
	[ring A] [algebra K A] (a : K) (h : is_algebraic K (⇑(algebra_map K A) a)) :
	is_algebraic F a :=
sorry

theorem exercise_5_4_3 {C : Type*}
	[comm_ring C] (a : C) (p : C → ℕ) (h : (num * a).is_root p)
	(hp : p 0 = 0) (hnum : is_domain C) :
	(is_root.of_num a p).is_root a :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] (p : ℕ) [char_p F p]
	(hF : ∀ (m : ℕ), p ^ m ≠ 0) {m : ℕ} (hm : m ≠ 0) :
	(polynomial.X ^ m - polynomial.X).nth_roots_sub_one = 0 :=
sorry