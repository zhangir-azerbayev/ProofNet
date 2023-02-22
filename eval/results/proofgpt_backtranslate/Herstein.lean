import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18 {G : Type*} [fintype G] [has_inv G] (h2 : even (fintype.card G)) :
	∃ (a : G), a ≠ e ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 {α : Type u} [group α] [fintype α]
	(a : α) :
	∃ (n : ℕ) (H : n > 0), a ^ n = e :=
sorry

theorem exercise_2_2_3 {α : Type u} [group α] {i : ℤ}
	(h1 : i ≠ 1) (h2 : i ≠ 0) (h3 : i ≠ i * 1) :
	is_add_cyclic α :=
sorry

theorem exercise_2_2_6c {G : Type w} [group G]
	(n : ℤ) (a b : G) (h : ∀ (n : ℤ), a ^ n = b ^ n) (h' : 1 < n) :
	(a * b * a⁻¹ * b⁻¹) ^ (n * (n - 1)) = e :=
sorry

theorem exercise_2_3_16 {α : Type u} [group α]
	[fintype α] [is_cyclic α] (p : ℕ) [hp : fact (nat.prime p)]
	(h : ∀ (H : subgroup α), H.is_proper → H = ⊤) :
	is_cyclic α :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G]
	[hG : group.is_normal G] {a b : G} (h : a ≤ b) :
	b * a = a ^ j * b :=
sorry

theorem exercise_2_5_31 {G : Type*}
	[add_group G] {n : ℕ} [hp : fact (nat.prime p)] {H : add_subgroup G}
	(hH : fintype.card ↥H = p ^ n) :
	H.is_characteristic :=
sorry

theorem exercise_2_5_43 {G : Type u}
	[add_comm_group G] [fintype G] (h : fintype.card G = 3) :
	add_comm_group.add_action.is_abelian G :=
sorry

theorem exercise_2_5_52 {α : Type u} [fintype α]
	[decidable_eq α] {φ : add_aut α} (hφ : φ.is_add_cyclic)
	(h3 : 3 ≤ 4 * (fintype.card α)) (y : α) :
	⇑φ y = y⁻¹ :=
sorry

theorem exercise_2_7_7 {G : Type*} [group G] {G' : Type*}
	[group G'] (ϕ : G →* G') {N : subgroup G} (hN : N ≤ ϕ.ker) :
	subgroup.map ϕ.to_monoid_hom N = (ϕ.of_le hN).normal :=
sorry

theorem exercise_2_8_15 {p q : ℕ} (hp : nat.prime p)
	(hq : nat.prime q) (h : q ∣ p - 1) {G : Type*} [group G] :
	noncomm_group.of (p * q) G → noncomm_group.of p G :=
sorry

theorem exercise_2_10_1 {α : Type u} [group α]
	(A : subgroup α) (p : ℕ) [hp : fact (nat.prime p)] (b : α)
	(hb : order_of b = p) (hA : ↑p ∈ A) (h : b ∉ A) :
	↑p ∩ ⟨b, hb⟩ = {e} :=
sorry

theorem exercise_2_11_7 {G : Type u} [group G] {P : sylow G}
	(h : P.is_adjacent) :
	(∀ (ϕ : G ≃* G), subgroup.map ϕ.to_monoid_hom P = P) → P.to_subgroup = ⊤ :=
sorry

theorem exercise_3_2_21 {α : Type*}
	{σ τ : equiv.perm α} (e : equiv.perm α) (h1 : σ.prod = 1) (h2 : τ.prod = 1)
	(h3 : σ * τ = e) :
	σ = e :=
sorry

theorem exercise_4_1_34 {n : Type u} [decidable_eq n]
	[fintype n] {R : Type v} [comm_ring R] (A : matrix.special_linear_group n R) :
	↑(⇑matrix.special_linear_group.to_GL A) = (⇑matrix.special_linear_group.to_lin' A).to_linear_map :=
sorry

theorem exercise_4_2_6 {R : Type x} [mul_zero_class R] [has_mul R]
	{a : R} (h : a * a = 0) :
	commute a a :=
sorry

theorem exercise_4_3_1 {R : Type u} [comm_ring R] {a : R} :
	is_ideal (ideal.span {a}) :=
sorry

theorem exercise_4_4_9 (p : ℕ) [fact (nat.prime p)] :
	(p - 1) / 2 ∣ (p - 1) / 2 :=
sorry

theorem exercise_4_5_23 {F : Type*} [field F] (7 : ℕ)
	(3 : polynomial F) (X : fin 3 → F) (hX : ∀ (x : fin 3), X x ≠ 0)
	(hX' : ∀ (x : fin 3), x ≠ 0 → X x = 0) :
	algebra.adjoin F {p : fin 3 → F | irreducible p} = ⊥ :=
sorry

theorem exercise_4_6_2 {R : Type*} [comm_ring R]
	(f : polynomial R) :
	irreducible (polynomial.quaternion f) :=
sorry

theorem exercise_5_1_8 {F : Type*} [field F] (p : ℕ) [HF : char_p F p]
	{n : ℕ} (m : ℕ) (a b : F) (hm : p ^ n ≠ 0) :
	(a + b) ^ m = a ^ m + b ^ m :=
sorry

theorem exercise_5_3_7 {F K : Type*} [field F] [field K]
	[algebra F K] (a : K) (ha : is_algebraic F (a * a)) :
	is_algebraic F a :=
sorry

theorem exercise_5_4_3 {a : ℚ} (ha : polynomial.eval a (polynomial.map (algebra_map ℚ ℚ) (polynomial.X ^ 5 + polynomial.X ^ 3 + polynomial.X ^ 2 + polynomial.X + 1)) = 0) :
	is_algebraic ℚ a :=
sorry

theorem exercise_5_6_14 {F : Type u} [field F]
	{p : ℕ} [HF : char_p F p] {n : ℕ} (m : ℕ) (h : m ≠ 0) :
	(polynomial.X ^ m - polynomial.X).roots.nodup :=
sorry