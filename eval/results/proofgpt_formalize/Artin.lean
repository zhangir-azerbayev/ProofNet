import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory

universe u 

theorem exercise_10_1_13 {R : Type u} [ring R] {x : R} : x ≠ 0 → (∃ (n : ℕ), x ^ n = 0) → is_unit (1 + x) :=
sorry

theorem exercise_10_2_4 (R : Type*) [ring R] [char_zero R] : ↑(bit1) = bit1 :=
sorry

theorem exercise_10_4_6 {R : Type u} [ring R] {I J : ideal R} (h : ∀ (x : R), x ∈ I * J → is_nilpotent (R / ideal.map (ideal.quotient.mk I) x)) : is_nilpotent (ring_hom.ker (ideal.map (ideal.quotient.mk I) J)) :=
sorry

theorem exercise_10_4_7a {R : Type u} [ring R] {I J : ideal R} (h : I ⊔ J = ⊤) : I J = I ⊓ J :=
sorry

theorem exercise_10_5_16 proves_irreducible_iff (F : Type*) [field F] [fintype F] [decidable_eq F] : (∃ (x : F), irreducible (polynomial.X - ⇑polynomial.C x)) ↔ char_two F :=
sorry

theorem exercise_10_6_7 (R : Type*) [comm_ring R] (abv : absolute_value R ℤ) {x : R} (hx : x ≠ 0) : ∃ (n : ℕ), x ^ n = 0 :=
sorry

theorem exercise_10_7_10 {R : Type u} [ring R] {M : ideal R} (hM : ∀ (x : R), x ∈ M → ∀ (y : R), y ∈ M → x⁻¹ = y⁻¹ → x = y) (h : M.is_maximal) : M.is_maximal :=
sorry

theorem exercise_10_7_6 (R : Type*) [comm_ring R] [is_domain R] : is_field ↥(polynomial.gal.root_set R 5) :=
sorry

theorem exercise_11_12_3 (p : ℕ) (x y : ℝ) (h : x ^ 2 - 5 = p) : ∃ (z : ℤ) (H : z ∈ {z : ℤ | x ^ 2 + 5 = p ∨ 2 * x ^ 2 + 2 * x * y + 3 * y ^ 2 = p}), ↑z - x = y ∨ ↑z + x = -y ∨ ↑z + y = p :=
sorry

theorem exercise_11_13_3 (p : ℕ) [fact (nat.prime p)] : 4 ≠ 3 :=
sorry

theorem exercise_11_2_13 {a b : ℤ} : a ∣ b → ↑a ∣ ↑b :=
sorry

theorem exercise_11_3_1 {F : Type u} [field F] {a b : F} (ha : a ≠ 0) {f : polynomial F} (hf : irreducible (polynomial.map (algebra_map F a) f)) : irreducible f :=
sorry

theorem exercise_11_3_4 {p q : polynomial ℤ} : (∃ (ifp : ℤ), p.nat_abs = q.nat_abs ∧ p.coeff 0 = q.coeff 0 ∧ p.monic) ↔ (ideal.span {p}).is_principal.gcd (ideal.span {q}) = ⊤ :=
sorry

theorem exercise_11_4_1b (q : ℚ) : irreducible (⇑(polynomial.aeval q) (polynomial.X ^ 3 + 6 * polynomial.X + 12)) :=
sorry

theorem exercise_11_4_6a (x : ℤ) : irreducible (x ^ 2 + x + 1) :=
sorry

theorem exercise_11_4_6b {x : ℤ} : irreducible (x ^ 2 + 1) :=
sorry

theorem exercise_11_4_6c (x : ℤ) : irreducible (fermat_42.pow x 3 - ↑9) :=
sorry

theorem exercise_11_4_8 {p : ℕ} (hp : nat.prime p) (n : ℕ) (h : irreducible (polynomial.cyclotomic p ℚ)) : irreducible (polynomial.cyclotomic (↑n - p) ℚ) :=
sorry

theorem exercise_13_4_10 (k : ℕ) {r : ℕ} (p : ℤ) (h : nat.prime p) : ↑(p.digits k) + 1 = ↑(p.digits (2 ^ k + 1)) :=
sorry

theorem exercise_13_6_10 (K : Type*) [field K] [fintype K] : (finset.univ.prod (λ (k : K), (finset.Ioi k).prod (λ (m : K), m * k)⁻¹)) :=
sorry

theorem exercise_2_11_3 (G : Type*) [group G] : even (G ⧸ subgroup.center G) :=
sorry

theorem exercise_2_2_9 {G : Type*} [add_group G] {H : add_subgroup G} {a b : G} (h : a * b = b * a) : ∃ (h' : a + b = 0), H = add_subgroup.comap (add_monoid_hom.id G) H :=
sorry

theorem exercise_2_3_1 (x : ℝ) : real.vadd_P x.to_add_group = add_group.vadd x :=
sorry

theorem exercise_2_3_2 {α : Type u} [group α] (a b : α) : is_conj (a, b) (b, a) :=
sorry

theorem exercise_2_4_19 (G : Type*) [group G] [topological_space G] [topological_group G] [t2_space G] : ∃ (x : G), x * x = 1 :=
sorry

theorem exercise_2_8_6 (G H : Type*) [group G] [group H] : group.center (G × H) = group.center G × group.center H :=
sorry

theorem exercise_3_2_7 (F : Type*) [field F] {E : Type*} [field E] [algebra F E] : function.injective ⇑(algebra_map F E) :=
sorry

theorem exercise_3_5_6 (V : Type u) [add_comm_group V] [module ℝ V] [finite_dimensional ℝ V] (h : ∀ (s : set V), linear_independent ℝ coe → s.finite ∨ s.countably_generated) : ∃ (n : ℕ), s.finite ∧ cardinal.mk ↥(finset.filter (λ (x : V), x < n) (finset.range n)) = cardinal.mk V :=
sorry

theorem exercise_3_7_2 {F : Type u} {V : Type v} [field F] [add_comm_group V] [module F V] [infinite_dimensional F V] : ¬∃ (s : set V), s.finite ∧ ∃ (t : finset V), s ⊆ ↑t ∧ t.card < finite_dimensional.finrank F V :=
sorry

theorem exercise_6_1_14 {G : Type u} [group G] (Z : set G) (hZ : is_cyclic (G ⧸ Z)) : ∃ (g : G), ∀ (z : G), z ∈ Z → g * z = g :=
sorry

theorem exercise_6_4_12 (A : Type*) [comm_group A] [fintype A] : is_simple_prover A 0 :=
sorry

theorem exercise_6_4_2 (p q : ℕ) : lucas_lehmer.X.simple p = lucas_lehmer.X.simple q :=
sorry

theorem exercise_6_4_3 (p q : ℕ) : lucas_lehmer.X.simple (p ^ 2 * q) = 0 :=
sorry

theorem exercise_6_8_1 {G : Type*} [group G] (a b : G) : ⅟ a = b * b * a⁻¹ :=
sorry

theorem exercise_6_8_4 {α : Type u} [group α] (x y z : α) : free_group.mk (x * y * z) * free_group.mk (y * z) = 1 :=
sorry

theorem exercise_6_8_6 {G : Type*} [group G] {N : subgroup G} [hN : N.normal] (h1 : is_cyclic G) (h2 : is_cyclic (G ⧸ N)) : ∃ (a b : G), a ∈ N ∧ b ∈ N ∧ a * b = a :=
sorry