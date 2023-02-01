

proj_iso_unit_pow_one_add_self {R : Type u} [ring R] {x : R} :
	x ≠ 0 → (∃ (n : ℕ), x ^ n = 0) → is_unit (1 + x) :=
sorry

polynomial.int_cast_bit1 (R : Type*) [ring R] [char_zero R] :
	↑(bit1) = bit1 :=
sorry

ideal.ker_jacobson_mul_is_nilpotent {R : Type u} [ring R] {I J : ideal R}
	(h : ∀ (x : R), x ∈ I * J → is_nilpotent (R / ideal.map (ideal.quotient.mk I) x)) :
	is_nilpotent (ring_hom.ker (ideal.map (ideal.quotient.mk I) J)) :=
sorry

ideal.eq_inf_of_coprime_left {R : Type u} [ring R] {I J : ideal R}
	(h : I ⊔ J = ⊤) :
	I J = I ⊓ J :=
sorry

char_two. proves_irreducible_iff (F : Type*) [field F] [fintype F]
	[decidable_eq F] :
	(∃ (x : F), irreducible (polynomial.X - ⇑polynomial.C x)) ↔ char_two F :=
sorry

absolute_value.exists_int_coeff_nonzero_mem_range (R : Type*)
	[comm_ring R] (abv : absolute_value R ℤ) {x : R} (hx : x ≠ 0) :
	∃ (n : ℕ), x ^ n = 0 :=
sorry

maximal_ideal_of_unique {R : Type u} [ring R] {M : ideal R}
	(hM : ∀ (x : R), x ∈ M → ∀ (y : R), y ∈ M → x⁻¹ = y⁻¹ → x = y) (h : M.is_maximal) :
	M.is_maximal :=
sorry

polynomial.gal.is_field_coe_add_root_set_five (R : Type*) [comm_ring R]
	[is_domain R] :
	is_field ↥(polynomial.gal.root_set R 5) :=
sorry

real.pell_zd_aux (p : ℕ) (x y : ℝ) (h : x ^ 2 - 5 = p) :
	∃ (z : ℤ) (H : z ∈ {z : ℤ | x ^ 2 + 5 = p ∨ 2 * x ^ 2 + 2 * x * y + 3 * y ^ 2 = p}), ↑z - x = y ∨ ↑z + x = -y ∨ ↑z + y = p :=
sorry

mod_four_ne_three_balls (p : ℕ) [fact (nat.prime p)] :
	4 ≠ 3 :=
sorry

has_dvd.dvd.to_int_add_comm_group {a b : ℤ} :
	a ∣ b → ↑a ∣ ↑b :=
sorry

polynomial.irreducible_of_irreducible_map_X_add_C {F : Type u} [field F]
	{a b : F} (ha : a ≠ 0) {f : polynomial F} (hf : irreducible (polynomial.map (algebra_map F a) f)) :
	irreducible f :=
sorry

polynomial.int_fract_pair.exists_int_fract_pair_iff_exists_int_fract_pair
	{p q : polynomial ℤ} :
	(∃ (ifp : ℤ), p.nat_abs = q.nat_abs ∧ p.coeff 0 = q.coeff 0 ∧ p.monic) ↔ (ideal.span {p}).is_principal.gcd (ideal.span {q}) = ⊤ :=
sorry

polynomial.irreducible_aeval_three_add_six (q : ℚ) :
	irreducible (⇑(polynomial.aeval q) (polynomial.X ^ 3 + 6 * polynomial.X + 12)) :=
sorry

fermat_42.irreducible (x : ℤ) :
	irreducible (x ^ 2 + x + 1) :=
sorry

fermat_42.irreducible {x : ℤ} :
	irreducible (x ^ 2 + 1) :=
sorry

fermat_42.irreducible_pow_three_sub_9 (x : ℤ) :
	irreducible (fermat_42.pow x 3 - ↑9) :=
sorry

polynomial.cyclotomic_irreducible_rat {p : ℕ} (hp : nat.prime p)
	(n : ℕ) (h : irreducible (polynomial.cyclotomic p ℚ)) :
	irreducible (polynomial.cyclotomic (↑n - p) ℚ) :=
sorry

int.prime.digits_sum (k : ℕ) {r : ℕ} (p : ℤ)
	(h : nat.prime p) :
	↑(p.digits k) + 1 = ↑(p.digits (2 ^ k + 1)) :=
sorry

finite_field.prod_inv_one_of_nonzero (K : Type*) [field K] [fintype K] :
	(finset.univ.prod (λ (k : K), (finset.Ioi k).prod (λ (m : K), m * k)⁻¹)) :=
sorry

even_div_two_subset_even_odd (G : Type*) [group G] :
	even (G ⧸ subgroup.center G) :=
sorry

add_subgroup.exists_neg_comm_of_eq {G : Type*} [add_group G]
	{H : add_subgroup G} {a b : G} (h : a * b = b * a) :
	∃ (h' : a + b = 0), H = add_subgroup.comap (add_monoid_hom.id G) H :=
sorry

real.vadd_P_to_add_group (x : ℝ) :
	real.vadd_P x.to_add_group = add_group.vadd x :=
sorry

prod.is_conj_swap {α : Type u} [group α] (a b : α) :
	is_conj (a, b) (b, a) :=
sorry

submonoid.mem_center_of_two_le (G : Type*) [group G]
	[topological_space G] [topological_group G] [t2_space G] :
	∃ (x : G), x * x = 1 :=
sorry

group.center_prod_eq (G H : Type*) [group G] [group H] :
	group.center (G × H) = group.center G × group.center H :=
sorry

field.injective_algebra_map (F : Type*) [field F] {E : Type*} [field E]
	[algebra F E] :
	function.injective ⇑(algebra_map F E) :=
sorry

exists_linear_independent_bounded_of_infinite (V : Type u)
	[add_comm_group V] [module ℝ V] [finite_dimensional ℝ V]
	(h : ∀ (s : set V), linear_independent ℝ coe → s.finite ∨ s.countably_generated) :
	∃ (n : ℕ), s.finite ∧ cardinal.mk ↥(finset.filter (λ (x : V), x < n) (finset.range n)) = cardinal.mk V :=
sorry

infinite_dimensional.not_exists_finite_subspace_lt_top {F : Type u}
	{V : Type v} [field F] [add_comm_group V] [module F V] [infinite_dimensional F V] :
	¬∃ (s : set V), s.finite ∧ ∃ (t : finset V), s ⊆ ↑t ∧ t.card < finite_dimensional.finrank F V :=
sorry

abelianization_of_of_is_cyclic_center {G : Type u} [group G]
	(Z : set G) (hZ : is_cyclic (G ⧸ Z)) :
	∃ (g : G), ∀ (z : G), z ∈ Z → g * z = g :=
sorry

is_simple_prover (A : Type*) [comm_group A] [fintype A] :
	is_simple_prover A 0 :=
sorry

lucas_lehmer.X.simple_of_empty_group (p q : ℕ) :
	lucas_lehmer.X.simple p = lucas_lehmer.X.simple q :=
sorry

lucas_lehmer.X.simple_of_X_sq_zero (p q : ℕ) :
	lucas_lehmer.X.simple (p ^ 2 * q) = 0 :=
sorry

inv_of_eq_mul_self_mul_inv {G : Type*} [group G] (a b : G) :
	⅟ a = b * b * a⁻¹ :=
sorry

free_group.mul_swap_mul_self_eq_one {α : Type u} [group α]
	(x y z : α) :
	free_group.mk (x * y * z) * free_group.mk (y * z) = 1 :=
sorry

is_cyclic.exists_two_cycles {G : Type*} [group G] {N : subgroup G}
	[hN : N.normal] (h1 : is_cyclic G) (h2 : is_cyclic (G ⧸ N)) :
	∃ (a b : G), a ∈ N ∧ b ∈ N ∧ a * b = a :=
sorry