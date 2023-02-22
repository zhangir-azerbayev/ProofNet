import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_30 (n : ℕ) :
	¬(pgame.pow_half n + pgame.pow_half n).equiv (2 * (pgame.pow_half n).equiv 0) :=
sorry

theorem exercise_2_4 {q : ℕ+} {a : ℤ}
	(hq : a ≠ 0) {m n : ℕ} (hmn : m ≤ n) :
	lucas_lehmer.X.int_pow_add_one a q = 1 ∨ 2 * (a % 2) = 1 :=
sorry

theorem exercise_2_27a :
	filter.tendsto (λ (n : ℕ), 1 / ↑n) filter.at_top filter.at_top :=
sorry

theorem exercise_3_4 (y : ℤ) :
	¬fermat_42.solutions (3 * y ^ 2 + 2) :=
sorry

theorem exercise_3_10 (n : ℕ)
	(h : ¬nat.prime n) :
	(n - 1).factorial ≡ 0 (n) :=
sorry

theorem exercise_4_4 {p : ℕ} {a : ℕ}
	(hp : p = 4 * t + 1) (h : a ≠ 0) :
	is_primitive_root (-a) p ↔ is_primitive_root a p :=
sorry

theorem exercise_4_6 {R : Type*} [comm_ring R]
	{p : ℕ} {n : ℕ} (hp : is_fermat_prime p (2 ^ n + 1)) :
	is_primitive_root 3 (2 ^ n + 1) :=
sorry

theorem exercise_4_11 (p : ℕ+) (k : ℕ) :
	1 ^ k + 2 ^ k + 3 ^ k = 0(p) ↔ p - 1 ∣ k ∧ -1 * (p - 1) ∣ k :=
sorry

theorem exercise_5_28 64 (p : ℕ) [fact (nat.prime p)] :
	zmod.pow_four p = 2 * (↑p - 1) ↔ p = 1 ∨ p = 4 ∨ p = 8 :=
sorry

theorem exercise_12_12 :
	is_algebraic (real.sin (real.pi / 12)) :=
sorry