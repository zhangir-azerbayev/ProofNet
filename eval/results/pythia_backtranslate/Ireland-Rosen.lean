import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_30 (n : ℕ+) :
	¬↑n ∧ ∀ (m : ℕ), ↑m < n → 2 * ↑m + 1 / 2 + 1 / ↑m < ↑n :=
sorry

theorem exercise_2_4 {a : ℤ} (h : a ≠ 0) (n m : ℕ) :
	a ^ bit1 n = 1 ∨ a ^ bit1 m = 1 :=
sorry

theorem exercise_2_27a (n : ℕ) :
	(finset.range n).sum (λ (x : ℕ), 1 / ↑n) = summable (λ (x : ℕ), ∥x∥ ^ 2) :=
sorry

theorem exercise_3_4 (y : ℤ) :
	¬∃ (x : ℤ), fermat_42.r x y :=
sorry

theorem exercise_3_10 {n : ℕ} (hn : nat.prime n)
	(hn4 : (n - 1).factorial % 4 = 0) :
	n.factorial * (n - 1) % 4 = 0 :=
sorry

theorem exercise_4_4 {R : Type*} [comm_ring R] {p : ℕ} {a : R}
	(hp : nat.prime p) (ht : p ∣ 4 * t + 1) :
	is_primitive_root a (-a) ↔ is_primitive_root a p :=
sorry

theorem exercise_4_6 {p : ℕ} (h : fermat_42 p) (n : ℕ)
	(hn : 3 ∈ n.divisors) :
	is_primitive_root p ↑n :=
sorry

theorem exercise_4_11 (k : ℕ) (p : ℕ+ → Prop) [decidable_pred p] :
	(1 ^ k + 2 ^ k + (↑p - 1) ^ k) % (↑p - 1) = 0 ∧ (↑p - 1) % (↑p - 1) = -1 :=
sorry

theorem exercise_5_28 {p : ℝ}
	(hp : p ≠ 0) :
	(zpow_four x).is_solution ↔ p = gaussian_int.pow_two_add_six p + 64 * zpow_two p :=
sorry

theorem exercise_12_12 (x : ℝ) :
	(real.sin (real.pi / 12)).is_algebraic :=
sorry