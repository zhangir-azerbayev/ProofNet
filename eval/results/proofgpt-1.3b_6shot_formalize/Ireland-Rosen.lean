import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_30not_integer_of_not_integer_sum_of_not_integer_mul_of_not_integer_mul_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_integer_pow_of_not_:=
sorry

theorem exercise_2_4 {a : ℕ} (ha : odd a) :
  ∃ (n : ℕ) (m : ℕ), n ≠ m ∧ a ^ (2 ^ n) + 1 ≠ a ^ (2 ^ m) + 1 :=
sorry

theorem exercise_2_27a {n : ℕ} (hn : n ≠ 0) :
  ∃ (x : ℕ), n * x = 1 ∧ x ≠ 0 ∧ x * x = n :=
sorry

theorem exercise_3_4 : ∀ {x y : ℤ}, 3 x^{2}+2 = y^{2} → x = 0 ∨ x = -1 ∨ x = 1
| x y :=
sorry

theorem exercise_3_10_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_mod_n_prime_n_prime_congruent_:=
sorry

theorem exercise_4_4primitive_root_modulo_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime_of_prime:=
sorry

theorem exercise_4_6 {p : ℕ} [hp : fact (p.prime)] :
  p.primitive_root_mod_2_prime p :=
sorry

theorem exercise_4_11  {p : ℕ} {k : ℕ} {p' : ℕ} {k' : ℕ} (hk : p ≠ p') (hk' : p' ≠ p) :
  (p ^ k) + (p' ^ k') ≡ 0 (p) :=
sorry

theorem exercise_5_28  (p : ℕ) (h : is_square_root p) : is_square_root (p : ℕ) :=
sorry

theorem exercise_12_12 :
  algebraic_number (sin (π / 12)) :=
sorry