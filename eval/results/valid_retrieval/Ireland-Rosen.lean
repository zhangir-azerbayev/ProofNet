import .common 

open set function nat int fintype real polynomial mv_polynomial
open zsqrtd gaussian_int char_p nat.arithmetic_function 

open_locale big_operators
noncomputable theory





theorem exercise_1_27 {n : ℕ} (h : odd n) :
	8 ∣ n ^ 2 - 1 :=
sorry

theorem exercise_1_31  (i : ℤ) (h : i ^ 2 + 2 * i + 1 = 2) : 2 ∣ 2 :=
sorry

theorem exercise_2_21 {n : ℕ} :
  n.divisors.sum (λ (d : ℕ), ↑(⇑nat.arithmetic_function.moebius d) * ⇑nat.arithmetic_function.log d) = -⇑nat.arithmetic_function.von_mangoldt n :=
sorry

theorem exercise_3_1 :
  ∃ (p : ℕ), nat.prime p ∧ p ≡ -1 [MOD 6] :=
sorry

theorem exercise_3_5 :
  ∀ (x y : ℕ), 7 * x ^ 3 + 2 ≠ y ^ 3 :=
sorry

theorem exercise_3_14 {p q : ℕ}
  (hp : nat.prime p) (hq : nat.prime q) (h : p - 1 ∣ q - 1) (n : ℕ)
  (hpn : nat.coprime n (p * q)) :
  n ^ (q - 1) ≡ 1 [ZMOD (p * q)] :=
sorry

theorem exercise_4_5 {R : Type*}
  [comm_ring R] (p : ℕ) [nontrivial R] [h : char_p R p] (hp : p ≠ 2)
  (hpt : p % 4 = 3) (a : R) (ha : a ≠ 0) :
  is_primitive_root a p ↔ (a ^ ((p - 1) / 2) = -1) :=
sorry

theorem exercise_4_8 {p a : ℕ}
  (hp : nat.prime p) (hp1 : p % 2 = 1) (h : is_primitive_root a p) :
  ∀ (q : ℕ), nat.prime q → q ∣ p - 1 → a ^ ((p - 1) / q) % p ≠ 1 :=
sorry

theorem exercise_5_13 {x : ℕ} (hx : x ≠ 0)
  (hx4 : x ^ 4 - x ^ 2 + 1 = 0) :
  ∃ (p : ℕ), nat.prime p ∧ p ∣ x ^ 4 - x ^ 2 + 1 ∧ p ≡ 1 [MOD 12] :=
sorry

theorem exercise_5_37 {p q a : ℕ} (h : p ≡ q [MOD 4 * a])
  (hnd : ¬p ∣ a) :
  a / p = a / q :=
sorry

theorem exercise_18_4 (n : ℕ) :
	∃ (a b c d : ℕ), a ^ 3 + b ^ 3 = c ^ 3 + d ^ 3 ∧ a ≠ b ∧ c ≠ d ∧ a ^ 3 + b ^ 3 = n :=
sorry