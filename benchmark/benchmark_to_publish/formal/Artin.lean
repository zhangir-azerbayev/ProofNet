import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory

theorem exercise_2_2_9 {G : Type*} [group G] {a b : G}
  (h : a * b = b * a) :
  ∀ x y : closure {x | x = a ∨ x = b}, x*y = y*x :=
sorry

theorem exercise_2_3_2 {G : Type*} [group G] (a b : G) :
  ∃ g : G, b * a = g * a * b * g⁻¹ :=
begin
  exact ⟨b, by simp⟩,
end

theorem exercise_2_4_19 {G : Type*} [group G] {x : G}
  (hx : order_of x = 2) (hx1 : ∀ y, order_of y = 2 → y = x) :
  x ∈ center G :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H] :
  center (G × H) ≃* (center G) × (center H) :=
sorry

theorem exercise_2_11_3 {G : Type*} [group G] [fintype G]
  (hG : even (card G)) : ∃ x : G, order_of x = 2 :=
begin
  apply exists_prime_order_of_dvd_card,
  exact even_iff_two_dvd.1 hG,
end

theorem exercise_3_2_7 {F : Type*} [field F] {G : Type*} [field G]
  (φ : F →+* G) : injective φ :=
begin
  convert φ.injective,
end

theorem exercise_3_5_6 {K V : Type*} [field K] [add_comm_group V]
  [module K V] {S : set V} (hS : set.countable S)
  (hS1 : span K S = ⊤) {ι : Type*} (R : ι → V)
  (hR : linear_independent K R) : countable ι :=
sorry

theorem exercise_3_7_2 {K V : Type*} [field K] [add_comm_group V]
  [module K V] {ι : Type*} [fintype ι] (γ : ι → submodule K V)
  (h : ∀ i : ι, γ i ≠ ⊤) :
  (⋂ (i : ι), (γ i : set V)) ≠ ⊤ :=
sorry

theorem exercise_6_1_14 (G : Type*) [group G]
  (hG : is_cyclic $ G ⧸ (center G)) :
  center G = ⊤  :=
sorry

theorem exercise_6_4_2 {G : Type*} [group G] [fintype G] {p q : ℕ}
  (hp : prime p) (hq : prime q) (hG : card G = p*q) :
  is_simple_group G → false :=
sorry

theorem exercise_6_4_3 {G : Type*} [group G] [fintype G] {p q : ℕ}
  (hp : prime p) (hq : prime q) (hG : card G = p^2 *q) :
  is_simple_group G → false :=
sorry

theorem exercise_6_4_12 {G : Type*} [group G] [fintype G]
  (hG : card G = 224) :
  is_simple_group G → false :=
sorry

theorem exercise_6_8_1 {G : Type*} [group G]
  (a b : G) : closure ({a, b} : set G) = closure {b*a*b^2, b*a*b^3} :=
sorry

theorem exercise_10_1_13 {R : Type*} [ring R] {x : R}
  (hx : is_nilpotent x) : is_unit (1 + x) :=
sorry

theorem exercise_10_2_4 :
  span ({2} : set $ polynomial ℤ) ⊓ (span {X}) =
  span ({2 * X} : set $ polynomial ℤ) :=
sorry

theorem exercise_10_6_7 {I : ideal gaussian_int}
  (hI : I ≠ ⊥) : ∃ (z : I), z ≠ 0 ∧ (z : gaussian_int).im = 0 :=
sorry

theorem exercise_10_4_6 {R : Type*} [comm_ring R] 
  [no_zero_divisors R] {I J : ideal R} (x : I ⊓ J) : 
  is_nilpotent ((ideal.quotient.mk (I*J)) x) :=
sorry

theorem exercise_10_4_7a {R : Type*} [comm_ring R] [no_zero_divisors R]
  (I J : ideal R) (hIJ : I + J = ⊤) : I * J = I ⊓ J :=
sorry

theorem exercise_10_7_10 {R : Type*} [ring R]
  (M : ideal R) (hM : ∀ (x : R), x ∉ M → is_unit x) :
  is_maximal M ∧ ∀ (N : ideal R), is_maximal N → N = M :=
sorry

theorem exercise_11_2_13 (a b : ℤ) :
  (of_int a : gaussian_int) ∣ of_int b → a ∣ b :=
begin
  contrapose, 
  simp,
end

theorem exercise_11_4_1b {F : Type*} [field F] [fintype F] (hF : card F = 2) :
  irreducible (12 + 6 * X + X ^ 3 : polynomial F) :=
sorry

theorem exercise_11_4_6a {F : Type*} [field F] [fintype F] (hF : card F = 7) :
  irreducible (X ^ 2 + 1 : polynomial F) :=
sorry

theorem exercise_11_4_6b {F : Type*} [field F] [fintype F] (hF : card F = 31) :
  irreducible (X ^ 3 - 9 : polynomial F) :=
sorry

theorem exercise_11_4_6c : irreducible (X^3 - 9 : polynomial (zmod 31)) :=
sorry

theorem exercise_11_4_8 {p : ℕ} (hp : prime p) (n : ℕ) :
  irreducible (X ^ n - p : polynomial ℚ) :=
sorry

theorem exercise_11_13_3 (N : ℕ):
  ∃ p ≥ N, nat.prime p ∧ p + 1 ≡ 0 [MOD 4] :=
sorry

theorem exercise_13_4_10 
    {p : ℕ} {hp : nat.prime p} (h : ∃ r : ℕ, p = 2 ^ r + 1) :
    ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] [fintype Kˣ] :
  ∏ (x : Kˣ), x = -1 :=
begin
  exact finite_field.prod_univ_units_id_eq_neg_one,
end