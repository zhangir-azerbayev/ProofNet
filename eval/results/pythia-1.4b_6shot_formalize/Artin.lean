import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_3_2 {G H : Type*} [group G] [group H]
  [is_cyclic H] (f : G →* H) (hf : f.ker ≤ center G) :
  conjugate_elements (f.ker ≤ center G) (f.ker ≤ center G) :=
sorry

theorem exercise_2_8_6 {G H : Type*} [group G] [group H]
  [is_cyclic H] (f : G →* H) (hf : f.ker ≤ center G) :
  product_center G :=
sorry

theorem exercise_3_2_7 {F : Type*} [field F] [group F]
  [add_comm_group F] [module F F] [finite_dimensional F F]
  {f : F → F} (h : F.is_isomorphism F) :
  ∃ (f : F → F), f.ker ≤ F.comap F.comap F.comap F.comap :=
sorry

theorem exercise_3_7_2 {V : Type*} [field F] [module V] [finite_dimensional V] 
  {t : finset V} (h : finite_dimensional.finrank V + 1 < t.card) :
  ∃ (f : V → F), t.sum (λ (e : V), f e • e) = 0 ∧ t.sum (λ (e : V), f e) = 0 
  ∧ ∃ (x : V) (H : x ∈ t), f x ≠ 0 :=
sorry

theorem exercise_6_4_2 {G : Type*} [group G] [group G]
  [is_simple G] (p : ℤ) (q : ℤ) (h : G.order p q) :
  G.order p q = G.order p q + 1 :=
sorry

theorem exercise_6_4_12 {G : Type*} [group G] [group G]
  [is_simple G] (h : G.order 224) :
  ∃ (H : G.order 224 < G.order G), G.order G = G.order H :=
sorry

theorem exercise_10_1_13 {R : Type*} [ring R] 
  [nilpotent_element R] (x : R) (n : ℤ) :
  ∃ (y : R), x = 1 + y ∧ y = 0 :=
sorry

theorem exercise_10_4_7a {R : Type*} [ring R] 
  [ideal_of_ring R] (I : ideal R) (J : ideal R) :
  I ∩ J = I ∪ J :=
sorry

theorem exercise_10_6_7 {R : Type*} [ring R] [ideal R]
  (h : nonzero_ideal R) : nonzero_ideal R :=
sorry

theorem exercise_11_2_13 {R : Type*} [ring R] [field R]
  [add_comm_group R] [module R R] [finite_dimensional R R] 
  {a : ℤ} (h : finite_dimension R R + 1 < a.card) :
  ∃ (b : ℤ), a.divides b :=
sorry

theorem exercise_11_4_6a {F : Type*} [field F] [group F]
  [add_comm_group F] [module F F] [finite_dimensional F F]
  {x : F} (hx : ∀ (i : ℕ), x ^ i ≠ 1) :
  ∃ (f : F → F), x ^ 2 + x + 1 ≠ 0 :=
sorry

theorem exercise_11_4_6c {F : Type*} [field F] [group F]
  [add_comm_group F] [module F F] [finite_dimensional F F]
  (h : finite_dimension F F + 1 < 31) :
  irreducible_in_F31_3 F :=
sorry

theorem exercise_11_13_3 {p : ℕ} [prime_number p]
  (p : primes p) (q : primes q) (r : primes r) :
  p ≡ q ∧ p ≡ r ∧ p ≡ r ∧ p ≡ q ∧ p ≡ r ∧ p ≡ q ∧ p ≡ r :=
sorry

theorem exercise_13_6_10 {K : Type*} [field K] 
  [add_comm_group V] [module K V] [finite_field K V] 
  {t : finset V} (h : finite_field.finrank K V + 1 < t.card) :
  ∃ (f : V → K), t.product (λ (e : V), f e • e) = -1 ∧ t.product (λ (e : V), f e) = -1 
  ∧ ∃ (x : V) (H : x ∈ t), f x ≠ 0 :=
sorry