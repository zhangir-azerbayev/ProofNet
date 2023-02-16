import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory

theorem exercise_1_1_2a : ∃ a b : ℤ, a - b ≠ b - a :=
begin
  use [0, 1]
end

theorem exercise_1_1_3 (n : ℤ) : 
  ∀ (a b c : ℤ), (a+b)+c ≡ a+(b+c) [ZMOD n] :=
begin 
  intros a b c, 
  ring_nf
end

theorem exercise_1_1_4 (n : ℕ) : 
  ∀ (a b c : ℕ), (a * b) * c ≡ a * (b * c) [ZMOD n] :=
begin 
  intros a b c, 
  ring_nf, 
end

theorem exercise_1_1_5 (n : ℕ) (hn : 1 < n) : 
  is_empty (group (zmod n)) := 
sorry 

theorem exercise_1_1_15 {G : Type*} [group G] (as : list G) :
  as.prod⁻¹ = (as.reverse.map (λ x, x⁻¹)).prod :=
begin 
  simp only [list.prod_hom _, list.map_reverse, list.prod_reverse],
  induction as generalizing, 
  simp, 
  simp *, 
end

theorem exercise_1_1_16 {G : Type*} [group G] 
  (x : G) (hx : x ^ 2 = 1) :
  order_of x = 1 ∨ order_of x = 2 :=
sorry 

theorem exercise_1_1_17 {G : Type*} [group G] {x : G} {n : ℕ}
  (hxn: order_of x = n) :
  x⁻¹ = x ^ (n-1) :=
sorry 

theorem exercise_1_1_18 {G : Type*} [group G]
  (x y : G) : x * y = y * x ↔ y⁻¹ * x * y = x ↔ x⁻¹ * y⁻¹ * x * y = 1 :=
sorry

theorem exercise_1_1_20 {G : Type*} [group G] {x : G} :
  order_of x = order_of x⁻¹ :=
sorry 

theorem exercise_1_1_22a {G : Type*} [group G] (x g : G) :
  order_of x = order_of (g⁻¹ * x * g) :=
sorry 

theorem exercise_1_1_22b {G: Type*} [group G] (a b : G) : 
  order_of (a * b) = order_of (b * a) :=
sorry

theorem exercise_1_1_25 {G : Type*} [group G] 
  (h : ∀ x : G, x ^ 2 = 1) : ∀ a b : G, a*b = b*a :=
sorry 

theorem exercise_1_1_29 {A B : Type*} [group A] [group B] :
  ∀ x y : A × B, x*y = y*x ↔ (∀ x y : A, x*y = y*x) ∧ 
  (∀ x y : B, x*y = y*x) :=
sorry

theorem exercise_1_1_34 {G : Type*} [group G] {x : G} 
  (hx_inf : order_of x = 0) (n m : ℤ) :
  x ^ n ≠ x ^ m :=
sorry

theorem exercise_1_3_8 : infinite (equiv.perm ℕ) :=
sorry

theorem exercise_1_6_4 : 
  is_empty (multiplicative ℝ ≃* multiplicative ℂ) :=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B] : 
  A × B ≃* B × A :=
sorry 

theorem exercise_1_6_17 {G : Type*} [group G] (f : G → G) 
  (hf : f = λ g, g⁻¹) :
  ∀ x y : G, f x * f y = f (x*y) ↔ ∀ x y : G, x*y = y*x :=   
sorry

theorem exercise_1_6_23 {G : Type*} 
  [group G] (σ : mul_aut G) (hs : ∀ g : G, σ g = 1 → g = 1) 
  (hs2 : ∀ g : G, σ (σ g) = g) :
  ∀ x y : G, x*y = y*x :=
sorry

theorem exercise_2_1_5 {G : Type*} [group G] [fintype G] 
  (hG : card G > 2) (H : subgroup G) [fintype H] : 
  card H ≠ card G - 1 :=
sorry 

theorem exercise_2_1_13 (H : add_subgroup ℚ) {x : ℚ} 
  (hH : x ∈ H → (1 / x) ∈ H):
  H = ⊥ ∨ H = ⊤ :=
sorry

theorem exercise_2_4_4 {G : Type*} [group G] (H : subgroup G) : 
  subgroup.closure ((H : set G) \ {1}) = ⊤ :=
sorry 

theorem exercise_2_4_16a {G : Type*} [group G] {H : subgroup G}  
  (hH : H ≠ ⊤) : 
  ∃ M : subgroup G, M ≠ ⊤ ∧
  ∀ K : subgroup G, M ≤ K → K = M ∨ K = ⊤ ∧ 
  H ≤ M :=
sorry 

theorem exercise_2_4_16b {n : ℕ} {hn : n ≠ 0} 
  {R : subgroup (dihedral_group n)} 
  (hR : R = subgroup.closure {dihedral_group.r 1}) : 
  R ≠ ⊤ ∧ 
  ∀ K : subgroup (dihedral_group n), R ≤ K → K = R ∨ K = ⊤ :=
sorry 

theorem exercise_2_4_16c {n : ℕ} (H : add_subgroup (zmod n)) : 
  ∃ p : ℕ, nat.prime p ∧ H = add_subgroup.closure {p} ↔ 
  H ≠ ⊤ ∧ ∀ K : add_subgroup (zmod n), H ≤ K → K = H ∨ K = ⊤ := 
sorry 

theorem exercise_3_1_3a {A : Type*} [comm_group A] (B : subgroup A) :
  ∀ a b : A ⧸ B, a*b = b*a :=
sorry

theorem exercise_3_1_22a (G : Type*) [group G] (H K : subgroup G) 
  [subgroup.normal H] [subgroup.normal K] :
  subgroup.normal (H ⊓ K) :=
sorry

theorem exercise_3_1_22b {G : Type*} [group G] (I : Type*)
  (H : I → subgroup G) (hH : ∀ i : I, subgroup.normal (H i)) : 
  subgroup.normal (⨅ (i : I), H i):=
sorry

theorem exercise_3_2_8 {G : Type*} [group G] (H K : subgroup G)
  [fintype H] [fintype K] 
  (hHK : nat.coprime (fintype.card H) (fintype.card K)) : 
  H ⊓ K = ⊥ :=
sorry 

theorem exercise_3_2_11 {G : Type*} [group G] {H K : subgroup G}
  (hHK : H ≤ K) : 
  H.index = K.index * H.relindex K :=
sorry 

theorem exercise_3_2_16 (p : ℕ) (hp : nat.prime p) (a : ℕ) :
  nat.coprime a p → a ^ p ≡ a [ZMOD p] :=
sorry

theorem exercise_3_2_21a (H : add_subgroup ℚ) (hH : H ≠ ⊤) : H.index = 0 :=
sorry

theorem exercise_3_3_3 {p : primes} {G : Type*} [group G] 
  {H : subgroup G} [hH : H.normal] (hH1 : H.index = p) : 
  ∀ K : subgroup G, K ≤ H ∨ H ⊔ K = ⊤ ∨ (K ⊓ H).relindex K = p :=
sorry 

theorem exercise_3_4_1 (G : Type*) [comm_group G] [is_simple_group G] :
    is_cyclic G ∧ ∃ G_fin : fintype G, nat.prime (@card G G_fin) :=
sorry

theorem exercise_3_4_4 {G : Type*} [comm_group G] [fintype G] {n : ℕ}
    (hn : n ∣ (fintype.card G)) :
    ∃ (H : subgroup G) (H_fin : fintype H), @card H H_fin = n  :=
sorry

theorem exercise_3_4_5a {G : Type*} [group G] 
  (H : subgroup G) [is_solvable G] : is_solvable H :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G] [is_solvable G] 
  (H : subgroup G) [subgroup.normal H] : 
  is_solvable (G ⧸ H) :=
sorry

theorem exercise_3_4_11 {G : Type*} [group G] [is_solvable G] 
  {H : subgroup G} (hH : H ≠ ⊥) [H.normal] : 
  ∃ A ≤ H, A.normal ∧ ∀ a b : A, a*b = b*a :=
sorry 

theorem exercise_4_2_8 {G : Type*} [group G] {H : subgroup G} 
  {n : ℕ} (hn : n > 0) (hH : H.index = n) : 
  ∃ K ≤ H, K.normal ∧ K.index ≤ n.factorial :=
sorry 

theorem exercise_4_3_26 {α : Type*} [fintype α] (ha : fintype.card α > 1)
  (h_tran : ∀ a b: α, ∃ σ : equiv.perm α, σ a = b) : 
  ∃ σ : equiv.perm α, ∀ a : α, σ a ≠ a := 
sorry

theorem exercise_4_2_9a {G : Type*} [fintype G] [group G] {p α : ℕ} 
  (hp : p.prime) (ha : α > 0) (hG : card G = p ^ α) : 
  ∀ H : subgroup G, H.index = p → H.normal :=
sorry 

theorem exercise_4_2_14 {G : Type*} [fintype G] [group G] 
  (hG : ¬ (card G).prime) (hG1 : ∀ k ∣ card G, 
  ∃ (H : subgroup G) (fH : fintype H), @card H fH = k) : 
  ¬ is_simple_group G :=
sorry 

theorem exercise_4_4_2 {G : Type*} [fintype G] [group G] 
  {p q : nat.primes} (hpq : p ≠ q) (hG : card G = p*q) : 
  is_cyclic G :=
sorry 

theorem exercise_4_4_6a {G : Type*} [group G] (H : subgroup G)
  [subgroup.characteristic H] : subgroup.normal H  :=
sorry

theorem exercise_4_4_6b {G : Type*} [group G] : 
  ∃ H : subgroup G, H.characteristic ∧ ¬ H.normal :=
sorry 

theorem exercise_4_4_7 {G : Type*} [group G] {H : subgroup G} [fintype H]
  (hH : ∀ (K : subgroup G) (fK : fintype K), card H = @card K fK → H = K) : 
  H.characteristic :=
sorry 

theorem exercise_4_4_8a {G : Type*} [group G] (H K : subgroup G)  
  (hHK : H ≤ K) [hHK1 : (H.subgroup_of K).normal] [hK : K.normal] : 
  H.normal :=
sorry 

theorem exercise_4_5_1a {p : ℕ} {G : Type*} [group G] 
  {P : subgroup G} (hP : is_p_group p P) (H : subgroup G) 
  (hH : P ≤ H) : is_p_group p H :=
sorry

theorem exercise_4_5_13 {G : Type*} [group G] [fintype G]
  (hG : card G = 56) :
  ∃ (p : ℕ) (P : sylow p G), P.normal :=
sorry

theorem exercise_4_5_14 {G : Type*} [group G] [fintype G]
  (hG : card G = 312) :
  ∃ (p : ℕ) (P : sylow p G), P.normal :=
sorry

theorem exercise_4_5_15 {G : Type*} [group G] [fintype G] 
  (hG : card G = 351) : 
  ∃ (p : ℕ) (P : sylow p G), P.normal :=
sorry 

theorem exercise_4_5_16 {p q r : ℕ} {G : Type*} [group G] 
  [fintype G]  (hpqr : p < q ∧ q < r) 
  (hpqr1 : p.prime ∧ q.prime ∧ r.prime)(hG : card G = p*q*r) : 
  nonempty (sylow p G) ∨ nonempty(sylow q G) ∨ nonempty(sylow r G) :=
sorry 

theorem exercise_4_5_17 {G : Type*} [fintype G] [group G] 
  (hG : card G = 105) : 
  nonempty(sylow 5 G) ∧ nonempty(sylow 7 G) :=
sorry 

theorem exercise_4_5_18 {G : Type*} [fintype G] [group G] 
  (hG : card G = 200) : 
  ∃ N : sylow 5 G, N.normal :=
sorry 

theorem exercise_4_5_19 {G : Type*} [fintype G] [group G] 
  (hG : card G = 6545) : ¬ is_simple_group G :=
sorry 

theorem exercise_4_5_20 {G : Type*} [fintype G] [group G]
  (hG : card G = 1365) : ¬ is_simple_group G :=
sorry 

theorem exercise_4_5_21 {G : Type*} [fintype G] [group G]
  (hG : card G = 2907) : ¬ is_simple_group G :=
sorry 

theorem exercise_4_5_22 {G : Type*} [fintype G] [group G]
  (hG : card G = 132) : ¬ is_simple_group G :=
sorry 

theorem exercise_4_5_23 {G : Type*} [fintype G] [group G]
  (hG : card G = 462) : ¬ is_simple_group G :=
sorry 

theorem exercise_4_5_28 {G : Type*} [group G] [fintype G] 
  (hG : card G = 105) (P : sylow 3 G) [hP : P.normal] : 
  comm_group G :=
sorry 

theorem exercise_4_5_33 {G : Type*} [group G] [fintype G] {p : ℕ} 
  (P : sylow p G) [hP : P.normal] (H : subgroup G) [fintype H] : 
  ∀ R : sylow p H, R.to_subgroup = (H ⊓ P.to_subgroup).subgroup_of H ∧
  nonempty (sylow p H) :=
sorry 

theorem exercise_5_4_2 {G : Type*} [group G] (H : subgroup G) : 
  H.normal ↔ ⁅(⊤ : subgroup G), H⁆ ≤ H := 
sorry 

theorem exercise_7_1_2 {R : Type*} [ring R] {u : R}
  (hu : is_unit u) : is_unit (-u) :=
sorry 

theorem exercise_7_1_11 {R : Type*} [ring R] [is_domain R] 
  {x : R} (hx : x^2 = 1) : x = 1 ∨ x = -1 :=
sorry 

theorem exercise_7_1_12 {F : Type*} [field F] {K : subring F}
  (hK : (1 : F) ∈ K) : is_domain K :=
sorry 

theorem exercise_7_1_15 {R : Type*} [ring R] (hR : ∀ a : R, a^2 = a) :
  comm_ring R :=
sorry 

theorem exercise_7_2_2 {R : Type*} [ring R] (p : polynomial R) :
  p ∣ 0 ↔ ∃ b : R, b ≠ 0 ∧ b • p = 0 := 
sorry 

theorem exercise_7_2_12 {R G : Type*} [ring R] [group G] [fintype G] : 
  ∑ g : G, monoid_algebra.of R G g ∈ center (monoid_algebra R G) :=
sorry 

theorem exercise_7_3_16 {R S : Type*} [ring R] [ring S] 
  {φ : R →+* S} (hf : surjective φ) : 
  φ '' (center R) ⊂ center S :=
sorry 

theorem exercise_7_3_37 {R : Type*} {p m : ℕ} (hp : p.prime) 
  (N : ideal $ zmod $ p^m) : 
  is_nilpotent N ↔  is_nilpotent (ideal.span ({p} : set $ zmod $ p^m)) :=
sorry 

theorem exercise_7_4_27 {R : Type*} [comm_ring R] (hR : (0 : R) ≠ 1) 
  {a : R} (ha : is_nilpotent a) (b : R) : 
  is_unit (1-a*b) :=
sorry 

theorem exercise_8_1_12 {N : ℕ} (hN : N > 0) {M M': ℤ} {d : ℕ}
  (hMN : M.gcd N = 1) (hMd : d.gcd N.totient = 1) 
  (hM' : M' ≡ M^d [ZMOD N]) : 
  ∃ d' : ℕ, d' * d ≡ 1 [ZMOD N.totient] ∧ 
  M ≡ M'^d' [ZMOD N] :=
sorry 

theorem exercise_8_2_4 {R : Type*} [ring R][no_zero_divisors R] 
  [cancel_comm_monoid_with_zero R] [gcd_monoid R]
  (h1 : ∀ a b : R, a ≠ 0 → b ≠ 0 → ∃ r s : R, gcd a b = r*a + s*b)
  (h2 : ∀ a : ℕ → R, (∀ i j : ℕ, i < j → a i ∣ a j) → 
  ∃ N : ℕ, ∀ n ≥ N, ∃ u : R, is_unit u ∧ a n = u * a N) : 
  is_principal_ideal_ring R :=
sorry  

theorem exercise_8_3_4 {R : Type*} {n : ℤ} {r s : ℚ} 
  (h : r^2 + s^2 = n) : 
  ∃ a b : ℤ, a^2 + b^2 = n :=
sorry 

theorem exercise_8_3_5a {n : ℤ} (hn0 : n > 3) (hn1 : squarefree n) : 
  irreducible (2 :zsqrtd $ -n) ∧ 
  irreducible (⟨0, 1⟩ : zsqrtd $ -n) ∧ 
  irreducible (1 + ⟨0, 1⟩ : zsqrtd $ -n) :=
sorry 

theorem exercise_8_3_6a {R : Type*} [ring R]
  (hR : R = (gaussian_int ⧸ ideal.span ({⟨0, 1⟩} : set gaussian_int))) :
  is_field R ∧ ∃ finR : fintype R, @card R finR = 2 :=
sorry 

theorem exercise_8_3_6b {q : ℕ} (hq0 : q.prime) 
  (hq1 : q ≡ 3 [ZMOD 4]) {R : Type*} [ring R]
  (hR : R = (gaussian_int ⧸ ideal.span ({q} : set gaussian_int))) : 
  is_field R ∧ ∃ finR : fintype R, @card R finR = q^2 :=
sorry 
   
theorem exercise_9_1_6 : ¬ is_principal 
  (ideal.span ({X 0, X 1} : set (mv_polynomial (fin 2) ℚ))) :=
sorry 

theorem exercise_9_1_10 {f : ℕ → mv_polynomial ℕ ℤ} 
  (hf : f = λ i, X i * X (i+1)): 
  infinite (minimal_primes (mv_polynomial ℕ ℤ ⧸ ideal.span (range f))) := 
sorry 

theorem exercise_9_3_2 {f g : polynomial ℚ} (i j : ℕ)
  (hfg : ∀ n : ℕ, ∃ a : ℤ, (f*g).coeff = a) :
  ∃ a : ℤ, f.coeff i * g.coeff j = a :=
sorry 

theorem exercise_9_4_2a : irreducible (X^4 - 4*X^3 + 6 : polynomial ℤ) := 
sorry 

theorem exercise_9_4_2b : irreducible 
  (X^6 + 30*X^5 - 15*X^3 + 6*X - 120 : polynomial ℤ) :=
sorry 

theorem exercise_9_4_2c : irreducible 
  (X^4 + 4*X^3 + 6*X^2 + 2*X + 1 : polynomial ℤ) :=
sorry 

theorem exercise_9_4_2d {p : ℕ} (hp : p.prime ∧ p > 2) 
  {f : polynomial ℤ} (hf : f = (X + 2)^p): 
  irreducible (∑ n in (f.support - {0}), (f.coeff n) * X ^ (n-1) : 
  polynomial ℤ) :=
sorry 

theorem exercise_9_4_9 : 
  irreducible (X^2 - C sqrtd : polynomial (zsqrtd 2)) :=
sorry 

theorem exercise_9_4_11 : 
  irreducible ((X 0)^2 + (X 1)^2 - 1 : mv_polynomial (fin 2) ℚ) :=
sorry 

theorem exercise_11_1_13 {ι : Type*} [fintype ι] : 
  (ι → ℝ) ≃ₗ[ℚ] ℝ :=
sorry 
