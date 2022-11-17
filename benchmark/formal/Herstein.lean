import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory

theorem exercise_2_1_18 {G : Type*} [group G] 
  [fintype G] (hG2 : even (fintype.card G)) :
  ∃ (a : G), a ≠ 1 ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_21 (G : Type*) [group G] [fintype G]
  (hG : card G = 5) :
  comm_group G :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] 
  [fintype G] (a : G) : ∃ (n : ℕ), a ^ n = 1 :=
sorry

theorem exercise_2_1_27 {G : Type*} [group G] 
  [fintype G] : ∃ (m : ℕ), ∀ (a : G), a ^ m = 1 :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G]
  {P : ℕ → Prop} {hP : P = λ i, ∀ a b : G, (a*b)^i = a^i * b^i}
  (hP1 : ∃ n : ℕ, P n ∧ P (n+1) ∧ P (n+2)) : comm_group G :=
sorry

theorem exercise_2_2_5 {G : Type*} [group G] 
  (h : ∀ (a b : G), (a * b) ^ 3 = a ^ 3 * b ^ 3 ∧ (a * b) ^ 5 = a ^ 5 * b ^ 5) :
  comm_group G :=
sorry

theorem exercise_2_2_6c {G : Type*} [group G] {n : ℕ} (hn : n > 1) 
  (h : ∀ (a b : G), (a * b) ^ n = a ^ n * b ^ n) :
  ∀ (a b : G), (a * b * a⁻¹ * b⁻¹) ^ (n * (n - 1)) = 1 :=
sorry

theorem exercise_2_3_17 {G : Type*} [has_mul G] [group G] (a x : G) :  
  set.centralizer {x⁻¹*a*x} = 
  (λ g : G, x⁻¹*g*x) '' (set.centralizer {a}) :=
sorry

theorem exercise_2_3_19 {G : Type*} [group G] {M : subgroup G}
  (hM : ∀ (x : G), (λ g : G, x⁻¹*g*x) '' M ⊂ M) :
  ∀ x : G, (λ g : G, x⁻¹*g*x) '' M = M :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G]
  (hG : ∀ H : subgroup G, H = ⊤ ∨ H = ⊥) :
  is_cyclic G ∧ ∃ (p : ℕ) (fin : fintype G), nat.prime p ∧ @card G fin = p :=
sorry

theorem exercise_2_4_36 {a n : ℕ} (h : a > 1) :
  n ∣ (a ^ n - 1).totient :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G] 
  (hG : ∀ (H : subgroup G), H.normal) (a b : G) :
  ∃ (j : ℤ) , b*a = a^j * b:=
sorry

theorem exercise_2_5_30 {G : Type*} [group G] [fintype G]
  {p m : ℕ} (hp : nat.prime p) (hp1 : ¬ p ∣ m) (hG : card G = p*m) 
  {H : subgroup G} [fintype H] [H.normal] (hH : card H = p):
  characteristic H :=
sorry

theorem exercise_2_5_31 {G : Type*} [comm_group G] [fintype G]
  {p m n : ℕ} (hp : nat.prime p) (hp1 : ¬ p ∣ m) (hG : card G = p^n*m)
  {H : subgroup G} [fintype H] (hH : card H = p^n) : 
  characteristic H :=
sorry

theorem exercise_2_5_37 (G : Type*) [group G] [fintype G]
  (hG : card G = 6) (hG' : is_empty (comm_group G)) :
  G ≅ equiv.perm (fin 3) :=
sorry

theorem exercise_2_5_43 (G : Type*) [group G] [fintype G]
  (hG : card G = 9) :
  comm_group G :=
sorry

theorem exercise_2_5_44 {G : Type*} [group G] [fintype G] {p : ℕ}
  (hp : nat.prime p) (hG : card G = p^2) :
  ∃ (N : subgroup G) (fin : fintype N), @card N fin = p ∧ N.normal :=
sorry

theorem exercise_2_5_52 {G : Type*} [group G] [fintype G]
  (φ : G ≃* G) {I : finset G} (hI : ∀ x ∈ I, φ x = x⁻¹)
  (hI1 : 0.75 * card G ≤ card I) : 
  ∀ x : G, φ x = x⁻¹ ∧ ∀ x y : G, x*y = y*x :=
sorry

theorem exercise_2_6_15 {G : Type*} [comm_group G] {m n : ℕ} 
  (hm : ∃ (g : G), order_of g = m) 
  (hn : ∃ (g : G), order_of g = n) 
  (hmn : m.coprime n) :
  ∃ (g : G), order_of g = m * n :=
sorry

theorem exercise_2_7_7 {G : Type*} [group G] {G' : Type*} [group G']
  (φ : G →* G') (N : subgroup G) [N.normal] : 
  (map φ N).normal  :=
sorry

theorem exercise_2_8_12 {G H : Type*} [fintype G] [fintype H] 
  [group G] [group H] (hG : card G = 21) (hH : card H = 21) 
  (hG1 : is_empty(comm_group G)) (hH1 : is_empty (comm_group H)) :
  G ≃* H :=
sorry 

theorem exercise_2_8_15 {G H: Type*} [fintype G] [group G] [fintype H]
  [group H] {p q : ℕ} (hp : nat.prime p) (hq : nat.prime q) 
  (h : p > q) (h1 : q ∣ p - 1) (hG : card G = p*q) (hH : card G = p*q) :
  G ≃* H :=
sorry

theorem exercise_2_9_2 {G H : Type*} [fintype G] [fintype H] [group G] 
  [group H] (hG : is_cyclic G) (hH : is_cyclic H) :
  is_cyclic (G × H) ↔ (card G).coprime (card H) :=
sorry

theorem exercise_2_10_1 {G : Type*} [group G] (A : subgroup G) 
  [A.normal] {b : G} (hp : nat.prime (order_of b)) :
  A ⊓ (closure {b}) = ⊥ :=
sorry

theorem exercise_2_11_6 {G : Type*} [group G] {p : ℕ} (hp : nat.prime p) 
  {P : sylow p G} (hP : P.normal) :
  ∀ (Q : sylow p G), P = Q :=
sorry

theorem exercise_2_11_7 {G : Type*} [group G] {p : ℕ} (hp : nat.prime p)
  {P : sylow p G} (hP : P.normal) : 
  characteristic (P : subgroup G) :=
sorry

theorem exercise_2_11_22 {p : ℕ} {n : ℕ} {G : Type*} [fintype G] 
  [group G] (hp : nat.prime p) (hG : card G = p ^ n) {K : subgroup G}
  [fintype K] (hK : card K = p ^ (n-1)) : 
  K.normal :=
sorry

theorem exercise_3_2_21 {α : Type*} [fintype α] {σ τ: equiv.perm α} 
  (h1 : ∀ a : α, σ a = a ↔ τ a ≠ a) (h2 : τ ∘ σ = id) : 
  σ = 1 ∧ τ = 1 :=
sorry

theorem exercise_4_1_19 : infinite {x : quaternion ℝ | x^2 = -1} :=
sorry

theorem exercise_4_1_34 : equiv.perm (fin 3) ≃* general_linear_group (fin 2) (zmod 2) :=
sorry

theorem exercise_4_2_5 {R : Type*} [ring R] 
  (h : ∀ x : R, x ^ 3 = x) : comm_ring R :=
sorry

theorem exercise_4_2_6 {R : Type*} [ring R] (a x : R) 
  (h : a ^ 2 = 0) : a * (a * x + x * a) = (x + x * a) * a :=
sorry

theorem exercise_4_2_9 {p : ℕ} (hp : nat.prime p) (hp1 : odd p) :
  ∃ (a b : ℤ), a / b = ∑ i in finset.range p, 1 / (i + 1) → ↑p ∣ a :=
sorry

theorem exercise_4_3_1 {R : Type*} [comm_ring R] (a : R) :
  ∃ I : ideal R, {x : R | x*a=0} = I :=
sorry

theorem exercise_4_3_25 (I : ideal (matrix (fin 2) (fin 2) ℝ)) : 
  I = ⊥ ∨ I = ⊤ :=
sorry

theorem exercise_4_4_9 (p : ℕ) (hp : nat.prime p) :
  ∃ S : finset (zmod p), S.card = (p-1)/2 ∧ ∃ x : zmod p, x^2 = p ∧ 
  ∃ S : finset (zmod p), S.card = (p-1)/2 ∧ ¬ ∃ x : zmod p, x^2 = p :=
sorry

theorem exercise_4_5_16 {p n: ℕ} (hp : nat.prime p) 
  {q : polynomial (zmod p)} (hq : irreducible q) (hn : q.degree = n) :
  ∃ is_fin : fintype $ polynomial (zmod p) ⧸ ideal.span ({q} : set (polynomial $ zmod p)), 
  @card (polynomial (zmod p) ⧸ ideal.span {q}) is_fin = p ^ n ∧ 
  is_field (polynomial $ zmod p):=
sorry

theorem exercise_4_5_23 {p q: polynomial (zmod 7)} 
  (hp : p = X^3 - 2) (hq : q = X^3 + 2) : 
  irreducible p ∧ irreducible q ∧ 
  (nonempty $ polynomial (zmod 7) ⧸ ideal.span ({p} : set $ polynomial $ zmod 7) ≃+*
  polynomial (zmod 7) ⧸ ideal.span ({q} : set $ polynomial $ zmod 7)) :=
sorry

theorem exercise_4_5_25 {p : ℕ} (hp : nat.prime p) :
  irreducible (∑ i : finset.range p, X ^ p : polynomial ℚ) :=
sorry

theorem exercise_4_6_2 : irreducible (X^3 + 3*X + 2 : polynomial ℚ) :=
sorry

theorem exercise_4_6_3 :
  infinite {a : ℤ | irreducible (X^7 + 15*X^2 - 30*X + a : polynomial ℚ)} :=
sorry

theorem exercise_5_1_8 {p m n: ℕ} {F : Type*} [field F] 
  (hp : nat.prime p) (hF : char_p F p) (a b : F) (hm : m = p ^ n) : 
  (a + b) ^ m = a^m + b^m :=
sorry

theorem exercise_5_2_20 {F V ι: Type*} [infinite F] [field F] 
  [add_comm_group V] [module F V] {u : ι → submodule F V} 
  (hu : ∀ i : ι, u i ≠ ⊤) : 
  (⋃ i : ι, (u i : set V)) ≠ ⊤ :=
sorry

theorem exercise_5_3_7 {K : Type*} [field K] {F : subfield K} 
  {a : K} (ha : is_algebraic F (a ^ 2)) : is_algebraic F a :=
sorry 

theorem exercise_5_3_10 : is_algebraic ℚ (cos (real.pi / 180)) :=
sorry

theorem exercise_5_4_3 {a : ℂ} {p : ℂ → ℂ} 
  (hp : p = λ x, x^5 + real.sqrt 2 * x^3 + real.sqrt 5 * x^2 + 
  real.sqrt 7 * x + 11)
  (ha : p a = 0) : 
  ∃ p : polynomial ℂ , p.degree < 80 ∧ a ∈ p.roots ∧ 
  ∀ n : p.support, ∃ a b : ℤ, p.coeff n = a / b :=
sorry

theorem exercise_5_5_2 : irreducible (X^3 - 3*X - 1 : polynomial ℚ) :=
sorry 

theorem exercise_5_6_14 {p m n: ℕ} (hp : nat.prime p) {F : Type*} 
  [field F] [char_p F p] (hm : m = p ^ n) : 
  card (root_set (X ^ m - X : polynomial F) F) = m :=
sorry