theorem exercise_2_1_18 {G : Type*} [group G] 
    (hG : fintype G) (hG2 : 2 ∣ fintype.card G) :
    ∃ (a : G), a ≠ 1 ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_21 (G : Type*) [group G] (hG : fintype.card G = 5) :
    abelian_group G :=
sorry

theorem exercise_2_1_26 {G : Type*} [group G] (a : G) 
    (hG : fintype G) : ∃ (n : ℕ), a ^ n = 1 :=
sorry

theorem exercise_2_1_27 {G : Type*} [group G] 
    (hG : fintype G) : ∃ (m : ℕ), ∀ (a : G), a ^ m = 1 :=
sorry

theorem exercise_2_2_3 {G : Type*} [group G] 
    (a b : G) (i j k : ℕ) (h : (a * b) ^ i = a ^ i * b ^ i) 
    (h' : (a * b) ^ j = a ^ j * b ^ j) (h'' : (a * b) ^ k = a ^ k * b ^ k) :
    is_abelian G :=
sorry

theorem exercise_2_2_5 {G : Type*} [group G] 
    (h : ∀ (a b : G), (a * b) ^ 3 = a ^ 3 * b ^ 3 ∧ (a * b) ^ 5 = a ^ 5 * b ^ 5) :
    abelian G :=
sorry

theorem exercise_2_2_6c 
    {G : Type*} [comm_group G] {n : ℕ} (hn : n > 1) 
    (h : ∀ (a b : G), (a * b) ^ n = a ^ n * b ^ n) :
    ∀ (a b : G), (a * b * a⁻¹ * b⁻¹) ^ (n * (n - 1)) = 1 :=
sorry

theorem exercise_2_3_17 {G : Type*} [group G] (a x : G) 
    (h : a ∈ comm_group.comm_of G) :
    conjugate x⁻¹ a x = x⁻¹ * conjugate a x :=
sorry

theorem exercise_2_3_19 {G : Type*} [group G] {M : subgroup G}
    (hM : ∀ (x : G), x⁻¹ * M * x ⊆ M) :
    ∀ (x : G), x⁻¹ * M * x = M :=
sorry

theorem exercise_2_3_16 {G : Type*} [group G] 
    (hG : ¬ ∃ (H : subgroup G), H ≠ ⊥ ∧ H ≠ ⊤) :
    ∃ (p : ℕ), nat.prime p ∧ G ≅ (multiplicative (fin p)) :=
sorry

theorem exercise_2_3_21 {G : Type*} [group G] 
    {A B : subgroup G} (h : ∀ b : G, b ∈ B → ∀ a : G, a ∈ A → b⁻¹ * a * b ∈ A) :
    is_subgroup (A * B) :=
sorry

theorem exercise_2_3_22 {G : Type*} [group G] 
    (A B : subgroup G) (hA : fintype.card A.carrier = nat.prime_factors A.card) 
    (hB : fintype.card B.carrier = nat.prime_factors B.card) 
    (hAB : nat.relprime A.card B.card) :
    is_subgroup (A.carrier * B.carrier) :=
sorry

theorem exercise_2_3_28 {G : Type*} [group G] 
    {M N : subgroup G} (hM : ∀ (x : G), x⁻¹ * M * x ⊆ M) 
    (hN : ∀ (x : G), x⁻¹ * N * x ⊆ N) :
    ∀ (x : G), x⁻¹ * (M * N) * x ⊆ M * N :=
sorry

theorem exercise_2_3_29 {G : Type*} [group G] {M : subgroup G} 
    (hM : ∀ (x : G), x⁻¹ * M * x ⊆ M) :
    ∀ (x : G), x⁻¹ * M * x = M :=
sorry

theorem exercise_2_4_8 {G : Type*} [group G] 
    (H : subgroup G) (h : ∀ (a : G), H * a = a * H) :
    ∀ (a : G), a * H * a⁻¹ = H :=
sorry

theorem exercise_2_4_26 {G : Type*} [group G] 
    (H : subgroup G) (S : set (set G)) (T : set (set G)) 
    (hS : S = set.univ.image (λ (g : G), g • H)) 
    (hT : T = set.univ.image (λ (g : G), H • g)) :
    ∃ (f : S → T), function.bijective f :=
sorry

theorem exercise_2_4_32 {G : Type*} [group G] [fintype G]
    (H : subgroup G) (a : G) :
    ∃ (m : ℕ), a ^ m ∈ H ∧ nat.dvd (order a) m :=
sorry

theorem exercise_2_4_36 (a n : ℕ) (h : a > 1) :
    n ∣ nat.euler_phi (a ^ n - 1) :=
sorry

theorem exercise_2_5_6 {G : Type*} [group G] {G' : Type*} [group G']
    (f : G → G') (hf : is_group_hom f) : is_subgroup (set.range f) :=
sorry

theorem exercise_2_5_23 {G : Type*} [group G] 
    (hG : ∀ (H : subgroup G), H ≤ normalizer G H) (a b : G) :
    ∃ (j : ℕ), b * a = a ^ j * b :=
sorry

theorem exercise_2_5_30 {G : Type*} [group G] 
    (H : subgroup G) (hH : H.order = p) (hHn : H.normal) 
    (hG : G.order = p * m) (hp : p ∣ m) :
    H.is_char :=
sorry

theorem exercise_2_5_31 {G : Type*} [group G] 
    (p : ℕ) (h : nat.prime p) (n : ℕ) (m : ℕ) (hG : G.card = p ^ n * m) 
    (H : subgroup G) (hH : H.card = p ^ n) :
    characteristic G H :=
sorry

theorem exercise_2_5_37 (G : Type*) [group G] 
    (hG : G.card = 6) (hG' : ¬ abelian G) :
    G ≅ symmetric_group 3 :=
sorry

theorem exercise_2_5_43 (G : Type*) [group G] (hG : order G = 9) :
    abelian G :=
sorry

theorem exercise_2_5_44 {G : Type*} 
    [group G] (p : ℕ) (hp : nat.prime p) (hG : (p ^ 2 : ℕ) ∣ G.card) :
    ∃ (N : subgroup G), N.card = p ∧ N.normal :=
sorry

theorem exercise_2_5_52 {G : Type*} [group G] 
    (φ : G → G) (hφ : is_automorphism φ) (h : ∀ x : G, φ x = x⁻¹) : 
    is_abelian G :=
sorry

theorem exercise_2_6_15 {G : Type*} [group G] 
    (m n : ℕ) (hm : ∃ (g : G), g.order = m) (hn : ∃ (g : G), g.order = n) 
    (hmn : nat.relprime m n) :
    ∃ (g : G), g.order = m * n :=
sorry

theorem exercise_2_7_3 :
    quotient_group (nonzero ℝ) (group_of_order_two) ≅ multiplicative ℝ :=
sorry

theorem exercise_2_7_7 {G : Type*} [group G] {G' : Type*} [group G']
    (φ : G →* G') (hφ : function.surjective φ) (N : set G) 
    (hN : is_normal_subgroup N) : is_normal_subgroup (φ '' N) :=
sorry

theorem exercise_2_8_7 {G : Type*} [group G] 
    {A B : set G} (hA : is_subgroup A) (hB : is_subgroup B) 
    (hA_finite : fintype.card A < ⊤) (hB_finite : fintype.card B < ⊤) 
    (hAB_coprime : nat.coprime (fintype.card A) (fintype.card B)) :
    fintype.card (A * B) = fintype. :=
sorry

theorem exercise_2_8_12 (G H : Type*) 
    [group G] [group H] (hG : G.card = 21) (hH : H.card = 21) 
    (hG_nonabelian : ¬ abelian G) (hH_nonabelian : ¬ abelian H) :
    G ≃ H :=
sorry

theorem exercise_2_8_15 (p q : ℕ) 
    (hp : nat.prime p) (hq : nat.prime q) (h : q ∣ p - 1) :
    ∀ (G H : Type*) [group G] [group H] [fintype G] [fintype H] 
    (hG : G.card = p * q) (hH : H.card = p * q) (hGn : ¬ abelian G) 
    (hHn : ¬ abelian H), G ≃ H :=
sorry

theorem exercise_2_9_2 {G₁ G₂ : Type*} [group G₁] [group G₂]
    [is_cyclic G₁] [is_cyclic G₂] :
    is_cyclic (G₁ × G₂) ↔ nat.gcd (order G₁) (order G₂) = 1 :=
sorry

theorem exercise_2_10_1 
    {G : Type*} [group G] {A : subgroup G} (hA : A.normal) 
    (b : G) (hb : b.prime_order) (hbA : b ∉ A) :
    A ∩ (b) = (1) :=
sorry

theorem exercise_2_11_6 {p : ℕ} {G : Type*} 
    [group G] {P : subgroup G} (hP : is_p_group p P) (hPn : P ≤ normalizer G P) :
    ∀ (Q : sylow p G), P = Q :=
sorry

theorem exercise_2_11_7 {p : ℕ} {G : Type*} 
    [group G] {P : subgroup G} (hP : is_p_group p P) (hP_sylow : is_sylow p G P) 
    (φ : G →+* G) : φ '' P = P :=
sorry

theorem exercise_2_11_22 {p : ℕ} {n : ℕ} {G : Type*} [group G]
    (hG : order_of G = p ^ n) (H : subgroup G) (hH : order_of H = p ^ (n - 1)) :
    is_normal H G :=
sorry

theorem exercise_3_2_21 {α : Type*} [decidable_eq α]
  (σ τ : perm α) (hστ : disjoint_support σ τ) (hστ_eq_one : σ * τ = 1) :
  σ = 1 ∧ τ = 1 :=
sorry

theorem exercise_3_2_23 {α : Type*} 
    [fintype α] [decidable_eq α] (σ τ : perm α) 
    (hσ : σ.is_cycle_decomposition) (hτ : τ.is_cycle_decomposition) 
    (h : ∀ i, ∃ j, hσ i = hτ j) :
    ∃ (β : perm α), τ = β * σ * β⁻¹ :=
sorry

theorem exercise_3_3_2 {k : ℕ} (hk : k > 0) (σ : perm k) 
    (hσ : is_k_cycle σ k) :
    is_odd σ ↔ k % 2 = 0 :=
sorry

theorem exercise_3_3_9 (n : ℕ) (h : n ≥ 5) 
    (N : subgroup (perm.perm_group n)) (hN : N ≠ ⊥) (hN_normal : N.normal) :
    ∃ (p : perm n), p.is_three_cycle :=
sorry

theorem exercise_4_1_19 :
    ∃ (x : quaternion ℂ), x ^ 2 = -1 :=
sorry

theorem exercise_4_1_28 {R : Type*} [ring R] [decidable_eq R] 
    [fintype R] [decidable_eq (matrix R 2 2)] :
    group (matrix R 2 2) :=
sorry

theorem exercise_4_1_29 {R : Type*} [comm_ring R] 
    (x : R) :
    det x = 0 ↔ x ≠ 0 ∧ x ∈ zero_divisors R :=
sorry

theorem exercise_4_1_34 (T : Type*) [group T] 
    (hT : ∀ (A : T), det A ≠ 0) :
    is_isomorphic T (perm.group 3) :=
sorry

theorem exercise_4_2_5 {R : Type*} [ring R] 
    (h : ∀ x : R, x ^ 3 = x) : comm_ring R :=
sorry

theorem exercise_4_2_6 {R : Type*} [ring R] (a x : R) 
    (h : a ^ 2 = 0) : a * (x + x * a) = (x + x * a) * a :=
sorry

theorem exercise_4_2_9 (p : ℕ) 
    (hp : nat.prime p) (hp1 : p % 2 = 1) :
    ∀ (a b : ℕ), a / b = ∑ i in (finset.range (p - 1) : finset ℕ), 1 / (i + 1) → p ∣ a :=
sorry

theorem exercise_4_3_1 {R : Type*} [comm_ring R] (a : R) :
    ideal.left_mul a = {x | x * a = 0} :=
sorry

theorem exercise_4_3_4 {R : Type*} [comm_ring R] (I J : ideal R) :
    ideal.is_ideal (I + J) :=
sorry

theorem exercise_4_3_25 {R : Type*} [ring R] 
    (M : Type*) [add_comm_group M] [module R M] [fintype M] [decidable_eq M] 
    (I : ideal R) :
    I = ⊥ ∨ I = ⊤ :=
sorry

theorem exercise_4_4_9 (p : ℕ) (hp : nat.prime p) :
    (p - 1) / 2 = card {x : ℕ | x < p ∧ x.quadratic_residue p} ∧
    (p - 1) / 2 = card {x : ℕ | x < p ∧ x.quadratic_nonresidue p} :=
sorry

theorem exercise_4_5_12 {F K : Type*} [field F] [field K]
    (hF : F ⊆ K) (f g : polynomial F) (hfg : f.coprime g) :
    f.map hF.to_embedding.to_fun.coprime g.map hF.to_embedding.to_fun :=
sorry

theorem exercise_4_5_16 {p : ℕ} (hp : nat.prime p) 
    (n : ℕ) (q : polynomial ℤ_p) (hq : irreducible q) :
    ∃ (F : Type*) [field F], cardinal.mk F = p ^ n :=
sorry

theorem exercise_4_5_23 
    {F : Type*} [field F] (p : polynomial F) (h : p.degree = 3) 
    (h2 : ∀ (a : F), p.eval a = 0 → is_root_of_unity a) : 
    is_irreducible p :=
sorry

theorem exercise_4_5_25 (p : ℕ) (hp : nat.prime p) :
    irreducible (polynomial.q_polynomial p) :=
sorry

theorem exercise_4_6_2 (x : polynomial ℚ) :
  irreducible (x^3 + 3*x + 2) :=
sorry

theorem exercise_4_6_3 (a : ℕ) :
    ∃ (a : ℕ), irreducible (polynomial.C a * polynomial.X ^ 7 + polynomial.C 15 * polynomial.X ^ 2 - polynomial.C 30 * polynomial.X + polynomial.C a) :=
sorry

theorem exercise_5_1_8 {F : Type*} [field F] (hF : char_p F) 
    (a b : F) (m : ℕ) (hmn : m = (char_p.pos hF).pow n) (hn : 0 < n) :
    (a + b) ^ m = a ^ m + b ^ m :=
sorry

theorem exercise_5_2_20 {V : Type*} [add_comm_group V] 
    [vector_space ℂ V] (hV : ∀ (n : ℕ), ∃ (W : submodule ℂ V), 
    (finite.card W = n) ∧ (W ≠ ⊤)) : false :=
sorry

theorem exercise_5_3_7 {K : Type*} [field K] {F : Type*} 
    [field F] (a : K) (h : is_algebraic K (a ^ 2) F) : is_algebraic K a F :=
sorry

theorem exercise_5_3_10 : algebraic ℚ (cos (1 : ℝ)) :=
sorry

theorem exercise_5_4_3 
    (a : ℂ) (h : polynomial.eval ℂ (polynomial.X ^ 5 + √2 * polynomial.X ^ 3 + 
    √5 * polynomial.X ^ 2 + √7 * polynomial.X + √11) a = 0) :
    ∃ (p : polynomial ℂ), degree p ≤ 80 ∧ polynomial.eval ℂ p a = 0 :=
sorry

theorem exercise_5_5_2 : irreducible (polynomial.C (-1) * X^3 + polynomial.C 3 * X + 1) :=
sorry

theorem exercise_5_6_3 (p : polynomial ℚ) 
    (hp : p = X^4 + X^3 + X^2 + X + 1) :
    ∃ (K : Type*) [field K] [algebra ℚ K] (hK : degree K = 4), 
    is_splitting_field ℚ p K :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] 
    (p : ℕ) (hp : p ≠ 0) (n : ℕ) :
    (∀ (x : F), x ^ p ^ n - x = 0) → 
    ∀ (x y : F), x ≠ y → x ^ p ^ n ≠ y ^ p ^ n :=
sorry