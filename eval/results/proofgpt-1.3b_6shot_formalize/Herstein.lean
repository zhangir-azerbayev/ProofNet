import .common 

open set function nat fintype real subgroup ideal polynomial submodule zsqrtd 
open char_p mul_aut matrix
open_locale big_operators
noncomputable theory





theorem exercise_2_1_18  {G : Type*} [group G] [group G] [order G] [finite_order G]
  (hG : even_order G) :
  ∃ a : G, a ≠ e ∧ a = a⁻¹ :=
sorry

theorem exercise_2_1_26 {G : Type*} 
  [group G] [fintype G] (a : G) (n : ℕ) :
  ∃ (f : G →* G), a.sum (λ (e : G), f e • e) = 0 ∧ a.sum (λ (e : G), f e) = 0 ∧ ∃ (x : G) (H : x ∈ a.support), f x ≠ 0 :=
sorry

theorem exercise_2_2_3 :
  abelian_group (ℤ/3ℤ) :=
sorry

theorem exercise_2_2_6cgroup_of_order_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_pow_n_:=
sorry

theorem exercise_2_3_16 {G : Type*} [group G] [is_cyclic G] :
  G.is_cyclic_of_prime_order ↔ G.is_prime :=
sorry

theorem exercise_2_5_23 {G : Type*} 
  [group G] [group_with_zero G] [fintype G] {a b : G} (ha : a ≠ 1) (hb : b ≠ 1) :
  ∃ (f : G →* G), a.sum (λ (e : G), f e • e) = 0 ∧ b.sum (λ (e : G), f e) = 0 ∧
    ∃ (x : G) (H : x ∈ a.normalizer ⧸ a.normalizer.subtype), f x ≠ 0 :=
sorry

theorem exercise_2_5_31 {G : Type*} [group G]
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G}
  (hH : abelian H) (hG : order G = p ^ n) :
  H.char_subgroup = G :=
sorry

theorem exercise_2_5_43 {G : Type*} [group G] [order_9 G] :
  abelian G :=
sorry

theorem exercise_2_5_52  {G : Type*} [group G] [fintype G] [fintype G] [fintype G] [fintype G] [fintype G]
  (h : fintype.card G = 3) (hG : fintype.card G = 4) (hG' : fintype.card G = 6) :
  abelian G :=
sorry

theorem exercise_2_7_7 {G G' : Type*} [group G] [group G']
  [fintype G] [fintype G'] {N : subgroup G} (hN : N.normalizer = G) :
  card (N.normalizer.comap N.normalizer.subtype) ≡ card (G ⧸ N) [MOD p] :=
sorry

theorem exercise_2_8_15 {G H : Type*} [group G] [group H]
  [is_cyclic H] (f : G →* H) (hf : f.ker ≤ center G) :
  is_isomorphic G H ↔ card (G ⧸ H) = 1 :=
sorry

theorem exercise_2_10_1 {G : Type*} [group G]
  [group_with_zero G] (A : subgroup G) (b : G) (hA : A.normal) (hb : b ∈ A) :
  A ∩ (b) = (e) :=
sorry

theorem exercise_2_11_7  {G : Type*} [group G] [group_with_zero G] [is_prime_order G]
  (P : subgroup G) (hP : P.is_prime) :
  P.is_prime :=
sorry

theorem exercise_3_2_21 {X Y : Type*} [metric_space X] [metric_space Y]
  (hX : X ≃ X) (hY : Y ≃ Y) (h : X ≅ Y) (h_eq : h ∘ h = h ∘ h_eq) :
  permute_by_isometry h h_eq = h :=
sorry

theorem exercise_4_1_34 {K : Type*} [field K]
  [add_comm_group K] [module K K] [finite_dimensional K K]
  (h : ∀ A : K, ∃ (x : K), A = x * x) :
  is_isomorphic K (λ A : K, A.det) :=
sorry

theorem exercise_4_2_6commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with_a_iff_commutes_with:=
sorry

theorem exercise_4_3_1 {R : Type*} [comm_ring R]
  (a : R) : ideal R :=
sorry

theorem exercise_4_4_9 {p : ℕ} (h : p ≡ 1 [MOD 2]) :
  (p - 1)/2 ≡ 1 [MOD p] :=
sorry

theorem exercise_4_5_23 [fintype G] [fintype H]
  {p : G → G} {q : G → G} (h : ∀ x, p x ≠ 1 → q x ≠ 1) :
  is_prime (p.degree) (q.degree) :=
sorry

theorem exercise_4_6_2  {f : Q[x]} (hf : irreducible f) : irreducible (f.polynomial) :=
sorry

theorem exercise_5_1_8char_p_pow_eq_char_p_pow_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_char_p_pow_:=
sorry

theorem exercise_5_3_7 {K : Type*} [field K] [field F]
  [field K] [algebraic_closure K F] {a : K} (ha : a^2 ∈ F) :
  algebraic K a :=
sorry

theorem exercise_5_4_3  {K : Type*} [field K] [field K] [algebra K K] [is_scalar_tower K K]
  {a : K} (ha : a ≠ 0) (h : is_root_of_unity a) :
  algebraic_degree K a ≤ 80 :=
sorry

theorem exercise_5_6_14 {F : Type*} [field F] [char_p F p]
  (x : F) (hx_p : x ≠ 0) :
  ∀ m : ℕ, m ≠ p → x ^ m ≠ x :=
sorry