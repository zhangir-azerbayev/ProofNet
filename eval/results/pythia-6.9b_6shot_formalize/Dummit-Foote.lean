import .common 

open set function nat int fintype real polynomial mv_polynomial
open subgroup ideal submodule zsqrtd gaussian_int char_p mul_aut matrix

open_locale pointwise
open_locale big_operators
noncomputable theory





theorem exercise_1_1_2a {Z : Type*} [field Z] [add_comm_group Z]
  [module Z Z] (a : Z) (b : Z) (h : a.star b ≠ b.star a) :
  a.star b ≠ b.star a :=
sorry

theorem exercise_1_1_4 {n : ℕ} [ring Z] :
  (∀ (a : Z), ∀ (b : Z), a * (b * (a * b)) ≡ a * b) :=
sorry

theorem exercise_1_1_15 {G : Type*} [group G] 
  [add_comm_group G] [module G G] [finite_dimensional G] 
  (a : G) (h : ∀ (i : ℤ), a ^ i = 1) :
  ∀ (i : ℤ), a ^ i = 1 :=
sorry

theorem exercise_1_1_17 {G : Type*} [group G] (x : G)
  (hx_inf : ∀ n : ℕ, x ^ n ≠ 1) :
  ∀ m n : ℤ, m ≠ n → x ^ m ≠ x ^ n :=
sorry

theorem exercise_1_1_20 {G : Type*} [group G] 
  [group G] (x : G) (hx : x.order = x.inverse_order) :
  x.order = x.inverse_order :=
sorry

theorem exercise_1_1_22b {G H : Type*} [group G] [group H]
  [is_group G] [is_group H] (f : G →* H) (hf : f.ker ≤ center G) :
  group_law G :=
sorry

theorem exercise_1_1_29  {A B : Type*} [group A] [group B] [group (A × B)] (h : abelian A) (h' : abelian B) :
  abelian (A × B) :=
sorry

theorem exercise_1_3_8 {S : Type*} [set S]
  [group S] (s : S) : infinite_group S :=
sorry

theorem exercise_1_6_11 {A B : Type*} [group A] [group B]
  [is_group A] [is_group B] (f : A →* B) (g : B →* A) (h : f.is_isomorphic g) :
  f.opposite_product = g.opposite_product :=
sorry

theorem exercise_1_6_23 {G : Type*} [group G] 
  [group G] (g : G) (h : ∀ x : G, x = 1 → x = g x) (h : ∀ x : G, x = g x → x = x) :
  ∀ x : G, x = 1 → x = x :=
sorry

theorem exercise_2_1_13 [additive_group R] 
  [field R] [ring R] [finite_dimensional R] (h : ∀ x : R, x ∈ H → x ∈ H) :
  ∀ x : R, x ∈ H → x ∈ H :=
sorry

theorem exercise_2_4_16a {G : Type*} [group G] 
  [finite_group G] (H : proper_subgroup G) (h : ∃ M : ℤ, M ≤ G.card) :
  ∃ M : ℤ, M ≤ G.card :=
sorry

theorem exercise_2_4_16c  {G : Type*} [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G]:=
sorry

theorem exercise_3_1_22a {G : Type*} [group G] 
  [group H] [group K] (h : normal_subgroup H ∩ normal_subgroup K) :
  normal_subgroup (H ∩ K) :=
sorry

theorem exercise_3_2_8 {G : Type*} [group G] 
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : card H = p ^ n) :
  H.intersect H = 1 :=
sorry

theorem exercise_3_2_16fermat_little_theorem_lagrange_theorem_in_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_p_z_:=
sorry

theorem exercise_3_3_3  {G H : Type*} [group G] [group H] [group G] [group H]
  (f : G →* H) (hf : f.ker ≤ center G) (h : prime.index f = prime.index f.ker)
  (h : normal_subgroup G) : normal_subgroup H :=
sorry

theorem exercise_3_4_4 {G : Type*} [group G] 
  [finite_group G] (n : ℕ) (h : ∃ (H : subgroup G), order H = n) :
  ∃ (H : subgroup G), order H = n :=
sorry

theorem exercise_3_4_5b {G : Type*} [group G]
  [solvable G] (f : G →* G) (hf : f.ker ≤ center G) :
  solvable G :=
sorry

theorem exercise_4_2_8 {G : Type*} [group G] 
  [finite_index G] (H : normal G) (n : ℕ) :
  index G H ≤ n :=
sorry

theorem exercise_4_2_9a {G : Type*} [group G] 
  [fintype G] {p : ℕ} [hp : fact (nat.prime p)] {H : subgroup G} 
  (hH : index H = p) :
  index H = p :=
sorry

theorem exercise_4_4_2  {G : Type*} [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group G] [group G] [group G] [group G] [group G]
  [group G] [group G] [group:=
sorry

theorem exercise_4_4_6b {G : Type*} [group G] 
  [group G] (h : normal G) :
  ∃ (H : subgroup G), H ≠ G :=
sorry

theorem exercise_4_4_8a  {G H K : Type*} [group G] [group H] [group K] [group G] [group H] [group K]
  (h : H ≤ K) (k : K ≤ G) (hk : H ≤ K) (hk : K ≤ G) (h : H ≤ K) (k : K ≤ G)
  (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G)
  (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G)
  (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G)
  (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G)
  (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G)
  (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G)
  (h : H ≤ K) (k : K ≤ G) (h : H ≤ K) (k : K ≤ G) (h ::=
sorry

theorem exercise_4_5_13 {G : Type*} [group G] [group G]
  [is_cyclic G] (p : ℕ) (H : p.divides 56) :
  Sylow p.subgroup G :=
sorry

theorem exercise_4_5_15 {G : Type*} [group G] [group G]
  [is_cyclic G] (p : ℕ) (n : ℕ) (H : order G = p ^ n) :
  Sylow p.subgroup G :=
sorry

theorem exercise_4_5_17 {G : Type*} [group G] [group G]
  [is_105 G] (h : 105 = |G|) :
  Sylow_5_7_subgroups G :=
sorry

theorem exercise_4_5_19 {G : Type*} [group G] [group G]
  [is_simple G] (h : 6545 = |G|) : simple G :=
sorry

theorem exercise_4_5_21 {G : Type*} [group G] [group G]
  [is_simple G] (h : 2907 = |G|) : simple_group G :=
sorry

theorem exercise_4_5_23 {G : Type*} [group G] [order G]
  [is_simple G] (h : order G = 462) : not (simple G) :=
sorry

theorem exercise_4_5_33 {G : Type*} [group G] 
  [fintype G] {p n : ℕ} [hp : fact (nat.prime p)] {P : Sylow p G} 
  (hP : P = Sylow p G) (hH : P ∩ H = H) :
  P ∩ H = Sylow p H :=
sorry

theorem exercise_7_1_2  {R : Type*} [ring R] [ring R] [ring R] [ring R] [ring R] [ring R]
  (u : unit R) (u_neg : -u ≡ 1) (u_unit : unit u) (u_unit_neg : unit_of_unit u_neg)
  (u_unit_unit : unit_of_unit u_unit) (u_unit_unit_neg : unit_of_unit u_unit_neg)
  (u_unit_unit_unit : unit_of_unit u_unit_unit) : unit R :=
sorry

theorem exercise_7_1_12 {K : Type*} [field K]
  [add_comm_group K] [module K K] [subring K] [subfield K]
  (h : subring.contains_identity) : integral_domain K :=
sorry

theorem exercise_7_2_2  {R : Type*} [ring R] [ring R] [ring R] [ring R] [ring R] [ring R]
  (p : R[x]) (b : R) (h : b p = 0) :
  zero_divisor p → zero_divisor (b p) :=
sorry

theorem exercise_7_3_16 {R S : Type*} [ring R] [ring S]
  [is_surjective R] [is_surjective S] (f : R →* S) (hf : f.ker ≤ center R) :
  image_of_center R ≤ center S :=
sorry

theorem exercise_7_4_27  {R : Type*} [ring R] [ring R] [ring R] [ring R] [ring R] [ring R] [ring R]
  (a : nilpotent R) (b : unit R) (c : unit R) (d : unit R) (e : unit R)
  (f : unit R) :
  (1-a b) * (1-c d) = (1-a b) * (1-c d) * (1-a b) * (1-c d) :=
sorry

theorem exercise_8_2_4  {R : Type*} [integral_domain R] [ring R] [ring R] [ring R]
  (a b : R) (r s : gcd a b = r a + s b) (h : gcd a b = 1) :
  principal_ideal_domain R :=
sorry

theorem exercise_8_3_5a {R : Type*} [ring R] [finite_field R]
  [is_field R] (n : ℕ) (h : 2 ≠ 0) (h2 : ∀ x : R, x ^ 2 ≠ 1) (h1 : ∀ x : R, x ^ 1 ≠ 1) :
  ∃ (x : R), x ≠ 0 ∧ x ^ 2 ≠ 1 ∧ x ^ 1 ≠ 1 :=
sorry

theorem exercise_8_3_6b  {q : ℕ} [field_with_q_squared_elements q] (p : prime q) :
  quotient_ring_of_prime_mod_4 p :=
sorry

theorem exercise_9_1_10 {n : ℕ} (p : ℕ) (I : p.prime) :
  ∃ (x : ℕ), p.prime.is_minimal I.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is_prime.is:=
sorry

theorem exercise_9_4_2a :
  irreducible_in_Z_x_4_x_3_6 :=
sorry

theorem exercise_9_4_2c  {n : ℕ} (p : n → ℕ) (q : n → ℕ) (r : n → ℕ) (s : n → ℕ) (t : n → ℕ)
  (u : n → ℕ) (v : n → ℕ) (w : n → ℕ) (x : n → ℕ) (y : n → ℕ) (z : n → ℕ)
  (wz : n → ℕ) (xw : n → ℕ) (yw : n → ℕ) (zw : n → ℕ) (xwz : n → ℕ)
  (ywz : n → ℕ) (zw : n → ℕ) (xwz : n → ℕ) (ywz : n → ℕ) (zw : n → ℕ)
  (xwz : n → ℕ) (ywz : n → ℕ) (zw : n → ℕ) (xwz : n → ℕ) (ywz : n → ℕ)
  (zw : n → ℕ) (xwz : n → ℕ) (ywz : n → ℕ) (zw : n → ℕ) (xwz : n → ℕ)
  (ywz : n → ℕ) (zw : n → ℕ) (xwz : n → ℕ) (ywz : n → ℕ) (zw : n →:=
sorry

theorem exercise_9_4_9 {Z : Type*} [field Z] [UFD Z]
  [polynomial Z] (x : Z) (h : x ^ 2 - sqrt 2 ≠ 0) :
  irreducible_over_Z_sqrt_2 x :=
sorry

theorem exercise_11_1_13 [field R] [field Q]
  [add_comm_group R] [add_comm_group Q] [module R Q] [finite_dimensional R Q]
  {n : ℕ} (h : finite_dimensional.finrank R + 1 < n) :
  vector_spaces_over_Q_R_n_cong_R_n_Q :=
sorry