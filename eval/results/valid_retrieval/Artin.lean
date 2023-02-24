import .common 

open function fintype subgroup ideal polynomial submodule zsqrtd char_p
open_locale big_operators
noncomputable theory





theorem exercise_2_2_9 {G : Type*} [group G] {a b : G} (hab : a * b = b * a) :
  is_abelian (subgroup.generated (set.singleton a) (set.singleton b)) :=
sorry

theorem exercise_2_4_19 {G : Type*}
  [group G] (h : ∃ (x : G), ∀ (y : G), y * x = x * y ↔ y = 1 ∨ y = x) :
  ∀ (x : G), ∃! (y : G), y * x = x * y :=
sorry

theorem exercise_2_11_3 {G : Type u} {x : G} {n : ℕ}
  [add_monoid G] (h : n.coprime (add_order_of x)) :
  ∃ (m : ℕ), m • n • x = x :=
sorry

theorem exercise_3_5_6 {K : Type*} {V : Type u}
  [division_ring K] [add_comm_group V] [module K V] {s : set V}
  (hs : s.countable) (h : linear_independent K (λ (x : ↥s), ↑x)) :
  ∃ (h : s.finite ∨ s.countable), true :=
sorry

theorem exercise_6_1_14 {p : ℕ}
  {G : Type*} [group G] [fintype G] [fact (nat.prime p)]
  (hG : fintype.card G = p ^ 2) :
  is_cyclic (G ⧸ subgroup.center G) :=
sorry

theorem exercise_6_4_3 {α : Type u} [group α] [fintype α]
  {p q : ℕ} [hp : fact (nat.prime p)] [hq : fact (nat.prime q)]
  (h : fintype.card α = p * q) :
  ¬ is_simple_group α :=
sorry

theorem exercise_6_8_1 {G : Type*} [group G] {a b c : G}
  (h : a = c * b⁻¹) :
  subgroup.generated (a :: b :: []) = subgroup.generated (b * a * b^2 :: b * a * b^3 :: []) :=
sorry

theorem exercise_10_2_4 {R : Type u} [comm_ring R] {x : R}
  (I J : ideal R) :
  I ∩ J = ideal.span {x} * J ↔ (∀ (zI : R), zI ∈ I → (∃ (zJ : R) (H : zJ ∈ J), x * zJ = zI)) ∧ ∀ (z : R), z ∈ J → x * z ∈ I :=
sorry

theorem exercise_10_4_6 {R : Type u} [comm_semiring R]
  {I J : ideal R} (h : I ⊔ J = ⊤) {x : R} (hx : x ∈ I ⊓ J) :
  ∃ (n : ℕ), x ^ n ∈ I ^ n ⊓ J :=
sorry

theorem exercise_10_7_10 {α : Type u} {a : α} [comm_semiring α]
  (h : a ∈ nonunits α) :
  ∃ (I : ideal α), I.is_maximal ∧ a ∈ I :=
sorry

theorem exercise_11_4_1b {R : Type u} [comm_ring R] [is_domain R]
  (r : R) :
  irreducible (polynomial.X - ⇑polynomial.C r) :=
sorry

theorem exercise_11_4_6b {R : Type u} [field R]
  {p : polynomial R} (hp1 : p.degree = 1) (hm : p.monic) :
  irreducible p :=
sorry

theorem exercise_11_4_8 {p : ℕ} [fact (nat.prime p)] (n : ℕ) :
  irreducible (polynomial.X ^ n - p) :=
sorry

theorem exercise_13_4_10 {p : ℕ}
  (hp : nat.prime p) (h : p = 2 ^ (nat.find (λ (n : ℕ), p % 2 ^ n = 1) p) + 1) :
  ∃ (k : ℕ), p = 2 ^ (2 ^ k) + 1 :=
sorry