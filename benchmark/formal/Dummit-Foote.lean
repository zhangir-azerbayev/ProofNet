import algebra.group.basic
import group_theory.order_of_element
import data.fintype.basic
universes u v

noncomputable theory

-- Defines some predicates, as opposed to the comm_group structure
def is_comm (G : Type*) [group G] : Prop := ∀ (a b : G), a * b = b * a
def is_hom {G H: Type*} [group G] [group H] (f : G → H) : Prop :=
  f 1 = 1 ∧ ∀ x : G, f x⁻¹ = (f x)⁻¹ ∧
  ∀ x y : G, f (x*y) = (f x) * (f y)

theorem exercise_1_20 (G: Type*) [group G] (x: G) :
order_of x = order_of x⁻¹ :=
begin
  simp,
end

theorem exercise_1_22_parta (G: Type*) [group G] (x g: G):
order_of x = order_of (g⁻¹ * x⁻¹ *g) := sorry

theorem exercise_1_22_partb (G: Type*) [group G] (a b: G):
order_of (a*b) = order_of (b*a) := sorry


theorem exercise_1_25 (G: Type*) [group G]
  (h : ∀ x:G, x * x = 1):
is_comm G :=
begin
   intros a b,
   have ha : _ := h a,
   have hb : _ := h b,
   have hab : _ := h (a*b),
   rw mul_eq_one_iff_eq_inv at ha,
   rw mul_eq_one_iff_eq_inv at hb,
   rw mul_eq_one_iff_eq_inv at hab,
   rw [hab, mul_inv_rev, ←ha, ←hb],
end

theorem exercise_1_29
  (G : Type*) [group G]
  (H : Type*) [group H] :
(is_comm (G × H)) ↔ (is_comm G ∧ is_comm H) :=
begin
  sorry,
end

theorem exercise_1_34
  (G: Type*) [group G]
  (x : G)
  (h : order_of x = 0) :
∀ n m : ℕ, n ≠ m → x ^ n ≠ x ^ m := sorry

theorem exercise_6_11
  (A : Type*) [group A]
  (B : Type*) [group B] :
(A × B) ≃* (B × A) :=
begin
  sorry
end

theorem exercise_6_17
  (G : Type*) [group G]
  (f : G → G)
  (hf : f = λ x : G, x⁻¹) :
is_hom f ↔ is_comm G := sorry

theorem exercise_6_18
  (G : Type*) [group G]
  (f : G → G)
  (hf : f = λ x : G, x ^ 2) :
is_hom f ↔ is_comm G := sorry
