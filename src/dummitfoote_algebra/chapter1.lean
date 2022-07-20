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

theorem section1_exercise20 (G: Type*) [group G] (x: G) : 
order_of x = order_of x⁻¹ := 
begin 
  simp, 
end 

theorem section1_exercise22_parta (G: Type*) [group G] (x g: G): 
order_of x = order_of (g⁻¹ * x⁻¹ *g) := sorry 

theorem section1_exercise22_partb (G: Type*) [group G] (a b: G): 
order_of (a*b) = order_of (b*a) := sorry 


theorem section1_exercise25 (G: Type*) [group G] 
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

theorem section1_exercise29 
  (G : Type*) [group G]
  (H : Type*) [group H] : 
(is_comm (G × H)) ↔ (is_comm G ∧ is_comm H) :=
begin 
  sorry, 
end 

theorem section1_exercise34
  (G: Type*) [group G]
  (x : G)
  (h : order_of x = 0) : 
∀ n m : ℕ, n ≠ m → x ^ n ≠ x ^ m := sorry 

/-
theorem section3_exercise13
  (α: Type*) [fintype α]
  (S : equiv.perm α)
  (σ : S)
  (hs : σ^2 = 1) : 
∃ 
-/ 

theorem section6_exercise11
  (A : Type*) [group A]
  (B : Type*) [group B] : 
(A × B) ≃* (B × A) := 
begin 
  sorry
end

theorem section6_exercise17
  (G : Type*) [group G]
  (f : G → G)
  (hf : f = λ x : G, x⁻¹) : 
is_hom f ↔ is_comm G := sorry 

theorem section6_exercise18
  (G : Type*) [group G]
  (f : G → G)
  (hf : f = λ x : G, x ^ 2) :
is_hom f ↔ is_comm G := sorry 







