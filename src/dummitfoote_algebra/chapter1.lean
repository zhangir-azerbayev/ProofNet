import algebra.group.basic
import group_theory.order_of_element

universes u v
noncomputable theory 

theorem section1_exercise20 (G: Type u) [group G] (x: G) : 
order_of x = order_of x⁻¹ := 
begin 
  simp, 
end 

theorem section1_exercise22_parta (G: Type u) [group G] (x g: G): 
order_of x = order_of (g⁻¹ * x⁻¹ *g) := sorry 

theorem section1_exercise22_partb (G: Type u) [group G] (a b: G): 
order_of (a*b) = order_of (b*a) := sorry 

theorem section1_exercise25 (G: Type u) [group G] 
  (h : ∀ x:G, x * x = 1): 
/- 
Here, am I asserting that G is commutative with same multiplication as 
in group G, or just that there is a comm_group structure on G, not 
necessarily commensurate with group G?
-/  
comm_group G := 
-- How do I use `refine` correctly here? 
begin 
 have h_comm : ∀ (a b : G), a * b = b * a :=
  begin 
   intros a b, 
   have ha : _ := h a, 
   have hb : _ := h b, 
   have hab : _ := h (a*b), 
   rw mul_eq_one_iff_eq_inv at ha, 
   rw mul_eq_one_iff_eq_inv at hb, 
   rw mul_eq_one_iff_eq_inv at hab, 
   rw [hab, mul_inv_rev, ←ha, ←hb], 
  end, 
 sorry, 
end

theorem section1_exercise29 
  (G : Type u) [group G]
  (H : Type v) [group H]: Type := 
-- How do I say comm_group G with a Prop? 
-- (comm_group (G × H)) ↔ (comm_group G ∧ comm_group H) errors 
begin 
  sorry, 
end 

theorem section1_exercise34
  (G: Type u) [group G]
  (x : G)
  (h : order_of x = 0) : 
∀ n m : ℕ, n ≠ m → x ^ n ≠ x ^ m := sorry 






