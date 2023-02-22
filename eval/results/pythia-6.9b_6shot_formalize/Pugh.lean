import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_26 {M : Type*} [metric_space M]
  [metric_space M] (U : set M) (h : ∀ x, x ∉ U → x ∉ U.complement) :
  open U :=
sorry

theorem exercise_2_32a {A : Type*} [set A] [set A]
  (h : ∀ x : A, x ∈ clopen A) : clopen A :=
sorry

theorem exercise_2_46  {A B : Type*} [metric_space A] [metric_space B] [metric_space M]
  (a0 : A) (b0 : B) (a : A) (b : B) (d : M) (h : disjoint a b) :
  disjoint a b :=
sorry

theorem exercise_2_92  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : nested_decreasing (g ∘ f)) : nested_decreasing (f ∘ g) :=
sorry

theorem exercise_3_1  {X : Type*} [metric_space X] [metric_space X] [metric_space X]
  (f : X → X) (g : X → X) (h : continuous g) (hgi : function.injective g)
  (h : 2nd_order_inequality (g ∘ f)) : 2nd_order_inequality f :=
sorry

theorem exercise_3_63a [p : ℕ] :
  ∃ (k : ℕ), sum 1/k (log k)^p = 1/k (log k)^p :=
sorry

theorem exercise_4_15a {a b : Type*} [metric_space a] [metric_space b]
  (f : a → b) (g : b → b) (h : continuous g) (hgc : continuous g.comp)
  (hgi : function.injective g) (h : uniform_continuous (g ∘ f)) :
  uniform_continuous f :=
sorry