import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_26 {M : Type*} [metric_space M] [metric_space.is_complete_space M]
  {U : set M} (hU : U ∈ 𝓝 x) :
  U ∈ 𝓝 x ↔ ∀ y ∈ U, y ∉ U :=
sorry

theorem exercise_2_32a {A : Type*} [set A] [set.finite A]
  (hA : A.card < ℵ₀) :
  clopen_of_infinite A :=
sorry

theorem exercise_2_46  {A B : Type*} [metric_space A] [metric_space B] [metric_space M]
  (hA : compact_space A) (hB : compact_space B) (f : A → M) (g : B → M) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_2_92 {ι : Type*} {K : ι → Type*}
  [nonempty ι] [nonempty K] (hK : ∀ i, compact_space (K i))
  (h : ∀ i, nonempty (K i)) :
  nonempty (⋃ i, K i) :=
sorry

theorem exercise_3_1  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : constant (g ∘ f)) : constant f :=
sorry

theorem exercise_3_63asum_div_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_log_pow_of_:=
sorry

theorem exercise_4_15a {a b : ℝ}
  (h : a ≤ b) (f : [a, b] → ℝ) (hf : continuous_on f [a, b]) :
  uniform_continuous f ↔ continuous_on f [a, b] :=
sorry