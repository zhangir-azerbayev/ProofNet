import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_12a {α β : Type*} [linear_ordered_field α]
  [topological_space α] [order_topology α] {f : ℕ → α} (hf : summable f⁻¹)
  (hf' : ∀ (n : ℕ), 0 < f n) {g : ℕ → ℕ} (hg : function.injective g) :
  filter.tendsto f filter.at_top filter.at_top :=
sorry

theorem exercise_2_29 {α : Type*}
  [topological_space α] :
  function.bijective topological_space.opens.compl :=
sorry

theorem exercise_2_41 {m : ℕ} :
  is_compact {x : ℝ ^ m | ∥x∥ ≤ 1} :=
sorry

theorem exercise_2_57 {α : Type u}
  [topological_space α] {s : set α} (h : is_connected s) :
  ¬ is_connected (interior s) :=
sorry

theorem exercise_2_126 {α : Type u}
  [pseudo_emetric_space α] {x : α} {E : set α} (h : x ∉ closure E) :
  ∃ (ε : ℝ), 0 < ε ∧ ennreal.of_real ε < emetric.inf_edist x E :=
sorry

theorem exercise_3_4 (n : ℕ) :
	nat.sqrt n * nat.sqrt n < n + 1 :=
sorry

theorem exercise_3_63b {p : ℝ} (h : p ≤ 1) :
	¬ summable (λ (n : ℕ), 1 / (↑n * (real.log ↑n) ^ p)) :=
sorry