import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_1 {X : Type*} [topological_space X]
  {A : set X} (hA : ∀ (x : X), x ∈ A → ∃ (U : set X), is_open U ∧ x ∈ U ∧ U ⊆ A) :
  is_open A :=
sorry

theorem exercise_13_4a1 {β ι : Type*} {X : ι → Type u_3}
  [t : Π (i : ι), topological_space (X i)] {T : Π (i : ι), set (set (X i))}
  (cond : ∀ (i : ι), topological_space.is_topological_basis (T i))
  (f : Π (i : ι), β → X i) :
  topological_space.is_topological_basis {S : set β | ∃ (U : Π (i : ι), set (X i)) (F : finset ι), (∀ (i : ι), i ∈ F → U i ∈ T i) ∧ S = ⋂ (i : ι) (hi : i ∈ F), f i ⁻¹' U i} :=
sorry

theorem exercise_13_4b1 {α : Type u}
  [t : topological_space α] {β : Type v} {s : set (set α)}
  (h : ∀ (b : β), s b ∈ t.is_topological_basis) :
  (⋂ (b : β), s b) ∈ t.is_topological_basis :=
sorry

theorem exercise_13_5a  {α : Type u} [t : topological_space α] {B : set (set α)}
  (hB : topological_space.is_topological_basis B) :
  t = ⋂ (t' : topological_space α) (h : ∀ (u : set α), u ∈ B → is_open u), t' :=
sorry

theorem exercise_13_6  {K : topological_space.compacts ℝ} (hK : topological_space.is_topological_basis K) :
  K ≠ ⊤ :=
sorry

theorem exercise_13_8b :
  topological_space.is_topological_basis (⋃ (a b : ℚ) (h : a < b), {set.Ioo ↑a ↑b}) :=
sorry

theorem exercise_16_4 {X Y : Type*} [topological_space X] [topological_space Y] :
  is_open (pi.fst : X × Y → X) :=
sorry

theorem exercise_17_4 {α : Type u} [topological_space α] {s t : set α}
  (h₁ : is_open s) (h₂ : is_closed t) :
  is_open (s \ t) :=
sorry

theorem exercise_18_8b {α : Type u} {β : Type v} [topological_space α]
  [linear_order α] [order_closed_topology α] {f g : β → α} [topological_space β]
  (hf : continuous f) (hg : continuous g) :
  continuous (λ (b : β), linear_order.min (f b) (g b)) :=
sorry

theorem exercise_19_6a {α : Type u} {β : Type v}
  [topological_space α] [topological_space β] {f : β → α} {x : filter β}
  {a : α} (h : filter.tendsto f x (nhds a)) :
  filter.tendsto (λ (b : β), (f b, a)) x (nhds (a, a)) :=
sorry

theorem exercise_21_6a {x : ℝ} (hx : x ≤ 1) :
  filter.tendsto (λ (n : ℕ), x ^ n) filter.at_top (nhds 0) :=
sorry

theorem exercise_21_8 {α β γ : Type*} [uniform_space α]
  [uniform_space β] [uniform_space γ] {g : β → γ} {f : α → β}
  (hg : uniform_continuous g) (hf : uniform_continuous f) :
  uniform_continuous (g ∘ f) :=
sorry

theorem exercise_22_2b {X A : Type*} [topological_space X]
  [topological_space A] (r : X → A) (hr : continuous r)
  (h : ∀ (a : A), r a = a) : quotient_map r :=
sorry

theorem exercise_23_2 {α : Type u} [topological_space α]
  {ι : Type*} [nonempty ι] {s : ι → set α} (H : ∀ (i : ι), is_connected (s i))
  (K : ∀ (i j : ι), relation.refl_trans_gen (λ (i j : ι), (s i ∩ s j).nonempty) i j) :
  is_connected (⋃ (n : ι), s n) :=
sorry

theorem exercise_23_4 {X : Type*} [fintype X] [infinite X] :
  connected (finite_compl_topology X) :=
sorry

theorem exercise_23_9  {X Y : Type*} [topological_space X] [topological_space Y] {A : set X} {B : set Y}
  (hX : is_connected X) (hY : is_connected Y) (hA : is_proper_subset A X)
  (hB : is_proper_subset B Y) :
  is_connected ((X × Y) - (A × B)) :=
sorry

theorem exercise_24_2 {f : circle_deg1_lift} (hf : continuous ⇑f) :
  ∃ (x : ℝ), ⇑f x = ⇑f (-x) :=
sorry

theorem exercise_25_4 {X : Type*}
  [topological_space X] [loc_path_connected_space X] {U : set X}
  (hU : is_open U) (hUc : is_connected U) : is_path_connected U :=
sorry

theorem exercise_26_11  {X : Type*} [topological_space X] [compact_space X] [t2_space X]
  {A : set (set X)} (hA : ∀ (a b : set X), a ∈ A → b ∈ A → a ⊆ b ∨ b ⊆ a)
  (hA' : ∀ (a : set X), a ∈ A → is_connected a) :
  is_connected (⋂ (a : set X) (h : a ∈ A), a) :=
sorry

theorem exercise_27_4  {X : Type*} [metric_space X] [connected_space X] (hX : ∃ (x y : X), x ≠ y) :
  infinite X :=
sorry

theorem exercise_28_5  {X : Type*} [topological_space X] [topological_space.second_countable_topology X]
  : countably_compact X ↔ ∀ (C : ℕ → set X), (∀ (n : ℕ), is_closed (C n)) ∧ (∀ (n : ℕ), C n.succ ⊆ C n) ∧ (∀ (n : ℕ), C n.succ.nonempty) → (⋂ (n : ℕ), C n).nonempty :=
sorry

theorem exercise_29_1 {α : Type u} [topological_space α]
  [t2_space α] [noncompact_space α] :
  ¬locally_compact_space α :=
sorry

theorem exercise_29_10 {α : Type u}
  [topological_space α] [locally_compact_space α] [t2_space α] {K U : set α}
  (hK : is_compact K) (hU : is_open U) (hKU : K ⊆ U) :
  ∃ (V : set α), is_open V ∧ K ⊆ V ∧ closure V ⊆ U ∧ is_compact (closure V) :=
sorry

theorem exercise_30_13 {X : Type*}
  [topological_space X] [t2_space X] (hX : ∃ (s : set X), s.countable ∧ dense s)
  (h : ∀ (U : set X), is_open U → ∀ (V : set X), is_open V → disjoint U V → U.countable) :
  ∀ (U : set X), is_open U → U.countable :=
sorry

theorem exercise_31_2 {α : Type u} [topological_space α]
  [t2_5_space α] {x y : α} (h : x ≠ y) :
  ∃ (s : set α) (H : s ∈ nhds x) (t : set α) (H : t ∈ nhds y), disjoint (closure s) (closure t) :=
sorry

theorem exercise_32_1 {α : Type u} {β : Type v}
  [topological_space α] [topological_space β] [normal_space β] {f : α → β}
  (hf : closed_embedding f) :
  normal_space α :=
sorry

theorem exercise_32_2b {α : Type u}
  {X : α → Type*} [Π (a : α), topological_space (X a)]
  (h : regular_space (Π (a : α), X a)) :
  ∀ (a : α), regular_space (X a) :=
sorry

theorem exercise_32_3 {X : Type*}
  [topological_space X] [locally_compact_space X] [t2_space X] :
  regular_space X :=
sorry

theorem exercise_33_8 {X : Type*}
  [topological_space X] (A B : set X) (hA : is_compact A) (hB : is_closed B)
  (h : disjoint A B) :
  ∃ (f : X → ℝ), continuous f ∧ f '' A = {0} ∧ f '' B = {1} :=
sorry

theorem exercise_38_6 {X : Type*}
  [topological_space X] [compact_space X] [compact_space (β X)]
  [t2_space X] [t2_space (β X)] (hX : connected_space X) :
  connected_space (β X) :=
sorry