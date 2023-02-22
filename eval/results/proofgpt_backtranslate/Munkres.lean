import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3b {X : Type*} [topological_space X] :
	¬alexandroff.infty ∉ nhds alexandroff.infty :=
sorry

theorem exercise_13_4a2 {α : Type u}
	{ι : Sort w} [t : ι → topological_space α] (h : (⋃ (i : ι), t i).is_topological) :
	¬is_topological_space α :=
sorry

theorem exercise_13_4b2 {α : Type u} {ι : Sort w}
	(t : ι → topological_space α) (h : ∀ (i : ι), topological_space.generate_from (t i) ≤ t i) :
	∃! (s : topological_space α), (∀ (i : ι), s ≤ t i) ∧ s ≤ topological_space.generate_from (set.range t) :=
sorry

theorem exercise_13_5b {α : Type u} {ι : Sort w}
	{s : ι → set (set α)} (hs : ∀ (t : set (set α)), t.is_subbasis → t ⊆ s) :
	topological_space.generate_from s = ⨅ (i : ι), topological_space.generate_from (s i) :=
sorry

theorem exercise_13_8a :
	(nhds_within 0 (set.Ioo 0 1)).has_basis (λ (a b : ℚ), a < b ∧ a ≠ 0 ∧ b ≠ 1 ∧ ∀ (x : ℚ), x ∈ set.Ioo 0 1 → x ≠ 0 ∧ x ≠ 1) (λ (a b : ℚ), (a, b)) :=
sorry

theorem exercise_16_1 {α : Type u}
	{t : topological_space α} {a : set α} {s : set ↥a} (h : s ⊆ t) :
	⊤ = has_subset.subset a :=
sorry

theorem exercise_16_6 :
	(⋃ (a b : ℚ) (c d : ℚ), {↑a :=
sorry

theorem exercise_18_8a {α : Type u} {β : Type v} [topological_space α]
	[preorder α] [t : order_closed_topology α] [topological_space β] {f g : β → α}
	(hf : continuous f) (hg : continuous g) :
	is_closed {b : β | f b ≤ g b} :=
sorry

theorem exercise_18_13 {α β : Type*} [topological_space α]
	[topological_space β] {A : set α} {f : C(α, β)} [t2_space β]
	(h : ∃ (g : C(↥A, β)), continuous_map.extend f g = f) {g : C(↥A, β) → β}
	(hg : continuous g) :
	g = continuous_map.extend f g :=
sorry

theorem exercise_20_2 {ι : Type u} [hι : nonempty ι] :
	topological_space.metrizable_space (d2_real ι) :=
sorry

theorem exercise_21_6b (f : ℕ → ℝ) {n : ℕ}
	(hn : n ≠ 0) :
	¬tendsto_uniformly (λ (x : ℝ), f x ^ n) (nhds 0) :=
sorry

theorem exercise_22_2a {α β : Type*}
	[topological_space α] [topological_space β] (p : C(α, β)) (h : continuous_map.refl β p) :
	quotient_map p :=
sorry

theorem exercise_22_5 {α β : Type*} [topological_space α]
	[topological_space β] {p : α → Prop} {q : β → Prop} (hp : is_open_map p)
	(hq : is_open_map q) :
	is_open_map (λ (a : α), q a) :=
sorry

theorem exercise_23_3 {α : Type u} {β : Type v}
	[topological_space α] {s : set β} {f : β → set α}
	(h₁ : ∀ (i : β), i ∈ s → is_connected (f i))
	(h₂ : ∀ (i : β), i ∈ s → (f i ∩ f (⇑f i)).nonempty) :
	is_connected (s ∪ ⋃ (i : β) (H : i ∈ s), f i) :=
sorry

theorem exercise_23_6 {α : Type u} [topological_space α]
	{s t : set α} (h : is_connected s) (h' : sᶜ ∩ t = ⊥) :
	is_connected t :=
sorry

theorem exercise_23_11 {α β : Type*} [topological_space α]
	[topological_space β] {p : α → β} (quotient_map : quotient_map p)
	(h : ∀ (y : β), is_connected (p ⁻¹' {y}))
	(h' : ∀ (x : α), is_connected (⇑p x)) :
	is_connected (⇑p ⁻¹' {⟨y, _⟩}) :=
sorry

theorem exercise_24_3a {α : Type*} [metric_space α]
	{K : nnreal} {f : α → α} (hf : contracting_with K f) (hK : K ≠ 0) :
	∃ (x : α), function.is_fixed_pt f x :=
sorry

theorem exercise_25_9 {G : Type*} [topological_space G]
	[group G] [topological_group G] (C : subgroup G)
	(hC : is_component C) (h : C ≤ subgroup.comap continuous_id G) :
	C.normal :=
sorry

theorem exercise_26_12 {α β : Type*}
	[topological_space α] [topological_space β] (p : C(α, β)) (hp : is_closed {x : α | ⇑p x ≠ x})
	(h : ∀ (y : β), is_compact (⇑p ⁻¹' {y})) :
	is_compact (space.compact α) :=
sorry

theorem exercise_28_4 (α : Type u)
	[topological_space α] :
	t1_space α ↔ ∀ (ι : Type u) (U : ι → set α), (∀ (i : ι), is_open (U i)) → (∃ (K : set α), (∀ (i : ι), is_compact (U i)) ∧ K ⊆ ⋃ (i : ι), U i) :=
sorry

theorem exercise_28_6 {α : Type u}
	[metric_space α] (f : α → α) (h : ∀ (x y : α), has_dist.dist (f x) (f y) = has_dist.dist x y) :
	function.bijective f :=
sorry

theorem exercise_29_4 :
	¬locally_compact (cardinal.Icc 0 1) :=
sorry

theorem exercise_30_10 {β : Type*} [topological_space β]
	{ι : Type*} [encodable ι] [topological_space.countable_space β]
	(hd : ∀ (i : ι), dense (set.range (λ (b : β), (i, b))) :
	∃ (s : set β), s.countable ∧ dense s :=
sorry

theorem exercise_31_1 {α : Type u} [topological_space α]
	(h : ∀ (x y : α), nhds x ⊓ nhds y = ⊥) :
	regular_space α :=
sorry

theorem exercise_31_3 {α : Type*} [partial_order α]
	[order_topology α] :
	order_topology.is_regular α :=
sorry

theorem exercise_32_2a {α : Type u}
	[t : topological_space α] {ι : Type*} {f : ι → set α}
	(H : ∀ (i : ι), is_Hausdorff (f i)) :
	nonempty (set.range f) :=
sorry

theorem exercise_32_2c {ι α : ι → Type*}
	[Π (i : ι), set (α i)] (H : ∀ (i : ι), nonempty (α i)) :
	normal (set.univ.pi set.univ) :=
sorry

theorem exercise_33_7 {X : Type*} [topological_space X]
	[locally_compact_space X] [t2_space X] :
	is_complete_regular X :=
sorry

theorem exercise_34_9 {X : Type*} [topological_space X]
	[compact_space X] [t2_space X] (s : set X) (hs : is_closed s) (hne : s.nonempty)
	(h₁ : topological_space.metrizable_space ↥s)
	(h₂ : topological_space.metrizable_space ↥s) :
	topological_space.metrizable_space X :=
sorry

theorem exercise_43_2 {X : Type*} [metric_space X]
	{Y : Type*} [metric_space Y] {s : set X} [complete_space Y] {f : X → Y}
	(hf : uniform_continuous f) (hs : is_complete s) :
	unique (metric.extend f) :=
sorry