import .common 

open set topological_space filter 
open_locale classical topology filter 
noncomputable theory 





theorem exercise_13_3b {X : Type*} [topological_space X]
	(h : ¬nhds alexandroff.infty = ⊤ ∨ ∀ (x : X), is_empty (set.Ioi x)) :
	¬nhds alexandroff.infty = ⊤ :=
sorry

theorem exercise_13_4a2 {α : Type u} {ι : Sort w}
	[topological_space α] {p : ι → Prop} {s : ι → set (set α)}
	(h : ∀ (i : ι), p i → (s i).nonempty) (a : α) :
	¬(nhds a).has_basis p s :=
sorry

theorem exercise_13_4b2 {α : Type u} {ι : Sort w}
	(f : ι → topological_space α) (h : ∀ (i : ι), topological_space.is_topological_space α) :
	∃! (i : ι), topological_space.is_topological_space α :=
sorry

theorem exercise_13_5b {α : Type u} (s : set (set α))
	(hs : is_subbasis s) :
	topological_space.generate_from s = ⋂ (t : topological_space α) (h : t ∈ {t : set α | t.is_open s}), t :=
sorry

theorem exercise_13_8a {a b : ℝ} (h : a < b) :
	(nhds 0).is_basis (λ (ε : ℝ), 0 < ε) (λ (ε : ℝ), set.Ioo a b) :=
sorry

theorem exercise_16_1 {α : Type*}
	[topological_space α] {s : set α} (hs : s ⊆ ↑⊤) (A : set α) :
	sub_topology_of_topological_space s A = topological_space.to_topological_space A :=
sorry

theorem exercise_16_6 :
	topological_space.is_topological_basis (⋃ (p : ℚ × ℚ) (hp : p.fst < p.snd), {p : ℚ × ℚ | p.fst < p.snd ∧ p.snd < p.fst}) :=
sorry

theorem exercise_18_8a {α : Type u} {β : Type v}
	[topological_space α] [preorder α] [t : order_closed_topology α]
	[topological_space β] {f g : β → α} (hf : continuous f) (hg : continuous g) :
	is_closed {b : β | f b ≤ g b} :=
sorry

theorem exercise_18_13 {α : Type u₁} {β : Type u₂}
	[topological_space α] [topological_space β] {A : set α} {f : α → β}
	(hf : continuous f) (hA : is_closed A)
	(h : ∀ (x : α), x ∈ A → (∃ (y : α) (H : y ∈ A), f y = f x)) :
	continuous (extend_functor (topological_space.generate_from A) f hf) :=
sorry

theorem exercise_20_2 (x : ℝ) :
	topological_space.metrizable_space ↥((continuous_map.of_real ℝ x).to_continuous_map.to_fun) :=
sorry

theorem exercise_21_6b (f : ℕ → ℝ) (n : ℕ) :
	¬tendsto_uniformly (λ (x : ℝ), f n / x) (f n) 0 :=
sorry

theorem exercise_22_2a {α β : Type*} [topological_space α]
	[topological_space β] (p : C(α, β)) (h : ∃ (f : C(β, α)), ⇑f = id) :
	quotient_map ⇑p :=
sorry

theorem exercise_22_5 {α β : Type*} [topological_space α]
	[topological_space β] {p : α → β} (h : is_open_map p) (s : set α) :
	is_open_map (set.cod_restrict p s h) :=
sorry

theorem exercise_23_3 {α : Type u} {β : Type v}
	[topological_space α] [topological_space β] {s : set α} (hs : is_connected s)
	(hdisjoint : ∀ (t : set α), t ⊆ s → is_connected t → disjoint s t) :
	is_connected (⋃ (t : set α) (H : t ⊆ s), t) :=
sorry

theorem exercise_23_6 {α : Type*}
	[topological_space α] [add_group α] [topological_add_group α] {A X : set α}
	(hA : is_connected A) (hX : ∀ (s : set α), s ∈ A → s ⊆ X → is_connected s)
	(hA' : ∀ (s : set α), s ∈ A → s ⊆ X → s = X) :
	intersecting (A ⊆ X) :=
sorry

theorem exercise_23_11 {α : Type u} {β : Type v} [topological_space α]
	[topological_space β] {p : α → β} (h : quotient_map p)
	(h_1 : ∀ (y : β), is_connected {p ⁻¹' {y}) → is_connected {y})
	(h_2 : ∀ (x : α), p x ∈ set.range p → is_connected {x}) :
	is_connected α :=
sorry

theorem exercise_24_3a {α : Type*} [topological_space α]
	{f : α → α} (hf : continuous f) (h : ∃ (x : α), f x = x) (a : α) :
	∃ᶠ (x : α) in nhds a, f x = x :=
sorry

theorem exercise_25_9 {G : Type*} [group G]
	[topological_space G] [topological_group G] {C : set G}
	(hC : is_normal_subgroup C) (hCone : C = ⊤) :
	C.normal :=
sorry

theorem exercise_26_12 {α β : Type*} [topological_space α]
	[topological_space β] {p : α → β} (hp : is_closed_map p)
	(h : ∀ (y : β), is_compact (p ⁻¹' {y})) :
	compact_space α :=
sorry

theorem exercise_28_4 {α : Type u}
	[topological_space α] [topological_space.first_countable_topology α]
	[t2_space α] :
	t1_space α ↔ compact_space α ∧ ∀ (s : topological_space.opens α), (∃ (f : ℕ → set α), (∀ (i : ℕ), is_open (f i)) ∧ (∀ (i : ℕ), f i ⊆ s) ∧ s.nonempty :=
sorry

theorem exercise_28_6 {α : Type u}
	[metric_space α] {f : α → α} (h : ∀ (x y : α), has_dist.dist (f x) (f y) = has_dist.dist x y)
	(h' : isometry f) :
	isometry f :=
sorry

theorem exercise_29_4 (α : Type u)
	[topological_space α] [measure_theory.measure_space α]
	[topological_space.separation_set α]
	[measure_theory.sigma_finite measure_theory.measure_space.volume] :
	¬measure_theory.locally_compact_space (α × α) :=
sorry

theorem exercise_30_10 {α : Type u}
	[topological_space α] {s : set (set α)} (hs : dense s)
	[h : ∀ (t : set α), t ∈ s → dense t] [h' : s.countable] :
	dense s :=
sorry

theorem exercise_31_1 {X : algebraic_geometry.Scheme}
	(U : topological_space.opens ↥(X.to_LocallyRingedSpace.to_SheafedSpace.to_PresheafedSpace.carrier)) :
	(∀ (x y : ↥(X.to_LocallyRingedSpace.to_SheafedSpace.to_PresheafedSpace.presheaf.obj (opposite.op U))), disjoint (algebraic_geometry.Scheme.Spec.obj (opposite.op (X.to_LocallyRingedSpace.to_SheafedSpace.to_PresheafedSpace.presheaf.obj (opposite.op U)))) (algebraic_geometry.Scheme.Spec.obj (opposite.op (X.to_LocallyRingedSpace.to_SheafedSpace.to_PresheafedSpace.presheaf.obj (opposite.op U))))) :=
sorry

theorem exercise_31_3 {α : Type*} [partial_order α]
	(f : order_topology α) :
	f.regular :=
sorry

theorem exercise_32_2a {ι X : ι → Type*}
	[Π (i : ι), topological_space (X i)]
	(h : ∀ (i : ι), nonempty (X i)) :
	nonempty (Π (i : ι), X i) :=
sorry

theorem exercise_32_2c {α : Type*}
	(h1 : (set.univ.pi (λ (i : α), {s : set α | is_normal_set s})).finite)
	(h2 : ∀ (s : set α), s.nonempty → (s ∈ nhds s)) :
	normal (set.univ.pi s) :=
sorry

theorem exercise_33_7 (X : Type*) [topological_space X]
	[locally_compact_space X] [t2_space X] :
	is_totally_regular X :=
sorry

theorem exercise_34_9 {X : Type*} [topological_space X]
	[compact_space X] [t2_space X] (h1 : topological_space.metrizable_space X)
	(h2 : topological_space.closeds.nonempty) :
	topological_space.metrizable_space X :=
sorry

theorem exercise_43_2 {α : Type u} {β : Type v} [metric_space α]
	[metric_space β] [complete_space β] {f : α → β}
	(hf : uniform_continuous f) (A : set α)
	(H : ∀ (x : α), x ∈ A → (∃ (y : β), f y = x)) :
	metric.extend f A = f :=
sorry