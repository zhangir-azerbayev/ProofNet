

is_open_of_forall_open_subset {α : Type u} [topological_space α]
	[t0_space α] {s : set α} (h : ∀ (x : α), x ∈ s → (∃ (t : set α) (H : t ⊆ s), is_open t ∧ x ∈ t)) :
	is_open s :=
sorry

is_open_of_countable_disjoint_subtype {α : Type u}
	{s : set (set α)} [t : set α] (hs : s.countable)
	(hd : ∀ (t : set α), t ⊆ s → is_open t) :
	is_open s :=
sorry

alexandroff.nhds_infty_eq_bot {X : Type*} [topological_space X] :
	nhds alexandroff.infty = ⊥ :=
sorry

topological_space.is_topological_infi_of_cover {α : Type u}
	{ι : Sort w} {t : ι → topological_space α} {f : α → set α}
	(h : ∀ (i : ι), topological_space.is_topological_basis (f i)) :
	topological_space.is_topological_basis (⋂ (i : ι), f i) :=
sorry

not_nonempty_of_is_topological_space {α : Type u}
	[topological_space α] [h : nonempty α] {f : set α} :
	¬is_topological_space f :=
sorry

generate_from_eq_of_unique_topology {α : Type u}
	{t : set (set α)} (h : t.is_open) :
	∃! (s : set (set α)), s = {t : set α | t.is_open s} :=
sorry

generate_from_eq_of_unique_topology {α : Type u}
	{t : set (set α)} (h : t = topological_space.generate_from (set.range (λ (s : set α), s)))
	(h_eq : ∀ (s : set α), s ∈ t → is_open s) :
	topological_space.generate_from t = t :=
sorry

nhds_basis_infi {α : Type u} {ι : Sort w} [topological_space α]
	{p : ι → Prop} {s : ι → set α} (h : (nhds 0).has_basis p s) :
	(nhds 0).has_basis p (λ (i : ι), ⋂ (hi : p i), s i) :=
sorry

le_generate_from_of_subbasis {α : Type u} {s : set (set α)}
	(hs : s.finite) :
	topological_space.generate_from s ≤ topological_space.generate_from s :=
sorry

R_or_C.not_disjoint {K : Type*} [topological_space K] {l : list R} :
	¬disjoint l (R_or_C.obj K) :=
sorry

rat.basis_of_basis_of_dense {α : Type*} [linear_ordered_field α]
	[archimedean α] (p : α → Prop) (h_gen : ∀ (a : α), p a → p (order.pred a))
	(h_dense : ∀ (a : α), p a → p (order.pred a)) (a : α) :
	p a :=
sorry

rat.nhds_basis_strict {α : Type*} [linear_ordered_field α]
	[topological_space α] [order_topology α] [no_min_order α] [no_max_order α]
	(a : α) :
	(nhds a).has_basis (λ (b : α), a < b) (λ (b : α), {b : α | a < b}) :=
sorry

nhds_subtype_coe_subtype {α : Type u} {β : Type v} [topological_space α]
	{p : β → Prop} [Π (x : β), decidable (p x)] (a : subtype p) :
	nhds a = filter.comap coe (nhds ↑a) :=
sorry

is_open_map_prod_mk {α : Type u} {β : Type v} [topological_space α]
	[topological_space β] {f : α → β} :
	is_open_map f ↔ ∀ (s : set α), is_open s → is_open (f '' s) :=
sorry

real.mk_basis_Ioo_Ioo_Ioo_Ioo_Ioo_Ioo_Ioo_Ioo_Ioo_Ioo_I
	{ι : Type u} [hι : nonempty ι] {a b c d : ℝ} (h : a < b) (h' : c < d)
	(hab : a ≠ b) (hcd : c ≠ d) :
	(real.mk_basis ↥(set.Ioo a b) (set.Ioo c d)).has_basis (λ (x : ℝ × ℝ), x.fst < a.fst ∧ x.snd < c.fst) (λ (x : ℝ × ℝ), x.fst.fst) ∧ set.Ioo a.fst b.fst ∩ set.Ioo c.fst d.fst = set.Ioo (a.snd_1) (b.snd_1) :=
sorry

is_open.sub_mem {α : Type u} [topological_space α] [add_group α]
	[topological_add_group α] {U A : set α} (hU : is_open U) (hA : A ∈ nhds 0) :
	U - A ∈ nhds 0 :=
sorry

continuous_map.extend_unique {α β : Type*} [topological_space α]
	[topological_space β] [t2_space β] {A : set α} {f : C(α, β)}
	{Y : Type*} [topological_space Y] [t2_space Y] (h : continuous f)
	(g : C(α, β)) (hg : continuous g) :
	g = h.extend f :=
sorry

continuous.le_closed_property {α : Type u} {β : Type v}
	[topological_space α] [linear_order α] [order_closed_topology α]
	[topological_space β] [preorder β] [order_closed_topology β] {f g : α → β}
	(hf : continuous f) (hg : continuous g) :
	continuous (λ (x : α), f x ≤ g x) :=
sorry

continuous.min {α : Type u} {β : Type v} [topological_space α]
	[linear_order α] [order_closed_topology α] {f g : α → β} [topological_space β]
	(hf : continuous f) (hg : continuous g) (h : α → β) (h0 : 0 < h 0)
	(hle : ∀ (x : α), f x ≤ h x) (hle' : ∀ (x : α), g x ≤ h x) :
	continuous (λ (x : α), linear_order.min (f x) (g x)) :=
sorry

pi_nat.tendsto_cylinder_iff {ι π : ι → Type*} [fintype ι]
	[Π (i : ι), topological_space (π i)] {l : filter ι} {x₁ x₂ : Π (i : ι), π i}
	{f : Π (i : ι), π i} :
	filter.tendsto f l (nhds (x₁ x₂)) ↔ ∀ (i : ι), filter.tendsto (λ (a : Π (i : ι), π i), f i a) l (nhds (x₁ i)) :=
sorry

real.mul_self_topology_of_nhds_zero {x : ℝ} (hx : nhds x = ⊤) :
	(nhds x).mul_self < ⊤ :=
sorry

tendsto_pow_of_tendsto_nhds_within {𝕜 : Type*}
	[linear_ordered_field 𝕜] [topological_space 𝕜] [order_topology 𝕜] {n : ℕ}
	{f : fin n → 𝕜} {a : 𝕜}
	(hf : ∀ (i : fin n), filter.tendsto f (nhds_within i (set.Ici i)) (nhds a)) :
	filter.tendsto (λ (x : 𝕜), x ^ n) (nhds_within a (set.Ici a)) :=
sorry

tendsto_pow_neg_nhds_within_Ici_of_neg_aux {𝕜 : Type u}
	[linear_ordered_field 𝕜] [topological_space 𝕜] [order_topology 𝕜]
	[topological_ring 𝕜] {a : 𝕜} {n : ℕ} {f : fin n → 𝕜} {s : set 𝕜}
	(h : ∀ (i : fin n), -f i ∈ nhds_within a s) :
	filter.tendsto (λ (x : 𝕜), x ^ -n) filter.at_top (nhds_within (-a) s) :=
sorry

continuous_map.tendsto_of_tendsto_uniformly_of_tendsto_uniformly
	{α : Type*} [topological_space α] {β : Type*} [metric_space β] {ι : Type*}
	{F : ι → C(α, β)} {f : C(α, β)} {x : filter α}
	(h : tendsto_uniformly F f x) (hx : filter.tendsto x F filter.at_top) :
	filter.tendsto (λ (n : ι), ⇑(F n)) x (nhds (⇑f x)) :=
sorry

continuous_map.quotient_of_exists_quotient_map {α β : Type*}
	[topological_space α] [topological_space β] (p : α → Prop) (f : C(β, α))
	(h : ∃ (a : α), p (⇑f a)) :
	quotient_map p :=
sorry

continuous_map.quotient_mk_of_is_quotient_mk {α : Type*}
	[topological_space α] {A : set α} (r : α → α → Prop) [t2_space α]
	[is_refl α r] (h : ∀ (a : α), r a a) :
	⟦A⟧ = A :=
sorry

is_open_map.of_open_map {α β : Type*} [topological_space α]
	[topological_space β] {p : α → Prop} (h : is_open_map p) (hp : is_open_map p) :
	is_open_map (λ (a : α), q) :=
sorry

quotient_map.connected_preimage_of_preimage_connected_space {α : Type u}
	{β : Type v} [topological_space α] [topological_space β] {p : α → Prop}
	(h : ∀ (a : α), p a → is_connected (set.range p))
	(h' : ∀ (a : β), is_connected (set.range p)) :
	connected_space α :=
sorry

is_connected.Union_of_nonempty_inter_connected {α : Type u}
	[conditionally_complete_linear_order α] [topological_space α]
	[order_topology α] {s : set α} (H : s.nonempty)
	(K : ∀ (n : ℕ), is_connected (s ∩ set.Ico n (order.succ n))) :
	is_connected (⋃ (n : ℕ), s) :=
sorry

is_connected.union_nonempty_of_nonempty_connected_subspace {α : Type u}
	[topological_space α] {s : set α} (h : is_connected s)
	(h' : ∀ (t : set α), is_connected t → (s ∩ t).nonempty → (s ∩ t).nonempty) :
	is_connected (s ∪ ⋃ (t : set α) (H : t ∈ s), t) :=
sorry

finite_compl_topology_of_infinite {α : Type u} [infinite α] :
	topological_space.finite_compl ⊤ :=
sorry

is_connected.intermediate_value {α : Type u} [pseudo_emetric_space α]
	{s : set α} (hs : is_connected s) (h's : sᶜ ∩ s) :
	is_connected s :=
sorry

is_connected.from_prod_mk_of_is_connected_aux {α : Type u} {β : Type v}
	[topological_space α] [topological_space β] {s : set α} {t : set β}
	(hs : is_connected s) (ht : is_connected t) (hst : s ×ˢ t ⊆ s)
	(h : is_connected (s ×ˢ t)) :
	is_connected (s ×ˢ t) :=
sorry

circle_deg1_lift.continuous_map_apply (f : circle_deg1_lift)
	(x : ℝ) :
	⇑f x = ⇑f (-x) :=
sorry

continuous_map.coe_mk {α : Type*} [topological_space α]
	(f : C(α, α)) (h₁ : f 1 = 1) (h₂ : ∀ (x : α), ⇑f x = x)
	(h₃ : ∀ (x y : α), ⇑f x = ⇑f y ↔ x = y) :
	⇑{to_continuous_map := f, continuous_to_fun := h₁, continuous_inv_fun := h₂, continuous_inv_fun := h₃} = f :=
sorry

loc_path_connected.assert {X : Type*} [topological_space X]
	[loc_path_connected_space X] {s : set X} (hs : is_connected s) :
	loc_path_connected s :=
sorry

is_normal_subgroup_of_is_open_of_mem_nhds_one {G : Type*} [group G]
	[topological_space G] [has_continuous_mul G] {e : G}
	(he : e ∈ nhds 1) :
	is_normal_subgroup (is_open_of_mem_nhds 1) :=
sorry

is_connected_Inter_of_open_of_lt_of_nonempty {α : Type u}
	[topological_space α] [compact_space α] [nonempty α] {s : set α}
	(hs : ∀ (a : α), a ∈ s → is_connected a) :
	is_connected (⋂ (a : α) (H : a ∈ s), s) :=
sorry

is_compact_of_surjective_compact_of_is_compact_of_is_compact_of_is_compact_of_surjective
	{α : Type u} {β : Type v} [topological_space α] [topological_space β]
	{p : α → Prop} {f : β → α} (hf : is_closed_map f) (hf' : p ∘ f)
	(hf : is_compact_of_surjective f) (h : ∀ (y : β), p (f y)) :
	is_compact_space α :=
sorry

is_connected.exists_is_greatest_of_succ_le {α : Type u}
	[conditionally_complete_linear_order α] [topological_space α]
	[order_topology α] [densely_ordered α] {s : set α} (h : is_connected s) :
	∃ (x : α), is_greatest s x :=
sorry

first_order.language.Theory.model_countable_compact_convergence
	{L : first_order.language} {X : Type w} [L.Structure X]
	[topological_space X] [topological_space.second_countable_topology X] :
	(∀ (f : L.functions (fin (X ⊕ fin 1)), X ⊕ {f // f.finite}) → (∃ (T : L.Theory), T ∈ f ∧ ↑T = set.univ) ∧ L.countable_compact_convergence.to_Lhom T) :=
sorry

emetric.nonempty_inter_of_directed_on {α : Type u}
	[pseudo_emetric_space α] [nonempty α] {s : set α} :
	(∀ (C : ennreal), (∀ (x : α), x ∈ s → C ≤ x) → (∃ (t : set α) (H : t ∈ s), (∀ (z : α), z ∈ t → z ∈ s) ∧ ∀ (z : α), z ∈ t → (∃ (y : α) (H : y ∈ s), y ∈ t)) :=
sorry

isometric.of_compact_to_equiv {α : Type u} [pseudo_emetric_space α]
	(h : ∀ (x y : α), has_dist.dist (⇑h x) (⇑h y) = has_dist.dist x y) :
	isometric.of_compact (isometric.of_equiv h) :=
sorry

rat.not_locally_compact_of_not_locally_compact_ℚ :
	¬locally_compact_space ℚ :=
sorry

exists_nhds_locally_compact_subset {α : Type u} [topological_space α]
	[locally_compact_space α] (x : α) :
	∃ (s : set α) (H : s ∈ nhds x), is_compact s ∧ is_open s ∧ x ∈ s :=
sorry

continuous_map.not_locally_compact_mem_uniformity (α : Type*)
	[topological_space α] [compact_space α] :
	continuous_map.compact_mem_uniformity α :=
sorry

dense_pi_system_of_countable {α : Type*} [topological_space α]
	[nonempty α] {s : set (set α)} (hs : (set.univ.pi s).nonempty)
	(hd : ∀ (t : set α), t ∈ s → t.nonempty → is_open t) :
	dense s :=
sorry

dense.countable_disjoint_open {α : Type u} [topological_space α]
	{s : set α} (hs : dense s) :
	∃ (t : set α), s ⊆ t ∧ t.countable ∧ disjoint t :=
sorry

is_regular_of_disjoint_of_singleton_of_regular {X : Type*}
	[topological_space X] [t0_space X] :
	is_regular {x : X | disjoint {x} {x} :=
sorry

normal_pairwise_disjoint {α : Type*} [topological_space α]
	[normal_space α] :
	normal_pairwise (λ (s t : set α), s ∩ t) :=
sorry

is_regular_of_order_topology {α : Type u} [topological_space α]
	[partial_order α] [order_topology α] :
	is_regular (set.range coe) :=
sorry

normal_subtype_normal {α : Type*} [topological_space α]
	[normal_space α] {p : α → Prop} :
	normal_subtype p → (normal_subtype p).normal :=
sorry

is_Hausdorff.nonempty {α : Type u} [topological_space α]
	{s : set α} (hs : is_Hausdorff s) :
	s.nonempty :=
sorry

is_regular_of_nonempty_pi {α : Type*} [topological_space α]
	{π : α → Type*} {s : Π (a : α), set (π a)} (h : is_regular (s π)) :
	is_regular (s π) :=
sorry

nonempty_pi_lift_of_eval_nonempty {ι : Type*} (α : ι → Type*)
	[Π (i : ι), nonempty (α i)] :
	(nonempty (Π (i : ι), α i)).lift :=
sorry

loc_compact_is_regular_of_locally_compact_space {α : Type u}
	[topological_space α] [locally_compact_space α] :
	is_regular (locally_compact_space α) :=
sorry

loc_compact_t2_of_locally_compact_space {X : Type*}
	[topological_space X] [t2_space X] {f : X → X} :
	locally_compact_space X → (∃ (c : X) (H : c ∈ nhds f), ∀ (x : X), x ∈ f c → (∃ (y : X) (H : y ∈ set.Icc (f c) (f x)), y ∈ set.Icc (f c) (f x)) :=
sorry

continuous_map.compact_open_of_disjoint_closed_compact_of_continuous_of_le
	{X : Type*} [topological_space X] [topological_space.separable_space X]
	{A B : set X} (hA : is_compact A) (hB : is_closed B) (h : A ⊆ B)
	(hA₁ : ∀ (x : X), x ∈ A → ∀ (y : X), y ∈ B → f x = 0 → f y = 1)
	(hB₁ : ∀ (x : X), x ∈ B → f x ∈ continuous_map.compact_open A)
	(hB₂ : ∀ (x : X), x ∈ B → f x ∈ continuous_map.compact_open.gen A) :
	continuous_map.compact_open.of_compact_open hA₁ hB₁ ≤ continuous_map.compact_open.of_compact_open hA₂ hB₂ :=
sorry

compact_t2_of_is_compact_union_closed_subspaces {α : Type u}
	[topological_space α] [compact_space α] [t2_space α] {X : set α}
	(hX : is_compact X) (hX' : X ⊆ X) :
	is_metrizable X → is_compact_t2 X :=
sorry

urysohns.CU.exists_continuous_surjective_of_compact_open {X : Type*}
	[topological_space X] [normal_space X] [compact_space X] (β : urysohns.CU X) :
	∃ (x : X), continuous_map.surjective (λ (n : ℕ), urysohns.CU.approx n β x) :=
sorry

Profinite.is_connected_of_stone_cocompact {X : Profinite}
	(hX : category_theory.limits.has_products (forget ↥(X.to_CompHaus.to_Top)) (X.to_CompHaus.to_Top)) :
	category_theory.is_connected (Profinite.of_to_CompHaus.to_Top) :=
sorry

uniform_continuous.extend_of_complete {α : Type*} [uniform_space α]
	{β : Type*} [uniform_space β] {γ : Type*} [uniform_space γ] {δ : Type*}
	[complete_space δ] {f : α → γ} (hf : uniform_continuous f) {g : γ → δ}
	(hg : uniform_continuous g) :
	uniform_continuous (function.extend f g) :=
sorry