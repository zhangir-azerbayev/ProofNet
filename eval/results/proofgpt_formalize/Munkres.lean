

is_open_of_forall_mem_nhds_within {α : Type u} [topological_space α]
	{s : set α} (h : ∀ (a : α), a ∈ s → s ∈ nhds_within a (set.Ioi a)) :
	is_open s :=
sorry

generate_from_of_cover_of_countable_or_eq_top {α : Type u}
	( : set (set α)) (hc : is_countable_cofiltered c)
	(hX : ∀ (s : set α), s ∈ c → s.countable ∨ s = ⊤) :
	topological_space.generate_from c = c :=
sorry

alexandroff.not_nhds_infty_ne_bot {X : Type*} [topological_space X]
	(U : set X) :
	¬nhds alexandroff.infty U = ⊥ ∨ (∀ (x : X), x ∈ U → (∃ (n : ℕ), x = alexandroff.infty ^ n)) ∨ ∀ (x : X), x ∈ U → (∃ (n : ℕ), x = alexandroff.infty ^ n) :=
sorry

topological_space.generate_from_infi_is_top {α : Type u}
	{ι : Sort v} (f : ι → topological_space.generate_from α) :
	(⨅ (i : ι), f i).is_top :=
sorry

topological_space.not_has_continuous_supr_of_encodable {α : Type u}
	{ts : set (topological_space α)} (h : ∀ (t : topological_space α), t ∈ ts → is_topological_space.separable t.carrier t.snd)
	[encodable ts] :
	¬has_continuous_supr ts :=
sorry

topological_space.eq_generate_from_of_eq_generate_from {α : Type u}
	{T : α → topological_space α}
	(h : ∀ (s : set (set α)), T s = topological_space.generate_from s) :
	T = topological_space.generate_from (set.range T) :=
sorry

topological_space.ext {α : Type u} {ι : Sort w}
	{t : ι → topological_space α}
	(h : ∀ (i : ι), topological_space.is_topological_space α) :
	topological_space.ext t :=
sorry

topological_space.is_topological_basis.eq_infi {α : Type u}
	{s : set (set α)} (hs : topological_space.is_topological_basis s) :
	topological_space.generate_from s = ⨅ (t : topological_space α) (H : t ∈ s), topological_space.generate_from t :=
sorry

generate_from_eq_Inter {α : Type u} (s : set (set α)) :
	topological_space.generate_from s = ⋂ (t : topological_space α) (h : s ⊆ t), t :=
sorry

algebraic_topology.dold_kan.not_discrete_topology_K {K : Type*}
	[category_theory.field K] [category_theory.preadditive K]
	(l : list (topological_space.opens K)) :
	¬discrete_topology ↥(l.to_finset) :=
sorry

real.is_basis_nhds_zero_of_has_basis_lt_and_lt {a : ℝ} (h : a < 0) :
	(nhds 0).has_basis (λ (b : ℝ), b < a ∧ a < b) (λ (b : ℝ), {b : ℝ | b < a}) :=
sorry

real.has_basis_nhds_zero_of_has_basis_Ioo_rat {a b : ℝ} (h : a < b) :
	(nhds 0).has_basis (λ (ε : ℝ), 0 < ε) (λ (ε : ℝ), {p : ℝ × ℝ | p.fst < ε.snd}) :=
sorry

subtype.top_inducing {α : Type u} {p : α → Prop} :
	inducing p :=
sorry

is_open_map_prod_of_open_map {α β : Type*} [topological_space α]
	[topological_space β] {f : α → β} :
	is_open_map f → is_open_map (prod.map f f) :=
sorry

real.is_basis_of_pairwise_approx {a b c d : ℝ}
	(h : a < b ∧ c < d) (h' : a ≠ 0 ∧ b ≠ 0 ∧ c ≠ 0 ∧ d ≠ 0) :
	topological_space.is_topological_basis {p : ℝ × ℝ | a < p.fst ∧ c < p.snd} :=
sorry

is_open.sub_closed {α : Type u} [topological_space α] [add_group α]
	[topological_add_group α] {s t : set α} (hs : is_open s) (ht : is_closed t) :
	is_open (s - t) :=
sorry

continuous_map.extend_unique {α : Type*} [topological_space α]
	{A X Y : Type*} [topological_space A] [topological_space X] [topological_space Y]
	[t2_space Y] (f : C(A, Y)) (h : ∀ (x : A), ⇑f x ∈ closure A) (g : C(A, Y))
	(hg : ∀ (x : A), ⇑g x ∈ closure A) :
	g = f.extend h :=
sorry

is_closed_le_of_le {α : Type u} {β : Type v} [topological_space α]
	[preorder α] [t : order_closed_topology α] [topological_space β] {f g : β → α}
	(hf : continuous f) (hg : continuous g) :
	is_closed {b : β | f b ≤ g b} :=
sorry

continuous.min {α : Type u} {β : Type v} [topological_space α]
	[linear_order α] [order_closed_topology α] [topological_space β] {f g : β → α}
	(hf : continuous f) (hg : continuous g) (h : β → α) (hh : continuous h) :
	continuous (λ (x : β), linear_order.min (f x) (g x)) :=
sorry

pi_nat.cylinder_eq_pi_iff_of_tendsto {α : Type*} [comm_pi_class α]
	{x : ℕ → α} {f : filter ℕ} :
	filter.tendsto (λ (n : ℕ), pi_nat.cylinder (x n) (f n)) filter.at_top (nhds (pi_nat.cylinder x f)) ↔ filter.tendsto (λ (n : ℕ), f n.succ) filter.at_top (nhds (pi_nat.cylinder x f)) :=
sorry

measurable_space.metrizable_space_prod {α β : Type*}
	[topological_space α] [measurable_space α] [opens_measurable_space α]
	[topological_space β] [measurable_space β] [borel_space β] :
	measurable_space.metrizable_space (α × β) :=
sorry

path.tendsto_of_fn {X : Type*} [topological_space X] {n : ℕ}
	{f : fin (n + 1) → X} {x₀ x₁ : X} (p : path (f 0) x₀ x₁) :
	filter.tendsto f (nhds x₀) (nhds x₁) :=
sorry

tendsto_uniformly_on_pow {α β : Type*} [uniform_space α] [group α]
	[uniform_group α] {f : ℕ → β} {s : set β} (n : ℕ) :
	tendsto_uniformly_on (λ (x : β), x ^ n) f s :=
sorry

tendsto_uniformly.tendsto_comp {α β ι : Type*} [uniform_space β]
	{F : ι → α → β} {f : α → β} {x : filter α} {p : filter ι} (h : tendsto_uniformly F f p)
	(hf : ∀ (n : ι), continuous (F n)) (hx : filter.tendsto x filter.at_top (nhds x)) :
	filter.tendsto (λ (n : ι), F n (x n)) filter.at_top (nhds (f x)) :=
sorry

continuous_map.quotient_map {α β : Type*} [topological_space α]
	[topological_space β] (p : C(α, β)) (hp : continuous ⇑p) :
	quotient_map p :=
sorry

Top.presheaf.le_rng_of_subset {C : Type u} [category_theory.category C]
	(A X : Top) (r : X ⟶ A) (h : ∀ (a : ↥A), ⇑(Top.presheaf.pushforward C r) a = a) :
	category_theory.presheaf.is_quotient (Top.presheaf.rng A X) :=
sorry

is_open_map.of_restrict {α β : Type*} [topological_space α]
	[topological_space β] {p : α → β} (h : is_open_map p)
	(hp : is_open_map (set.cod_restrict p (set.range p))) :
	is_open_map (set.cod_restrict p (set.range p)) :=
sorry

quotient_map.is_connected {α : Type u} {β : Type v} [topological_space α]
	[topological_space β] {p : α → β} (y : β)
	(h : ∀ (s : set β), is_connected s → is_connected (p ⁻¹' s))
	(h' : is_connected β) :
	is_connected s :=
sorry

is_connected.bUnion_of_chain {α : Type u} {β : Type v}
	[topological_space α] [linear_order β] [succ_order β] [is_succ_archimedean β]
	{s : β → set α} (H : ∀ (n : β), is_connected (s n))
	(K : ∀ (n : β), (s n ∩ s (order.succ n)).nonempty) :
	is_connected (⋃ (n : β), s n) :=
sorry

is_connected.union_Union_of_nonempty {α : Type u} {β : Type v}
	[topological_space α] [nonempty β] {s : set β} {f : β → set α}
	(hs : is_connected (f '' s)) (h : ∀ (b : β), b ∈ s → (f b).nonempty) :
	is_connected (s ∪ ⋃ (b : β) (H : b ∈ s), f b) :=
sorry

connected_of_infinite {α : Type u} [topological_space α] {s : set α}
	(h : s.infinite) :
	is_connected s :=
sorry

subset_interior_of_connected_subspace {α : Type u} [topological_space α]
	{s : set α} (h : is_connected (interior s))
	(h' : ∀ (C : set α), C ⊆ s → (∃ (a : α) (H : a ∈ s), C = set.Ici a)) :
	interior s ∩ (closure s ∩ C) = C :=
sorry

is_connected.prod_sub_prod {α : Type u} {β : Type v} [topological_space α]
	[topological_space β] {s : set α} {t : set β} (hs : is_connected s)
	(ht : is_connected t) (h : (s ×ˢ t).nonempty) :
	is_connected (s ×ˢ t) :=
sorry

continuous_map.exists_eq_of_mem_compact_open {α : Type*}
	[topological_space α] {β : Type*} [topological_space β] [compact_space α]
	(f : C(α, β)) :
	∃ (x : α), ⇑f x = ⇑f (-x) :=
sorry

continuous.exists_fixed_point {α : Type u} [topological_space α]
	{f : α → α} (hf : continuous f) [t2_space α]
	(h : ∀ (x : α), x = 1 → (∃ (y : α), f y = x)) (a : α) :
	∃ (x : α), f x = a :=
sorry

locally_path_connected_of_open {X : Type*} [topological_space X]
	[locally_path_connected_space X] {U : set X} (hU : is_open U)
	(h : ∀ (V : set X), V ∈ nhds_within 0 (set.Ioi 0) → is_open V) :
	is_path_connected U :=
sorry

subgroup.normal_of_component_one_eq_top {G : Type*} [group G]
	[topological_space G] [topological_group G] {C : set G}
	(hC : is_normal_subgroup C) (hCone : C = ⊤) :
	C.normal :=
sorry

is_connected_sInter {α : Type u} [topological_space α] [t2_space α]
	[compact_space α] (s : set (set α)) (hs : s.ord_connected)
	(hne : ∀ (t : set α), t ∈ s → is_closed t) (hfin : ∀ (t : set α), t ∈ s → t.nonempty) :
	is_connected (⋂₀ s) :=
sorry

closed_continuous_map.compact_space_induced {α β : Type*}
	[topological_space α] [uniform_space β] {p : C(α, β)}
	(hp : is_closed {x : α | is_compact {y : β | p y x}) (hsurj : closed_continuous_map p)
	(h : ∀ (y : β), is_compact {x : α | p x y}) :
	compact_space α :=
sorry

connected_space.uncountable_of_one_point {X : Type*} [metric_space X]
	[connected_space X] [nonempty X] :
	¬metric_space.uncountable_of_one_point X :=
sorry

topological_space.compact_space_iff_countable_compact_covering
	(α : Type u) [topological_space α] :
	topological_space.compact_space α ↔ ∀ (s : topological_space.opens α), s.countable → (∃ (t : topological_space.compact_covering α), s ⊆ t.carrier ∧ t.finite) :=
sorry

compact_space_iff_exists_interior_nonempty_of_closed_cover {α : Type u}
	[topological_space α] :
	compact_space α ↔ ∀ (C₁ C₂ : set (set α)), is_closed C₁ → is_closed C₂ → C₁ ⊆ C₂ → (∃ (h₁ : C₁ ⊆ C₂), (∀ (h₂ : C₂ ⊆ C₁), h₂ ∈ C₁ → h₁ ∈ C₂) ∧ C₁.nonempty) :=
sorry

isometry_of_condition_compacts_to_equiv {α : Type u} [metric_space α]
	{f : α → α} (hf : ∀ (x y : α), has_dist.dist (f x) (f y) = has_dist.dist x y)
	(s : topological_space.compacts α) :
	(isometry_of_condition_compacts f hf s).to_equiv = equiv.of_injective f _ :=
sorry

rat.not_locally_compact_space :
	¬locally_compact_space ℚ :=
sorry

exists_locally_compact_closure_subset {α : Type u} [topological_space α]
	[locally_compact_space α] {x : α} {U : set α} (hU : U ∈ nhds x) :
	∃ (V : set α) (H : V ∈ nhds x), closure V ⊆ U ∧ is_compact (closure V) :=
sorry

measure_theory.locally_compact_univ_of_omega_complete_space {α : Type*}
	[topological_space α] [omega_complete_space α]
	[topological_space.separable_space ↥(set.Icc 0 1)] :
	¬measure_theory.locally_compact_space ↥(set.Icc 0 1) :=
sorry

dense.countable_product {α β : Type*} [topological_space α]
	[topological_space β] {s : set (set α)} (hs : dense s)
	(h : ∀ (t : set α), t ∈ s → t.countable) :
	dense s :=
sorry

Top.countable_of_Dense (X : Top) [category_theory.category X]
	[category_theory.limits.has_countable_dense_set X]
	(U : set (topological_space.opens ↥X)) :
	(∀ (V : topological_space.opens ↥X), V ∈ U → disjoint V U) → U.countable :=
sorry

{X : Type*} [topological_space X]
	[regular_space X] {x y : X} :
	disjoint (closure (set.range (λ (p : X × X), p.fst ^ p.snd)) (x, y)) :=
sorry

normal_space.disjoint_closed_of_disjoint {α : Type u}
	[topological_space α] [normal_space α]
	(hd : ∀ (s t : set α), is_closed s → is_closed t → disjoint s t) :
	disjoint (closure s) (closure t) :=
sorry

order_topology.regular_of_top {α : Type*} [preorder α]
	(f : order_topology α) :
	f.is_regular :=
sorry

normal_space_closed_subspace (𝕜 E : Type*) [normed_field 𝕜]
	[add_comm_group E] [module 𝕜 E] [topological_space E] [has_continuous_smul 𝕜 E]
	[t2_space E] :
	normal_space (submodule 𝕜 E) :=
sorry

emetric.nonempty_of_Hausdorff_edist {α : Type u} [emetric_space α]
	(H : ∀ (s : set α), s.nonempty → emetric.Hausdorff_edist s ≠ ⊤) (a : α) :
	(emetric.Hausdorff_edist a).nonempty :=
sorry

is_regular.of_pi {η X : η → Type*} [Π (i : η), nonempty (X i)]
	(h : is_regular (Π (i : η), X i)) :
	is_regular (Π (i : η), X i) :=
sorry

hyperreal.is_normal_of_nonempty {X : ℝ → Type*}
	(hX : (hyperreal.of_real X).is_normal) :
	(hyperreal.of_real X).nonempty :=
sorry

loc_compact_t2_regular (X : Type*) [topological_space X]
	[locally_compact_space X] [t2_space X] :
	∃ (n : ℕ), regular_space.core.nth X n :=
sorry

loc_compact_t2_is_totally_regular (X : Type*) [topological_space X]
	[locally_compact_space X] [t2_space X] :
	is_totally_regular X :=
sorry

compact_exists_zero_one_of_closed {X : Type*} [topological_space X]
	[complete_space X] {A B : set X} (hA : is_closed A) (hB : is_closed B)
	(hAB : disjoint A B) (hX : is_compact A) :
	∃ (f : C(X, ℝ)), set.eq_on ⇑f 0 A ∧ set.eq_on ⇑f 1 B ∧ ∀ (x : X), ⇑f x ∈ set.Icc 0 1 :=
sorry

spectral_theorem_of_compact_t2_of_metrizable_space {X : Type*}
	[topological_space X] [compact_space X] [t2_space X]
	[topological_space.metrizable_space X] (hX : ∃ (n : ℕ), topological_space.is_metrizable_of_compact X n) :
	topological_space.metrizable_space X :=
sorry

StoneCech_compactification.exists_continuous_surjective_closed_map_eq_id
	{X : Type u} (Y : Top) [category_theory.category Y]
	[category_theory.limits.has_products Y] [category_theory.limits.has_limits Y]
	[category_theory.limits.has_limits_of_shape Y Top]
	[category_theory.limits.has_limits_of_shape Yᵒᵖ X] [category_theory.limits.has_limits_of_shape Yᵒᵖ (category_theory.forget X)]
	[category_theory.limits.preserves_limits_of_shape Yᵒᵖ (category_theory.forget X)]
	[category_theory.limits.preserves_limits_of_shape Y Yᵒᵖ (category_theory.forget X)]
	(g : StoneCech_compactification X ⟶ Y) (hg : continuous ⇑g) :
	∃ (f : Y ⟶ Top.of (β(X)ᵒᵖ)), f = category_theory.limits.closed_map.id (Top.of (β(X)ᵒᵖ) :=
sorry

stone_cech_connected_iff_connected {α : Type u} [topological_space α]
	[t2_space α] [regular_space α] :
	connected_space stone_cech ↔ connected_space α :=
sorry

Kuratowski_embedding.extend_unique {α : Type u} {β : Type v}
	[metric_space α] [metric_space β] {Y : Type v} [metric_space Y]
	[complete_space Y] {A : set α} {f : α → β}
	(hf : uniform_continuous f)
	(h : ∀ (x : α), x ∈ A → (∃ (y : β), filter.tendsto f (filter.principal A) (nhds y))) :
	∃ (g : α → β), continuous g ∧ uniform_continuous g :=
sorry