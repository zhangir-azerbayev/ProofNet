

holor.cprank_max_1 {α : Type} {ds : list ℕ} [monoid α] [add_monoid α]
	{f : holor α ds} :
	holor.cprank_max 1 f → f.cprank_max :=
sorry

holor.cprank_max_1 {α : Type} {ds : list ℕ} [monoid α] [add_monoid α]
	{f : holor α ds} :
	holor.cprank_max 1 f :=
sorry

holor.cprank_max_nil {α : Type} {ds : list ℕ} [monoid α] [add_monoid α]
	{f : holor α ds} :
	holor.cprank_max 1 f → f.nil.cprank_max :=
sorry

power_series.not_summable_const_mul_pow {A : Type*} [comm_ring A]
	[algebra ℚ A] [topological_space A] [topological_ring A] (n : ℕ) :
	¬summable (λ (a : A), ↑n * a) :=
sorry

exp_series_summable_of_mem_circle {𝕂 𝔸 : Type*} [is_R_or_C 𝕂]
	[normed_ring 𝔸] [normed_algebra 𝕂 𝔸] [complete_space 𝔸] (n : ℕ) :
	summable (λ (x : 𝔸), ↑(n / ↑x)) :=
sorry

power_series.tendsto_nhds_one {E : Type*} [normed_group E]
	[normed_space ℝ E] [complete_space E] {f : power_series ℝ E} :
	filter.tendsto ⇑(power_series.nhds f) (nhds 1) :=
sorry