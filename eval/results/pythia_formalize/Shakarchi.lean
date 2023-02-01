

complex.eq_of_re_eq_const {f : ℂ → ℂ} {z : ℂ} (h : z.re = f) :
	f = function.const ℂ z :=
sorry

complex.eq_of_im_eq_const {f : ℂ → ℂ} {s : set ℂ}
	(hs : is_open s) (hf : ∀ (z : ℂ), z ∈ s → z.im = x) :
	f = function.const ℂ s (f z) :=
sorry

complex.eq_of_abs_eq_const {f : ℂ → ℂ} {z : ℂ} (h : complex.abs f = z) :
	f = function.const ℂ z :=
sorry

norm_num.not_summable_on_unit_circle (𝕜 : Type*) [normed_field 𝕜]
	(z : ↥(metric.sphere 0 1)) :
	¬summable_on 𝕜 (λ (x : ↥(metric.sphere 0 1)), ↑z ^ x) (metric.sphere 0 1) :=
sorry

circle_integral.has_sum_nhds_within_units_circle (z : ℂˣ) :
	has_sum (λ (x : ℝ), ↑x * ↑x - z * ↑z) (↑z * ↑z - z * ↑z) :=
sorry

complex.summable_one_div_of_circle_except_one (z : ℂ) :
	(∀ (x : ℂ), x ∈ metric.sphere z |1| → z = 1) → summable (λ (x : ℂ), x * ↑(z.re) / ↑(z.im)) :=
sorry