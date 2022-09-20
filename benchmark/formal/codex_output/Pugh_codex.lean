theorem exercise_2_12a {α : Type*} [linear_order α] {f : ℕ → α} 
    (hf : injective f) (l : filter α) (a : α) :
    tendsto (λ n, (f n)) l a ↔ tendsto f l a :=
sorry

theorem exercise_2_12b {α : Type*} [linear_order α] {f : ℕ → α} 
    (hf : surjective f) {l : filter α} {a : α} (h : tendsto (λ n, f n) at_top l) :
    tendsto (λ n, f (f.symm n)) at_top l :=
sorry

theorem exercise_2_26 {M : Type*} [topological_space M]
    (U : set M) : is_open U ↔ ∀ (x : M), x ∈ U → ¬ is_limit_point (set.compl U) x :=
sorry

theorem exercise_2_29 (M : Type*) [metric_space M] :
    function.bijective (λ (U : set M), closure U) :=
sorry

theorem exercise_2_32a (A : set ℕ) : is_clopen A :=
sorry

theorem exercise_2_41 (m : ℕ) (norm : ℝ^m → ℝ) :
  compact (set.ball (0 : ℝ^m) 1) :=
sorry

theorem exercise_2_46 {M : Type*} [metric_space M]
    {A B : set M} (hA : compact A) (hB : compact B) (hAB : disjoint A B) 
    (hA0 : A ≠ ∅) (hB0 : B ≠ ∅) :
    ∃ (a0 : M) (b0 : M), a0 ∈ A ∧ b0 ∈ B ∧ ∀ (a : M) (b : M), a ∈ A → b ∈ B → 
    dist a0 b0 ≤ dist a b :=
sorry

theorem exercise_2_48 : 
    embedding ℝ ℝ² :=
sorry

theorem exercise_2_56 : 
  ¬ homeomorphic (sphere (0 : ℝ) 1) (euclidean_plane) :=
sorry

theorem exercise_2_57 {X : Type*} [topological_space X]
    (S : set X) (hS : is_connected S) :
    ¬ is_connected (interior S) :=
sorry

theorem exercise_2_79 
    {M : Type*} [topological_space M] [compact_space M] 
    [locally_path_connected_space M] (hM : nonempty M) 
    (hM : connected_space M) : path_connected_space M :=
sorry

theorem exercise_2_85 
    (M : Type*) [topological_space M] [compact_space M] 
    (U : set (set M)) (hU : ∀ (p : M), ∃ (U₁ U₂ : set M), p ∈ U₁ ∧ p ∈ U₂ ∧ U₁ ∈ U ∧ U₂ ∈ U) :
    ∃ (V : set (set M)), finite V ∧ ∀ (p : M), ∃ (U₁ U₂ : :=
sorry

theorem exercise_2_92 {α : Type*} 
    [topological_space α] {s : set (set α)} (hs : ∀ t ∈ s, is_compact t) 
    (hc : ∀ t ∈ s, ∃ u ∈ s, t ⊆ u) (hne : ∀ t ∈ s, t.nonempty) :
    (⋂₀ s).nonempty :=
sorry

theorem exercise_2_109 {M : Type*} [metric_space M] 
    (d : M → M → ℝ) (h : ultrametric d) : 
    totally_disconnected M :=
sorry

theorem exercise_2_126 {E : set ℝ} 
    (hE : ¬ countable E) : ∃ (p : ℝ), condensation_point E p :=
sorry

theorem exercise_2_137 {M : Type*} [metric_space M] 
    [separable_space M] [complete_space M] {P : set M} (hP : is_closed P) 
    (hP' : is_perfect P) :
    ∀ (x : M), x ∈ P → is_condensation_point P x :=
sorry

theorem exercise_2_138 
    {M : Type*} [topological_space M] [metric_space M] [separated M] 
    [complete_space M] (hM : is_cantor_space M) (p q : M) (hpq : p ≠ q) 
    (hMpq : p ∉ M ∧ q ∉ M) (epsilon : ℝ) (hepsilon : epsilon > 0) :
    ∃ (A : set M), is :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ} (hf : ∀ x y, abs (f x - f y) ≤ abs (x - y) ^ 2) :
  function.is_constant f :=
sorry

theorem exercise_3_4 (n : ℕ) :
  tendsto (λ n, (sqrt (n + 1) - sqrt n)) at_top (𝓝 0) :=
sorry

theorem exercise_3_11a {f : ℝ → ℝ} {a b : ℝ} (hf : ∀ x ∈ (a, b), deriv_two f x) :
  ∀ x ∈ (a, b), ∃ (l : ℝ), tendsto (λ h, (f (x - h) - 2 * f x + f (x + h)) / h ^ 2) (𝓝 0) (𝓝 l) :=
sorry

theorem exercise_3_17c-i (x : ℝ) : smooth_at ℝ ℝ (λ x, exp 2 * exp (1 - x) * exp (x + 1)) x :=
sorry

theorem exercise_3_17c-ii (x : ℝ) : 
    x ≤ -1 ∨ x ≥ 1 → bump_function x = 0 :=
sorry

theorem exercise_3_18 {L : set ℝ} (hL : is_closed L) :
    ∃ (f : ℝ → ℝ), is_smooth f ∧ ∀ (x : ℝ), f x = 0 ↔ x ∈ L :=
sorry

theorem exercise_3_43a {f : ℝ → ℝ} 
    (hf : riemann_integrable f) :
    riemann_integrable (f ∘ λ x, x * sin (1 / x)) :=
sorry

theorem exercise_3_53 {α : Type*} [linear_ordered_field α] 
    {f g : ℝ → α} (hf : integrable f) (hg : integrable g) :
    integrable (λ x, max (f x) (g x)) ∧ integrable (λ x, min (f x) (g x)) :=
sorry

theorem exercise_3_59 {α : Type*} [linear_ordered_semiring α]
    {a : ℕ → α} (ha : ∀ n, 0 ≤ a n) (h : series a) :
    series (λ n, sqrt (a n) / n) :=
sorry

theorem exercise_3_63 (p : ℕ) (h : p > 1) :
  series.converges (λ k, (1 : ℝ) / (k * (log k) ^ p)) :=
sorry

theorem exercise_4_15a {α : Type*} 
    [linear_order α] [topological_space α] [uniform_space α] 
    {f : α → ℝ} (hf : continuous f) :
    uniform_continuous f ↔ ∃ (μ : (0, ∞) → (0, ∞)), 
    (∀ (s : (0, ∞)), tendsto μ s at_top (𝓝 0)) ∧ 
    (∀ (s t : α), s ≤ t → abs (f t - f s) ≤ μ ( :=
sorry

theorem exercise_4_15b {α : Type*} 
    [topological_space α] {β : Type*} [topological_space β] 
    {f : α → β} {s : set (α → β)} (h : ∀ x ∈ s, continuous_on f (set.univ)) :
    equicontinuous_on s f ↔ uniform_continuous_on s f :=
sorry

theorem exercise_4_19 {M : Type*} [metric_space M] 
    [compact_space M] (A : set M) (hA : dense A) (δ : ℝ) (hδ : δ > 0) :
    ∃ (a : finset M), ∀ (x : M), ∃ (i : finset.fin a), dist x (a.val i) < δ :=
sorry

theorem exercise_4_36a {f : ℝ → ℝ} (hf : ∀ x, abs (f x) ≤ M) :
    ∀ (x₀ : ℝ) (t₀ : ℝ), ∃ (t₁ : ℝ), t₁ > t₀ ∧ ∀ (t : ℝ), t₁ ≥ t → abs (x₀ + ∫ t₀ t f) ≤ M :=
sorry

theorem exercise_4_42 (C : set ℝ) (hC : is_cantor_set C) :
    ∀ (n : ℕ), ∃ (Cn : set ℝ), is_cantor_set Cn ∧ Cn ⊆ C ∧ Cn ⊆ Ioo (-(1/n)) (1/n) :=
sorry

theorem exercise_5_2 {V : Type*} [normed_group V] 
    [normed_space ℂ V] {W : Type*} [normed_group W] [normed_space ℂ W] :
    normed_space ℂ (continuous_linear_map V W) :=
sorry

theorem exercise_5_20 {n m : ℕ} {U : set ℝ^n} 
    (hU : is_connected U) (hUo : is_open U) (f : U → ℝ^m) 
    (hf : ∀ p ∈ U, differentiable_at ℝ f p) (hDf : ∀ p ∈ U, (Df p) = 0) :
    ∃ (c : ℝ^m), f = function.const U c :=
sorry

theorem exercise_5_22 {Y : Type*} 
    [metric_space Y] {f : ℝ × Y → ℝ} (hf : continuous f) :
    continuous (λ y : Y, integral (f ∘ (λ x : ℝ, (x, y))) a b) :=
sorry

theorem exercise_5_43a {R : Type*} [normed_ring R] 
    {n m : ℕ} {T : matrix (fin n) (fin m) R} (hT : T.rank < n) :
    ∃ (δ : R), δ > 0 ∧ ∀ (S : matrix (fin n) (fin m) R), 
    ∥S - T∥ < δ → S.rank ≥ T.rank :=
sorry

theorem exercise_6_38 {α : Type*} [measurable_space α] 
    [measure_space α] {f g : α → ℝ} (hf : integrable f) (hg : integrable g) :
    integrable (λ x, max (f x) (g x)) ∧ integrable (λ x, min (f x) (g x)) :=
sorry

theorem exercise_6_39 
    {α : Type*} [measurable_space α] {μ : measure α} {f g : α → ℝ} 
    (hf : measurable f) (hg : measurable g) (hf2 : integrable μ (λ x, f x ^ 2)) 
    (hg2 : integrable μ (λ x, g x ^ 2)) :
    measurable (λ x, f x * g x) ∧ integrable μ (λ x, f x * g x) ∧ 
    (∫ x, f :=
sorry

theorem exercise_6_43 {y : ℝ} : differentiable ℝ (λ x, integral (λ x, exp (-x) * sin (x + y)) x) :=
sorry

theorem exercise_6_49a {α : Type*} 
    [measurable_space α] {β : Type*} [measurable_space β] 
    {f : α → β} :
    is_measurable f ↔ ∀ (s : set β), is_measurable s → is_measurable (f ⁻¹' s) :=
sorry