

theorem exercise_2_109 {M : Type*} [metric_space M]
  (d : M → M → ℝ) (h : ultrametric d) :
  totally_disconnected_space M :=
sorry

theorem exercise_2_126 {E : set ℝ} (hE : ¬ countable E) :
  ∃ (p : ℝ), condensation_point E p :=
sorry

theorem exercise_2_12a {α : Type*} [linear_order α]
  {f : α → ℕ} (hf : function.injective f) {a : α → ℝ} (ha : tendsto a at_top (𝓝 0)) :
  tendsto (λ n, a (f n)) at_top (𝓝 0) :=
sorry

theorem exercise_2_12b {α : Type*} [linear_order α] {f : ℕ → α} {l : filter α}
  (hf : surjective f) :
  tendsto (λ n, f n) at_top l ↔ tendsto f at_top l :=
sorry

theorem exercise_2_137 {M : Type*} [metric_space M]
  [separable_space M] [complete_space M] {P : set M} (hP : closed P)
  (hP_perf : perfect_space P) :
  ∀ (x : M), x ∈ P → x ∈ condensation_points P :=
sorry

theorem exercise_2_26 {M : Type*} [topological_space M]
  (U : set M) : is_open U ↔ ∀ (x : M), x ∈ U → ¬ x ∈ closure (set.compl U) :=
sorry

theorem exercise_2_29 {M : Type*} [metric_space M] :
  function.bijective (λ (U : set M), Uᶜ) :=
sorry

theorem exercise_2_32a (s : set ℕ) : is_clopen s :=
sorry

theorem exercise_2_41 {m : ℕ} (n : ℝ → ℝ) [normed_space ℝ n] :
  compact_space (set.range n) :=
sorry

theorem exercise_2_46 {M : Type*} [metric_space M]
  {A B : set M} (hA : compact A) (hB : compact B) (hAB : disjoint A B)
  (hA0 : A ≠ ∅) (hB0 : B ≠ ∅) :
  ∃ (a0 : M) (b0 : M), a0 ∈ A ∧ b0 ∈ B ∧ ∀ (a : M) (b : M), a ∈ A → b ∈ B → dist a0 b0 ≤ dist a b :=
sorry

theorem exercise_2_57 {X : Type*} [topological_space X]
  (S : set X) (hS : is_connected S) (hS_int : is_connected (interior S)) :
  false :=
sorry

theorem exercise_2_79  {M : Type*} [topological_space M] [compact_space M] [locally_path_connected M]
  (hM : nonempty M) (hM' : connected_space M) : path_connected_space M :=
sorry

theorem exercise_2_85 {M : Type*} 
  [topological_space M] [compact_space M] {U : Type*} [fintype U] 
  (hU : ∀ (u : U), is_open (u : set M)) (hU' : ∀ (p : M), ∃ (u v : U), p ∈ u ∧ p ∈ v) :
  ∃ (U' : finset U), ∀ (p : M), ∃ (u v : U), p ∈ u ∧ p ∈ v ∧ u ∈ U' ∧ v ∈ U' ∧ 
  (∀ (u' : U), u' ∈ U' → u' ∈ U) ∧ (∀ (u' : U), u' ∈ U' → is_open (u' : set M)) :=
sorry

theorem exercise_2_92 {X : Type*} [topological_space X]
  [compact_space X] {I : Type*} [fintype I] {s : I → set X}
  (h : ∀ i j : I, i ≤ j → s i ⊆ s j) (hc : ∀ i : I, is_compact (s i))
  (hnc : ∀ i : I, s i ≠ ∅) :
  ∃ x : X, ∀ i : I, x ∈ s i :=
sorry

theorem exercise_3_1 {f : ℝ → ℝ}
  (hf : uniform_continuous f) (hf' : ∀ x t, abs (f t - f x) ≤ abs (t - x) ^ 2) :
  ∃ c : ℝ, f = function.const ℝ c :=
sorry

theorem exercise_3_11a {f : ℝ → ℝ} {x : ℝ}
  (hf : differentiable_at ℝ f x) (hf' : differentiable_at ℝ f' x)
  (hf'' : differentiable_at ℝ f'' x) :
  tendsto (λ h, (f (x - h) - 2 * f x + f (x + h)) / h^2) (𝓝 0) (𝓝 f'' x) :=
sorry

theorem exercise_3_18 {L : set ℝ} (hL : is_closed L) :
  ∃ (f : ℝ → ℝ), is_smooth f ∧ ∀ (x : ℝ), f x = 0 ↔ x ∈ L :=
sorry

theorem exercise_3_4 
  (n : ℕ) : tendsto (λ n, (sqrt (n + 1) - sqrt n)) at_top (𝓝 0) :=
sorry

theorem exercise_3_63a (p : ℝ) (hp : p > 1) :
  ∃ (N : ℕ), ∀ (n : ℕ), n ≥ N → (∑ k in finset.range n, 1 / (k * (log k) ^ p)) < ∞ :=
sorry

theorem exercise_3_63b (p : ℝ) (h : p ≤ 1) :
  ∑ k in range 1, 1 / (k * (log k) ^ p) = ∞ :=
sorry

theorem exercise_4_15a {α : Type*}
  [linear_order α] [topological_space α] [metric_space α]
  {f : α → ℝ} (hf : continuous f) :
  uniform_continuous f ↔ ∃ (μ : ℝ → ℝ),
    (∀ (s : ℝ), 0 < s → 0 < μ s) ∧
    (∀ (s : ℝ), 0 < s → μ s < 1) ∧
    (∀ (s : ℝ), μ s = 0 → s = 0) ∧
    (∀ (s : ℝ), μ s ≠ 0 → ∀ (t : ℝ), 0 < t → μ s ≤ μ (s + t)) ∧
    (∀ (s : ℝ), μ s ≠ 0 → ∀ (t : ℝ), 0 < t → μ s ≤ μ (s * t)) ∧
    (∀ (s : ℝ), μ s ≠ 0 → ∀ (t : ℝ), 0 < t → μ s ≤ μ (s / t)) ∧
    (∀ (s : ℝ), μ s ≠ 0 → ∀ (t : ℝ), 0 < t → μ s ≤ μ (t / s)) ∧
    (∀ (s : ℝ), μ s ≠ 0 → ∀ (t : ℝ), 0 < t → μ s ≤ μ (s - t)) ∧
    (∀ (s : ℝ), μ s ≠ 0 → ∀ (t : ℝ), 0 < t → μ s ≤ μ (:=
sorry

theorem exercise_4_19 {M : Type*} [metric_space M]
  (hM : compact_space M) (A : set M) (hA : dense A) (δ : ℝ) (hδ : 0 < δ) :
  ∃ (a : finset M), a.finite ∧ a.dense δ :=
sorry

theorem exercise_5_2 {V W : Type*} [normed_group V]
  [normed_group W] [normed_space ℝ V] [normed_space ℝ W]
  (L : Type*) [add_comm_group L] [module ℝ L]
  (f : V →ₗ[ℝ] L) (g : L →ₗ[ℝ] W) (hf : continuous f) (hg : continuous g) :
  normed_space ℝ L :=
sorry