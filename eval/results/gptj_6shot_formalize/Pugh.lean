

theorem exercise_2_109 {M : Type*} [metric_space M]
  (h : metric_space.is_ultrametric M) :
  metric_space.is_tot_disc M :=
sorry

theorem exercise_2_126 {E : ℕ → Set} [uncountable E] :
  uncountable.subset E.to_set.to_uncountable.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set.to_set:=
sorry

theorem exercise_2_12a {p₀ p₁ : ℕ} [finite_sequence p₀] [finite_sequence p₁]
  (f : ℕ → ℕ) (hf : injective f)
  (h : limit p₀ = limit p₁) :
  limit (f p₀) = limit (f p₁) :=
sorry

theorem exercise_2_12b {p₀ p₁ : ℕ}
  [finite_sequence p₀] [finite_sequence p₁]
  (f : ℕ → ℕ) (hf : surjection f)
  (h : limit p₀ = limit p₁) :
  limit p₀ = limit p₁ :=
sorry

theorem exercise_2_137 {M : Type*} [metric_space M] 
  [closed_perfect_set M] (p : perfect_set M) :
  p.condensation_point :=
sorry

theorem exercise_2_26 {M : Type*} [metric_space M]
  (h : open M) (hU : none_of_points h.complement.limit) :
  open M :=
sorry

theorem exercise_2_29 {M : Type*} [metric_space M] :
  bijection (open_sets M) (closed_sets M) :=
sorry

theorem exercise_2_32a {n : ℕ} : clopen (subset n) :=
sorry

theorem exercise_2_41 {m : ℕ} [norm_space m] (hB : ball m ≤ B) :
  compact B :=
sorry

theorem exercise_2_46  {A B : Type*} [metric_space A] [metric_space B] [metric_space M]
  (hA : compact A) (hB : disjoint B) (hM : nonempty M)
  (h : A.nonempty) (h : B.nonempty) (h : M.nonempty)
  (h : metric_space M) :
  (∃ a_0 : A, ∃ b_0 : B, ∀ a : A, ∀ b : B, h a b ≤ h a_0 b_0) :=
sorry

theorem exercise_2_57 {S : Type*} [connected S] :
  interior S ≠ connected S :=
sorry

theorem exercise_2_79 {M : Type*} [topological_space M]
  (hM : nonempty_compact_locally_path_connected) (hM : connected)
  (hM : path_connected) : path_connected :=
sorry

theorem exercise_2_85 {M : Type*} [compact M] [open M]
  (h : ∃ (U : open M), ∀ p : M, p ∈ U) :
  ∃ (U : open M) (hU : ∀ p : M, p ∈ U), ∀ p : M, p ∈ U :=
sorry

theorem exercise_2_92 {X : Type*} [compact_space X]
  (h : nonempty_intersection (covering_compact_sets.decreasing_intersection X)) :
  nonempty_intersection (covering_compact_sets.decreasing_intersection X) :=
sorry

theorem exercise_3_1  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : constant f :=
sorry

theorem exercise_3_11a {a b : ℝ} [continuous f] [continuous g]
  (h : a < b) (h2 : f'' (x - h) = f'' (x)) (h3 : g'' (y - h) = g'' (y))
  (h4 : f'' (x + h) = f'' (x)) (h5 : g'' (y + h) = g'' (y)) :
  lim_{h \rightarrow 0} (f (x - h) - 2f (x) + f (x + h)) = f'' (x) :=
sorry

theorem exercise_3_18 {L : ℝ} [closed_set L] :
  ∃ f : ℝ → [0, 1] :
    f (x : ℝ) = 0 ↔ x ∈ L :=
sorry

theorem exercise_3_4 {n : ℕ} :
  ∀ n, n > 0 → sqrt n - sqrt n = 0 :=
sorry

theorem exercise_3_63a {p : ℕ} :
  ∑ k (log k) p = ∞ → p > 1 :=
sorry

theorem exercise_3_63b {p : ℕ} :
  ∑ k : ℕ, k < p → (log k) ^ p < 1 :=
sorry

theorem exercise_4_15a {f : ℝ → ℝ} [continuous f]
  (h : uniform_continuous f) :
  (∃ μ : ℝ → ℝ, μ.is_continuous) →
  (∃ μ : ℝ → ℝ, μ.is_mod_of_uniform_continuous f) :=
sorry

theorem exercise_4_19 {M A : Type*} [metric_space M] [metric_space A]
  (hM : compact_space M) (hA : dense_in_M A) (h : δ_dense_in_M (hA.subset A))
  (h : δ_dense_in_M (hA.subset A)) : δ_dense_in_M (hA.subset A) :=
sorry

theorem exercise_5_2 {V W : Type*} [normed_space V] [normed_space W]
  (hV : normed_space V) (hW : normed_space W)
  (h : normed_space.norm_of_linear_operator hV hW) :
  normed_space.norm_of_linear_operator hV.norm_of_linear_operator hW :=
sorry