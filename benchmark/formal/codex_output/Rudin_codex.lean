theorem exercise_1_1a {r : ℚ} (hr : r ≠ 0) 
    {x : ℚ} (hx : ¬ is_rational x) : ¬ is_rational (r + x) :=
sorry

theorem exercise_1_1b {r : ℚ} (hr : r ≠ 0) 
    {x : ℚ} (hx : ¬ is_rational x) : ¬ is_rational (r * x) :=
sorry

theorem exercise_1_2 : ¬ ∃ (q : ℚ), q ^ 2 = 12 :=
sorry

theorem exercise_1_4 {α : Type*} 
    [linear_order α] {E : set α} (hE : E ≠ ∅) (hα : ∀ x ∈ E, α ≤ x) 
    (hβ : ∀ x ∈ E, x ≤ β) : α ≤ β :=
sorry

theorem exercise_1_5 {α : Type*} [linear_order α] [decidable_linear_order α]
    (A : set α) (hA : A.nonempty) (hA_bdd_below : bdd_below A) :
    inf A = -sup (-A) :=
sorry

theorem exercise_1_8 : ¬ ∃ (r : ℂ → ℂ → Prop), is_linear_order r :=
sorry

theorem exercise_1_8 {k : ℕ} 
    {R : Type*} [ring R] {x y : vector R k} :
    (norm (x + y) ^ 2 + norm (x - y) ^ 2) = 
    2 * (norm x ^ 2 + norm y ^ 2) :=
sorry

theorem exercise_1_14 (z : ℂ) (hz : z.norm = 1) :
    (norm (1 + z) ^ 2 + norm (1 - z) ^ 2) = 2 * norm (1 + z * z) :=
sorry

theorem exercise_1_18a {R : Type*} [comm_ring R] 
    {k : ℕ} (hk : k ≥ 2) (x : vector R k) :
    ∃ (y : vector R k), y ≠ 0 ∧ x ⬝ y = 0 :=
sorry

theorem exercise_1_25 (K : Type*) [metric_space K] 
    [compact_space K] :
    ∃ (B : set (set K)), countable B ∧ is_topological_basis B :=
sorry

theorem exercise_1_27a {E : set ℝ} 
    (hE : uncountable E) : perfect (condensation_points E) :=
sorry

theorem exercise_1_27b {E : Type*} [metric_space E] 
    [separable_space E] (hE : uncountable E) :
    ∃ (P : set E), condensation_points P E ∧ 
    (∀ (x : E), x ∉ P → countable {y : E | y ≠ x ∧ y ∈ E}) :=
sorry

theorem exercise_1_28 {X : Type*} [metric_space X] 
    [separable_space X] (A : set X) :
    is_closed A ↔ ∃ (P : set X) (C : set X), is_perfect P ∧ A = P ∪ C ∧ C.countable :=
sorry

theorem exercise_1_29 (s : set ℝ) (hs : is_open s) :
  ∃ (t : set (set ℝ)), finite t ∧ ∀ (u ∈ t), is_segment u ∧ u ⊆ s ∧ ∀ (v ∈ t), u ≠ v → u ∩ v = ∅ :=
sorry

theorem exercise_2_19a {X : Type*} [metric_space X] 
    {A B : set X} (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
    separated A B :=
sorry

theorem exercise_2_24 {X : Type*} [metric_space X]
    (hX : ∀ (A : set X), infinite A → ∃ (x : X), x ∈ closure A) :
    ∃ (D : set X), dense D :=
sorry

theorem exercise_3_1a {α : Type*} [linear_ordered_field α] 
    {s : ℕ → α} (hs : tendsto s at_top (𝓝 0)) :
    tendsto (λ n, abs (s n)) at_top (𝓝 0) :=
sorry

theorem exercise_3_3 : ∀ (n : ℕ), sqrt 2 < 2 :=
sorry

theorem exercise_3_5 {α : Type*} [linear_order α]
    {f g : ℕ → α} (hf : ∀ n, f n ≤ f (n + 1)) (hg : ∀ n, g n ≤ g (n + 1)) :
    ∀ n, f n + g n ≤ f (n + 1) + g (n + 1) →
    limsup (λ n, f n + g n) ≤ limsup f + limsup g :=
sorry

theorem exercise_3_7 {α : Type*} [linear_ordered_semiring α] 
    (a : ℕ → α) (h : ∀ n, 0 ≤ a n) (hf : is_sum a (finset.univ) (finset.univ) s) :
    is_sum (λ n, sqrt (a n) / n) (finset.univ) (finset.univ) (sqrt s / 2) :=
sorry

theorem exercise_3_8 {α : Type*} 
    [add_comm_monoid α] {f : ℕ → α} {g : ℕ → ℝ} (hf : summable f) 
    (hg : monotone g) (hb : bounded (range g)) :
    summable (λ n, f n * g n) :=
sorry

theorem exercise_3_13 {α : Type*} 
    [comm_ring α] {β : Type*} [add_comm_group β] [module α β] 
    {f g : ℕ → β} (hf : abs_converges f) (hg : abs_converges g) :
    abs_converges (cauchy_prod f g) :=
sorry

theorem exercise_3_20 {X : Type*} [metric_space X] 
    {p : ℕ → X} (hp : cauchy_seq p) {q : ℕ → ℕ} (hq : seq_subseq q p) 
    (hqp : tendsto q p) : tendsto p p :=
sorry

theorem exercise_3_21 
    {X : Type*} [metric_space X] [complete_space X] 
    (E : ℕ → set X) (hE : ∀ n, is_closed (E n)) (hE' : ∀ n, is_bounded (E n)) 
    (hE'' : ∀ n, E n ≠ ∅) (hE''' : ∀ n, E n ⊆ E (n + 1)) 
    (hE'''' : tendsto (λ n, diam (E n)) at_top ( :=
sorry

theorem exercise_3_22 (X : Type*) [metric_space X] [complete_space X] 
    (G : ℕ → set X) (hG : ∀ n, is_open (G n) ∧ dense (G n)) :
    ∃ x, ∀ n, x ∈ G n :=
sorry

theorem exercise_4_2a {X : Type*} [metric_space X] {Y : Type*} 
    [metric_space Y] {f : X → Y} (hf : continuous f) (E : set X) :
    f '' closure E ⊆ closure (f '' E) :=
sorry

theorem exercise_4_3 {X : Type*} [metric_space X] 
    {f : X → ℝ} (hf : continuous f) (hZ : ∀ x, f x = 0 → x ∈ Z) : 
    is_closed Z :=
sorry

theorem exercise_4_4a {X : Type*} [metric_space X] {Y : Type*} 
    [metric_space Y] {f : X → Y} {g : X → Y} (hf : continuous f) 
    (hg : continuous g) (hE : dense (set.range f)) :
    dense (set.range g) :=
sorry

theorem exercise_4_5a {E : Type*} [topological_space E]
    [compact_space E] {f : E → ℝ} (hf : continuous f) :
    ∃ (g : ℝ → ℝ), continuous g ∧ ∀ (x : E), g x = f x :=
sorry

theorem exercise_4_6 {E : Type*} [topological_space E] 
    [compact_space E] {f : E → ℝ} :
    continuous f ↔ compact (set.range (λ x, (x, f x))) :=
sorry

theorem exercise_4_8a {E : Type*} 
    [metric_space ℝ E] {f : E → ℝ} (hf : uniform_continuous_on f E) 
    (hE : metric.bounded E) : metric.bounded (set.range f) :=
sorry

theorem exercise_4_11a {X : Type*} [metric_space X] 
    {Y : Type*} [metric_space Y] {f : X → Y} (hf : uniform_continuous f) 
    {x : ℕ → X} (hx : cauchy x) : cauchy (λ n, f (x n)) :=
sorry

theorem exercise_4_12 {α : Type u} {β : Type v} {γ : Type w} 
    [uniform_space α] [uniform_space β] [uniform_space γ] 
    {f : α → β} {g : β → γ} (hf : uniform_continuous f) 
    (hg : uniform_continuous g) : uniform_continuous (g ∘ f) :=
sorry

theorem exercise_4_14 {I : Type*} [topological_space I]
    [linear_order I] (f : I → I) (hf : continuous f) :
    ∃ (x : I), f x = x :=
sorry

theorem exercise_4_15 {f : ℝ → ℝ} 
    (hf : continuous f) (hof : is_open (set.range f)) :
    monotone f :=
sorry

theorem exercise_4_19 
    {f : ℝ → ℝ} (hf : ∀ a b c, a < b → f a < c → c < f b → ∃ x, a < x ∧ x < b ∧ f x = c) 
    (hg : ∀ r, is_closed {x | f x = r}) : continuous f :=
sorry

theorem exercise_4_21a {X : Type*} [metric_space X]
    (K F : set X) (hK : compact K) (hF : is_closed F) (hKF : disjoint K F) :
    ∃ (δ : ℝ), δ > 0 ∧ ∀ (p q : X), p ∈ K → q ∈ F → dist p q ≥ δ :=
sorry

theorem exercise_4_24 {f : ℝ → ℝ} 
    (hf : continuous f) (h : ∀ x y : ℝ, a < x → x < b → a < y → y < b → 
    f ((x + y) / 2) ≤ (f x + f y) / 2) :
    convex_on f (Icc a b) :=
sorry

theorem exercise_4_26a 
    {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z] 
    (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hg : continuous g) 
    (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry

theorem exercise_5_1 {α : Type*} [linear_ordered_field α] 
    {f : α → α} (hf : ∀ x y : α, abs (f x - f y) ≤ (x - y) ^ 2) :
    ∃ (c : α), f = function.const α c :=
sorry

theorem exercise_5_2 {α : Type*} [linear_order α] 
    [topological_space α] [has_deriv_at α ℝ] {f : α → ℝ} (hf : ∀ x ∈ Icc a b, f' x > 0) 
    (hf_inc : monotone_on f (Icc a b)) (g : α → ℝ) (hg : g = f.inverse) 
    (hg_diff : differentiable_at ℝ g b) :
    (g.deriv_at b).has_der :=
sorry

theorem exercise_5_3 {g : ℝ → ℝ} (hg : continuous g) 
    (hg' : ∃ M : ℝ, ∀ x : ℝ, abs (g' x) ≤ M) (ε : ℝ) (hε : ε > 0) :
    is_injective (λ x : ℝ, x + ε * g x) :=
sorry

theorem exercise_5_4 {n : ℕ} 
    (C : fin n → ℝ) (hC : C 0 + (C 1 : ℝ) / 2 + (C 2 : ℝ) / 3 + (C 3 : ℝ) / 4 + 
    (C 4 : ℝ) / 5 + (C 5 : ℝ) / 6 + (C 6 : ℝ) / 7 + (C 7 : ℝ) / 8 + (C 8 : ℝ) / 9 + 
    (C 9 : ℝ) / 10 + ( :=
sorry

theorem exercise_5_5 {α : Type*} [topological_space α]
    [metric_space α] {f : α → ℝ} {x : α} {l : ℝ} (hf : tendsto f at_top (𝓝 l)) :
    tendsto (λ x, f x - l) at_top (𝓝 0) :=
sorry

theorem exercise_5_6 {α : Type*} [linear_ordered_field α]
    [topological_space α] [metric_space α] [normed_field α]
    {f : α → α} (hf : continuous f) (hf' : ∀ x, 0 < x → deriv f x ≠ 0)
    (hf0 : f 0 = 0) (hf'_mono : ∀ x y, 0 < x → 0 < y → x < y → deriv f x < deriv f y) :
    monotone f :=
sorry

theorem exercise_5_7 {α : Type*} [linear_order α]
    [topological_space α] [topological_space ℝ] {f g : α → ℝ}
    (hf : tendsto f (𝓝 x) (𝓝 0)) (hg : tendsto g (𝓝 x) (𝓝 0))
    (hg' : tendsto (λ x, g x - g x) (𝓝 x) (𝓝 0))
    (hg'_ne_0 : ∀ x, g' :=
sorry

theorem exercise_5_15 {f : ℝ → ℝ} (hf : twice_differentiable_at ℝ f a) 
    (hM0 : ∃ M0, ∀ x, a < x → abs (f x) ≤ M0) 
    (hM1 : ∃ M1, ∀ x, a < x → abs (fderiv f x) ≤ M1) 
    (hM2 : ∃ M2, ∀ x, a < x → abs (fderiv fderiv f x) ≤ M2) :
    (M1 ^ 2) ≤ 4 * M0 * M2 :=
sorry

theorem exercise_5_17 
    {f : ℝ → ℝ} (hf : three_times_differentiable_on ℝ f I) 
    (hf0 : f (-1) = 0) (hf1 : f 0 = 0) (hf2 : f 1 = 1) (hf3 : fderiv ℝ f 0 = 0) :
    ∃ (x : ℝ), x ∈ I ∧ fderiv ℝ (fderiv ℝ (fderiv ℝ f)) x ≥ 3 :=
sorry

theorem exercise_6_1 {α : Type*} [linear_order α] 
    [topological_space α] [measurable_space α] [measure_space α] 
    (f : α → ℝ) (hf : ∀ x, f x = 0 ∨ f x = 1) (hα : ∀ x y, x ≤ y → α x ≤ α y) 
    (hα_cont : continuous_at α (λ x, x = x₀)) (x₀ : α) (hx₀ : f x₀ = 1) : :=
sorry

theorem exercise_6_2 {α : Type*} [linear_ordered_field α]
    [measurable_space α] [measurable_space β] [measurable_space γ]
    [measurable_space δ] [measurable_space ε] [measurable_space ζ]
    [measurable_space η] [measurable_space θ] [measurable_space ι]
    [measurable_space κ] [measurable_space λ] [measurable_space μ]
    [measurable :=
sorry

theorem exercise_6_4 
    {f : ℝ → ℝ} (hf : ∀ x : ℝ, ¬ is_irrational x → f x = 1) 
    (hf' : ∀ x : ℝ, is_irrational x → f x = 0) :
    ∀ (a b : ℝ), a < b → ¬ continuous_on f (Icc a b) :=
sorry

theorem exercise_6_6 {f : ℝ → ℝ} 
    (hf : continuous_on (set.compl (cantor_set)) f) 
    (hb : metric.bounded (set.range f)) :
    is_r_integrable f :=
sorry