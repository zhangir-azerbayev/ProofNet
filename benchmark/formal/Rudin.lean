import .common

open real
open topological_space
open filter
open_locale topological_space
open_locale big_operators
open_locale complex_conjugate
open_locale filter


noncomputable theory

theorem exercise_1_1a
  (x : ℝ) (y : ℚ) :
  ( irrational x ) -> irrational ( x + y ) :=
begin
  apply irrational.add_rat,
end

theorem exercise_1_1b
(x : ℝ)
(y : ℚ)
(h : y ≠ 0)
: ( irrational x ) -> irrational ( x * y ) :=
begin
  intro g,
  apply irrational.mul_rat g h,
end

theorem exercise_1_2
: ¬ ∃ (x : ℚ), ( x ^ 2 = 12 ) :=
begin
  simp, intros x h,
  have h₁: (12 : ℚ) ≠ 0 := by norm_num,
  have h₂: (x.denom ^ 2 : ℚ) ≠ 0 := by { simp , intro e, have := x.pos, linarith},
  have h₃: (12 : ℚ) = 3 * 4 := by norm_num,
  have h₄ : (factorization (3 : ℚ)) 3 = 1 := by sorry,
  have h₅ : (factorization (4 : ℚ)) 3 = 0 := by sorry,
  have h₆: (12 : ℚ) * (x.denom ^ 2) = x.num ^ 2 := by {rw [←h, ←mul_pow], simp},
  have h₇ : factorization ((12 : ℚ) * (x.denom ^ 2)) 3 = factorization ((x.num : ℚ) ^ 2) 3 := by rw h₆,
  have h₈ : 2 ∣ factorization ((x.num : ℚ) ^ 2) 3 := by {rw factorization_pow, sorry},
  have h₉ : ¬ 2 ∣ factorization ((12 : ℚ) * x.denom ^ 2) 3 :=
  begin
  rw factorization_mul h₁ h₂,
  rw factorization_pow,
  rw h₃,
  rw factorization_mul (by norm_num : (3 : ℚ) ≠ 0) (by norm_num : (4 : ℚ) ≠ 0),
  simp [h₄, h₅],
  sorry, 
  end,
  have h₀ : 2 ∣ factorization ((12 : ℚ) * (x.denom ^ 2)) 3 := by {rw h₇, exact h₈},
  exact absurd h₀ h₉,
end

theorem exercise_1_4
(α : Type*) [partial_order α]
(s : set α)
(x y : α)
(h₀ : set.nonempty s)
(h₁ : x ∈ lower_bounds s)
(h₂ : y ∈ upper_bounds s)
: x ≤ y :=
begin
  have h : ∃ z, z ∈ s := h₀,
  cases h with z,
  have xlez : x ≤ z :=
  begin
  apply h₁,
  assumption,
  end,
  have zley : z ≤ y :=
  begin
  apply h₂,
  assumption,
  end,
  exact xlez.trans zley,
end

theorem exercise_1_5
  (A minus_A : set ℝ) (hA : A.nonempty) (hA_bdd_below : bdd_below A)
  (hminus_A : minus_A = {x | -x ∈ A}) :
  Inf A = Sup minus_A :=
sorry

theorem exercise_1_8
  : ¬ ∃ (r : ℂ → ℂ → Prop), is_linear_order ℂ r :=
sorry

section
open complex

theorem exercise_1_11a
  (z : ℂ) : ∃ (r : ℝ) (w : ℂ), abs w = 1 ∧ z = r * w :=
begin
  by_cases h : z = 0,
  {
  use [0, 1],
  simp,
  assumption,
  },
  {
  use abs z,
  use z / ↑(abs z),
  split,
  {
  simp,
  field_simp [h],
  },
  {
  field_simp [h],
  apply mul_comm,
  },
  },
end

theorem exercise_1_12
  (n : ℕ) (f : ℕ → ℂ)
  : abs (∑ i in finset.range n, f i) ≤ ∑ i in finset.range n, abs (f i) :=
sorry 

theorem exercise_1_13
  (x y : ℂ)
  : |(abs x) - (abs y)| ≤ abs (x - y) :=
sorry

theorem exercise_1_14
  (z : ℂ) (h : abs z = 1)
  : (abs (1 + z)) ^ 2 + (abs (1 - z)) ^ 2 = 4 :=
sorry

end

theorem exercise_1_16a
  (n : ℕ)
  (d r : ℝ)
  (x y z : euclidean_space ℝ (fin n)) -- R^n
  (h₁ : n ≥ 3)
  (h₂ : ∥x - y∥ = d)
  (h₃ : d > 0)
  (h₄ : r > 0)
  (h₅ : 2 * r > d)
  : set.infinite {z : euclidean_space ℝ (fin n) | ∥z - x∥ = r ∧ ∥z - y∥ = r} :=
sorry

theorem exercise_1_17
  (n : ℕ)
  (x y : euclidean_space ℝ (fin n)) -- R^n
  : ∥x + y∥^2 + ∥x - y∥^2 = 2*∥x∥^2 + 2*∥y∥^2 :=
sorry

theorem exercise_1_18a
  (n : ℕ)
  (h : n > 1)
  (x : euclidean_space ℝ (fin n)) -- R^n
  : ∃ (y : euclidean_space ℝ (fin n)), y ≠ 0 ∧ (inner x y) = (0 : ℝ) :=
sorry

theorem exercise_1_18b
  : ¬ ∀ (x : ℝ), ∃ (y : ℝ), y ≠ 0 ∧ x * y = 0 :=
begin
  simp,
  use 1,
  intros x h₁ h₂,
  cases h₂,
  {norm_num at h₂},
  {exact absurd h₂ h₁},
end

theorem exercise_1_19
  (n : ℕ)
  (a b c x : euclidean_space ℝ (fin n))
  (r : ℝ)
  (h₁ : r > 0)
  (h₂ : 3 • c = 4 • b - a)
  (h₃ : 3 * r = 2 * ∥x - b∥)
  : ∥x - a∥ = 2 * ∥x - b∥ ↔ ∥x - c∥ = r :=
sorry

theorem exercise_2_19a {X : Type*} [metric_space X]
  (A B : set X) (hA : is_closed A) (hB : is_closed B) (hAB : disjoint A B) :
  separated_nhds A B :=
sorry

theorem exercise_2_24 {X : Type*} [metric_space X]
  (hX : ∀ (A : set X), infinite A → ∃ (x : X), x ∈ closure A) :
  separable_space X :=
sorry

theorem exercise_2_25 {K : Type*} [metric_space K] [compact_space K] :
  ∃ (B : set (set K)), set.countable B ∧ is_topological_basis B :=
sorry

theorem exercise_2_27a (k : ℕ) (E P : set (euclidean_space ℝ (fin k)))
  (hE : E.nonempty ∧ ¬ set.countable E)
  (hP : P = {x | ∀ U ∈ 𝓝 x, ¬ set.countable (P ∩ E)}) :
  is_closed P ∧ P = {x | cluster_pt x (𝓟 P)}  :=
sorry

theorem exercise_2_27b (k : ℕ) (E P : set (euclidean_space ℝ (fin k)))
  (hE : E.nonempty ∧ ¬ set.countable E)
  (hP : P = {x | ∀ U ∈ 𝓝 x, (P ∩ E).nonempty ∧ ¬ set.countable (P ∩ E)}) :
  set.countable (E \ P) :=
sorry

theorem exercise_2_28 (X : Type*) [metric_space X] [separable_space X]
  (A : set X) (hA : is_closed A) :
  ∃ P₁ P₂ : set X, A = P₁ ∪ P₂ ∧
  is_closed P₁ ∧ P₁ = {x | cluster_pt x (𝓟 P₁)} ∧
  set.countable P₂ :=
sorry

theorem exercise_2_29 (U : set ℝ) (hU : is_open U) :
  ∃ (f : ℕ → set ℝ), (∀ n, ∃ a b : ℝ, f n = {x | a < x ∧ x < b}) ∧ (∀ n, f n ⊆ U) ∧
  (∀ n m, n ≠ m → f n ∩ f m = ∅) ∧
  U = ⋃ n, f n :=
sorry

theorem exercise_3_1a
  (f : ℕ → ℝ)
  (h : ∃ (a : ℝ), tendsto (λ (n : ℕ), f n) at_top (𝓝 a))
  : ∃ (a : ℝ), tendsto (λ (n : ℕ), |f n|) at_top (𝓝 a) :=
begin
  cases h with a h,
  use |a|,
  apply filter.tendsto.abs h,
end

theorem exercise_3_2a
  : tendsto (λ (n : ℝ), (sqrt (n^2 + n) - n)) at_top (𝓝 (1/2)) :=
begin
  have h : ∀ (n : ℝ), n > 0 → sqrt (n^2 + n) - n = 1 / (sqrt (1 + 1 / n) + 1) :=
  begin
  intro n,
  intro h,
  have h₁ : sqrt (n^2 + n) + n ≠ 0 := by {intro h₁, simp at *, rw ←h₁ at h, simp at h,
    have : sqrt (n ^ 2 + n) ≥ 0 := sqrt_nonneg (n ^ 2 + n), linarith,},
  have h₂ : sqrt (n^2 + n) + n = sqrt (n^2 + n) + n := by refl,
  have h₃ : n ≥ 0 := by linarith,
  have h₄ : n ≠ 0 := by linarith,
  have h₅ : n^2 + n ≥ 0 := by {simp, transitivity, apply h₃, simp, apply sq_nonneg},
  calc  _ = (sqrt (n^2 + n) - n) * 1 : by rw mul_one _
  ... = (sqrt (n^2 + n) - n) * ((sqrt (n^2 + n) + n) /
          (sqrt (n^2 + n) + n)) : by rw ←((div_eq_one_iff_eq h₁).2 h₂)
  ... = n / (sqrt (n^2 + n) + n) : by {field_simp, ring, sorry}
  ... = 1 / (sqrt (n^2 + n) / sqrt (n^2) + n / sqrt (n^2)) : by {field_simp, simp [sqrt_sq h₃]}
  ... = 1 / (sqrt (n^2 + n) / sqrt (n^2) + 1) : by simp [sqrt_sq h₃, (div_eq_one_iff_eq h₄).2]
  ... = 1 / (sqrt (1 + n / (n ^ 2)) + 1): by {rw ←(sqrt_div h₅ (n^2)), field_simp}
  ... = 1 / (sqrt (1 + 1 / n) + 1): by simp [pow_succ]
  end,
  refine (tendsto_congr' _).mp _,
  exact λ n, 1 / (sqrt (1 + 1 / n) + 1),
  refine eventually_at_top.mpr _,
  use 1,
  intros b bgt1, symmetry, apply h, linarith,
  have g : tendsto (λ (n : ℝ), 1 / n) at_top (𝓝 0) :=
  begin
  simp,
  apply tendsto_inv_at_top_zero,
  end,
  have h : tendsto (λ (n : ℝ), 1 / (sqrt (1 + n) + 1)) (𝓝 0) (𝓝 (1/2)) :=
  begin
  have : (1/2 : ℝ) = (λ (n : ℝ), 1 / (sqrt (1 + n) + 1)) 0 := by {simp, norm_num}, rw this,
  apply continuous_at.tendsto, simp,
  refine continuous_at.comp _ _, simp,
  refine continuous_at.add _ _,
  refine continuous_at.sqrt _, simp,
  refine continuous_at.add _ _,
  exact continuous_at_const,
  exact continuous_at_id,
  exact continuous_at_const,
  end,
  apply tendsto.comp h g,
end

noncomputable def f : ℕ → ℝ
| 0 := sqrt 2
| (n + 1) := sqrt (2 + sqrt (f n))

theorem exercise_3_3
  : ∃ (x : ℝ), tendsto f at_top (𝓝 x) ∧ ∀ n, f n < 2 :=
sorry

theorem exercise_3_5 -- TODO fix
  (a b : ℕ → ℝ)
  (h : limsup a + limsup b ≠ 0) :
  limsup (λ n, a n + b n) ≤ limsup a + limsup b :=
sorry

def g (n : ℕ) : ℝ := sqrt (n + 1) - sqrt n

theorem exercise_3_6a
: tendsto (λ (n : ℕ), (∑ i in finset.range n, g i)) at_top at_top :=
begin
  simp,
  have : (λ (n : ℕ), (∑ i in finset.range n, g i)) = (λ (n : ℕ), sqrt (n + 1)) := by sorry,
  rw this,
  apply tendsto_at_top_at_top_of_monotone,
  unfold monotone,
  intros a b a_le_b,
  apply sqrt_le_sqrt,
  simp, assumption,
  intro x,
  --use x ^ 2 - 1,
  --apply filter.tendsto.sqrt,
  sorry
end

theorem exercise_3_7
  (a : ℕ → ℝ)
  (h : ∃ y, (tendsto (λ n, (∑ i in (finset.range n), a i)) at_top (𝓝 y))) :
  ∃ y, tendsto (λ n, (∑ i in (finset.range n), sqrt (a i) / n)) at_top (𝓝 y) :=
sorry

theorem exercise_3_8
  (a b : ℕ → ℝ)
  (h1 : ∃ y, (tendsto (λ n, (∑ i in (finset.range n), a i)) at_top (𝓝 y)))
  (h2 : monotone b)
  (h3 : metric.bounded (set.range b)) :
  ∃ y, tendsto (λ n, (∑ i in (finset.range n), (a i) * (b i))) at_top (𝓝 y) :=
sorry

theorem exercise_3_13
  (a b : ℕ → ℝ)
  (ha : ∃ y, (tendsto (λ n, (∑ i in (finset.range n), |a i|)) at_top (𝓝 y)))
  (hb : ∃ y, (tendsto (λ n, (∑ i in (finset.range n), |b i|)) at_top (𝓝 y))) :
  ∃ y, (tendsto (λ n, (∑ i in (finset.range n),
  λ i, (∑ j in finset.range (i + 1), a j * b (i - j)))) at_top (𝓝 y)) :=
sorry

theorem exercise_3_20 {X : Type*} [metric_space X]
  (p : ℕ → X) (l : ℕ) (r : X)
  (hp : cauchy_seq p)
  (hpl : tendsto (λ n, p (l * n)) at_top (𝓝 r)) :
  tendsto p at_top (𝓝 r) :=
sorry

theorem exercise_3_21
  {X : Type*} [metric_space X] [complete_space X]
  (E : ℕ → set X)
  (hE : ∀ n, E n ⊃ E (n + 1))
  (hE' : tendsto (λ n, metric.diam (E n)) at_top (𝓝 0)) :
  ∃ a, set.Inter E = {a} :=
sorry

theorem exercise_3_22 (X : Type*) [metric_space X] [complete_space X]
  (G : ℕ → set X) (hG : ∀ n, is_open (G n) ∧ dense (G n)) :
  ∃ x, ∀ n, x ∈ G n :=
sorry

theorem exercise_4_1a
  : ∃ (f : ℝ → ℝ), (∀ (x : ℝ), tendsto (λ y, f(x + y) - f(x - y)) (𝓝 0) (𝓝 0)) ∧ ¬ continuous f :=
begin
  use λ x, if x = 0 then (1 : ℝ) else (0 : ℝ),
  split,
  {
  intro x,
  by_cases h : x = 0,
  {
  rw h, simp,
  exact tendsto_const_nhds,
  },
  {
  intros X hX,
  refine mem_nhds_iff.2 _,
  use {z | -|x| < z ∧ z < |x|},
  simp,
  split,
  {
  set f := (λ (y : ℝ), ite (x + y = 0) (1 : ℝ) 0 - ite (x - y = 0) 1 0),
  set f₁ := (λ (y : ℝ), ite (x + y = 0) (1 : ℝ) 0),
  set f₂ := (λ (y : ℝ), - ite (x - y = 0) (1 : ℝ) 0),
  set Y := {z : ℝ | - | x | < z ∧ z < | x |},
  have : (0 : ℝ) ∈ X := mem_of_mem_nhds hX,
  have h₁: {(0 : ℝ)} ⊆ X := set.singleton_subset_iff.mpr this,
  have h₂ : f = f₁ + f₂ := rfl,
  have g₁ : ∀ y ∈ Y, ¬x + y = 0 := by {
    simp,
    intros y hy₁ hy₂ hy₃,
    by_cases hx : 0 < x,
    rw abs_of_pos hx at *,
    linarith,
    simp at hx,
    have hx : x < 0 := lt_of_le_of_ne hx h,
    rw abs_of_neg hx at *,
    linarith,
  },
  have g₂ : ∀ y ∈ Y, ¬x - y = 0 := by {
    simp,
    intros y hy₁ hy₂ hy₃,
    by_cases hx : 0 < x,
    rw abs_of_pos hx at *,
    linarith,
    simp at hx,
    have hxx : x < 0 := lt_of_le_of_ne hx h,
    rw abs_of_neg hxx at *,
    linarith,
  },
  have gg₁ : ∀ y ∈ Y, f₁ y = (0 : ℝ) := by {
    intros a b,
    simp,
    intro c,
    exact g₁ a b c,
  },
  have gg₂ : ∀ y ∈ Y, f₂ y = (0 : ℝ) := by {
    intros a b,
    simp,
    intro c,
    exact g₂ a b c,
  },
  have gg : ∀ z ∈ Y, f z = (0 : ℝ) := by {
    intros a b,
    simp [h₂],
    rw [gg₁ a b, gg₂ a b],
    norm_num,
  },
  have : f ⁻¹' {(0 : ℝ)} ⊆ f ⁻¹' X := set.preimage_mono h₁,
  exact set.subset.trans gg this,
  },
  {
  split,
  {
    rw set.set_of_and,
    apply is_open.inter _ _,
    apply is_open_lt' (-|x|),
    apply is_open_gt' (|x|),
  },
  {
    exact h,
  }
  },
  },
  },
  {
  intro h,
  let f : (ℝ → ℝ) := λ x, if x = 0 then (1 : ℝ) else 0,
  have g : f 0 = 1 := if_pos rfl,
  have g₁ : f 1 = 0 := by {refine if_neg _, norm_num,},
  have : continuous_at f 0 := continuous.continuous_at h,
  have := continuous_at.tendsto this,
  rw g at this,
  unfold tendsto at this,
  have := filter.le_def.1 this,
  simp at this,
  have := this (set.Ioo (0.5 : ℝ) (1.5 : ℝ)),
  have i : set.Ioo (0.5 : ℝ) (1.5 : ℝ) ∈ 𝓝 (1 : ℝ) := by {
  apply is_open.mem_nhds,
  exact is_open_Ioo,
  norm_num,
  },
  have h₁ : set.range f  = {(0 : ℝ), 1} := by {
  ext, split,
  {
  intro h,
  simp,
  cases h,
  by_cases r : h_w = 0,
  rw r at h_h,
  rw g at h_h,
  right,
  exact eq.symm h_h,
  have ii : f h_w = 0 := if_neg r,
  rw ii at h_h,
  left,
  symmetry,
  exact h_h,
  },
  intro h,
  apply set.mem_range.2,
  by_cases r : x = 0,
  {
  use 1,
  rw g₁,
  apply eq.symm _,
  exact r,
  },
  {
  have i : x ∈ {(1 : ℝ)} := by {
    apply set.mem_of_mem_insert_of_ne _ r,
    exact h,
  },
  use 0,
  rw g,
  exact eq.symm i,
  },
  },
  have h₂ : set.Ioo ((1 / 2 : ℝ)) (3 / 2) ∩ {(0 : ℝ), 1} = {(1 : ℝ)} := by {
  unfold set.Ioo,
  ext, split,
  {
  intro h,
  simp at h,
  cases h,
  cases h_right,
  rw h_right at h_left,
  norm_num at h_left,
  exact h_right,
  },
  {
  intro h,
  have : x = 1 := h,
  rw this,
  norm_num,
  },
  },
  have h₃ : {0} ⊆ f ⁻¹' {(1 : ℝ)} := set.singleton_subset_iff.mpr g,
  have j : f ⁻¹' set.Ioo (1 / 2) (3 / 2) = {0} := by {
  rw [← set.preimage_inter_range, h₁, h₂],
  ext,
  split,
  {
  intro hx,
  by_contradiction h₄,
  have : ¬ x = 0 := h₄,
  have h₅ : f x = 0 := if_neg this,
  have : f x = 1 := hx,
  rw h₅ at this,
  norm_num at this,
  },
  intro x, exact h₃ x,
  },
  have := this i,
  rw j at this,
  have := mem_nhds_iff.1 this,
  cases this with s h,
  cases h with k g,
  cases g,
  by_cases a : s = {0},
  {
  rw a at g_left,
  have := dense_compl_singleton (0 : ℝ),
  have := dense_compl_singleton_iff_not_open.1 this,
  contradiction,
  },
  {
  have : {(0 : ℝ)} ⊆ s := set.zero_subset.mpr g_right,
  have : s = {(0 : ℝ)} := set.subset.antisymm k this,
  contradiction,
  },
  },
end

theorem exercise_4_2a
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f : α → β)
  (h₁ : continuous f)
  : ∀ (x : set α), f '' (closure x) ⊆ closure (f '' x) :=
begin
  intros X x h₂ Y h₃,
  simp at *,
  cases h₃ with h₃ h₄,
  cases h₂ with w h₅,
  cases h₅ with h₅ h₆,
  have h₈ : is_closed (f ⁻¹' Y) := is_closed.preimage h₁ h₃,
  have h₉ : closure X ⊆ f ⁻¹' Y := closure_minimal h₄ h₈,
  rw ←h₆,
  exact h₉ h₅,
end

theorem exercise_4_3
  {α : Type} [metric_space α]
  (f : α → ℝ) (h : continuous f) (z : set α) (g : z = f⁻¹' {0})
  : is_closed z :=
begin
  rw g,
  apply is_closed.preimage h,
  exact is_closed_singleton,
end

theorem exercise_4_4a
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f : α → β)
  (s : set α)
  (h₁ : continuous f)
  (h₂ : dense s)
  : f '' set.univ ⊆ closure (f '' s) :=
begin
  simp,
  exact continuous.range_subset_closure_image_dense h₁ h₂,
end

theorem exercise_4_4b
  {α : Type} [metric_space α]
  {β : Type} [metric_space β]
  (f g : α → β)
  (s : set α)
  (h₁ : continuous f)
  (h₂ : continuous g)
  (h₃ : dense s)
  (h₄ : ∀ x ∈ s, f x = g x)
  : f = g :=
begin
  have h₅ : is_closed {x | f x = g x} := is_closed_eq h₁ h₂,
  unfold dense at h₃,
  set t := {x : α | f x = g x} with h,
  have h₆ : s ⊆ t := h₄,
  have h₇ : closure s ⊆ closure t := closure_mono h₆,
  --have h₁₀ : closure s = set.univ := by { ext, simp, apply h₃,},
  --exact h₃, -- does not work ...
  have h₈ : ∀ x, x ∈ closure t := by { intro, apply h₇ (h₃ x), },
  have h₉ : closure t = t := closure_eq_iff_is_closed.2 h₅,
  rw h₉ at h₈,
  ext,
  exact h₈ x,
end

theorem exercise_4_5a
  (f : ℝ → ℝ)
  (E : set ℝ)
  (h₁ : is_closed E)
  (h₂ : continuous_on f E)
  : ∃ (g : ℝ → ℝ), continuous g ∧ ∀ x ∈ E, f x = g x :=
sorry

theorem exercise_4_5b
  : ∃ (E : set ℝ) (f : ℝ → ℝ), (continuous_on f E) ∧
  (¬ ∃ (g : ℝ → ℝ), continuous g ∧ ∀ x ∈ E, f x = g x) :=
begin
  set E : set ℝ := (set.Iio 0) ∪ (set.Ioi 0) with hE,
  let f : ℝ → ℝ := λ x, if x < 0 then 0 else 1,
  use E, use f,
  split,
  {
  refine continuous_on_iff.mpr _,
  intros x h₁ X h₂ h₃,
  by_cases h₄ : x < 0,
  {
  use set.Ioo (x - 1) 0,
  have h₅ : f x = 0 := if_pos h₄,
  split, exact is_open_Ioo,
  split,
  {
  have h₆ : x - 1 < x := by linarith,
  exact set.mem_sep h₆ h₄,
  },
  have h₆ : set.Ioo (x - 1) 0 ⊆ set.Iio 0 := set.Ioo_subset_Iio_self,
  have h₇ : set.Ioo (x - 1) 0 ∩ E = set.Ioo (x - 1) 0 := by {
  rw hE, simp, exact set.subset_union_of_subset_left h₆ (set.Ioi 0),
  },
  rw h₇,
  have h₈ : (0 : ℝ) ∈ X := by {rw h₅ at h₃, exact h₃,},
  have h₉ : {(0 : ℝ)} ⊆ X := set.singleton_subset_iff.mpr h₈,
  have h₁₀ : set.Iio 0 ⊆ f ⁻¹' {0} := by {
  intros y hy,
  apply set.mem_preimage.2,
  have : f y = 0 := if_pos hy,
  rw this, simp,
  },
  have h₁₁ : f ⁻¹' {0} ⊆ f ⁻¹' X := set.preimage_mono h₉,
  have h₁₂ : set.Iio 0 ⊆ f ⁻¹' X := set.subset.trans h₁₀ h₁₁,
  exact set.subset.trans h₆ h₁₂,
  },
  {
  use set.Ioo 0 (x + 1),
  have h₄' : x > 0  := by {
  have : x ≠ 0 := by {rw hE at h₁, simp at h₁, exact h₁,},
  refine lt_of_le_of_ne _ this.symm,
  exact not_lt.mp h₄,
  },
  have h₅ : f x = 1 := if_neg h₄,
  split, exact is_open_Ioo,
  split,
  {
  have h₆ : x < x + 1:= by linarith,
  exact set.mem_sep h₄' h₆,
  },
  have h₆ : set.Ioo 0 (x + 1) ⊆ set.Ioi 0 := set.Ioo_subset_Ioi_self,
  have h₇ : set.Ioo 0 (x + 1) ∩ E = set.Ioo 0 (x + 1) := by {
  rw hE, simp, exact set.subset_union_of_subset_right h₆ (set.Iio 0),
  },
  rw h₇,
  have h₈ : (1 : ℝ) ∈ X := by {rw h₅ at h₃, exact h₃,},
  have h₉ : {(1 : ℝ)} ⊆ X := set.singleton_subset_iff.mpr h₈,
  have h₁₀ : set.Ioi 0 ⊆ f ⁻¹' {1} := by {
  intros y hy,
  have : y ∈ set.Ici (0 : ℝ) := set.mem_Ici_of_Ioi hy,
  have : ¬ y < 0 := asymm hy,
  apply set.mem_preimage.2,
  have : f y = 1 := if_neg this,
  rw this, simp,
  },
  have h₁₁ : f ⁻¹' {1} ⊆ f ⁻¹' X := set.preimage_mono h₉,
  have h₁₂ : set.Ioi 0 ⊆ f ⁻¹' X := set.subset.trans h₁₀ h₁₁,
  exact set.subset.trans h₆ h₁₂,
  },
  },
  {
  by_contradiction h₁,
  cases h₁ with g h₁,
  cases h₁ with h₁ h₂,
  have h₃ : continuous_at g 0 := continuous.continuous_at h₁,
  have h₄ := continuous_at.tendsto h₃,
  unfold tendsto at h₄,
  have h₅ := filter.le_def.1 h₄,
  simp at h₅,
  by_cases h₆ : g 0 > 0.5,
  {
  have h₇ : set.Ioi (0 : ℝ) ∈ 𝓝 (g 0) := by { refine Ioi_mem_nhds _, linarith,},
  have h₈ := h₅ (set.Ioi (0 : ℝ)) h₇,
  have h₉ : g ⁻¹' set.Ioi 0 = set.Ici 0 := by {
  ext,
  split,
  {
    intro h,
    simp at h,
    by_cases hw : x = 0,
    {rw hw, exact set.left_mem_Ici,},
    {
    have : x ∈ E := by {rw hE, simp, exact hw,},
    rw ←(h₂ x this) at h,
    by_contradiction hh,
    simp at hh,
    have : f x = 0 := if_pos hh,
    linarith,
    },
  },
  {
    intro h,
    simp,
    by_cases hw : x = 0,
    {rw hw, linarith,},
    {
    have h₉ : x > 0 := (ne.symm hw).le_iff_lt.mp h,
    have : x ∈ E := (set.Iio 0).mem_union_right h₉,
    rw ←(h₂ x this),
    have : ¬ x < 0 := asymm h₉,
    have : f x = 1 := if_neg this,
    linarith,
    },
  },
  },
  rw h₉ at h₈,
  have h₁₀ := interior_mem_nhds.2 h₈,
  simp at h₁₀,
  have := mem_of_mem_nhds h₁₀,
  simp at this,
  exact this,
  },
  {
  have h₇ : set.Iio (1 : ℝ) ∈ 𝓝 (g 0) := by { refine Iio_mem_nhds _, linarith, },
  have h₈ := h₅ (set.Iio (1 : ℝ)) h₇,
  have h₉ : g ⁻¹' set.Iio 1 = set.Iic 0 := by {
  ext,
  split,
  {
    intro h,
    simp at h,
    by_cases hw : x = 0,
    {simp [hw],},
    {
    have : x ∈ E := by {rw hE, simp, exact hw,},
    rw ←(h₂ x this) at h,
    by_contradiction hh,
    simp at hh,
    have : f x = 1 := if_neg ((by linarith) : ¬x < 0),
    linarith,
    },
  },
  {
    intro h,
    simp,
    by_cases hw : x = 0,
    {rw hw, linarith,},
    {
    have h₉ : x < 0 := (ne.le_iff_lt hw).mp h,
    have : x ∈ E := (set.Ioi 0).mem_union_left h₉,
    rw ←(h₂ x this),
    have : f x = 0 := if_pos h₉,
    linarith,
    },
  },
  },
  rw h₉ at h₈,
  have h₁₀ := interior_mem_nhds.2 h₈,
  simp at h₁₀,
  have := mem_of_mem_nhds h₁₀,
  simp at this,
  exact this,
  }
  }
end

theorem exercise_4_6
  (f : ℝ → ℝ)
  (E : set ℝ)
  (G : set (ℝ × ℝ))
  (h₁ : is_compact E)
  (h₂ : G = {(x, f x) | x ∈ E})
  : continuous_on f E ↔ is_compact G :=
sorry

theorem exercise_4_8a
  (E : set ℝ) (f : ℝ → ℝ) (hf : uniform_continuous_on f E)
  (hE : metric.bounded E) : metric.bounded (set.image f E) :=
sorry

theorem exercise_4_8b
  (E : set ℝ) :
  ∃ f : ℝ → ℝ, uniform_continuous_on f E ∧ ¬ metric.bounded (set.image f E) :=
sorry

theorem exercise_4_11a
  {X : Type*} [metric_space X]
  {Y : Type*} [metric_space Y]
  (f : X → Y) (hf : uniform_continuous f)
  (x : ℕ → X) (hx : cauchy_seq x) :
  cauchy_seq (λ n, f (x n)) :=
sorry

theorem exercise_4_12
  {α β γ : Type*} [uniform_space α] [uniform_space β] [uniform_space γ]
  {f : α → β} {g : β → γ}
  (hf : uniform_continuous f) (hg : uniform_continuous g) :
  uniform_continuous (g ∘ f) :=
sorry

abbreviation I : set ℝ := set.Icc 0 1

theorem exercise_4_14 [topological_space I]
  [linear_order I] (f : I → I) (hf : continuous f) :
  ∃ (x : I), f x = x :=
sorry

theorem exercise_4_15 {f : ℝ → ℝ}
  (hf : continuous f) (hof : is_open_map f) :
  monotone f :=
sorry

theorem exercise_4_19
  {f : ℝ → ℝ} (hf : ∀ a b c, a < b → f a < c → c < f b → ∃ x, a < x ∧ x < b ∧ f x = c)
  (hg : ∀ r : ℚ, is_closed {x | f x = r}) : continuous f :=
sorry

theorem exercise_4_21a {X : Type*} [metric_space X]
  (K F : set X) (hK : is_compact K) (hF : is_closed F) (hKF : disjoint K F) :
  ∃ (δ : ℝ), δ > 0 ∧ ∀ (p q : X), p ∈ K → q ∈ F → dist p q ≥ δ :=
sorry

theorem exercise_4_24 {f : ℝ → ℝ}
  (hf : continuous f) (a b : ℝ) (hab : a < b)
  (h : ∀ x y : ℝ, a < x → x < b → a < y → y < b → f ((x + y) / 2) ≤ (f x + f y) / 2) :
  convex_on ℝ (set.Ioo a b) f :=
sorry

theorem exercise_5_1
  {f : ℝ → ℝ} (hf : ∀ x y : ℝ, | (f x - f y) | ≤ (x - y) ^ 2) :
  ∃ c, f = λ x, c :=
sorry

theorem exercise_5_2 {a b : ℝ}
  {f g : ℝ → ℝ} (hf : ∀ x ∈ set.Ioo a b, deriv f x > 0)
  (hg : g = f⁻¹)
  (hg_diff : differentiable_on ℝ g (set.Ioo a b)) :
  differentiable_on ℝ g (set.Ioo a b) ∧
  ∀ x ∈ set.Ioo a b, deriv g x = 1 / deriv f x :=
sorry

theorem exercise_5_3 {g : ℝ → ℝ} (hg : continuous g)
  (hg' : ∃ M : ℝ, ∀ x : ℝ, | deriv g x | ≤ M) :
  ∃ N, ∀ ε > 0, ε < N → function.injective (λ x : ℝ, x + ε * g x) :=
sorry

theorem exercise_5_4 {n : ℕ}
  (C : ℕ → ℝ)
  (hC : ∑ i in (finset.range (n + 1)), (C i) / (i + 1) = 0) :
  ∃ x, x ∈ (set.Icc (0 : ℝ) 1) ∧ ∑ i in finset.range (n + 1), (C i) * (x^i) = 0 :=
sorry

theorem exercise_5_5
  {f : ℝ → ℝ}
  (hfd : differentiable ℝ f)
  (hf : tendsto (deriv f) at_top (𝓝 0)) :
  tendsto (λ x, f (x + 1) - f x) at_top at_top :=
sorry

theorem exercise_5_6
  {f : ℝ → ℝ}
  (hf1 : continuous f)
  (hf2 : ∀ x, differentiable_at ℝ f x)
  (hf3 : f 0 = 0)
  (hf4 : monotone (deriv f)) :
  monotone_on (λ x, f x / x) (set.Ioi 0) :=
sorry

theorem exercise_5_7
  {f g : ℝ → ℝ} {x : ℝ}
  (hf' : differentiable_at ℝ f 0)
  (hg' : differentiable_at ℝ g 0)
  (hg'_ne_0 : deriv g 0 ≠ 0)
  (f0 : f 0 = 0) (g0 : g 0 = 0) :
  tendsto (λ x, f x / g x) (𝓝 x) (𝓝 (deriv f x / deriv g x)) :=
sorry

theorem exercise_5_15 {f : ℝ → ℝ} (a M0 M1 M2 : ℝ)
  (hf' : differentiable_on ℝ f (set.Ici a))
  (hf'' : differentiable_on ℝ (deriv f) (set.Ici a))
  (hM0 : M0 = Sup {(| f x | )| x ∈ (set.Ici a)})
  (hM1 : M1 = Sup {(| deriv f x | )| x ∈ (set.Ici a)})
  (hM2 : M2 = Sup {(| deriv (deriv f) x | )| x ∈ (set.Ici a)}) :
  (M1 ^ 2) ≤ 4 * M0 * M2 :=
sorry

theorem exercise_5_17
  {f : ℝ → ℝ}
  (hf' : differentiable_on ℝ f (set.Icc (-1) 1))
  (hf'' : differentiable_on ℝ (deriv f) (set.Icc 1 1))
  (hf''' : differentiable_on ℝ (deriv (deriv f)) (set.Icc 1 1))
  (hf0 : f (-1) = 0)
  (hf1 : f 0 = 0)
  (hf2 : f 1 = 1)
  (hf3 : deriv f 0 = 0) :
  ∃ x, x ∈ set.Ioo (-1 : ℝ) 1 ∧ deriv (deriv (deriv f)) x ≥ 3 :=
sorry
