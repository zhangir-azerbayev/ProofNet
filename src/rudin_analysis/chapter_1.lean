import tactic
import data.rat.basic
import data.real.basic
import data.real.irrational
import analysis.inner_product_space.basic
import analysis.inner_product_space.pi_L2

open complex
open_locale big_operators
open_locale complex_conjugate

-- exercise exercise_1 is already in mathlib (data.real.irrational.add_rat)
theorem exercise_1a
(x : real) (y : rat) : ( irrational x ) -> irrational ( x + y ) :=
begin
  sorry
end

theorem exercise_1b
(x : real) (y : rat) : ( irrational x ) -> irrational ( x * y ) :=
begin
  sorry
end

theorem exercise_2 :
  not exists (x : rat), ( x ^ 2 = 12 ) :=
begin
  sorry
end

theorem exercise_4
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

theorem exercise_11
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

theorem exercise_12
  (n : ℕ) (f : ℕ → ℂ)
  : abs (∑ i in finset.range n, f i) ≤ ∑ i in finset.range n, abs (f i) :=
begin
  sorry,
end

theorem exercise_13
  (x y : ℂ)
  : |(abs x) - (abs y)| ≤ abs (x - y) :=
begin
  sorry,
end

theorem exercise_14
  (z : ℂ) (h : abs z = 1)
  : (abs (1 + z)) ^ 2 + (abs (1 - z)) ^ 2 = 4 :=
begin
  sorry,
end

theorem exercise_16_a
  (n : ℕ)
  (d r : ℝ)
  (x y z : euclidean_space ℝ (fin n)) -- R^n
  (h₁ : n ≥ 3)
  (h₂ : ∥x - y∥ = d)
  (h₃ : d > 0)
  (h₄ : r > 0)
  (h₅ : 2 * r > d)
  : set.infinite {z : euclidean_space ℝ (fin n) | ∥z - x∥ = r ∧ ∥z - y∥ = r} :=
begin
  sorry,
end

theorem exercise_17
  (n : ℕ)
  (x y : euclidean_space ℝ (fin n)) -- R^n
  : ∥x + y∥^2 + ∥x - y∥^2 = 2*∥x∥^2 + 2*∥y∥^2 :=
begin
  sorry,
end

theorem exercise_18_a
  (n : ℕ)
  (h : n > 1)
  (x : euclidean_space ℝ (fin n)) -- R^n
  : ∃ (y : euclidean_space ℝ (fin n)), y ≠ 0 ∧ (inner x y) = (0 : ℝ) :=
begin
  sorry,
end

theorem exercise_18_b
  : ¬ ∀ (x : ℝ), ∃ (y : ℝ), y ≠ 0 ∧ x * y = 0 :=
begin
  simp,
  use 1,
  intros x h₁ h₂,
  cases h₂,
  {norm_num at h₂},
  {exact absurd h₂ h₁},
end

theorem exercise_19
  (n : ℕ)
  (a b c x : euclidean_space ℝ (fin n))
  (r : ℝ)
  (h₁ : r > 0)
  (h₂ : 3 • c = 4 • b - a)
  (h₃ : 3 * r = 2 * ∥x - b∥)
  : ∥x - a∥ = 2 * ∥x - b∥ ↔ ∥x - c∥ = r :=
begin
  sorry,
end
