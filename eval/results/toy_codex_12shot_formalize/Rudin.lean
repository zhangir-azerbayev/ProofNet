import .common
import .common

open real complex
open topological_space
open filter
open_locale real 
open_locale topology
open_locale big_operators
open_locale complex_conjugate
open_locale filter


noncomputable theory





theorem exercise_1_1a {r : ℚ} (hr : r ≠ 0) {x : ℚ} 
  (hx : ¬is_rat x) : ¬is_rat (r + x) :=
sorry

theorem exercise_1_2 :
  ∀ (q : ℚ), q ^ 2 ≠ 12 :=
sorry

theorem exercise_1_5 {α : Type*} [linear_order α] [decidable_linear_order α]
  (s : set α) (hs : s.nonempty) (hsb : s.bdd_below) :
  inf s = -sup (-s) :=
sorry

theorem exercise_1_11a {z : ℂ} :
  ∃ (r : ℝ) (w : ℂ), r ≥ 0 ∧ abs w = 1 ∧ z = r * w :=
sorry

theorem exercise_1_13 (x y : ℂ) :
  abs (abs x - abs y) ≤ abs (x - y) :=
sorry

theorem exercise_1_16a {k : ℕ} (hk : k ≥ 3)
  {x y : ℝ^k} (hxy : ∥x - y∥ = d) (hxd : d > 0) (r : ℝ) (hr : r > 0)
  (h2r : 2 * r > d) :
  ∃ (z : ℝ^k), ∥z - x∥ = r ∧ ∥z - y∥ = r ∧ ∀ (z' : ℝ^k), ∥z' - x∥ = r → ∥z' - y∥ = r → z' = z :=
sorry

theorem exercise_1_18a {k : ℕ} (hk : k ≥ 2) (x : ℝ^k) :
  ∃ (y : ℝ^k), y ≠ 0 ∧ x ⬝ y = 0 :=
sorry

theorem exercise_1_19 {k : ℕ} {a b : euclidean_space ℝ (fin k)} :
  ∃ (c : euclidean_space ℝ (fin k)) (r : ℝ),
  ∀ (x : euclidean_space ℝ (fin k)),
  ∥x - a∥ = 2 * ∥x - b∥ ↔ ∥x - c∥ = r ∧ 3 * c = 4 * b - a ∧ 3 * r = 2 * ∥b - a∥ :=
sorry

theorem exercise_2_24 {X : Type*} [metric_space X]
  (h : ∀ (s : set X), infinite s → ∃ (x : X), x ∈ closure s) :
  separable_space X :=
sorry

theorem exercise_2_27a {E : Type*} [metric_space E]
  [fintype E] (hE : ¬ countable E) (P : set E) (hP : P = condensation_points E) :
  perfect_space P :=
sorry