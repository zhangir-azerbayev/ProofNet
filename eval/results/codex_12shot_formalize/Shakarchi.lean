import data.rat.basic
import data.real.basic
import data.real.irrational
import data.real.sqrt
import analysis.inner_product_space.basic
import analysis.inner_product_space.pi_L2
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import analysis.box_integral.basic
import data.set.intervals.basic
import topology.basic
import topology.bases
import topology.metric_space.basic
import topology.instances.real

open complex filter function
open_locale big_operators
open_locale filter
open_locale topological_space


theorem exercise_1_13a {f : ℂ → ℂ} (hf : holomorphic f)
  (h : ∀ z, f z = f z.re) :
  ∀ z, f z = f 0 :=
sorry

theorem exercise_1_13b {f : ℂ → ℂ} {Ω : set ℂ}
  (hf : holomorphic f Ω) (hf_const : ∀ z ∈ Ω, f z.im = 0) :
  ∃ (c : ℂ), ∀ z ∈ Ω, f z = c :=
sorry

theorem exercise_1_13c {f : ℂ → ℂ} {s : set ℂ} (hf : holomorphic f s)
  (h : ∀ z ∈ s, abs (f z) = abs (f 0)) :
  ∀ z ∈ s, f z = f 0 :=
sorry

theorem exercise_1_19a (z : ℂ) (hz : abs z = 1) :
  ¬converges_on ℂ (λ n, n * z ^ n) :=
sorry

theorem exercise_1_19b (z : ℂ) :
  is_cau_seq (λ n, z ^ n / n ^ 2) :=
sorry

theorem exercise_1_19c {z : ℂ} (hz : abs z < 1) (hz1 : z ≠ 1) :
  is_cau_seq (λ n, z ^ n / n) :=
sorry