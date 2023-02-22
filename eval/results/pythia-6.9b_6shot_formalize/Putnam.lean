import .common 

open real topological_space filter polynomial
open_locale topology big_operators complex_conjugate filter ennreal 





theorem exercise_1998_b6  {a b c : ℕ} (n : ℕ) :
  ∃ n : ℕ, ∃ (a₁ : ℕ), ∃ (b₁ : ℕ), ∃ (c₁ : ℕ), ∃ (n₁ : ℕ),
    ∀ (i : ℕ), n.sqrt i ≠ n₁ →
    ∀ (j : ℕ), n.sqrt (n.sqrt i + a₁ n.sqrt j + b₁ n.sqrt (n.sqrt i + c₁ n.sqrt j)) ≠ n₁ :=
sorry

theorem exercise_1999_b4  {X : Type*} [metric_space X] [metric_space X] [metric_space X]
  (f : X → X) (g : X → X) (h : continuous g) (hgc : continuous g)
  (hgi : function.injective g)
  (h : positive_function.positive_second_derivative g) :
  positive_function.second_derivative g ≤ 2.second_derivative f :=
sorry

theorem exercise_2001_a5 :=
sorry

theorem exercise_2014_a5 {P Q : Type*} [property P] [property Q] [property P]
  [property Q] (h : P) (f : P → Q) (g : Q → P) (hgc : continuous g)
  (hgi : function.injective g) (h : continuous_function.injective f) :
  continuous_function.injective f :=
sorry

theorem exercise_2018_a5  {R : Type*} [real_field R] [real_field R] [real_field R]
  (f : R → R) (g : R → R) (h : g.is_positive) (h0 : f.is_zero) (h1 : f.is_one) :
  g.derivative f = -f' :=
sorry

theorem exercise_2018_b4 {a : ℝ} (n : ℕ) :
  ∃ (x : ℝ), x = 0 ∧ x = x.n :=
sorry