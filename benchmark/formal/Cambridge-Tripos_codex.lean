theorem exercise_2022.IA.1-II-9D-a
(a : ℕ → ℝ) (h : tendsto a at_top (𝓝 0)) : tendsto (λ n, (sum a (range n)) / n) at_top (𝓝 0) :=
sorry

theorem exercise_2022.IA.1-II-10D-c
{α : Type*} [linear_ordered_field α] {g : ℝ → α} (hg : continuous g) (hb : metric.bounded (set.range g)) (T : ℝ) (hT : 0 < T) : ∃ (x : ℕ → ℝ), tendsto (λ n, g (x n + T) - g (x n)) at_top (𝓝 0) :=
sorry

theorem exercise_2022.IA.4-I-1E-a
: ∃ (n : ℕ), nat.prime (3 * n + 2) :=
sorry

theorem exercise_2022.IA.4-I-2D-a
(a b : ℕ) (h : a ^ 3 = 2 * b ^ 3) : false :=
sorry

theorem exercise_2022.IB.3-II-13G-a-i
{U : Type*} [normed_group U] [normed_space ℂ U] [complete_space U] [open_mapping ℂ U] (hU : nonempty U) (f : ℕ → U → ℂ) (hf : ∀ n : ℕ, holomorphic ℂ U (f n)) (h : ∀ K : set U, is_compact K → uniform_limit (λ n : ℕ, f n) (λ n : ℕ, f n) ( :=
sorry

theorem exercise_2022.IB.3-II-11G-b
{E : Type*} [normed_group E] [normed_space ℝ E] {F : Type*} [normed_group F] [normed_space ℝ F] {f : E → F} {x : E} : is_differentiable_at ℝ f x ↔ ∃ (l : E →L[ℝ] F), f = l.comp x :=
sorry

theorem exercise_2022.IB.1-I-3G-i
{f : ℂ → ℂ} (hf : f = λ z, z / sin z) : removable_singularity_at 0 f :=
sorry

theorem exercise_2022.IB.3-I-1E-ii
{R S : Type*} [ring R] [ring S] (hRS : R ⊆ S) (J : ideal S) : is_ring_hom (quotient_add_group.mk (hRS) (J)) :=
sorry

theorem exercise_2022.IIB.1-II-8F-a-i
{V : Type*} [inner_product_space ℂ V] [finite_dimensional ℂ V] {α : V → V} (hα : is_normal α) : ∃ (v : V), is_eigenvector α v ∧ is_eigenvector α.adjoint v :=
sorry

theorem exercise_2021.IIB.3-II-11F-ii
(X : Type*) [topological_space X] [metric_space X] [metric_space.to_euclidean_space X] (hX : is_open X) : is_connected X ↔ is_path_connected X :=
sorry

theorem exercise_2021.IIB.2-I-1G
{R : Type*} [comm_ring R] [principal_ideal_domain R] {M : Type*} [add_comm_group M] [module R M] {N : submodule R M} : fg R M ↔ fg R N ∧ fg R (quotient_module.quotient N) :=
sorry

theorem exercise_2021.IIB.3-I-1G-i
{G : Type*} [fintype G] [group G] (H : subgroup G) (hH : H ≠ ⊤) (n : ℕ) (hn : nat.prime_factors n = H.card.factors) : ∃ (K : subgroup G), K.normal ∧ K ≠ ⊥ ∧ nat.dvd (fintype.card G / K.card) n :=
sorry

theorem exercise_2021.IIB.1-II-9G-v
(R : Type*) [comm_ring R] (hR : ∀ (f : ℝ → ℝ), continuous f → f ∈ R) : ¬ is_noetherian_ring R :=
sorry

theorem exercise_2018.IA.1-I-3E-b
{f : ℝ → ℝ} (hf : ∀ x y, x ≤ y → f x ≤ f y) (hf' : ∀ x, 0 < f x) : ∀ x₁, ∃ n, ∀ x₂, x₁ ≤ x₂ → x₂ ≤ x₁ + f x₁ + f (x₁ + f x₁) + f (x₁ + f x₁ + f (x₁ + f x₁)) + f (x₁ :=
sorry