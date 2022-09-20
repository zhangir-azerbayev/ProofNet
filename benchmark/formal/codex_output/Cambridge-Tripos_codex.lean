theorem exercise_2022_IA_1-II-9D-a (a : ℕ → ℝ) (h : tendsto a at_top (𝓝 0)) :
  tendsto (λ n, (sum a (range n)) / n) at_top (𝓝 0) :=
sorry

theorem exercise_2022_IA_1-II-10D-c {α : Type*} [linear_ordered_field α] 
    {f : ℝ → α} (hf : continuous f) (hb : metric.bounded (set.range f)) 
    (T : ℝ) (hT : 0 < T) :
    ∃ (x : ℕ → ℝ), tendsto (λ n, f (x n + T) - f (x n)) at_top (𝓝 0) :=
sorry

theorem exercise_2022_IA_4-I-1E-a :
  ∃ (n : ℕ), nat.prime (3 * n + 2) :=
sorry

theorem exercise_2022_IA_4-I-2D-a (a b : ℕ) (h : a ^ 3 = 2 * b ^ 3) : false :=
sorry

theorem exercise_2022_IB_3-II-13G-a-i {U : Type*} 
    [normed_group U] [normed_space ℂ U] [complete_space U] 
    [open_mapping ℂ U] (hU : nonempty U) (f : ℕ → U → ℂ) 
    (hf : ∀ n : ℕ, holomorphic ℂ U (f n)) (h : ∀ K : set U, is_compact K → 
    uniform_limit ℂ (λ n, f n) (λ n, f n) K) :
    hol :=
sorry

theorem exercise_2022_IB_3-II-11G-b {X : Type*} [topological_space X]
    [metric_space X] {f : X → X} (hf : continuous f) (hc : compact_space X) :
    ∃ x, f x = x :=
sorry

theorem exercise_2022_IB_1-I-3G-i {f : ℂ → ℂ} 
    (hf : f = λ z, z / sin z) :
    removable_singularity_at 0 f :=
sorry

theorem exercise_2022_IB_3-I-1E-ii {R S : Type*} [ring R] [ring S] 
    (hRS : R ⊆ S) (J : ideal S) :
    is_ring_hom (quotient_add_group.mk (hRS) (J)) :=
sorry

theorem exercise_2022_IIB_1-II-8F-a-i {V : Type*} [inner_product_space ℂ V]
    [finite_dimensional ℂ V] {α : V → V} (hα : is_normal α) :
    ∃ (v : V), is_eigenvector α v ∧ is_eigenvector α.adjoint v :=
sorry

theorem exercise_2021_IIB_3-II-11F-ii (X : Type*) [topological_space X] 
    [metric_space X] [metric_space.euclidean_space X] (hX : is_open X) :
    is_connected X ↔ is_path_connected X :=
sorry

theorem exercise_2021_IIB_2-I-1G {R : Type*} [comm_ring R] [principal_ideal_domain R]
    {M : Type*} [add_comm_group M] [module R M] {N : submodule R M} :
    fg R M ↔ fg R N ∧ fg R (quotient_module.quotient N) :=
sorry

theorem exercise_2021_IIB_3-I-1G-i {G : Type*} [fintype G] [group G]
    (H : subgroup G) (hH : H ≠ ⊤) (hHG : H < G) (n : ℕ) (hn : nat.prime_factors n = 
    finset.univ.filter (λ p, p ∣ nat.card G)) :
    ∃ (K : subgroup G), K < G ∧ K ≠ ⊤ ∧ K.normal ∧ nat.card (G / K) ∣ n! :=
sorry

theorem exercise_2021_IIB_1-II-9G-v (R : Type*) [comm_ring R] 
    (hR : ∀ (f : ℝ → ℝ), continuous f → f ∈ R) :
    ¬ is_noetherian_ring R :=
sorry

theorem exercise_2018_IA_1-I-3E-b {f : ℝ → ℝ} (hf : ∀ x y, x ≤ y → f x ≤ f y) 
    (hf' : ∀ x, 0 < f x) :
    ∀ x₁, ∃ n, ∀ x₂, x₁ ≤ x₂ → x₂ ≤ x₁ + f x₁ + f (x₁ + f x₁) + f (x₁ + f x₁ + f (x₁ + f x₁)) + 
    f (x₁ :=
sorry