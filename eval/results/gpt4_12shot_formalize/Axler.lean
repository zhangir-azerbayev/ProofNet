import .common 

open set fintype complex polynomial submodule linear_map finite_dimensional
open module module.End inner_product_space

open_locale big_operators


theorem exercise_1_2 : ((-1 + complex.I * real.sqrt 3) / 2) ^ 3 = 1 :=
sorry

theorem exercise_1_4 {F : Type*} [field F] {V : Type*}
  [add_comm_group V] [vector_space F V] (a : F) (v : V) (h : a • v = 0) :
  a = 0 ∨ v = 0 :=
sorry

theorem exercise_1_7 : ∃ (U : set (euclidean_space ℝ (fin 2))),
  (∀ (c : ℝ) (x : euclidean_space ℝ (fin 2)), x ∈ U → c • x ∈ U) ∧ ¬(submodule.span ℝ U = ⊤) :=
sorry

theorem exercise_1_9 {V : Type*} [add_comm_group V]
  [vector_space ℝ V] (W₁ W₂ : submodule ℝ V) :
  (W₁ : set V) ∪ W₂ ≤ W₁ ∨ W₁ ≤ W₂ ↔ W₁ ⊔ W₂ = max W₁ W₂ :=
sorry

theorem exercise_3_8 {K V W : Type*}
  [division_ring K] [add_comm_group V] [module K V] [add_comm_group W] [module K W]
  [finite_dimensional K V] (T : linear_map K V W) :
  ∃ (U : submodule K V), U ⊓ T.ker = ⊥ ∧ T.range = T.of_submodule_image U :=
sorry

theorem exercise_5_1 {V : Type*} [add_comm_group V] [module ℝ V]
  {T : V →ₗ[ℝ] V} {m : ℕ} (U : ℕ → submodule ℝ V)
  (hU : ∀ i, T.restrict (U i) = (U i).subtype.comp T) :
  T.restrict (U 0 ⊔ ⨆ i in finset.range m, U (i + 1)) =
  (U 0 ⊔ ⨆ i in finset.range m, U (i + 1)).subtype.comp T :=
sorry

theorem exercise_5_11 {K V : Type*} [field K]
  [add_comm_group V] [module K V] [finite_dimensional K V]
  (S T : linear_map K V V) :
  linear_map.eigenvalues S T = linear_map.eigenvalues T S :=
sorry

theorem exercise_5_13 {K V : Type*}
  [field K] [add_comm_group V] [module K V] [finite_dimensional K V]
  (T : linear_map K V V) (hT : ∀ (W : submodule K V),
  finite_dimensional.finrank K W = finite_dimensional.finrank K V - 1 → T.isometry W) :
  ∃ (c : K), T = c • linear_map.id :=
sorry

theorem exercise_5_24 {V : Type*}
  [add_comm_group V] [module ℝ V] [finite_dimensional ℝ V] (T : linear_map ℝ V V)
  (hT : ∀ λ : ℝ, ¬(T - λ • linear_map.id).ker ≠ ⊥) (W : submodule ℝ V)
  (hTW : ∀ (w : V), w ∈ W → T w ∈ W) : even (finite_dimensional.finrank ℝ W) :=
sorry

theorem exercise_6_3 {n : ℕ} (a b : fin n → ℝ) :
  (∑ j in finset.univ, a j * b j) ^ 2 ≤
  (∑ j in finset.univ, (j + 1) * (a j) ^ 2) * (∑ j in finset.univ, (b j) ^ 2 / (j + 1)) :=
sorry

theorem exercise_6_13 {V : Type*} [inner_product_space ℝ V]
  {m : ℕ} {e : fin m → V} (he : orthonormal ℝ e) (v : V) :
  ∥v∥^2 = ∑ i in finset.range m, ∥⟪v, e i⟫∥^2 ↔ v ∈ submodule.span ℝ (set.range e) :=
sorry

theorem exercise_7_5 {K V : Type*} [field K] [add_comm_group V]
  [module K V] [finite_dimensional K V] (h_dim : 2 ≤ finite_dimensional.finrank K V) :
  ¬ is_submodule (λ (T : linear_map K V V), T.is_normal) :=
sorry

theorem exercise_7_9 {𝕜 : Type*} [is_R_or_C 𝕜]
  {E : Type*} [inner_product_space 𝕜 E] [finite_dimensional 𝕜 E] (T : E →ₗ[𝕜] E)
  (hT : T.is_normal) :
  T.is_self_adjoint ↔ ∀ (x : E), x ∈ eigenspace T → is_R_or_C.re (inner x x) = 0 :=
sorry

theorem exercise_7_11 {V : Type*} [inner_product_space ℂ V]
  (T : V →ₗ[ℂ] V) (hT : T.is_normal) :
  ∃ S : V →ₗ[ℂ] V, S * S = T :=
sorry