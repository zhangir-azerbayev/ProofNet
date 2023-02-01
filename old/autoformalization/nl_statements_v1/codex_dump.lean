import algebra.group.basic
import group_theory.order_of_element
import group_theory.subgroup.basic
import data.fintype.basic
import data.real.basic 
import data.complex.basic
import topology.basic
import topology.constructions
import topology.bases
import topology.metric_space.basic
import data.real.basic
import data.set.countable
import tactic
import data.rat.basic
import data.real.basic
import data.real.irrational
import analysis.inner_product_space.basic
import analysis.inner_product_space.pi_L2
import tactic
import data.real.sqrt
import analysis.specific_limits.basic
import analysis.specific_limits.normed
import data.finset.lattice 

open filter real

open_locale big_operators 
open_locale topological_space

noncomputable theory 
/- 
Copied preamble from working_preamble.lean 
The rest comes from codex_autoformalize.py
I also changed the theorem name of Munkres_2_22_2a to avoid 
a naming conflict with a different theorem
-/
/-
df_1_1_15
Prove that $(a_1a_2\dots a_n)^{-1} = a_n^{-1}a_{n-1}^{-1}\dots a_1^{-1}$ for all $a_1, a_2, \dots, a_n\in G$.
-/
theorem inverse_of_product_is_product_of_inverses
  (G : Type*) [group G]
  (a : ℕ → G)
  (n : ℕ)
  : (∏ i in finset.range n, a i)⁻¹ = ∏ i in finset.range n, (a i)⁻¹
:= sorry

/-
df_1_1_16
Let $x$ be an element of $G$. Prove that $x^2=1$ if and only if $|x|$ is either $1$ or $2$.
-/
theorem order_of_squared_element_is_one_or_two
  (G : Type*) [group G]
  (x : G)
  : x ^ 2 = 1 ↔ order x = 1 ∨ order x = 2
:= sorry

/-
df_1_1_17
Let $x$ be an element of $G$. Prove that if $|x|=n$ for some positive integer $n$ then $x^{-1}=x^{n-1}$.
-/
theorem inverse_of_element_of_finite_order
  (G : Type*) [group G]
  (x : G)
  (hx : x ≠ 1)
  (hx_fin : ∃ n : ℕ, x ^ n = 1)
  : x⁻¹ = x ^ (nat.find hx_fin - 1)
:= sorry

/-
df_1_1_18
Let $x$ and $y$ be elements of $G$. Prove that $xy=yx$ if and only if $y^{-1}xy=x$ if and only if $x^{-1}y^{-1}xy=1$.
-/
theorem commutativity_iff_conjugation_is_id
  (G : Type*) [group G]
  (x y : G)
  : x * y = y * x ↔ y⁻¹ * x * y = x
:= sorry

/-
df_1_1_20
For $x$ an element in $G$ show that $x$ and $x^{-1}$ have the same order.
-/
theorem order_of_inverse_eq_order
  (G : Type*) [group G]
  (x : G)
  (hx : x ≠ 1)
  (hx_inf : ∀ n : ℕ, x ^ n ≠ 1)
  : order x = order (x⁻¹)
:= sorry

/-
df_1_1_22a
If $x$ and $g$ are elements of the group $G$, prove that $|x|=\left|g^{-1} x g\right|$.
-/
theorem order_conjugate_eq
  (G : Type*) [group G]
  (x g : G)
  : order x = order (g⁻¹ * x * g)
:= sorry

/-
df_1_1_22b
Deduce that $|a b|=|b a|$ for all $a, b \in G$.
-/
theorem norm_mul_comm
  (G : Type*) [normed_group G]
  (a b : G)
  : ∥a * b∥ = ∥b * a∥
:= sorry

/-
df_1_1_25
Prove that if $x^{2}=1$ for all $x \in G$ then $G$ is abelian.
-/
theorem group_with_all_squares_equal_to_one_is_abelian
  (G : Type*) [group G]
  (hG : ∀ x : G, x^2 = 1)
  : ∀ x y : G, x * y = y * x
:= sorry

/-
df_1_1_29
Prove that $A \times B$ is an abelian group if and only if both $A$ and $B$ are abelian.
-/
theorem prod_is_abelian_iff_factors_are_abelian
  (A B : Type*) [group A] [group B]
  : abelian (A × B) ↔ abelian A ∧ abelian B
:= sorry

/-
df_1_1_34
If $x$ is an element of infinite order in $G$, prove that the elements $x^{n}, n \in \mathbb{Z}$ are all distinct.
-/
theorem distinct_powers_of_infinite_order_element
  (G : Type*) [group G]
  (x : G)
  (hx : x ≠ 1)
  (hx_inf : ∀ n : ℕ, x ^ n ≠ 1)
  : ∀ m n : ℤ, m ≠ n → x ^ m ≠ x ^ n
:= sorry

/-
df_1_3_8
Prove that if $\Omega=\{1,2,3, \ldots\}$ then $S_{\Omega}$ is an infinite group
-/
theorem symmetric_group_of_natural_numbers_is_infinite
  : infinite (symmetric_group ℕ)
:= sorry

/-
df_1_6_4
Prove that the multiplicative groups $\mathbb{R}-\{0\}$ and $\mathbb{C}-\{0\}$ are not isomorphic.
-/
theorem not_isomorphic_real_and_complex_multiplicative_groups
  : ¬ (multiplicative ℝ ≃* multiplicative ℂ)
:= sorry

/-
df_1_6_11
Let $A$ and $B$ be groups. Prove that $A \times B \cong B \times A$.
-/
theorem prod_comm
  (A B : Type*) [group A] [group B]
  : A × B ≃ B × A
:= sorry

/-
df_1_6_17
Let $G$ be any group. Prove that the map from $G$ to itself defined by $g \mapsto g^{-1}$ is a homomorphism if and only if $G$ is abelian.
-/
theorem inv_is_homomorphism_iff_abelian
  (G : Type*) [group G]
  : function.is_group_hom (λ g : G, g⁻¹) ↔ abelian G
:= sorry

/-
df_1_6_23
Let $G$ be a finite group which possesses an automorphism $\sigma$ such that $\sigma(g)=g$ if and only if $g=1$. If $\sigma^{2}$ is the identity map from $G$ to $G$, prove that $G$ is abelian.
-/
theorem finite_group_with_automorphism_is_abelian
  (G : Type*) [group G] [fintype G]
  (σ : G → G)
  (hσ : ∀ g : G, σ g = g ↔ g = 1)
  (hσ2 : ∀ g : G, σ (σ g) = g)
  : abelian G
:= sorry

/-
df_1_7_5
Prove that the kernel of an action of the group $G$ on a set $A$ is the same as the kernel of the corresponding permutation representation $G\to S_A$.
-/
theorem kernel_of_action_is_kernel_of_permutation_representation
  (G : Type*) [group G]
  (A : Type*) [fintype A]
  (f : G → perm A)
  : set.kernel f = set.kernel (λ g, f g 1)
:= sorry

/-
df_1_7_6
Prove that a group $G$ acts faithfully on a set $A$ if and only if the kernel of the action is the set consisting only of the identity.
-/
theorem faithful_action_iff_kernel_is_trivial
  (G : Type*) [group G]
  (A : Type*) [fintype A]
  (f : G → perm A)
  : faithful f ↔ ∀ g : G, g ≠ 1 → f g ≠ 1
:= sorry

/-
df_2_1_5
Prove that $G$ cannot have a subgroup $H$ with $|H|=n-1$, where $n=|G|>2$.
-/
theorem no_subgroup_of_order_n_minus_1
  (G : Type*) [group G]
  (n : ℕ)
  (hn : n > 2)
  (hG : card G = n)
  (H : set G)
  (hH : is_subgroup H)
  (hH_card : card H = n - 1)
  : false
:= sorry

/-
df_2_1_13
Let $H$ be a subgroup of the additive group of rational numbers with the property that $1 / x \in H$ for every nonzero element $x$ of $H$. Prove that $H=0$ or $\mathbb{Q}$.
-/
theorem subgroup_of_rationals_with_inverse_property_is_trivial_or_all_of_rationals
  (H : set ℚ) [is_subgroup H]
  (hH : ∀ x ∈ H, x ≠ 0 → 1 / x ∈ H)
  : H = {0} ∨ H = set.univ
:= sorry

/-
df_2_2_4
Prove that if $H$ is a subgroup of $G$ then $H$ is generated by the set $H-\{1\}$.
-/
theorem subgroup_generated_by_subgroup_minus_one
  (G : Type*) [group G]
  (H : set G) [is_subgroup H]
  : is_subgroup (subgroup.generated (H \ {1}))
:= sorry

/-
df_2_2_13
Prove that the multiplicative group of positive rational numbers is generated by the set $\left\{\frac{1}{p} \mid \text{$p$ is a prime} \right\}$.
-/
theorem multiplicative_group_of_positive_rationals_generated_by_inverses_of_primes
  : multiplicative (ℚ_pos) = subgroup.generated (ℚ_pos) (λ p : ℕ, p.prime → ℚ_pos) (λ p hp, 1 / p)
:= sorry

/-
df_2_2_16a
A subgroup $M$ of a group $G$ is called a maximal subgroup if $M \neq G$ and the only subgroups of $G$ which contain $M$ are $M$ and $G$. Prove that if $H$ is a proper subgroup of the finite group $G$ then there is a maximal subgroup of $G$ containing $H$.
-/
theorem maximal_subgroup_containing_proper_subgroup
  (G : Type*) [group G] [fintype G]
  (H : set G)
  (hH : H ≠ G)
  (hH_subgroup : is_subgroup H)
  : ∃ M : set G, is_subgroup M ∧ M ≠ G ∧ ∀ N : set G, is_subgroup N → H ⊆ N → N = G ∨ N = M
:= sorry

/-
df_2_2_16c
Show that if $G=\langle x\rangle$ is a cyclic group of order $n \geq 1$ then a subgroup $H$ is maximal if and only $H=\left\langle x^{p}\right\rangle$ for some prime $p$ dividing $n$.
-/
theorem maximal_subgroup_of_cyclic_group_is_prime_power
  (G : Type*) [group G]
  (x : G)
  (hx : x ≠ 1)
  (hx_inf : ∀ n : ℕ, x ^ n ≠ 1)
  (H : subgroup G)
  (hH : is_maximal H)
  : ∃ p : ℕ, prime p ∧ ∃ n : ℕ, H = ⟨x ^ p ^ n⟩
:= sorry

/-
df_3_1_3a
Let $A$ be an abelian group and let $B$ be a subgroup of $A$. Prove that $A / B$ is abelian.
-/
theorem quotient_group_of_abelian_group_is_abelian
  (A : Type*) [group A] [abelian A]
  (B : set A) [is_subgroup B]
  : abelian (quotient_group.quotient B)
:= sorry

/-
df_3_1_22a
Prove that if $H$ and $K$ are normal subgroups of a group $G$ then their intersection $H \cap K$ is also a normal subgroup of $G$.
-/
theorem normal_intersection_is_normal
  (G : Type*) [group G]
  (H K : set G)
  (hH : is_normal_subgroup H)
  (hK : is_normal_subgroup K)
  : is_normal_subgroup (H ∩ K)
:= sorry

/-
df_3_1_22b
Prove that the intersection of an arbitrary nonempty collection of normal subgroups of a group is a normal subgroup (do not assume the collection is countable).
-/
theorem intersection_of_normal_subgroups_is_normal
  (G : Type*) [group G]
  (S : set (set G))
  (hS : ∀ s ∈ S, is_normal_subgroup s)
  (hS_nonempty : S.nonempty)
  : is_normal_subgroup (⋂ s ∈ S, s)
:= sorry

/-
df_3_2_8
Prove that if $H$ and $K$ are finite subgroups of $G$ whose orders are relatively prime then $H \cap K=1$.
-/
theorem finite_subgroups_of_relatively_prime_order_have_trivial_intersection
  (G : Type*) [group G]
  (H K : set G)
  (hH : is_subgroup H)
  (hK : is_subgroup K)
  (hH_finite : fintype H)
  (hK_finite : fintype K)
  (hH_K_rel_prime : nat.coprime (fintype.card H) (fintype.card K))
  : H ∩ K = {1}
:= sorry

/-
df_3_2_11
Let $H \leq K \leq G$. Prove that $|G: H|=|G: K| \cdot|K: H|$ (do not assume $G$ is finite).
-/
theorem index_of_subgroup_of_subgroup_eq_index_of_subgroup_times_index_of_subgroup
  (G : Type*) [group G]
  (H K : set G)
  (hH : is_subgroup H)
  (hK : is_subgroup K)
  (hKH : H ≤ K)
  : fintype.card (left_cosets K H) * fintype.card (left_cosets G K) = fintype.card (left_cosets G H)
:= sorry

/-
df_3_2_16
Use Lagrange's Theorem in the multiplicative group $(\mathbb{Z} / p \mathbb{Z})^{\times}$to prove Fermat's Little Theorem: if $p$ is a prime then $a^{p} \equiv a(\bmod p)$ for all $a \in \mathbb{Z}$.
-/
theorem fermat_little_theorem
  (p : ℕ) (hp : prime p)
  (a : ℕ)
  : a ^ p ≡ a [MOD p]
:= sorry

/-
df_3_2_21a
Prove that $\mathbb{Q}$ has no proper subgroups of finite index.
-/
theorem no_proper_subgroups_of_finite_index_in_Q
  : ∀ (G : Type*) [group G] [fintype G] [decidable_eq G] (H : set G) [is_subgroup H] (hH : H ≠ G), fintype.card H = 0
:= sorry

/-
df_3_3_3
Prove that if $H$ is a normal subgroup of $G$ of prime index $p$ then for all $K \leq G$ either $K \leq H$ or $G=H K$ and $|K: K \cap H|=p$.
-/
theorem normal_subgroup_of_prime_index_has_complement
  (G : Type*) [group G]
  (H : set G)
  (hH : is_subgroup H)
  (hH_normal : is_normal_subgroup H)
  (p : ℕ)
  (hp : prime p)
  (hH_index : fintype.card (quotient_group.quotient H) = p)
  (K : set G)
  (hK : is_subgroup K)
  : K ≤ H ∨ G = H * K ∧ fintype.card (quotient_group.quotient (K ∩ H)) = p
:= sorry

/-
Rudin_1_1
If $r$ is rational $(r \neq 0)$ and $x$ is irrational, prove that $r+x$ and $r x$ are irrational.
-/
theorem sum_and_product_of_irrational_and_nonzero_rational_is_irrational
  (r : ℚ) (x : ℚ)
  (hr : r ≠ 0)
  (hx : ¬ is_rational x)
  : ¬ is_rational (r + x) ∧ ¬ is_rational (r * x)
:= sorry

/-
Rudin_1_2
Prove that there is no rational number whose square is $12$.
-/
theorem no_rational_square_eq_12
  : ¬ ∃ (x : ℚ), x^2 = 12
:= sorry

/-
Rudin_1_5
Let $A$ be a nonempty set of real numbers which is bounded below. Let $-A$ be the set of all numbers $-x$, where $x \in A$. Prove that $\inf A=-\sup (-A)$
-/
theorem inf_neg_sup_neg
  (A : set ℝ)
  (hA : A ≠ ∅)
  (hA_bdd_below : bdd_below A)
  : inf A = -sup (-A)
:= sorry

/-
Rudin_1_14
If $z$ is a complex number such that $|z|=1$, that is, such that $z \bar{z}=1$, compute $|1+z|^{2}+|1-z|^{2}$.
-/
theorem sum_of_squares_of_sum_and_difference
  (z : ℂ)
  (h : abs z = 1)
  : ∥1 + z∥^2 + ∥1 - z∥^2 = 4
:= sorry

/-
Rudin_1_18a
If $k \geq 2$ and $\mathbf{x} \in R^{k}$, prove that there exists $\mathbf{y} \in R^{k}$ such that $\mathbf{y} \neq 0$ but $\mathbf{x} \cdot \mathbf{y}=0$
-/
theorem exists_nonzero_orthogonal_vector
  (k : ℕ) (hk : k ≥ 2)
  (x : vector ℝ k)
  : ∃ y : vector ℝ k, y ≠ 0 ∧ x ⬝ y = 0
:= sorry

/-
Rudin_3_1a
Prove that convergence of $\left\{s_{n}\right\}$ implies convergence of $\left\{\left|s_{n}\right|\right\}$.
-/
theorem abs_converges_of_converges
  {α : Type*} [linear_ordered_field α]
  {s : ℕ → α}
  (hs : tendsto s at_top (𝓝 0))
  : tendsto (λ n, abs (s n)) at_top (𝓝 0)
:= sorry

/-
Rudin_3_3
If $s_{1}=\sqrt{2}$, and $s_{n+1}=\sqrt{2+\sqrt{s_{n}}} \quad(n=1,2,3, \ldots),$ prove that $\left\{s_{n}\right\}$ converges, and that $s_{n}<2$ for $n=1,2,3, \ldots$.
-/
--failed to generate statement
:= sorry

/-
Rudin_3_5
For any two real sequences $\left\{a_{n}\right\},\left\{b_{n}\right\}$, prove that $\limsup _{n \rightarrow \infty}\left(a_{n}+b_{n}\right) \leq \limsup _{n \rightarrow \infty} a_{n}+\limsup _{n \rightarrow \infty} b_{n},$ provided the sum on the right is not of the form $\infty-\infty$.
-/
theorem limsup_add_leq_limsup_add_limsup
  (a b : ℕ → ℝ)
  (h : limsup a + limsup b ≠ ∞)
  : limsup (λ n, a n + b n) ≤ limsup a + limsup b
:= sorry

/-
Rudin_3_7
Prove that the convergence of $\Sigma a_{n}$ implies the convergence of $\sum \frac{\sqrt{a_{n}}}{n}$ if $a_n\geq 0$.
-/
theorem sum_sqrt_div_n_converges_of_sum_converges
  (a : ℕ → ℝ)
  (h_nonneg : ∀ n, 0 ≤ a n)
  (h_converges : is_convergent (λ n, ∑ i in finset.range n, a i))
  : is_convergent (λ n, ∑ i in finset.range n, √(a i) / (i + 1))
:= sorry

/-
Rudin_3_8
If $\Sigma a_{n}$ converges, and if $\left\{b_{n}\right\}$ is monotonic and bounded, prove that $\Sigma a_{n} b_{n}$ converges.
-/
theorem sum_of_convergent_series_times_monotonic_bounded_sequence_converges
  (a : ℕ → ℝ) (b : ℕ → ℝ)
  (h_a : is_convergent_series a)
  (h_b : is_monotonic_sequence b)
  (h_b_bdd : is_bounded_sequence b)
  : is_convergent_series (λ n, a n * b n)
:= sorry

/-
Rudin_3_13
Prove that the Cauchy product of two absolutely convergent series converges absolutely.
-/
theorem cauchy_product_of_absolutely_convergent_series_converges_absolutely
  (X : Type*) [normed_group X] [normed_space ℝ X]
  (f g : ℕ → X)
  (hf : series_converges_absolutely f)
  (hg : series_converges_absolutely g)
  : series_converges_absolutely (λ n, ∑ i in finset.range n, f i * g (n - i))
:= sorry

/-
Rudin_3_20
20. Suppose $\left\{p_{n}\right\}$ is a Cauchy sequence in a metric space $X$, and some subsequence $\left\{p_{n l}\right\}$ converges to a point $p \in X$. Prove that the full sequence $\left\{p_{n}\right\}$ converges to $p$.
-/
theorem cauchy_subsequence_converges_to_p_implies_cauchy_converges_to_p
  (X : Type*) [metric_space X]
  (p : ℕ → X)
  (hp : cauchy_seq p)
  (pl : ℕ → ℕ)
  (hpl : ∀ n : ℕ, pl n > n)
  (hpl_converges : tendsto (p ∘ pl) at_top (𝓝 (p 0)))
  : tendsto p at_top (𝓝 (p 0))
:= sorry

/-
Rudin_3_21
If $\left\{E_{n}\right\}$ is a sequence of closed nonempty and bounded sets in a complete metric space $X$, if $E_{n} \supset E_{n+1}$, and if $\lim _{n \rightarrow \infty} \operatorname{diam} E_{n}=0,$ then $\bigcap_{1}^{\infty} E_{n}$ consists of exactly one point.
-/
theorem intersection_of_closed_bounded_nonempty_sets_is_singleton
  (X : Type*) [metric_space X] [complete_space X]
  (E : ℕ → set X)
  (hE : ∀ n, is_closed (E n))
  (hE_nonempty : ∀ n, E n ≠ ∅)
  (hE_bounded : ∀ n, is_bounded (E n))
  (hE_mono : ∀ n, E n ⊆ E (n+1))
  (hE_diam_tendsto_zero : tendsto (λ n, diam (E n)) at_top (𝓝 0))
  : ∃ x, ∀ n, x ∈ E n
:= sorry

/-
Rudin_3_22
Suppose $X$ is a nonempty complete metric space, and $\left\{G_{n}\right\}$ is a sequence of dense open subsets of $X$. Prove Baire's theorem, namely, that $\bigcap_{1}^{\infty} G_{n}$ is not empty. Hint: Find a shrinking sequence of neighborhoods $E_{n}$ such that $E_{n} \subset G_{n}$.
-/
theorem baire_theorem
  (X : Type*) [metric_space X] [complete_space X]
  (G : ℕ → set X)
  (hG : ∀ n : ℕ, is_open (G n) ∧ dense (G n))
  : ∃ x : X, ∀ n : ℕ, x ∈ G n
:= sorry

/-
Munkres_2_13_5a
Show that if $\mathcal{A}$ is a basis for a topology on $X$, then the topology generated by $\mathcal{A}$ equals the intersection of all topologies on $X$ that contain $\mathcal{A}$.
-/
theorem basis_topology_eq_intersection_of_topologies_containing_basis
  (X : Type*)
  (A : set (set X))
  (hA : is_topological_basis A)
  : topological_space.generate_from A =
    ⋂₀ {T : set (set X) | is_topology T ∧ A ⊆ T}
:= sorry

/-
Munkres_2_16_4
A map $f: X \rightarrow Y$ is said to be an open map if for every open set $U$ of $X$, the set $f(U)$ is open in $Y$. Show that $\pi_{1}: X \times Y \rightarrow X$ and $\pi_{2}: X \times Y \rightarrow Y$ are open maps.
-/
theorem open_map_of_product_projections
  (X Y : Type*) [topological_space X] [topological_space Y]
  : is_open_map (λ (p : X × Y), p.1) ∧ is_open_map (λ (p : X × Y), p.2)
:= sorry

/-
Munkres_2_17_2
Show that if $A$ is closed in $Y$ and $Y$ is closed in $X$, then $A$ is closed in $X$.
-/
theorem closed_subset_of_closed_subset_is_closed
  (X Y : Type*) [topological_space X] [topological_space Y]
  (A : set Y)
  (hA : is_closed A)
  (hY : is_closed Y)
  : is_closed A
:= sorry

/-
Munkres_2_17_3
Show that if $A$ is closed in $X$ and $B$ is closed in $Y$, then $A \times B$ is closed in $X \times Y$.
-/
theorem product_of_closed_sets_is_closed
  (X Y : Type*) [topological_space X] [topological_space Y]
  (A : set X) (B : set Y)
  (hA : is_closed A) (hB : is_closed B)
  : is_closed (A × B)
:= sorry

/-
Munkres_2_17_4
Show that if $U$ is open in $X$ and $A$ is closed in $X$, then $U-A$ is open in $X$, and $A-U$ is closed in $X$.
-/
theorem open_minus_closed_is_open
  (X : Type*) [topological_space X]
  (U A : set X)
  (hU : is_open U)
  (hA : is_closed A)
  : is_open (U \ A)
:= sorry

/-
Munkres_2_18_8a
Let $Y$ be an ordered set in the order topology. Let $f, g: X \rightarrow Y$ be continuous. Show that the set $\{x \mid f(x) \leq g(x)\}$ is closed in $X$.
-/
theorem set_of_points_where_f_leq_g_is_closed
  (X : Type*) [topological_space X]
  (Y : Type*) [topological_space Y] [ordered_topology Y]
  (f g : X → Y)
  (hf : continuous f)
  (hg : continuous g)
  : is_closed {x | f x ≤ g x}
:= sorry

/-
Munkres_2_18_8b
Let $Y$ be an ordered set in the order topology. Let $f, g: X \rightarrow Y$ be continuous. Let $h: X \rightarrow Y$ be the function $h(x)=\min \{f(x), g(x)\}.$ Show that $h$ is continuous. [Hint: Use the pasting lemma.]
-/
theorem min_of_continuous_functions_is_continuous
  (X Y : Type*) [topological_space X] [topological_space Y] [ordered_topology Y]
  (f g : X → Y)
  (hf : continuous f)
  (hg : continuous g)
  : continuous (λ x, min (f x) (g x))
:= sorry

/-
Munkres_2_18_13
Let $A \subset X$; let $f: A \rightarrow Y$ be continuous; let $Y$ be Hausdorff. Show that if $f$ may be extended to a continuous function $g: \bar{A} \rightarrow Y$, then $g$ is uniquely determined by $f$.
-/
theorem unique_continuous_extension_of_continuous_function
  (X : Type*) [topological_space X]
  (Y : Type*) [topological_space Y] [t2_space Y]
  (A : set X)
  (f : A → Y)
  (hf : continuous f)
  (g : closure A → Y)
  (hg : continuous g)
  (hgf : ∀ x ∈ A, g x = f x)
  : ∀ x ∈ closure A, g x = f x
:= sorry

/-
Munkres_2_21_6a
Define $f_{n}:[0,1] \rightarrow \mathbb{R}$ by the equation $f_{n}(x)=x^{n}$. Show that the sequence $\left(f_{n}(x)\right)$ converges for each $x \in[0,1]$.
-/
theorem sequence_of_powers_converges
  (x : ℝ)
  (hx : 0 ≤ x ∧ x ≤ 1)
  : ∀ n : ℕ, tendsto (λ n, x ^ n) at_top (𝓝 x)
:= sorry

/-
Munkres_2_21_6b
Define $f_{n}:[0,1] \rightarrow \mathbb{R}$ by the equation $f_{n}(x)=x^{n}$. Show that the sequence $\left(f_{n}\right)$ does not converge uniformly.
-/
theorem fn_does_not_converge_uniformly
  (n : ℕ)
  (x : ℝ)
  (hx : 0 ≤ x ∧ x ≤ 1)
  : ∀ ε > 0, ∃ N : ℕ, ∀ m ≥ N, abs (x ^ m - x ^ n) ≥ ε
:= sorry

/-
Munkres_2_21_8
Let $X$ be a topological space and let $Y$ be a metric space. Let $f_{n}: X \rightarrow Y$ be a sequence of continuous functions. Let $x_{n}$ be a sequence of points of $X$ converging to $x$. Show that if the sequence $\left(f_{n}\right)$ converges uniformly to $f$, then $\left(f_{n}\left(x_{n}\right)\right)$ converges to $f(x)$.
-/
theorem uniform_convergence_of_continuous_functions_implies_pointwise_convergence
  (X : Type*) [topological_space X]
  (Y : Type*) [metric_space Y]
  (f : ℕ → X → Y)
  (x : ℕ → X)
  (hx : tendsto x at_top (𝓝 (x 0)))
  (hf : uniform_limit f (𝓝 (f 0)) at_top)
  : tendsto (λ n, f n (x n)) at_top (𝓝 (f 0 (x 0)))
:= sorry

/-
Munkres_2_22_1
Let $p: X \rightarrow Y$ be a continuous map. Show that if there is a continuous map $f: Y \rightarrow X$ such that $p \circ f$ equals the identity map of $Y$, then $p$ is a quotient map.
-/
theorem continuous_map_with_continuous_inverse_is_quotient_map
  (X Y : Type*) [topological_space X] [topological_space Y]
  (p : X → Y)
  (f : Y → X)
  (hf : continuous f)
  (hpf : ∀ y : Y, p (f y) = y)
  : quotient_map p
:= sorry

/-
Munkres_2_22_2a
Let $p: X \rightarrow Y$ be a continuous map. Show that if there is a continuous map $f: Y \rightarrow X$ such that $p \circ f$ equals the identity map of $Y$, then $p$ is a quotient map.
-/
theorem continuous_map_with_continuous_inverse_is_quotient_map1
  (X Y : Type*) [topological_space X] [topological_space Y]
  (p : X → Y)
  (f : Y → X)
  (hf : continuous f)
  (hpf : ∀ y : Y, p (f y) = y)
  : quotient_map p
:= sorry

/-
Munkres_2_22_2b
If $A \subset X$, a retraction of $X$ onto $A$ is a continuous map $r: X \rightarrow A$ such that $r(a)=a$ for each $a \in A$. Show that a retraction is a quotient map.
-/
theorem retraction_is_quotient_map
  (X : Type*) [topological_space X]
  (A : set X)
  (r : X → A)
  (hr : continuous r)
  (hr_id : ∀ x ∈ A, r x = x)
  : quotient_map r
:= sorry

/-
Munkres_2_22_3
Let $H$ be a subspace of $G$. Show that if $H$ is also a subgroup of $G$, then both $H$ and $\bar{H}$ are topological groups.
-/
theorem subspace_subgroup_is_topological_group
  (G : Type*) [topological_space G] [group G]
  (H : set G)
  (hH : is_subgroup H)
  (hH_subspace : is_subspace H)
  : is_topological_group H
:= sorry

