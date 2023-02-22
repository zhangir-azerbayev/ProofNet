import .common 

open complex filter function interval_integral metric

open_locale big_operators
open_locale filter
open_locale topology





theorem exercise_1_13b  {f : open_set X → Y} (hY : Y) (hX : X) (h : constant_im f) :
  constant_im f :=
sorry

theorem exercise_1_19a {z : ℝ} (n : ℕ) :
  ∃ (z_n : ℝ), ∑ n z_n ^ n = 0 → ∃ (z_n : ℝ), ∑ n z_n ^ n ≠ 0 :=
sorry

theorem exercise_1_19c {z : ℝ} (n : ℕ) :
  ∃ (z_n : ℝ), ∑ z_n/n = z ∧ ∑ z_n/n = 1 → z = 1 :=
sorry

theorem exercise_2_2  {x : ℝ} (h : x < ∞) :
  ∫ 0 ∞ (λ y, sin y / y) d x = π / 2 :=
sorry

theorem exercise_2_13  {f : ℂ → ℂ} (z_0 : ℂ) (c : ℂ) (h : ∃ n : ℕ, c.n = 0) :
  polynomial f :=
sorry

theorem exercise_3_4  {a : ℝ} (x : ℝ) (f : a → ℝ) (g : a → ℝ) (h : f = g) :
  ∫ a f = ∫ a g :=
sorry

theorem exercise_3_14entire_function_of_form_a_b_c_d_e_f_g_h_i_j_k_l_m_n_o_p_q_r_s_t_u_v_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_y_z_w_x_:=
sorry

theorem exercise_5_1 {f : D → ℂ} 
  (h : holomorphic f) (z : ℂ) (hz : ∀ n : ℕ, hz (f.zero n) = 0) :
  ∃ (n : ℕ), ∑ n = 1 (1 - |z|) < ∞ :=
sorry