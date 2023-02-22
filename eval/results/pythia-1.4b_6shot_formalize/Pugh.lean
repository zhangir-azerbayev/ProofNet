import .common

open set real filter function ring_hom topological_space

open_locale big_operators
open_locale filter
open_locale topology 
noncomputable theory 





theorem exercise_2_26 {M : Type*} [set M] [open_set M]
  (h : open_set M) : open_set M :=
sorry

theorem exercise_2_32a {N : Type*} [set] [fintype N]
  [fintype N] (h : clopen N) : clopen N :=
sorry

theorem exercise_2_46disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_disjoint_compact_nonempty_sets_dis:=
sorry

theorem exercise_2_92 {X Y Z : Type*} 
  [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : nonempty_covering_intersection (g ∘ f)) : nonempty_covering_intersection f :=
sorry

theorem exercise_3_1  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : constant_function (g ∘ f)) : constant_function f :=
sorry

theorem exercise_3_63alog_sum_of_log_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_log_of_:=
sorry

theorem exercise_4_15a  {X Y Z : Type*} [metric_space X] [metric_space Y] [metric_space Z]
  (hY : compact_space Y) (f : X → Y) (g : Y → Z) (hgc : continuous g)
  (hgi : function.injective g)
  (h : uniform_continuous (g ∘ f)) : uniform_continuous f :=
sorry