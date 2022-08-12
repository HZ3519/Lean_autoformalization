/-- Generation 0 ---------------------- -/
lemma axiom_regularity : ∀ x, ∃ α, x ∈ V α :=


/-- Generation 1 ---------------------- -/
lemma map_from_set_lifts : ∀ {β : Ord} {S : Set} {T : Set} {T_α : Ord → Set}
  (h_colim : T = colim (λ α, T_α α)) (h_cf : cf β > card S) (φ : S → T),
  ∃ α, ∃ φ' : S → T_α α, φ = (colim_map (λ α, T_α α) α φ') :=


/-- Generation 2 ---------------------- -/
lemma exists_ordinals_large_cofinality (kappa : cardinal) :
  ∃ γ, cof γ > kappa :=


/-- Generation 3 ---------------------- -/
theorem reflection_principle (M0 : set α) (φ : α → Prop) :
  ∃ M : set α, M0 ⊆ M ∧ ∀ x, x ∈ M → φ x :=


/-- Generation 4 ---------------------- -/
lemma bounded_size : ∀ κ, ∃ A, ∀ X, X ∈ A → is_scheme X ∧ ∀ S, is_scheme S → size S ≤ κ → ∃ X' ∈ A, X ≅ X' :=


/-- Generation 5 ---------------------- -/
lemma construct_category : ∀ S_0, ∃ α,
  S_0 ⊆ V α ∧
  ∀ S, S ∈ V α → ∀ T, is_scheme T → size T ≤ Bound (size S) → ∃ S' ∈ V α, T ≅ S' ∧
  ∀ {ι : Type} [hι : fintype ι] {F : ι → Sch α},
    (∀ i, F i ∈ V α) →
    (∃ S, is_scheme S ∧ ∀ i, F i ≅ S) ↔
    (∃ S, is_scheme S ∧ ∀ i, F i ≅ S) ∧
    ∀ S, is_scheme S → ∀ i, F i ≅ S → S ≅ lim F ∧
  ∀ {ι : Type} [hι : fintype ι] {F : ι → Sch α},
    (∀ i, F i ∈ V α) →
    (∃ S, is_scheme S ∧ ∀ i, F i ≅ S) ↔
    (∃ S, is_scheme S ∧ ∀ i, F i ≅ S) ∧
    ∀ S, is_scheme S → ∀ i, F i ≅ S → S ≅ colim F :=


/-- Generation 6 ---------------------- -/
lemma bound_affine : ∀ S, is_affine S → size S = max ℕ (card (ring_of_global_sections S)) :=


/-- Generation 7 ---------------------- -/
lemma bound_size : ∀ S, is_scheme S → ∃ I, ∀ i, i ∈ I → is_open (S i) ∧ size S ≤ max (card I) (sup (λ i, size (S i))) :=


/-- Generation 8 ---------------------- -/
lemma bound_size_fibre_product : ∀ {X Y S : Scheme} (f : X ⟶ S) (g : Y ⟶ S),
  size (X ×ₜ S Y) ≤ max (size X) (size Y) :=



/-- Generation 9 ---------------------- -/
lemma bound_finite_type : ∀ {S : Scheme} {X : Scheme} (f : X ⟶ S),
  is_locally_of_finite_type f ∧ is_quasi_compact X → size X ≤ size S :=



/-- Generation 10 ---------------------- -/
lemma bound_monomorphism : ∀ {X Y : Scheme} (f : X ⟶ Y),
  is_monomorphism f → (is_quasi_compact X ∨ is_locally_of_finite_presentation f) → size X ≤ size Y :=



/-- Generation 11 ---------------------- -/
lemma what_is_in_it : ∀ α, is_category (Sch α) ∧
  (∀ {X Y S : Scheme} (f : X ⟶ S) (g : Y ⟶ S),
    is_fibre_product (X ×ₜ S Y) (fst (fiber_product f g))) ∧
  (∀ (I : Type) (S : I → Scheme),
    is_coproduct (⨆ i, S i) (coproduct_morphism S)) ∧
  (∀ {S : Scheme} (U : Scheme) (i : U ⟶ S),
    is_open i → ∃ V, is_isomorphic V U) ∧
  (∀ {S : Scheme} (T : Scheme) (i : T ⟶ S),
    is_closed i → ∃ S', is_isomorphic S' T) ∧
  (∀ {S : Scheme} (T : Scheme) (f : T ⟶ S),
    is_locally_of_finite_type f → ∃ S', is_isomorphic S' T) ∧
  (∀ {S : Scheme} (I : Type) (S : I → Scheme) (i : ∀ i, is_open (S i))
    (H : ∃ T, ∀ i, size (S i) ≤ size T ^ ℕ ∧ card I ≤ size T ^ ℕ),
    ∃ S', is_isomorphic S' S) ∧
  (∀ {S : Scheme} (T : Scheme) (f : T ⟶ S) (H : is_locally_of_finite_type f)
    (H' : ∃ n, ∀ U, is_open U → is_affine U → card (set.range (λ U, U)) ≤ n),
    ∃ S', is_isomorphic S' T) ∧
  (∀ {S : Scheme} (T : Scheme) (f : T ⟶ S) (H : is_monomorphism f)
    (H' : is_locally_of_finite_presentation f ∨ is_quasi_compact T),
    ∃ S', is_isomorphic S' T) ∧
  (∀ {T : Scheme} (H : is_affine T) (R : Ring) (H' : is_isomorphic R (ring_of_global_sections T)),
    ∀ (I : Ideal R) (R' : Ring) (H'' : is_finite_type R' R) (S : Ring) (H''' : is_localization S R)
      (p : Prime R) (R' : Ring) (H'''' : is_subring R' R) (S : Scheme) (H''''' : is_of_finite_type S R'),
    ∃ S', is_isomorphic S' S) :=
 


/-- Generation 12 ---------------------- -/
lemma bound_by_covering : ∀ {X Y : Scheme} (f : X ⟶ Y),
  ∃ J, ∀ j, j ∈ J → is_fpqc_covering (Y j) Y ∧ (Y j) ⟶ X ⟶ Y :=
 


/-- Generation 13 ---------------------- -/
lemma bound_fppf_covering : ∀ {X : Scheme} (I : Type) (f : Π i, i ∈ I → X ⟶ X),
  is_fppf_covering f → ∃ J, ∀ j, j ∈ J → is_fppf_covering (f j) ∧ size (⨆ j, f j) ≤ size X :=
 


/-- Generation 14 ---------------------- -/
lemma sets_with_group_action : ∀ {G : Group} (S_0 : Type) (S_0_G : Π s, s ∈ S_0 → G ⟶ S_0 s),
  ∃ α, ∀ S, S ∈ G_Sets_α → ∀ T, size T ≤ Bound (size S) → ∃ S', S' ∈ G_Sets_α ∧ is_isomorphic T S' :=
 


/-- Generation 15 ---------------------- -/
lemma what_is_in_it_G_sets : ∀ {α : Type} [is_well_order α] (G : Group),
  is_object ({}_GG : G-Set α) ∧
  is_coproduct (G-Set α) ∧ is_product (G-Set α) ∧ is_fibre_product (G-Set α) ∧ is_pushout (G-Set α) ∧
  ∀ {U : G-Set α}, ∀ {O : G-Set α}, is_subobject O U → is_object O :=
 


/-- Generation 16 ---------------------- -/
lemma coverings_site : ∀ {C : Category} (Cov : Type) (Cov_0 : Cov → Prop) (Cov_1 : Cov → Prop) (Cov_2 : Cov → Prop) (Cov_3 : Cov → Prop),
  ∀ c, c ∈ Cov → Cov_0 c ∧ Cov_1 c ∧ Cov_2 c ∧ Cov_3 c →
  ∃ κ, ∃ α, ∀ c, c ∈ Cov → Cov_0 c ∧ Cov_1 c ∧ Cov_2 c ∧ Cov_3 c :=
 


/-- Generation 17 ---------------------- -/
lemma abelian_injectives : ∀ {A : Category} [is_abelian A] [has_enough_injectives A],
  ∀ {S : Type} (A_s : S → A), ∃ A' : Category,
  is_set (Ob A') ∧ ∀ s, s ∈ S → is_object (A' s) ∧ is_abelian A' ∧ has_enough_injectives A' ∧
  ∀ {I : A'}, is_injective I ↔ is_injective I :=
 


