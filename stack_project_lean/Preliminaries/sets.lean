/-- Generation 0 ---------------------- -/
lemma axiom_regularity : ∀ x, ∃ α, x ∈ V α :=


/-- Generation 1 ---------------------- -/
lemma map_from_set_lifts : ∀ {β : Ord} {S : Set} {T : Set} {T_α : Ord → Set} {φ : S → T}
  (h : T = colimit (λ α, T_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ T_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ s : S, φ s ∈ T_α α :=



/-- Generation 2 ---------------------- -/

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3

-- Lean 3


/-- Generation 3 ---------------------- -/
theorem reflection_principle (M0 : set) (phi : ℕ → set → set → set → Prop) :
  ∃ M : set, M0 ⊆ M ∧ ∀ x y z, (∀ i, phi i^M x y z) ↔ (∀ i, phi i x y z) :=


/-- Generation 4 ---------------------- -/
lemma bounded_size : ∀ κ : Cardinal, ∃ A : Set, ∀ X : Set, X ∈ A → is_scheme X ∧
  (∀ S : Set, is_scheme S → size S ≤ κ → ∃ X : Set, X ∈ A ∧ X ≅ S) :=
 


/-- Generation 5 ---------------------- -/
lemma construct_category : ∀ {S_0 : Set} {κ : Cardinal} {α : Ord} {S : Set} {T : Set} {T_α : Ord → Set} {φ : S → T}
  (h : T = colimit (λ α, T_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ T_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ s : S, φ s ∈ T_α α :=
  


/-- Generation 6 ---------------------- -/
lemma bound_affine : ∀ {S : Set} {R : Set} {R_α : Ord → Set} {φ : R → R_α 0}
  (h : R_α 0 = colimit (λ α, R_α α)),
  (∀ r : R, ∃ α : Ord, φ r ∈ R_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ r : R, φ r ∈ R_α α :=
   


/-- Generation 7 ---------------------- -/
lemma bound_size : ∀ {S : Set} {S_α : Ord → Set} {φ : S → S_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ s : S, φ s ∈ S_α α :=
    


/-- Generation 8 ---------------------- -/
lemma bound_size_fibre_product : ∀ {X : Set} {Y : Set} {S : Set} {X_α : Ord → Set} {Y_α : Ord → Set} {S_α : Ord → Set} {φ : X → X_α 0} {ψ : Y → Y_α 0} {χ : S → S_α 0}
  (h : X_α 0 = colimit (λ α, X_α α)) (h' : Y_α 0 = colimit (λ α, Y_α α)) (h'' : S_α 0 = colimit (λ α, S_α α)),
  (∀ x : X, ∃ α : Ord, φ x ∈ X_α α) →
  (∀ y : Y, ∃ α : Ord, ψ y ∈ Y_α α) →
  (∀ s : S, ∃ α : Ord, χ s ∈ S_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β 


/-- Generation 9 ---------------------- -/
lemma bound_finite_type : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
     


/-- Generation 10 ---------------------- -/
lemma bound_monomorphism : ∀ {X : Set} {Y : Set} {X_α : Ord → Set} {Y_α : Ord → Set} {φ : X → X_α 0} {ψ : Y → Y_α 0}
  (h : X_α 0 = colimit (λ α, X_α α)) (h' : Y_α 0 = colimit (λ α, Y_α α)),
  (∀ x : X, ∃ α : Ord, φ x ∈ X_α α) →
  (∀ y : Y, ∃ α : Ord, ψ y ∈ Y_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
      


/-- Generation 11 ---------------------- -/
lemma what_is_in_it : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
       


/-- Generation 12 ---------------------- -/
lemma bound_by_covering : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
        


/-- Generation 13 ---------------------- -/
lemma bound_fppf_covering : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
          


/-- Generation 14 ---------------------- -/
lemma sets_with_group_action : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
             


/-- Generation 15 ---------------------- -/
lemma what_is_in_it_G_sets : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
                


/-- Generation 16 ---------------------- -/
lemma coverings_site : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
                 


/-- Generation 17 ---------------------- -/
lemma abelian_injectives : ∀ {S : Set} {X : Set} {S_α : Ord → Set} {X_α : Ord → Set} {φ : S → S_α 0} {ψ : X → X_α 0}
  (h : S_α 0 = colimit (λ α, S_α α)) (h' : X_α 0 = colimit (λ α, X_α α)),
  (∀ s : S, ∃ α : Ord, φ s ∈ S_α α) →
  (∀ x : X, ∃ α : Ord, ψ x ∈ X_α α) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  (∀ α : Ord, ∃ β : Ord, α < β ∧ β ≤ β) →
  ∃ α : Ord, ∀ x : X, ψ x ∈ X_α α :=
                  

