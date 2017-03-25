open import Data.Nat
open import Auto

module Base where

data Plus : ℕ → ℕ → ℕ → Set where
  plusZ : ∀ {n}     → Plus 0 n n
  plusS : ∀ {n m r} → Plus n m r → Plus (suc n) m (suc r)

rules : HintDB
rules = ε << plusS
          << plusZ

