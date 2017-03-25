open import Data.Nat
open import Auto

module Base where

data Even  : ℕ →  Set where
  isEven0  : Even 0
  isEven+2 : ∀ {n} → Even n → Even (suc (suc n))

even+ : ∀ {n m} → Even n → Even m → Even (n + m)
even+  isEven0      e2 = e2
even+ (isEven+2 e1) e2 = isEven+2 (even+ e1 e2)

isEven-2 : ∀ {n} → Even (2 + n) → Even n
isEven-2 (isEven+2 n) = n

rules : HintDB
rules = ε << isEven0
          << isEven+2
          << even+
