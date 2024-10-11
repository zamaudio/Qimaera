module Shor

import Data.Nat
import Data.Vect
import Data.List
import Qimaera

fromBool : Bool -> Nat
fromBool False = 0
fromBool True = 1

fromRegister : Nat -> Vect (a) Bool -> Nat
fromRegister _ [] = 0
fromRegister i (q :: qs) = ((fromBool q) * (power 2 i)) + (fromRegister (S i) qs)

-- Encode binary value n in q gates using swap from 0>
N : (q : Nat) -> {auto prf : (q > 0) = True} -> (n : Nat) -> Unitary q
N 1 0 = IdGate
N 1 1 = XGate
N (S q) n =
  let
    bit : (m : Nat) -> (i : Nat) -> (n : Nat) -> Nat
    bit m i n = (n `div` (power 2 (minus m i))) `mod` 2

    bitG : (j : Nat) -> (n : Nat) -> Unitary 1
    bitG j n = if ((bit q j n) == 1) then XGate else IdGate

    Nj : (j : Nat) -> Nat -> Unitary (S j)
    Nj 0 n = bitG 0 n
    Nj (S k) n = (bitG (S k) n) # (Nj k n)
  in
    Nj q n

Q' : (a : Nat) -> Unitary a
Q' a = adjoint (qft a)
--Q' a = IdGate {n = a} -- no-op a times

-- TODO implement (a^j mod N) in quantum gates
-- for now just use a no-op
modexp : (a : Nat) -> (b : Nat) -> Unitary (a + b)
modexp a b = IdGate {n = (a + b)} -- no-op (a+b) times

Qb : (a : Nat) -> (b : Nat) -> Unitary (a + b)
Qb a b = (Q' a) # (IdGate {n = b}) -- extend block with b auxiliary wires

||| Implementation of Shor's algorithm using quantum phase estimation:
|||
||| var  nQubits          Quantum Circuit
|||
||| j:   log2(sqrt(n)) 0> - H ----|m|- QFT'- Measure  <- interesting outputs
||| a:   log2(a)       0> - N(a) -|o|------- a register
||| n:   log2(n)       0> - N(n) -|d|------- n register
||| aux: log2(?)       0> - X ----|e|------- aux register for modexp (input as all ones via X gates)
||| c1:  1             0> --------|x|------- control qubit 1 for modexp
||| c2:  1             0> --------|p|------- control qubit 2 for modexp
|||
||| TODO: (modexp a j n aux c1 c2) computes a^j mod n
|||
quantum_phase_est : (q : Nat) -> {auto prf : (q > 0) = True} -> (aux : Nat) -> (n : Nat) -> Unitary (q + aux)
quantum_phase_est q aux n = (Qb q aux) . ((modexp q aux) . (((tensorn q HGate) . (N q n))  # (tensorn aux XGate)))
--quantum_phase_est q aux n = (N q n) # (IdGate {n=aux}) -- try outputting just n

drawQPE : (a : Nat) -> {auto prf : (a > 0) = True} -> (b : Nat) -> (n : Nat) -> IO ()
drawQPE a b n = do
  putStrLn $ show $ quantum_phase_est a b n
  draw $ quantum_phase_est a b n

computeQPE : (a : Nat) -> {auto prf : (a > 0) = True} -> (b : Nat) -> (n : Nat) -> QuantumOp t => IO (Vect (a + b) Bool)
computeQPE a b n =
  run (do
      qs <- newQubits {t=t} (a + b)
      qs <- applyUnitary qs (quantum_phase_est a b n)
      bs <- measureAll qs
      pure bs
      )

main : IO ()
main = do
  let
    n, log2n, auxgates : Nat
    log2n = 4
    n = minus (power 2 log2n) 1
    auxgates = 0
  drawQPE log2n auxgates n
  bs <- computeQPE log2n auxgates n {t = SimulatedOp}
  let nn = fromRegister 0 (Data.Vect.take log2n bs)
  let aux = fromRegister 0 (Data.Vect.drop log2n bs)
  putStrLn $ "Results: N=" ++ show nn ++ " aux=" ++ show aux

