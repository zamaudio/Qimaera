module Main

import Data.Nat
import Data.Vect
import Data.List
import LinearTypes
import Control.Linear.LIO
import Unitary
import QStateT
import Teleportation
import System.Random
import Injection
import QFT
import Grover
import AlterningBitsOracle
import VQE
import Complex
import QuantumState
import CoinToss
import QAOA
import Graph
import Examples
import RUS

-- %default total
  

||| Perform 1000 fair coin tosses and count the number of heads
||| (via simulating the quantum dynamics).
testCoins : IO ()
testCoins = do
  let f = coin {t = SimulatedState}
  s <- sequence (Data.List.replicate 1000 f)
  let heads = filter (== True) s
  putStrLn $ "Number of heads: " ++ (show (length heads))


||| Call the drawTeleportation function (using the SimulatedState implementation)
||| then execute the runTeleportation function 1000 times and report on the
||| observed measurement results on the third qubit
||| (which is in state |+> at the end of the teleportation protocol).
export
testTeleport : IO ()
testTeleport = do
  drawTeleportation {t = SimulatedState}
  l <- sequence (Data.List.replicate 1000 (runTeleportation {t = SimulatedState}))
  let nbT = length $ filter (\x => (last x) == True) l
  putStrLn "\n\nFor 1000 measurements"
  putStrLn ("Number of True measurements : " ++ show nbT) 

||| Test graph for the QAOA problem
export
graph1 : Graph 5
graph1 = AddVertex (AddVertex (AddVertex (AddVertex (AddVertex Empty []) [True]) [True, True]) [False, True, False]) [False, False, True, True]

||| Execute QAOA with 100 samples on the previous graph to solve the MAXCUT problem
export
testQAOA : IO (Cut 5)
testQAOA = do
  QAOA {t = SimulatedState} 100 1 graph1


||| Small test for the VQE algorithm
export
testVQE : IO Double
testVQE = do
  putStrLn "Test VQE"
  let hamiltonian = [(2, [PauliX, PauliY]),(3,[PauliZ, PauliI])]
  VQE {t = SimulatedState} 2 hamiltonian 5 10 5


export
main : IO ()
main = do

  -- Execute the example file and draw the circuit examples
  drawExamples

  -- Draw the Quantum Fourier Transform for n = 3
--  putStrLn "\n\n\nQuantum Fourier Transform for n = 3"
--  draw (qft 3)


  -- Execute the coin toss example
--  putStrLn "\nTest coin toss"
--  testCoins

  -- Teleportation protocol
--  putStrLn "\nTest Teleportation protocol"
--  testTeleport

  -- Repeat until success
  putStrLn "\nTest 'Repeat Until Success'. Probability of success is 2/3 for this example."
  testMultipleRUS 10000

  -- VQE
--  putStrLn "\nSmall test with VQE"
--  r <- testVQE
--  putStrLn $ "result from VQE : " ++ show r

  -- QAOA
--  putStrLn "\nSmall test with QAOA"
--  cut <- testQAOA
--  putStrLn $ "result from QAOA : " ++ show cut

  pure ()


  


