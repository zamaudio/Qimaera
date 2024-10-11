module Qimaera.Examples

import Control.Linear.LIO
import Data.Nat
import Data.Vect
import Data.List
import System.Random
import Qimaera.LinearTypes
import Qimaera.Unitary
import Qimaera.QStateT
import Qimaera.Injection
import Qimaera.Complex
import Qimaera.QuantumOp


------------------------ Example of circuits built with unitary contructors -----------------------

-- These functions only use the 4 constructors of the Unitary data type : IdGate, H, P, and CNOT

||| Put two qubits initally in state |00> in the Bell state
public export
toBellBasis : Unitary 2
toBellBasis = CNOT 0 1 (H 0 IdGate)

||| Draw the circuit toBellBasis using draw function
export
drawToBellBasis : IO ()
drawToBellBasis = do
  putStrLn "\nDrawing ToBellBasis: \nCNOT 0 1 (H 0 IdGate)"
  draw toBellBasis


constructorsExample : Unitary 3
constructorsExample = H 1 (P (pi/2) 2 (CNOT 2 0 IdGate))

drawConstructorsExample : IO ()
drawConstructorsExample = do
  putStrLn "An example of circuit built with H, P and CNOT constructors :"
  putStrLn " H 1 (P (pi/2) 2 (CNOT 2 0 IdGate))"
  draw constructorsExample


---------------------------------- Examples using composition -------------------------------------

-- Sequential composition of unitary circuits

compose_example1 : Unitary 1
compose_example1 = TGate . HGate

compose_example2 : Unitary 2
compose_example2 = (H 1 IdGate) . (P pi 0 IdGate) . toBellBasis

drawComposeExamples : IO ()
drawComposeExamples = do
  putStrLn "Examples using composition"
  putStrLn "Example 1 : TGate . HGate"
  draw compose_example1
  putStrLn "Example 2 : (H 1 IdGate) . (P pi 0 IdGate) . toBellBasis"
  draw compose_example2

------------------------------------ Examples using tensor product --------------------------------

-- Parallel composition (ie tensor product) of unitary circuits

||| Example using the # operator for tensor product
tensorExample1 : Unitary 4
tensorExample1 = HGate # PGate pi # CNOTGate

||| Example using tensorn function :
|||Make n tensor products of the same unitary of size 1
tensornExample : Unitary 3
tensornExample = tensorn 3 HGate

||| Example using tensorMapSimple function
||| Tensor product of a Vector of single-qubit Unitary operators
tensorMapSimpleExample : Unitary 3
tensorMapSimpleExample = tensorMapSimple [HGate, PGate pi, HGate]

||| Example using tensorMap function
||| Tensor product of a Vector of Unitary operators
tensorMapExample : Unitary 6
tensorMapExample = tensorMap [CNOTGate, toBellBasis, CNOTGate]

drawTensorExamples : IO ()
drawTensorExamples = do
  putStrLn "Examples using tensor product"
  putStrLn "Example 1 : HGate # PGate pi # CNOTGate"
  draw tensorExample1
  putStrLn "Example 2 : tensorn 3 HGate"
  draw tensornExample
  putStrLn "Example 3 : tensorMapSimple [HGate, PGate pi, HGate]"
  draw tensorMapSimpleExample
  putStrLn "Example 4 : tensorMap [CNOTGate, toBellBasis, CNOTGate]"
  draw tensorMapExample


||| Another version of toBellBasis using composition and tensor product
toBellBasis2 : Unitary 2
toBellBasis2 = CNOTGate . (HGate # IdGate)

drawToBellBasis2 : IO ()
drawToBellBasis2 = do
  putStrLn "\nAnother possibility for toBellBasis: \nCNOTGate . (HGate # IdGate)"
  draw toBellBasis2

---------------------------------------- Examples using adjoint -----------------------------------

-- The adjoint of a unitary circuit is the inverse unitary circuit

adjoint_example1 : Unitary 2
adjoint_example1 = adjoint toBellBasis

adjoint_example2 : Unitary 3
adjoint_example2 = adjoint toffoli

drawAdjointExamples : IO ()
drawAdjointExamples = do
  putStrLn "Examples using adjoint"
  putStrLn "Example 1 : adjoint toBellBasis"
  draw adjoint_example1
  putStrLn "Example 2 : adjoint toffoli"
  draw adjoint_example2


||| Draw an example of circuit using tensor, compose and adjoint
export
exampleComposeTensor1 : IO ()
exampleComposeTensor1 = do
  putStrLn "\nAn example of usage of compose, tensor and adjoint: \n(adjoint toBellBasis # IdGate) . (TGate # toBellBasis)"
  let circuit = (adjoint toBellBasis # IdGate) . (TGate # toBellBasis)
  draw circuit


---------------------------------------- Examples using apply -------------------------------------

-- Apply : apply a smaller unitary circuit of size i to a bigger one of size n, giving the vector v of wire indices on which we wish to apply the smaller circuit

U : Unitary 3
U = HGate # IdGate {n = 1} # (PGate pi)

apply_example1 : Unitary 3
apply_example1 = apply toBellBasis U [0,1]

apply_example2 : Unitary 3
apply_example2 = apply toBellBasis U [0,2]

apply_example3 : Unitary 3
apply_example3 = apply toBellBasis U [2,0]

apply_example4 : Unitary 3
apply_example4 = apply toBellBasis U [2,1]

apply_example5 : Unitary 3
apply_example5 = apply toffoli IdGate [2,0,1]

drawApplyExamples : IO ()
drawApplyExamples = do
  putStrLn "\nApply Examples \nU = HGate # IdGate {n = 1} # (PGate pi)\n"
  putStrLn "Example 1 : apply toBellBasis U [0,1]"
  draw apply_example1
  putStrLn "Example 2 : apply toBellBasis U [0,2]"
  draw apply_example2
  putStrLn "Example 3 : apply toBellBasis U [2,0]"
  draw apply_example3
  putStrLn "Example 4 : apply toBellBasis U [2,1]"
  draw apply_example4
  putStrLn "Example 5 : apply toffoli [2,0,1]"
  draw apply_example5

-------------------------------------- Example using controlled -----------------------------------

-- Compute the controlled version of a unitary circuit

controlled_example1 : Unitary 2
controlled_example1 = controlled TGate

||| Example using multipleQubitControlledNOT
||| Makes a multiple qubit CNOT gate : control on the first n qubits, target on the last
multipleQubitsCNOTExample : Unitary 4
multipleQubitsCNOTExample = multipleQubitControlledNOT 4

--------------------------------- Examples of parametrized circuits -------------------------------

-- Unitary circuits can be parametrized by classical information


parametrized_example1 : Bool -> Unitary 1
parametrized_example1 b = if b then HGate else PGate pi

parametrized_example2 : Bool -> Bool -> Double -> Unitary 2
parametrized_example2 b1 b2 p = CNOTGate . (if b1 then H 0 IdGate else IdGate) . (if b2 then IdGate else P p 1 IdGate)

drawParamExamples : IO ()
drawParamExamples = do
  putStrLn "Examples of circuits parametrized by classical data"
  putStrLn "Example 1 : for b : bool , if b then HGate else PGate pi"
  putStrLn "For b = True : "
  draw (parametrized_example1 True)
  putStrLn "For b = False : "
  draw (parametrized_example1 False)
  putStrLn "Example 2 : for b1, b2 : Bool and p : Double , CNOTGate . (if b1 then H 0 IdGate else IdGate) . (if b2 then IdGate else P p 1 IdGate)"
  putStrLn "For b1 = True, b2 = False, p = pi/2"
  draw (parametrized_example2 True False (pi/2))


------------------------------------ Example of depth computation ---------------------------------
-- Compute the depth of a circuit 


depthExample1 : Unitary 3
depthExample1 = CNOT 0 1 $ CNOT 2 1 $ H 1 $ CNOT 0 2 IdGate 

depthExample2 : Unitary 3
depthExample2 = H 2 $ H 1 $ H 0 $ H 1 IdGate

depthExample3 : Unitary 3
depthExample3 = CNOT 1 2 $ CNOT 0 2 $ CNOT 0 1 $ H 1 $ P pi 1 $ H 1 IdGate

drawDepthExamples : IO ()
drawDepthExamples = do
  putStrLn "Examples of depth computation"
  putStrLn "The depth of the following circuit"
  draw depthExample1
  putStrLn  ("is " ++ show (depth depthExample1))
  putStrLn "\n\nThe depth of the following circuit"
  draw depthExample2
  putStrLn $ "is " ++ show (depth depthExample2)
  putStrLn "\n\nThe depth of the following circuit"
  draw depthExample3
  putStrLn $ "is " ++ show (depth depthExample3)


----------------------------------- Examples of quantum operations --------------------------------


||| Sequencing quantum operations using run
||| 
quantum_operation4 : QuantumOp t => IO (Vect 3 Bool)
quantum_operation4 = 
  run (do
      [q1,q2] <- newQubits {t=t} 2                      --create 2 new qubits q1 and q2
      [q1,q2] <- applyUnitary [q1,q2] toBellBasis       --apply the toBellBasis unitary circuit to q1 and q2
      q3 <- newQubit                                    --create 1 new qubit q3
      [q1,q3,q2] <- applyUnitary [q1,q3,q2] toffoli     --apply toffoli gate on q1, q3 and q2
      [b2] <- measure [q2]                              --measure q2
      (q3 # q1) <- applyCNOT q3 q1                      --apply CNOT on q3 and q1
      [b1,b3] <- measure [q1,q3]                        --measure q1 and q3
      pure [b1,b2,b3]                                   --return the results
      )

drawQuantumOp : IO ()
drawQuantumOp = do
  [b1,b2,b3] <- quantum_operation4 {t = SimulatedOp}
  putStrLn "\n\nExecuting an example of quantum operations : sequencing quantum operations using run"
  putStrLn "Create 2 qubits q1 and q2"
  putStrLn "Apply `toBellBasis` circuit on q1 and q2"
  putStrLn "Create one new qubit q3"
  putStrLn "Apply the toffoli gate on q1,q3 and q2"
  putStrLn $ "Measure q2 : result is " ++ show b2
  putStrLn "Apply CNOT on q3 and q1"
  putStrLn $ "Measure q1 and q3 : results are " ++ show b1 ++ " and " ++ show b3

------------------------------------ Draw all example circuits ------------------------------------

export
drawExamples : IO ()
drawExamples = do
  drawToBellBasis
  drawConstructorsExample
  drawComposeExamples
  drawTensorExamples
  drawToBellBasis2
  drawAdjointExamples
  exampleComposeTensor1
  drawApplyExamples
  drawParamExamples
  drawDepthExamples
  drawQuantumOp
