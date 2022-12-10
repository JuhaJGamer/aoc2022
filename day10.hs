module Day10 where

import Data.Bool
import Data.Bifunctor
import Data.List.Split
import System.IO

import Debug.Trace

data Instruction = Noop | AddX Int
    deriving (Show, Eq)
data CPUState = Ready | CycleOp Int Instruction
    deriving (Show, Eq)
newtype CPURegisters = CPURegisters { getX :: Int }
    deriving (Show, Eq)
data CPU = CPU { getRegisters :: CPURegisters, getState :: CPUState }
    deriving (Show, Eq)

type Program = [Instruction]

readInstruction :: String -> Instruction
readInstruction "noop" = Noop
readInstruction s
  | take 4 s == "addx" = AddX $ read $ drop 5 s
  | otherwise          = undefined

willExecuteInstruction :: CPU -> Bool
willExecuteInstruction (CPU _ Ready) = True
willExecuteInstruction _             = False

stepCycle :: Instruction -> CPU -> CPU
stepCycle i cpu@(CPU regs state)
  | willExecuteInstruction cpu = startInstruction i cpu
  | otherwise
    = case state of
        CycleOp 0 i -> executeInstruction i $ CPU regs Ready
        CycleOp n i -> CPU regs $ CycleOp (n-1) i

startInstruction :: Instruction -> CPU -> CPU
startInstruction Noop cpu = executeInstruction Noop cpu
startInstruction i@(AddX _) (CPU regs _) = CPU regs (CycleOp 0 i)

executeInstruction :: Instruction -> CPU -> CPU
executeInstruction Noop cpu = cpu
executeInstruction (AddX n) (CPU (CPURegisters x) s) = CPU (CPURegisters (x+n)) s

executeProgram :: Program -> CPU -> [CPU]
executeProgram [] cpu = [cpu]
executeProgram (i:is) cpu
  | willExecuteInstruction cpu = nextCPU : executeProgram    is  nextCPU
  | otherwise                  = nextCPU : executeProgram (i:is) nextCPU
    where nextCPU = stepCycle i cpu

blankCPU :: CPU
blankCPU = CPU (CPURegisters 1) Ready

isPixelLit :: Int -> CPU -> Bool
isPixelLit cycle (CPU (CPURegisters x) _) = abs (col - x) < 2
    where col = (cycle-1) `mod` 40

main = do prog <- map readInstruction . lines <$> getContents
          let states = blankCPU : executeProgram prog blankCPU
              centerStates = map head $ chunksOf 40 $ drop 19 states
              rowStates = chunksOf 40 $ zip [1..] states
              rowPixels = map (map (bool '.' '#' . uncurry isPixelLit)) rowStates
          print $ sum $ zipWith (*) (map (subtract 20 . (*40)) [1..]) $ map (getX . getRegisters) centerStates
          mapM_ putStrLn rowPixels
          putStrLn "trolled"
