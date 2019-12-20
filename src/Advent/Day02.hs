{-# LANGUAGE FlexibleContexts #-}
module Advent.Day02 (
    IntCode,
    ProgramCounter,
    ProgramError,
    ProgramState(..),
    toIntCode,
    step,
    run,
    runDay02
) where

import Control.Monad.Except
import Data.Vector as V
import Data.Vector.Mutable (write)
import qualified Advent.Day02Input as Input

type IntCode = V.Vector Int
type ProgramCounter = Int
type ProgramError = String -- sorry

data ProgramState =
    Running ProgramCounter IntCode
    | Halted IntCode
    | Crash ProgramError IntCode
    deriving (Eq, Show)

handleCrash :: Either (IntCode, ProgramError) ProgramState -> ProgramState
handleCrash = either
    (uncurry (flip Crash))
    id

step :: ProgramState -> ProgramState
step p@(Halted _) = p
step p@(Crash _ _) = p
step (Running pc p) =
    let readReg i v = maybe
            (Left $ (v, mconcat ["Invalid reg: ", show i, " (program count: ", show (V.length v), ")"]))
            Right
            (v !? i)
        readOperandsFor i v = (,,) <$> readReg (i+1) v <*> readReg (i+2) v <*> readReg (i+3) v
        writeReg :: ProgramCounter -> Int -> IntCode -> IntCode
        writeReg i x = modify (\v -> write v i x)
        apply2 f v (a,b,store) =
            let s = f <$> readReg a v <*> readReg b v
            in (\s' -> writeReg store s' v) <$> s
        add = apply2 (+)
        mult = apply2 (*)
    in handleCrash $ do
        curr <- readReg pc p
        case curr of
            99 -> pure (Halted p)
            1 -> Running (pc+4) <$> (readOperandsFor pc p >>= add p)
            2 -> Running (pc+4) <$> (readOperandsFor pc p >>= mult p)
            x -> pure (Crash (mconcat ["Unknown opcode ", show x]) p)

toIntCode :: [Int] -> IntCode
toIntCode = V.fromList

run :: IntCode -> ProgramState
run =
    let run' p@(Halted _) = p
        run' p@(Crash _ _) = p
        run' p = run' (step p)
    in run' . Running 0

runDay02 :: ProgramState
runDay02 =
    let input = toIntCode Input.input // [(1, 12), (2, 2)]
    in run input


