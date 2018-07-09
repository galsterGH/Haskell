module CpuEmulator where
import qualified Data.HashMap.Strict as HM
import Data.Bits
import Control.Applicative
import Data.List.Split
import Data.Char
import Data.Word
import Data.Maybe
import Debug.Trace

type Register = Int
type Registers = HM.HashMap Register UInt
type UInt = Word32

data Instruction =  MOVR {r1:: Register, r2 :: Register}    |
                    MOVD {d :: UInt, r :: Register}         |
                    ADD  {r1 :: Register, r2 ::Register}    |
                    DEC  {r :: Register}                    |
                    INC  {r :: Register}                    |
                    INV  {r :: Register}                    |
                    JMP  {d :: UInt}                        |
                    JZ   {d :: UInt}                        |
                    NOP
                    deriving (Show)

type Instructions = HM.HashMap UInt Instruction
data Cpu = Cpu {ip :: UInt, regs :: Registers, instructions :: Instructions } deriving (Show)


cpuEmulator subroutine = readResult . emulate . initCpu 1 $ subroutine

initCpu :: UInt -> [String] -> Cpu
initCpu n [] = foldl (\(Cpu ip regs insts) i -> Cpu ip (HM.insert i 0 regs) insts)
                                (Cpu 1 HM.empty HM.empty) [0..42]

initCpu i (s:xs) = case retCpu of
                 (Cpu ip regs inst) -> (Cpu ip regs (HM.insert i (parseInstruction s) inst))
                 where retCpu = initCpu (i + 1) xs

parseInstruction s = case (words s) of
                     ("NOP":[]) -> NOP
                     ("MOV":xs) -> parseMove.head $  xs
                     ("ADD":xs) -> parseAdd.head $   xs
                     ("DEC":xs) -> parseDec.head $   xs
                     ("INC":xs) -> parseInc.head $   xs
                     ("INV":xs) -> parseInv.head $   xs
                     ("JMP":xs) -> parseJump.head $  xs
                     ("JZ" :xs) -> parseJz.head $    xs

makeNum a b = ((ord a) - (ord '0'))*10 + (ord b) - (ord '0')

parseMove xs =  case xs of
                ('R':a:b:',':'R': c:d:[]) -> MOVR  (makeNum a b) (makeNum c d)
                _ -> MOVD  (read . head  $ pr)  (read . tail . last  $ pr)
                 where pr = splitOn "," xs

parseAdd ('R':a:b:',':'R':c:d:[]) = ADD (makeNum a b) (makeNum c d)
parseDec ('R':a:b:[]) = DEC (makeNum a b)
parseInc ('R':a:b:[]) = INC (makeNum a b)
parseInv ('R':a:b:[]) = INV (makeNum a b)
parseJump xs = JMP (read xs)
parseJz   xs = JZ (read xs)

emulate :: Cpu -> Cpu
emulate cpu@(Cpu ip regs insts) = fromJust (((HM.lookup ip insts) >>= (\ins -> return . perform ins $ cpu)) <|> (Just cpu))

perform :: Instruction -> Cpu -> Cpu
perform (MOVR r1 r2) (Cpu i regs insts) = emulate (Cpu (i + 1) (HM.insert r2 readR1 regs) insts)
                                          where readR1 = fromJust (HM.lookup r1 regs)

perform (MOVD d r2) (Cpu i regs insts) = emulate (Cpu (i + 1) (HM.insert r2 d regs) insts)
perform (ADD r1 r2) (Cpu i regs insts) = emulate (Cpu (i + 1) (HM.insert r1 addRegs regs) insts)
                                         where addRegs = readr1 + readr2
                                               readr1  = fromJust (HM.lookup r1 regs)
                                               readr2  = fromJust (HM.lookup r2 regs)

perform (DEC r1) (Cpu i regs insts) = emulate (Cpu (i + 1) (HM.insert r1 (readr1 - 1) regs) insts)
                                      where readr1 = fromJust (HM.lookup r1 regs)

perform (INC r1) (Cpu i regs insts) = emulate (Cpu (i + 1) (HM.insert r1 (readr1 + 1) regs) insts)
                                      where readr1 = fromJust (HM.lookup r1 regs)

perform (INV r1) (Cpu i regs insts) = emulate (Cpu (i + 1) (HM.insert r1  (complement readr1) regs) insts)
                                      where readr1 = fromJust (HM.lookup r1 regs)

perform (JMP d) (Cpu i regs insts) = emulate (Cpu d regs insts)

perform (JZ d) (Cpu i regs insts) = case (HM.lookup 0 regs) of
                                    (Just 0) -> emulate (Cpu d regs insts)
                                    _ -> emulate (Cpu (i + 1) regs insts)

perform NOP (Cpu i regs insts) = emulate (Cpu (i + 1) regs insts)

readResult :: Cpu -> String
readResult (Cpu i regs insts) = show . fromJust $ (HM.lookup 42 regs)
