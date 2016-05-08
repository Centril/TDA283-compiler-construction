module Backend.Example where

import Backend.LLVM.LLVMAst
import Backend.LLVM.LLVMApi
import Backend.LLVM.Print

buildTestExecutable :: IO ()
buildTestExecutable = buildExecutable createTestLLVMAst "/tmp/test"

printTestLLVMAst :: IO()
printTestLLVMAst = putStrLn $ printLLVMAst createTestLLVMAst

createTestLLVMAst :: LLVMAst
createTestLLVMAst = LLVMAst
    [LConstGlobal
        "evenstring"
        (LArray 6 (LInt 8))
        "Even!",
    LConstGlobal
        "oddstring"
        (LArray 5 (LInt 8))
        "Odd!"]
    [LFunDecl
        LVoid
        "printString"
        [LPtr (LInt 8)]]
    [LFunDef
        (LInt 1)
        "main"
        [LArg (LInt 32) "n"]
        [
            LLabel
                "entry",
            LAssign
                "t1" (LCall (LInt 1)
                    (LFunRef "even" [LTValRef (LInt 32) (LVInt 20)])),
            LCBr
                (LTValRef (LInt 1) (LRef "t1")) "then" "else",
            LLabel
                "then",
            LAssign
                "t2" (LGElemPtr (LPtr (LArray 6 (LInt 8))) "evenstring"
                    (LInt 32, 0) [(LInt 32, 0)]),
            LVCall
                (LFunRef "printString" [LTValRef (LPtr (LInt 8)) (LRef "t2")]),
            LABr
                "kont",
            LLabel
                "else",
            LAssign
                "t3" (LGElemPtr (LPtr (LArray 5 (LInt 8))) "oddstring"
                        (LInt 32, 0) [(LInt 32, 0)]),
            LVCall
                (LFunRef "printString" [LTValRef (LPtr (LInt 8)) (LRef "t3")]),
            LABr
                "kont",
            LLabel
                "kont",
            LRet
                (LTValRef (LInt 1) (LVInt 0))],
    LFunDef
        (LInt 1)
        "even"
        [LArg (LInt 32) "n"]
        [
            LLabel
                "entry",
            LAssign
                "v1" (LAlloca (LInt 32)),
            LStore
                (LTValRef (LInt 32) (LRef "n"))
                (LTValRef (LPtr (LInt 32)) (LRef "v1")),
            LAssign
                "t1" (LLoad (LTValRef (LPtr (LInt 32)) (LRef "v1"))),
            LAssign
                "t2" (LICmp LEq (LTValRef (LInt 32) (LRef "t1")) (LVInt 0)),
            LCBr
                (LTValRef (LInt 1) (LRef "t2")) "then" "else",
            LLabel
                "then",
            LRet
                (LTValRef (LInt 1) (LVInt 1)),
            LLabel
                "else",
            LAssign
                "t3" (LLoad (LTValRef (LPtr (LInt 32)) (LRef "v1"))),
            LAssign
                "t4" (LSub (LTValRef (LInt 32) (LRef "t3")) (LVInt 1)),
            LAssign
                "t5" (LCall (LInt 1)
                    (LFunRef "odd" [LTValRef (LInt 32) (LRef "t4")])),
            LRet
                (LTValRef (LInt 1) (LRef "t5")),
            LLabel
                "kont",
            LUnreachable],
    LFunDef
        (LInt 1)
        "odd"
        [LArg (LInt 32) "n"]
        [
            LLabel
                "entry",
            LAssign
                "v1" (LAlloca (LInt 32)),
            LStore
                (LTValRef (LInt 32) (LRef "n"))
                (LTValRef (LPtr (LInt 32)) (LRef "v1")),
            LAssign
                "t1" (LLoad (LTValRef (LPtr (LInt 32)) (LRef "v1"))),
            LAssign
                "t2" (LICmp LEq (LTValRef (LInt 32) (LRef "t1")) (LVInt 0)),
            LCBr
                (LTValRef (LInt 1) (LRef "t2")) "then" "else",
            LLabel
                "then",
            LRet
                (LTValRef (LInt 1) (LVInt 0)),
            LLabel
                "else",
            LAssign
                "t3" (LLoad (LTValRef (LPtr (LInt 32)) (LRef "v1"))),
            LAssign
                "t4" (LSub (LTValRef (LInt 32) (LRef "t3")) (LVInt 1)),
            LAssign
                "t5" (LCall (LInt 1)
                    (LFunRef "even" [LTValRef (LInt 32) (LRef "t4")])),
            LRet
                (LTValRef (LInt 1) (LRef "t5")),
            LLabel
                "kont",
            LUnreachable]]