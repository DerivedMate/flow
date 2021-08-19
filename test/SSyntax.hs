module SSyntax where
import Test.HUnit
import Syntax
import Lexer

syntaxTests 
  = TestList [ TestLabel "BinOp"              testBinOp 
             , TestLabel "IO"                 testIo 
             , TestLabel "Primitives"         testPrimitives
             , TestLabel "Types"              testTypes
             , TestLabel "Func"               testFunc
             , TestLabel "Examples"           testExamples
             , TestLabel "Inline Expressions" testInlineExpr
             {-
             -}
             ]

testableExp :: PReturn Exp -> Either ParseError Exp
testableExp (Left e)  = Left e
testableExp (Right r) = Right . prResult $ r

prun :: String -> Either ParseError Exp
prun = testableExp . flParseString
  
properTree :: Exp -> Either ParseError Exp  
properTree t = testableExp . flFixRootProgram .
  Right $ ParseResult 
              { prResult     = t
              , prNewContext = qcCtxOfString ""
              }

testBinOp 
  = TestList [ TestCase (
                assertEqual "Add Ints" 
                  (properTree (
                    Flow (
                      Cell MNone 
                      ( BinOp OpAdd 
                        (LInt 1) 
                        (LInt 1)
                      )
                    ) Nil
                  ))
                  ( prun "{ + 1 1 }" ) 
               ),
               TestCase (
                assertEqual "Add Vars" 
                  (properTree (
                    Flow (
                      Cell MNone 
                      ( BinOp OpAdd 
                        (Var "a") 
                        (Var "b")
                      )
                    ) Nil
                  ))
                  ( prun "{ + a b }" ) 
               ),
               TestCase (
                assertEqual "Nested" 
                  (properTree (
                    Flow (
                      Cell MNone 
                      ( BinOp OpAdd 
                        ( BinOp OpSub
                          (Var "a")
                          (Var "b")
                        ) 
                        ( BinOp OpAdd
                          (LInt 1)
                          (LInt 3)
                        )
                      )
                    ) Nil
                  ))
                  ( prun "{ + - a b + 1 3 }" ) 
               ),
               TestCase (
                 assertEqual "Enclosed"
                 (properTree (
                   Flow (
                     Cell MNone 
                     ( BinOp OpAdd
                         ( BinOp OpSub
                           (LInt 1)
                           (LInt 2)
                         )
                         ( LInt 3 )
                     )
                   ) Nil
                 ))
                 (prun "{ + ( - 1 2 ) (3) }")
               )
             ]

testIo 
  = TestList [ 
               {--------------------------------:
                   StdIn
               :--------------------------------}
               TestCase (
                assertEqual "IoStdIn: Primitive"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoStdIn TInt ))
                    ) Nil
                  )
                  (prun "{ ~> Int }")
               ),
               TestCase (
                assertEqual "IoStdIn: List"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoStdIn ( TList TString ) ))
                    ) Nil
                  )
                  (prun "{ ~> [Str] }")
               ),




               {--------------------------------:
                   StdOut
               :--------------------------------}
               TestCase (
                assertEqual "IoStdOut: Primitive"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoStdOut (TInt) ))
                    ) Nil
                  )
                  (prun "{ <~ Int }")
               ),
               TestCase (
                assertEqual "IoStdOut: List"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoStdOut ( TList TString ) ))
                    ) Nil
                  )
                  (prun "{ <~ [Str] }")
               ),




               {--------------------------------:
                   File In
               :--------------------------------}
               TestCase (
                assertEqual "IoFileIn: Primitive"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoFileIn 
                                "input.txt"
                                TInt
                            ))
                    ) Nil
                  )
                  (prun "{ `input.txt` ~> Int }")
               ),
               TestCase (
                assertEqual "IoFileIn: List"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoFileIn 
                                "input.txt"
                                ( TList TString )
                            ))
                    ) Nil
                  )
                  (prun "{ `input.txt` ~> [Str] }")
               ),
               TestCase (
                assertEqual "IoFileIn: Long Path"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoFileIn 
                                "~/data/inputs/scores.min.csv"
                                ( TList TString )
                            ))
                    ) Nil
                  )
                  (prun "{ `~/data/inputs/scores.min.csv` ~> [Str] }")
               ),




               {--------------------------------:
                   File Out
               :--------------------------------}
               TestCase (
                assertEqual "IoFileOut: Primitive"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoFileOut 
                                "out.txt"
                                TInt
                            ))
                    ) Nil
                  )
                  (prun "{ `out.txt` <~ Int }")
               ),
               TestCase (
                assertEqual "IoFileOut: List"
                  (properTree $ 
                    Flow (
                      Cell MNone 
                        (Io ( IoFileOut 
                                "out.txt"
                                ( TList TString )
                            ))
                    ) Nil
                  )
                  (prun "{ `out.txt` <~ [Str] }")
               )
             ]

testPrimitives
  = TestList [ 
               {--------------------------------:
                   Int
               :--------------------------------}
               TestCase (
                assertEqual "Int signed +"
                  (properTree ( Flow (Cell MNone (LInt 1239)) Nil ))
                  (prun " { +1239 } ")
               ),
               TestCase (
                assertEqual "Int signed -"
                  (properTree ( Flow (Cell MNone (LInt (-1239))) Nil ))
                  (prun " { -1239 } ")
               ),
               TestCase (
                assertEqual "Int unsigned"
                  (properTree ( Flow (Cell MNone (LInt 1239)) Nil ))
                  (prun " { 1239 } ")
               ),
               TestCase (
                assertEqual "Int only digits"
                  (Left $ ParseError {
                    peContext = ParseContext {
                      ctxColumn = 7,
                      ctxLine = 1,
                      ctxString = "aa1 } "
                    },
                    peExpected = "}",
                    peGot = "a"
                  })
                  (prun " { 012aa1 } ")
               ),




               {--------------------------------:
                   Float
               :--------------------------------}
               TestCase (
                 assertEqual "Float regular"
                 (properTree ( Flow (Cell MNone ( LFloat 1.99 )) Nil ))
                 (prun " { 1.99 } ")
               ),
               TestCase (
                 assertEqual "Float separator only"
                 (properTree ( Flow (Cell MNone ( LFloat 9.0 )) Nil ))
                 (prun " { 9. } ")
               ),
               TestCase (
                 assertEqual "Float no middle signs"
                 (Left $ ParseError {
                    peContext = ParseContext {
                      ctxColumn = 6,
                      ctxLine = 1,
                      ctxString = "+.+12 } "
                    },
                    peExpected = "}",
                    peGot = "+"
                  })
                 (prun " { 12+.+12 } ")
               ),




               {--------------------------------:
                   Bool
               :--------------------------------}
               TestCase (
                 assertEqual "True"
                 (properTree (Flow (Cell MNone ( LBool True )) Nil ))
                 (prun " { True } ")
               ),
               TestCase (
                 assertEqual "False"
                 (properTree (Flow (Cell MNone ( LBool False )) Nil ))
                 (prun " { False } ")
               ),




               {--------------------------------:
                   String
               :--------------------------------}
               TestCase (
                 assertEqual "String empty"
                 (properTree (Flow(Cell MNone ( LString "" )) Nil))
                 (prun "{ `` }")
               ),
               TestCase (
                 assertEqual "String signs"
                 (properTree (Flow (Cell MNone ( LString "~!@#$%^&*(){};:|<>?/,._-=+" )) Nil))
                 (prun "{ `~!@#$%^&*(){};:|<>?/,._-=+` }")
               ),




               {--------------------------------:
                   List
               :--------------------------------}
               TestCase (
                 assertEqual "List Empty"
                 (properTree (Flow (Cell MNone ( LList [] )) Nil))
                 (prun "{ [] }")
               ),
               TestCase (
                 assertEqual "List Singleton"
                 (properTree (Flow (Cell MNone ( LList [LInt 1] )) Nil))
                 (prun "{ [1] }")
               ),
               TestCase (
                 assertEqual "List Regular"
                 (properTree (Flow (Cell MNone ( LList [LInt 1, LInt 2, LInt 3] )) Nil))
                 (prun "{ [1, 2, 3] }")
               ),
               TestCase (
                 assertEqual "List Nested"
                 (properTree (
                  Flow ( 
                   Cell MNone 
                    ( LList [ LList [LList [ LInt 1 ]]
                            , LList [LList [ LInt 2 ]]
                            ] 
                    ))
                   Nil
                 ))
                 (prun "{ [[[1]], [[2]]] }")
               ),




               {--------------------------------:
                   Tuple
               :--------------------------------}
               TestCase (
                 assertEqual "Tuple Empty"
                 (properTree ( Flow (Cell MNone Nil ) Nil))
                 (prun "{ () }")
               ),
               TestCase (
                 {----:
                   Tuples of 1 element are equivalent to that element
                 :----}
                 assertEqual "Tuple Singleton id"
                 (properTree ( Flow (Cell MNone ( LInt 1 ) ) Nil))
                 (prun "{( 1 )}")
               ),
               TestCase (
                 assertEqual "Tuple Regular"
                 (properTree ( Flow (Cell MNone ( LTuple [LInt 1, LInt 2, LInt 3] )) Nil ))
                 (prun "{( 1, 2, 3 )}")
               )
             ]

testTypes
  = TestList [ 
               {--------------------------------:
                   Regular
               :--------------------------------}
               TestCase (
                assertEqual "Primitive"
                (Right $ ParseResult {
                  prResult = TInt,
                  prNewContext = ParseContext 2 1 ""
                })
                (runParser flType (qcCtxOfString "Int"))
               ),
               TestCase (
                assertEqual "Compound"
                (Right $ ParseResult {
                  prResult = TList TInt,
                  prNewContext = ParseContext 2 1 ""
                })
                (runParser flType (qcCtxOfString "[Int]"))
               ),
               -- TestCase (
               --  assertEqual "No Compound of primitive"
               --  (Just (TString,"<Int>"))
               --  (runParser pType "Str<Int>")
               -- ),
               TestCase (
                assertEqual "Function Primitive"
                (Right $ ParseResult {
                  prResult = TFunc TString TInt,
                  prNewContext = ParseContext 2 1 ""
                })
                (runParser flType (qcCtxOfString "Str -> Int"))
               ),
               TestCase (
                assertEqual "Function List"
                (Right $ ParseResult {
                  prResult = TFunc TString (TList TString),
                  prNewContext = ParseContext 2 1 ""
                })
                (runParser flType (qcCtxOfString "Str -> [Str]"))
               ),
               TestCase (
                assertEqual "Function Long"
                (Right $ ParseResult {
                  prResult = TFunc 
                    TInt (
                      TFunc 
                        TString 
                        (TList TString)
                      ),
                  prNewContext = ParseContext 2 1 ""
                })
                (runParser flType (qcCtxOfString "Int->Str -> [Str]"))
               )
             ]

testFunc
  = TestList [ 
      {--------------------------------:
          No agrs
      :--------------------------------}
      TestCase (
        assertEqual "No args, No label"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                Nothing
                []
                TAny
                (Single (LInt 1) )
            )
          ) Nil
        ))
        (prun "{. 1 }")
      ),
      TestCase (
        assertEqual "No args, labeled"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "const_1")
                []
                TAny
                (Single (LInt 1) )
            )
          ) Nil
        ))
        (prun "{ ~const_1. 1 }")
      ),
      TestCase (
        assertEqual "No args, with bare return"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "const_1")
                []
                TInt
                (Single (LInt 1) )
            )
          ) Nil
        ))
        (prun "{ ~const_1.Int 1 }")
      ),
      TestCase (
        assertEqual "No args, with enclosed return"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "const_1")
                []
                TInt
                (Single (LInt 1) )
            )
          ) Nil
        ))
        (prun "{ ~const_1.(Int) 1 }")
      ),




      {--------------------------------:
          Single Arg
      :--------------------------------}
      TestCase (
        assertEqual "Increment, no label"  
        (properTree ( 
          Flow (
            Cell MNone (
              Func 
                Nothing
                [Arg "a" TInt]
                TAny
                (Single (
                  BinOp OpAdd
                    (Var "a")
                    (LInt 1)
                ))
            )
          ) Nil
         ))
         (prun "{a (Int). + a 1}")
      ),
      TestCase (
        assertEqual "Increment, labeled"  
        (properTree ( 
          Flow (
            Cell MNone (
              Func 
                (Just "inc")
                [Arg "a" TInt]
                TAny
                (Single (
                  BinOp OpAdd
                    (Var "a")
                    (LInt 1)
                ))
            )
          ) Nil
         ))
         (prun "{~inc a (Int). + a 1}")
      ),




      {--------------------------------:
          Multiarg
      :--------------------------------}
      TestCase (
        assertEqual "Sum"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "sum_int")
                [ Arg "a" TInt
                , Arg "b" TInt
                ]
                TAny
                (Single (
                  BinOp OpAdd
                    (Var "a")
                    (Var "b")
                ))
            )
          ) Nil
        ))
        (prun "{ ~sum_int a (Int), b(Int). + a b }")
      ),
      TestCase (
        assertEqual "Sum with return"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "sum_int")
                [ Arg "a" TInt
                , Arg "b" TInt
                ]
                TFloat
                (Single (
                  BinOp OpAdd
                    (Var "a")
                    (Var "b")
                ))
            )
          ) Nil
        ))
        (prun "{ ~sum_int a (Int), b (Int). Float + a b }")
      ),

      


      {--------------------------------:
          Return Type
      :--------------------------------}
      TestCase (
        assertEqual "Triple chained addition"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "sum_int")
                [ Arg "a" TInt
                , Arg "b" TInt
                ]
                (TFunc TInt TInt)
                (Single (
                  Flow 
                    (Cell MNone (
                      Func 
                        Nothing
                        [ Arg "c" TInt ]
                        TInt
                        (Single (
                          BinOp OpAdd
                            (BinOp OpAdd
                              (Var "a")
                              (Var "b")
                            )
                            (Var "c")
                          )
                        )
                    ))
                    Nil
                ))
            )
          ) Nil
        ))
        (prun "{ ~sum_int a (Int), b (Int). Int -> Int { c(Int). Int (+ (+ a b) c) } }")
      ),



      {--------------------------------:
          Conditional
      :--------------------------------}
      TestCase (
        assertEqual "Conditional: max2"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "max2")
                [ Arg "a" TInt
                , Arg "b" TInt
                ]
                TAny
                (Cond
                  ( BinOp OpGt
                      (Var "a")
                      (Var "b")
                  )
                  ( Var "a" )
                  (Single
                    (Var "b")
                  )
                )
            )
          ) Nil
        ))
        (prun "{~max2 a(Int), b(Int). \
               \  > a b | a           \
               \        | b           \
               \}")
      ),
      TestCase (
        assertEqual "Conditional: max3"
        (properTree (
          Flow (
            Cell MNone (
              Func 
                (Just "max3")
                [ Arg "a" TInt
                , Arg "b" TInt
                , Arg "c" TInt
                ]
                TAny
                (Cond
                  ( BinOp OpAnd
                      ( BinOp OpGt
                          ( Var "a" )
                          ( Var "b" )
                      )
                      ( BinOp OpGt
                          ( Var "b" )
                          ( Var "c" )
                      )
                  ) ( Var "a" )
                  (Cond
                    ( BinOp OpAnd
                      ( BinOp OpGt
                          ( Var "b" )
                          ( Var "a" )
                      )
                      ( BinOp OpGt
                          ( Var "a" )
                          ( Var "c" )
                      )
                    ) ( Var "b" )
                    ( Single ( Var "c" ) )
                  )
                )
            ) 
          ) Nil
        ))
        (prun "{~max3 a(Int), b(Int), c(Int). \
               \  && > a b > b c | a \
               \  && > b a > a c | b \
               \                 | c \
               \}")
      )
    ]


testInlineExpr 
  = TestList [  {--------------------------------:
                    Capture Literal
                :--------------------------------}

                TestCase (
                assertEqual "Single Capture LInt" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSingle (LInt 1)
                      )
                    ))
                    Nil
                ))
                (prun "{ &1 }")
               ),
               TestCase (
                assertEqual "Slice Capture LInts" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSlice 
                          (LInt 1)
                          (Just (LInt 12))
                      )
                    ))
                    Nil
                ))
                (prun "{ &1:12 }")
               ),
               TestCase (
                assertEqual "Slice Capture One LInt" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSlice 
                          (LInt 0)
                          Nothing
                      )
                    ))
                    Nil
                ))
                (prun "{ &0: }")
               ),



               {--------------------------------:
                   Capture BinOp
               :--------------------------------}
               TestCase (
                assertEqual "Single Capture BinOp" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSingle (
                          BinOp OpAdd 
                            (LInt 1)
                            (LInt 39)
                        )
                      )
                    ))
                    Nil
                ))
                (prun "{ &(+ 1 39) }")
               ),
               TestCase (
                assertEqual "Single Capture Nested BinOp" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSingle (
                          BinOp OpAdd 
                            (BinOp OpAdd
                              (LInt 1)
                              (LInt 2)
                            )
                            (BinOp OpSub
                              (LInt 3)
                              (LInt 1)
                            )
                        )
                      )
                    ))
                    Nil
                ))
                (prun "{ &(+ (+ 1 2) (- 3 1)) }")
               ),




               {--------------------------------:
                   Capture Io
               :--------------------------------}
               TestCase (
                assertEqual "Single Capture In Int" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSingle (
                          Io (IoStdIn TInt)
                        )
                      )
                    ))
                    Nil
                ))
                (prun "{ &(~> Int) }")
               ),
               TestCase (
                assertEqual "Slice Capture In Int" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSlice 
                          (Io (IoStdIn TInt))
                          Nothing
                      )
                    ))
                    Nil
                ))
                (prun "{ &(~> Int): }")
               ),
               TestCase (
                assertEqual "Slice Capture In Ints" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSlice 
                          (Io (IoStdIn TInt))
                          (Just (Io (IoStdIn TInt)))
                      )
                    ))
                    Nil
                ))
                (prun "{ &(~> Int):(~> Int) }")
               ),
               TestCase (
                assertEqual "Slice Capture Vars" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Capture (
                        CSlice 
                          (Var "l")
                          (Just (Var "r"))
                      )
                    ))
                    Nil
                ))
                (prun "{ &l:r }")
               ),
               


               {--------------------------------:
                   Casting
               :--------------------------------}
               TestCase (
                assertEqual "Capture Bare Cast" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Casting
                        (Capture (CSingle (LInt 19)))
                        TInt
                    ))
                    Nil
                ))
                (prun "{ &19 :: Int }")
               ),
               TestCase (
                assertEqual "Capture Slice Bare Cast" 
                (properTree (
                  Flow 
                    (Cell MNone (
                      Casting
                        (Capture (CSlice (LInt 19) Nothing))
                        TInt
                    ))
                    Nil
                ))
                (prun "{ &19: :: Int }")
               )
             ]

testExamples 
  = TestList [ TestCase (
                do 
                let 
                  exp = Program (Flow (Cell MNone (LTuple [Io (IoStdIn TInt),LInt 1])) (Flow (Cell MNone (Func (Just "fact") [Arg "a" TInt,Arg "b" TInt] TAny (Cond (BinOp OpGt (Var "a") (LInt 1)) (Flow (Cell MNone (LTuple [BinOp OpSub (Var "a") (LInt 1),BinOp OpMul (Var "a") (Var "b")])) (Flow (FRef "fact") Nil)) (Single (Flow (Cell MNone (Var "b")) Nil))))) (Flow (Cell MNone (Io (IoStdOut TInt))) Nil))) Nil
                  aux :: PReturn Exp -> Assertion
                  aux (Right r) = assertEqual "Factorial" exp  (prResult r)
                  aux (Left msg) = assertFailure $ show msg

                  ppErrorAux (Left msg) = assertFailure msg
                  ppErrorAux (Right r)  = aux r
                
                  in flParseFile "./test/example/fact.hf" >>= ppErrorAux
               ) 

             ]