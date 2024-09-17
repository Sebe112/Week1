module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval, printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Lambda expression evaluation" $
        case eval envEmpty (Lambda "x" (Add (Var "x") (CstInt 1))) of
          Right (ValFun _ "x" body) -> printExp body @?= "(Var x + Integer 1)"
          _ -> error "Expected a ValFun",
      --
      testCase "Apply Lambda function" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
          @?= Right (ValInt 6),
      --
      testCase "TryCatch with Add" $
        eval envEmpty (TryCatch (Add (CstInt 2) (CstInt 3)) (CstInt 42))
          @?= Right (ValInt 5),
      --
      testCase "TryCatch catches error" $
        eval envEmpty (TryCatch (Var "x") (CstInt 42))
          @?= Right (ValInt 42),
      -- Test for printing a simple integer
      testCase "CstInt 5" $
        printExp (CstInt 5) @?= "Integer 5",
      -- Test for printing a boolean value
      testCase "CstBool True" $
        printExp (CstBool True) @?= "Boolean True",
      -- Test for addition
      testCase "Addition" $
        printExp (Add (CstInt 2) (CstInt 3)) @?= "(Integer 2 + Integer 3)",
      -- Test for Let expression
      testCase "Let expression" $
        printExp (Let "x" (CstInt 5) (Add (Var "x") (CstInt 3)))
          @?= "let x = Integer 5 in (Var x + Integer 3)",
      -- Test for Lambda expression
      testCase "Lambda expression" $
        printExp (Lambda "x" (Mul (Var "x") (CstInt 2)))
          @?= "\\x -> (Var x * Integer 2)",
      -- Test for TryCatch expression
      testCase "TryCatch expression" $
        printExp (TryCatch (Div (Var "x") (CstInt 0)) (CstInt 42))
          @?= "(try (Var x / Integer 0) catch Integer 42)"
    ]
