> {-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans -XMultiParamTypeClasses -XFlexibleContexts #-} 

This file contains the definition of the abstract syntax for the 
WhilePP programming language, as well as a pretty printer. You 
should not need to modify this file.

> module WhilePP where

> import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
> import qualified Text.PrettyPrint.HughesPJ as PP

As before, we have variables, and expressions.

Variables can be assigned to:
Values
Functions
Pointers
Lists

Variables are just references. 
Therefore, we implement higher order referencing by allowing variables to 
refer to other variables. 

> type Variable = String
>
> data Evalable =
>     Expr Expression
>   | Stmt Statement
>   deriving (Show, Eq)
> 
> data Value =
>     Null
>   | IntVal Int
>   | BoolVal Bool
>   | DoubleVal Double
>   | CharVal Char
>   | List [Value]
>   | Var Variable
>   deriving (Show, Eq)
>
> data Expression =
>     Val Value
>   | Op  Bop Evalable Evalable
>   | Dereference Variable -- Can only dereference Variables
>   deriving (Show, Eq)
>
> data Bop = 
>     Plus     
>   | Minus    
>   | Times    
>   | Divide   
>   | Gt        
>   | Ge       
>   | Lt       
>   | Le       
>   deriving (Show, Eq)

Programs in the language are simply values of the type
Functions need a RETURN statement
Functions need a way to passing in ARGUMENTS
--> Implement STACK

> data Statement =
>     Assign Expression Expression
>     -- in order to be able to do *x = 5
>     -- our interpreter should throw errors when
>     -- the first expression is NOT Expression Val Var OR
>     -- NOT Expression Dereference 
>   | AssignFunc Expression Statement
>     -- Used to assign variables to function return values
>     -- e.g. x = foo(5,3)
>   | AssignRef Expression Expression
>     -- Used to assign references to an expression
>     -- e.g. x := 5
>   | AssignFuncRef Expression Statement
>     -- Used to assign references to function return values
>     -- e.g. x := foo(5,3)
>   | If Expression Statement Statement
>   | While Expression Statement     
>   | Sequence Statement Statement   
>   | Skip
>   | Print String Evalable
>   | Throw Expression
>   | Try Statement Variable Statement
>   | Return Expression
>   | CallFunction String [Expression]
>   deriving (Show, Eq)

> -- ([Arg names], function body)
> type Function = ([String], Statement)
> --  deriving (Show, Eq)

----------------------------

-- Pretty printing for the WHILE programming language


> class PP a where
>   pp :: a -> Doc
> 
> instance PP Bop where
>   pp Plus   = PP.char '+'
>   pp Minus  = PP.char '-'
>   pp Times  = PP.char '*'
>   pp Divide = PP.char '/'
>   pp Gt     = PP.char '>'
>   pp Ge     = PP.text ">="
>   pp Lt     = PP.char '<'
>   pp Le     = PP.text "<="
> 
> instance PP Value where
>   pp Null = PP.text "null"
>   pp (IntVal i)  = PP.int i 
>   pp (BoolVal b) = if b then PP.text "true" else PP.text "false"
>   pp (DoubleVal d) = PP.double d
>   pp (CharVal c)  = PP.char c
>   pp (List v) = PP.char '[' <+> pp v <+>PP.char ']'
>   pp (Var v) = PP.text v
> 
> 
> instance (PP a) => PP [a] where
>   pp [] = PP.text ""
>   pp (x:[]) = pp x
>   pp (x:xs) = pp x <+> PP.text ", " <+> pp xs
>
> instance PP Evalable where
>   pp (Expr e) = pp e
>   pp (Stmt s) = pp s
>
> 
> instance PP Expression where
>  -- pp (Var x) = PP.text x
>   pp (Val x) = pp x
>   pp (Dereference v) = PP.char '*' <+> PP.text v
>   pp e@(Op _ _ _) = ppPrec 0 (Expr e)  where
>      ppPrec n (Expr (Op bop e1 e2)) =
>         parens (level bop < n) $
>            ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2 
>      ppPrec _ e' = pp e'
>      parens b = if b then PP.parens else id
> 
> --instance PP Function where
> --  pp (str, stmt) = pp str <> pp stmt 
>
> -- use the C++ precendence level table
> level :: Bop -> Int
> level Plus   = 3
> level Minus  = 3 
> level Times  = 5
> level _      = 8
> 
> 
> 
> instance PP Statement where
>   pp (Assign e1 e2) = pp e1 <+> PP.text "=" <+> pp e2
>   pp (AssignFunc e1 s) = pp e1 <+> PP.text "=" <+> pp s
>   pp (AssignRef e1 e2) = pp e1 <+> PP.text ":=" <+> pp e2
>   pp (AssignFuncRef e s) = pp e <+> PP.text ":=" <+> pp s
>   pp (If e s1 s2) = 
>     PP.vcat [PP.text "if" <+> pp e <+> PP.text "then",
>          PP.nest 2 (pp s1), 
>          PP.text "else",
>          PP.nest 2 (pp s2),
>          PP.text "endif"]
>   pp (While e s)  = 
>      PP.vcat [PP.text "while" <+> pp e <+> PP.text "do",
>               PP.nest 2 (pp s),
>               PP.text "endwhile"]            
>   pp (Sequence s1@(Sequence _ _) s2) = 
>        PP.parens (pp s1) <> PP.semi $$ pp s2     
>   pp (Sequence s1 s2) = pp s1 <> PP.semi $$ pp s2
>   pp Skip = PP.text "skip"
>   pp (Print s e) = PP.text "print" <+> PP.doubleQuotes (PP.text s) <+> pp e
>   pp (Throw e) = PP.text "throw" <+> pp e 
>   pp (Try s1 v s2) = PP.vcat [ PP.text "try", 
>                                PP.nest 2 (pp s1), 
>                                PP.text "catch" <+> PP.text v <+> PP.text "with",
>                                PP.nest 2 (pp s2),
>                                PP.text "endwith" ]
>   pp (Return e) = PP.text "return" <+> pp e
>   pp (CallFunction fname args) = 
>      PP.text fname <+> PP.char '(' <+> 
>      pp args <+>
>      PP.char ')'
> 
> 
> 
> display :: PP a => a -> String
> display = show . pp
> 




