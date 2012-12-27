>module Language.Haskell.Her.Parsec
> (haskellTokenStream
> ,litC
> ,opeC
> ,cloC
> ,uidC
> ,lidC
> ,kwC
> ,symC
> ,spcC
> ,comC
> ,urkC
> ,nlC
> ,lit
> ,ope
> ,clo
> ,uid
> ,lid
> ,kw
> ,sym
> ,semi
> ,spc
> ,com
> ,urk
> ,nl) where

>import Language.Haskell.Her.HaLay hiding
> (ope
> ,clo
> ,uid
> ,lid
> ,sym
> ,spc)
>import Text.ParserCombinators.Parsec
>import Text.Parsec.Prim
>import Data.Functor.Identity
>import Text.Parsec.Pos

>markLines
> :: String
> -> [(Int,Tok)]
> -> [(SourcePos,Tok)]
>markLines
> fileName
> toks
> = map
>    (\((col,tok),line) ->
>      (newPos fileName line col,tok))
> $ snd
> $ foldr
>    markLines'
>    (0,[])
>    toks
> where
>  markLines'
>    tok
>    (oldLine,markedToks)
>   = case tok of
>      (_,NL (_,newLine)) -> (newLine, (tok,newLine):markedToks)
>      _ -> (oldLine,(tok,oldLine):markedToks)

>haskellTokenStream
> :: String
> -> String
> -> [(SourcePos,Tok)]
>haskellTokenStream
> haskellCode
> fileName
> = markLines
>    fileName
> $ tokenize
>    (((fileName,0),0),haskellCode)

>posTok (pos,_) = pos
>showT  (_,t)   = tokOut t
>justIf True v  = Just v
>justIf False _ = Nothing

herToken
 :: Text.Parsec.Prim.Stream
     s
     Data.Functor.Identity.Identity
     (SourcePos, Tok)
 => ((SourcePos, Tok) -> Maybe a)
 -> Text.Parsec.Prim.Parsec s u a

>herToken matchT = token showT posTok matchT

----

>litC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Lit c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>lit
> content
> = litC (\c -> c == content)

----

>opeC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Ope c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>ope
> content
> = opeC (\c -> c == content)

----

>cloC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Clo c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>clo
> content
> = cloC (\c -> c == content)

----

>uidC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Uid c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>uid
> content
> = uidC (\c -> c == content)

----

>lidC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Lid c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>lid
> content
> = lidC (\c -> c == content)

----

>kwC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(KW c))  = justIf (checker c) t
>   matchT (_,_)         = Nothing

>kw
> content
> = kwC (\c -> c == content)

----

>symC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Sym c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>sym
> content
> = symC (\c -> c == content)

----

>semi :: GenParser (SourcePos,Tok) st Tok
>semi
> = herToken matchT
>   where
>   matchT (_,t@Semi)    = Just t
>   matchT (_,_)         = Nothing

----

>spcC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Spc c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>spc
> content
> = spcC (\c -> c == content)

----

>comC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Com c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>com
> content
> = comC (\c -> c == content)

----

>urkC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(Urk c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>urk
> content
> = urkC (\c -> c == content)

----

>nlC
> checker
> = herToken matchT
>   where
>   matchT (_,t@(NL c)) = justIf (checker c) t
>   matchT (_,_)         = Nothing

>nl
> content
> = nlC (\c -> c == content)
