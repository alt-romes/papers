{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ExtendExtend where

import AST
import Extend

instance Instance Decorated "==" (Expr Decorated -> Expr Decorated -> Bool) where
    definition (Lit a b) (Lit c d) = a == c && b == d

