module Ast.Instance.Show
where

import Ast.Type
import Ast.Print

instance Show LispVal where show = showVal
