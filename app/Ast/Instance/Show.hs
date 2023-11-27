module Ast.Instance.Show where

import Ast.Print
import Ast.Type

instance Show LispVal where show = showVal
