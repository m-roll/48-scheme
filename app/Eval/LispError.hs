module Eval.LispError (LispError (..), Show) where -- shouldnt be tied to eval since parsing errors here too

import Eval.LispError.Instance ()
import Eval.Type (LispError (..))
