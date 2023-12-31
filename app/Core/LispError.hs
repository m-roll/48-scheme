module Core.LispError (LispError (..), Show) where -- shouldnt be tied to eval since parsing errors here too

import Core.LispError.Instance ()
import Core.Type (LispError (..))
