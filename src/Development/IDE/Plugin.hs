
module Development.IDE.Plugin(Plugin(..)) where

import Data.Default
import Development.Shake
import Development.IDE.LSP.Server

data Plugin = Plugin
    {pluginRules :: Rules ()
    ,pluginHandler :: PartialHandlers
    }

instance Default Plugin where
    def = Plugin mempty def

instance Semigroup Plugin where
    Plugin x1 y1 <> Plugin x2 y2 = Plugin (x1<>x2) (y1<>y2)

instance Monoid Plugin where
    mempty = def
