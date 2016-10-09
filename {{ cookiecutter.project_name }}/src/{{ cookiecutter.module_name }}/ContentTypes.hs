module {{ cookiecutter.module_name }}.ContentTypes
  ( HTML
  ) where

import Network.HTTP.Media ((//), (/:))
import Servant (Accept(..))

-- | HTML content type.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
