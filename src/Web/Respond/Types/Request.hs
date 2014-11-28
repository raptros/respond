{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Web.Respond.Types.Request where


import qualified Data.ByteString.Lazy as LBS

import Web.Respond.Types.Errors

-- | something that can be pulled from the body, restricted to
-- a ReportableError type.
class ReportableError e => FromBody e a | a -> e where
    -- | parse the body. note that the body is provided as a lazy
    -- ByteString. how that ByteString is loaded depends on the caller of
    -- fromBody.
    fromBody :: LBS.ByteString -> Either e a

