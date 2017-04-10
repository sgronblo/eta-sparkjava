{-# LANGUAGE MagicHash, FlexibleContexts #-}

module SparkJava where

import Java

data {-# CLASS "spark.Request" #-} Request = Request (Object# Request) deriving Class
data {-# CLASS "spark.Response" #-} Response = Response (Object# Response) deriving Class
data {-# CLASS "spark.Route" #-} Route a = Route (Object# (Route a)) deriving Class
data {-# CLASS "spark.Filter" #-} Filter = Filter (Object# Filter) deriving Class
data {-# CLASS "spark.ResponseTransformer" #-} ResponseTransformer a = ResponseTransformer (Object# (ResponseTransformer a)) deriving Class

-- Instance methods

foreign import java unsafe body :: Java Request JString
foreign import java unsafe params :: JString -> Java Request (Maybe JString)

-- Wrappers for functional interfaces

foreign import java unsafe "@wrapper handle" mkRoute :: (Request -> Response -> Java (Route a) Object) -> Route a
foreign import java unsafe "@wrapper handle" filter :: (Request -> Response -> Java Filter ()) -> Filter
foreign import java unsafe "@wrapper render" mkResTransformer :: (a -> Java (ResponseTransformer a) String) -> ResponseTransformer a

-- Helpers for handling up/downcasting in generic functional interfaces

route :: (Extends a Object) => (Request -> Response -> Java (Route a) a) -> Route a
route handler = mkRoute (\req res -> fmap superCast $ handler req res)
resTransformer :: (Extends a Object) => (a -> Java (ResponseTransformer a) String) -> ResponseTransformer a
resTransformer transformer = mkResTransformer (\val -> transformer $ unsafeCast val)

-- Static methods in Spark

foreign import java unsafe "@static spark.Spark.get" get :: String -> Route String -> IO ()
foreign import java unsafe "@static spark.Spark.connect" connect :: String -> Route String -> IO ()
foreign import java unsafe "@static spark.Spark.connect" connectTransformed :: String -> Route a -> ResponseTransformer a -> IO ()
foreign import java unsafe "@static spark.Spark.port" port :: Int -> IO ()