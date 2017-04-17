{-# LANGUAGE MagicHash, FlexibleContexts, MultiParamTypeClasses, TypeOperators #-}

module SparkJava where

import Java

data {-# CLASS "spark.Request" #-} Request = Request (Object# Request) deriving Class
data {-# CLASS "spark.Response" #-} Response = Response (Object# Response) deriving Class
data {-# CLASS "spark.Route" #-} Route a = Route (Object# (Route a)) deriving Class
data {-# CLASS "spark.Filter" #-} Filter = Filter (Object# Filter) deriving Class
data {-# CLASS "spark.Filter[]" #-} FilterArray = FilterArray (Object# FilterArray) deriving Class
data {-# CLASS "spark.ResponseTransformer" #-} ResponseTransformer a = ResponseTransformer (Object# (ResponseTransformer a)) deriving Class

instance JArray Filter FilterArray

-- Helpers for creating functions

-- Wraps Java returning array function in a function that takes an Eta list instead
toVariadicJava :: (JArray a arr) => (arr -> Java any b) -> ([a] -> Java any b)
toVariadicJava f = (\aList -> arrayFromList aList >>= f)

-- Wraps IO returning array function in a function that takes an Eta list instead
toVariadicIo :: (JArray a arr) => (arr -> IO b) -> ([a] -> IO b)
toVariadicIo f = (\aList -> (java $ arrayFromList aList) >>= f)

-- Instance methods

foreign import java unsafe body :: Java Request JString
foreign import java unsafe params :: JString -> Java Request (Maybe JString)

-- Wrappers for functional interfaces

foreign import java unsafe "@wrapper handle" mkRoute :: (a <: Object) => (Request -> Response -> Java (Route a) Object) -> Route a
foreign import java unsafe "@wrapper handle" filter :: (Request -> Response -> Java Filter ()) -> Filter
foreign import java unsafe "@wrapper render" mkResTransformer :: (a <: Object) => (a -> Java (ResponseTransformer a) JString) -> ResponseTransformer a

-- Helpers for handling up/downcasting in generic functional interfaces

route :: (Class a) => (Request -> Response -> Java (Route a) a) -> Route a
route handler = mkRoute (\req res -> fmap superCast $ handler req res)
resTransformer :: (Class a) => (a -> Java (ResponseTransformer a) JString) -> ResponseTransformer a
resTransformer transformer = mkResTransformer (\val -> transformer $ unsafeCast val)

-- Private static imports

foreign import java unsafe "@static spark.Spark.after" privateAfter :: FilterArray -> IO ()
foreign import java unsafe "@static spark.Spark.after" privatePrefixedAfter :: String -> FilterArray -> IO ()
foreign import java unsafe "@static spark.Spark.before" privateBefore :: FilterArray -> IO ()
foreign import java unsafe "@static spark.Spark.before" privatePrefixedBefore :: String -> FilterArray -> IO ()

-- Public static methods

foreign import java unsafe "@static spark.Spark.get" get :: JString -> Route JString -> IO ()
foreign import java unsafe "@static spark.Spark.get" getTransformed :: (a <: Object) => JString -> Route a -> ResponseTransformer a -> IO ()
foreign import java unsafe "@static spark.Spark.connect" connect :: String -> Route JString -> IO ()
foreign import java unsafe "@static spark.Spark.connect" connectTransformed :: (a <: Object) => JString -> Route a -> ResponseTransformer a -> IO ()
foreign import java unsafe "@static spark.Spark.port" port :: Int -> IO ()
foreign import java unsafe "@static spark.Spark.awaitInitialization" awaitInitialization :: IO ()

-- Re-exported static methods

after :: [Filter] -> IO ()
after = toVariadicIo privateAfter

prefixedAfter :: String -> [Filter] -> IO ()
prefixedAfter prefix = toVariadicIo $ privatePrefixedAfter prefix

before :: [Filter] -> IO ()
before = toVariadicIo privateBefore