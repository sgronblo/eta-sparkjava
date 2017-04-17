{-# LANGUAGE OverloadedStrings #-}

module SparkTests where

import SparkJava
import Test.HUnit.Base

startTestServer :: IO ()
startTestServer = get "/hello/:name" $ route (\_ _ -> return "Hello World")

tests :: IO [Test]
tests = do
    startTestServer
    return [ TestCase $ assertEqual "Tautology" False True ]