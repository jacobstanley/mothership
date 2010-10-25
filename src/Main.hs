{-# LANGUAGE OverloadedStrings #-}

module Main where

import Snap

------------------------------------------------------------------------

main :: IO ()
main = httpServe' defaultConfig site

------------------------------------------------------------------------

site :: Snap ()
site = writeBS "gitsnap"
