{-# LANGUAGE QuasiQuotes #-}
module View.Layout where

import View.Helpers

layout :: Html -> Html
layout body = [shamlet|
  <html>
    <body>
      <div>
        <a href="/people">People
        <a href="/equipment">Equipment
        <a href="/possessionContracts">Possession Contracts
      <div>
        ^{body}
  |]
