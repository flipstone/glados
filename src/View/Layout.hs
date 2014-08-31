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
        <a href="/doors">Doors
        <a href="/possessionContracts">Possession Contracts
        <a href="/agreement">Agreement
        <a href="/fob">Fob
      <div>
        ^{body}
  |]
