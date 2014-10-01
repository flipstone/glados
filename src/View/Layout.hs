{-# LANGUAGE QuasiQuotes #-}
module View.Layout where

import View.Helpers

layout :: Html -> Html
layout body = [shamlet|
  <html>
    <head>
      <title>GlaDOS
      <link rel="stylesheet" href="/style/main.css" type="text/css">
    <body>
      <div class="navbar">
        <a href="/">Home
        <a href="/people">People
        <a href="/equipment">Equipment
        <a href="/doors">Doors
        <a href="/doorKeys">Keys
        <a href="/possessionContracts">Possession Contracts
        <a href="/agreements">Agreements
        <a href="/fobs">Fobs
        <a href="/fobAssignments">Fob Assignments
      <div class="container">
        ^{body}
  |]
