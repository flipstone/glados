{-# LANGUAGE QuasiQuotes #-}
module View.Welcome where

import View.Helpers
import View.Layout

welcomeView :: Html
welcomeView = layout [shamlet|
  <h1>Welcome to GlaDOS

  <p>
    Use the nav bar up to to get things done! If you're not
    sure where to go, consult the glossary below.

  <h2>Glossary

  <dl>
    <dt>Door
    <dd>
      A door somewhere in the lab. Includes
      the hardware address of the Synapse chip
      that is at that door.

    <dt>Fob
    <dd>
      A physical object containing a unique token
      for opening doors.

    <dt>Person
    <dd>
      A human who might enter the lab

    <dt>Fob Assignment
    <dd>
      A record of when a Fob was given to a Person.
      The assignment can optionally expire.

    <dt>Door Key
    <dd>
      A record of when it was determined that Person
      should be able to open a given door.

      The key can optionally expire.

    <dt>Equipment
    <dd>
      A piece of equipment sitting in the lab

    <dt>Possession Contract
    <dd>
      A record recognizing the terms of how Equipment
      came to the lab from a specific Person.

|]
