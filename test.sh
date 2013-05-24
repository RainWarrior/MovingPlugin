#!/bin/bash
scalac -Xplugin:movingplugin.jar pluginAnns.scala Test.scala
scala test.Test
