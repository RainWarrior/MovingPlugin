#!/bin/bash
mkdir classes
cp scalac-plugin.xml classes/
scalac MovingPlugin.scala -d classes
(cd classes; jar -cf ../movingplugin.jar .)
rm -r classes
