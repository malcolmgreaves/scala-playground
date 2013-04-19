import scalariform.formatter.preferences._

name := "scala-playground"

organization := "adelbertc"

scalaVersion := "2.10.1"

scalariformSettings

ScalariformKeys.preferences := FormattingPreferences().
  setPreference(MultilineScaladocCommentsStartOnFirstLine, true).
  setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true).
  setPreference(DoubleIndentClassDeclaration, true)
