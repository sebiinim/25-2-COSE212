// Scala 3로 컴파일하도록 설정
ThisBuild / scalaVersion := "3.3.1"

// 프로젝트 버전 (SBT 1.9.9 환경에 맞춤)
ThisBuild / version := "1.0"

// 루트 프로젝트 설정
lazy val root = (project in file("."))
  .settings(
    name := "hw2-mini-fsharp",

    // 필수 라이브러리: 파서 컴비네이터 + 테스트 프레임워크
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    ),

    // 테스트 실행 관련 옵션
    Test / fork := true,
    Test / parallelExecution := false
  )
