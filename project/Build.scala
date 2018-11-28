import sbt._
import sbtassembly.AssemblyKeys
import sbtbuildinfo.BuildInfoPlugin

object Build extends sbt.Build {
  lazy val build = Project(
    "region-coder-build",
    file("."),
    settings = Seq(AssemblyKeys.assembly := file(".")) // no root assembly
  ).settings(BuildSettings.buildSettings : _*)
    .aggregate(regionCoder, geospace)

  private def p(name: String, settings: { def settings: Seq[Setting[_]] }, dependencies: ClasspathDep[ProjectReference]*) =
    Project(name, file(name)).settings(settings.settings : _*).configs(IntegrationTest).dependsOn(dependencies: _*)

  lazy val regionCoder = p("region-coder", RegionCoder, geospace)
    .enablePlugins(BuildInfoPlugin)

  lazy val geospace = p("geospace", Geospace)
}
