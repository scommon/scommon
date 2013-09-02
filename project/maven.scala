import sbt._
import Keys._
import scala.xml.NodeSeq

object MavenSettings {
  val defaults = Seq(
    licenses := Settings.licenses,
    publishMavenStyle := true,
    pomIncludeRepository := { _ => false },

    pomExtra <<= version { version => appendToPom(version) }
  )

  def appendToPom(version: String): NodeSeq =
    <url>{Settings.url}</url>
    <scm>
      <url>{Settings.vcsSpecification}</url>
      <connection>scm:git:{Settings.vcsSpecification}</connection>
    </scm>
    <developers>
      {for (developer <- Settings.developers) yield developer.toXml}
    </developers>
    <parent>
      <groupId>org.sonatype.oss</groupId>
      <artifactId>oss-parent</artifactId>
      <version>7</version>
    </parent>
}

case class Developer(id: String, name: String, email: String, url: String = "", organization: String = "", organizationUri: String = "", roles: Iterable[String] = Seq()) {
  def toXml =
    <developer>
      <id>{id}</id>
      <name>{name}</name>
      <email>{email}</email>
      <url>{url}</url>
      <organization>{organization}</organization>
      <organizationUrl>{organizationUri}</organizationUrl>
      <roles>
        {for (role <- roles) yield <role>{role}</role>}
      </roles>
    </developer>
}

