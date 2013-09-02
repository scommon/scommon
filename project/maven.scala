import sbt._
import Keys._
import scala.xml.NodeSeq
import scala.language.implicitConversions

object MavenSettings {
  import License._

  val defaults = Seq(
      licenses := Settings.licenses
    , homepage := Some(url(Settings.homepage))
    , publishMavenStyle := true
    , pomIncludeRepository := { _ => false }

    , pomExtra <<= version { version => appendToPom(version) }
  )

  def appendToPom(version: String): NodeSeq =
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

case class License(title: String, url: String)
object License {
  implicit def license2Tuple2(license: License): (String, URL) =
    license.title -> url(license.url)

  implicit def seqLicense2SeqTuple2(licenses: Seq[License]): Seq[(String, URL)] =
    for(license <- licenses) yield license2Tuple2(license)
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

