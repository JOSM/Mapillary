import com.github.spotbugs.SpotBugsTask
import net.ltgt.gradle.errorprone.*
import org.gradle.api.tasks.testing.logging.TestExceptionFormat
import org.gradle.api.tasks.testing.logging.TestLogEvent
import org.openstreetmap.josm.gradle.plugin.config.I18nConfig
import org.openstreetmap.josm.gradle.plugin.config.JosmManifest
import org.openstreetmap.josm.gradle.plugin.task.MarkdownToHtml
import java.net.URL

plugins {
  id("org.sonarqube") version "2.7"
  id("org.openstreetmap.josm") version "0.6.1"
  id("com.github.ben-manes.versions") version "0.21.0"
  id("com.github.spotbugs") version "1.6.10"
  id("net.ltgt.errorprone") version "0.7.1"

  eclipse
  jacoco
  java
  `maven-publish`
  pmd
}

repositories {
  jcenter()
}

// Set up ErrorProne
dependencies {
  errorprone("com.google.errorprone:error_prone_core:2.3.3")
  if (!JavaVersion.current().isJava9Compatible) {
    errorproneJavac("com.google.errorprone:javac:9+181-r4173-1")
  }
}
tasks.withType(JavaCompile::class).configureEach {
  options.compilerArgs.addAll(listOf("-Xlint:all", "-Xlint:-serial"))
  options.errorprone {
    check("ClassCanBeStatic", CheckSeverity.ERROR)
    check("StringEquality", CheckSeverity.ERROR)
    check("WildcardImport", CheckSeverity.ERROR)
    check("MethodCanBeStatic", CheckSeverity.WARN)
    check("RemoveUnusedImports", CheckSeverity.WARN)
    check("PrivateConstructorForUtilityClass", CheckSeverity.WARN)
    check("LambdaFunctionalInterface", CheckSeverity.WARN)
    check("ConstantField", CheckSeverity.WARN)
  }
}

apply(from = "gradle/tool-config.gradle")

java.sourceCompatibility = JavaVersion.VERSION_1_8
base.archivesBaseName = "Mapillary"

dependencies {
  testImplementation ("org.openstreetmap.josm:josm-unittest:SNAPSHOT"){ isChanging = true }
  testImplementation("com.github.tomakehurst:wiremock:2.21.0")
  val junitVersion = "5.4.0"
  testImplementation("org.junit.jupiter:junit-jupiter-api:$junitVersion")
  testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:$junitVersion")
  testImplementation("org.junit.vintage:junit-vintage-engine:$junitVersion")
  testImplementation("org.awaitility:awaitility:3.1.6")
  testImplementation("org.jmockit:jmockit:1.45")
  testImplementation("com.github.spotbugs:spotbugs-annotations:3.1.12")
}

sourceSets {
  getByName("test") {
    java {
      setSrcDirs(listOf("test/unit"))
    }
    resources {
      setSrcDirs(listOf("test/data"))
    }
  }
}

tasks {
  "processResources"(ProcessResources::class) {
    from(project.projectDir) {
      include("LICENSE")
      include("LICENSE_*")
    }
  }
}

tasks.create("md2html", MarkdownToHtml::class) {
  destDir = File(buildDir, "md2html")
  source(projectDir)
  include("README.md", "LICENSE.md")
  tasks.withType(ProcessResources::class)["processResources"].from(this)
}

josm {
  debugPort = 7051
  manifest {
    // See https://floscher.github.io/gradle-josm-plugin/kdoc/current/gradle-josm-plugin/org.openstreetmap.josm.gradle.plugin.config/-josm-manifest/old-version-download-link.html
    oldVersionDownloadLink(13733, "v1.5.15", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.15/Mapillary.jar"))
    oldVersionDownloadLink(13643, "v1.5.14", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.14/Mapillary.jar"))
    oldVersionDownloadLink(13558, "v1.5.12+pre13643", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.12%2Bpre13643/Mapillary.jar"))
    oldVersionDownloadLink(12987, "v1.5.10", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.10/Mapillary.jar"))
    oldVersionDownloadLink(12675, "v1.5.7", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.7/Mapillary.jar"))
    oldVersionDownloadLink(12128, "v1.5.5", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.5/Mapillary.jar"))
    oldVersionDownloadLink(10824, "v1.5.3", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.3/Mapillary.jar"))
  }
  i18n {
    pathTransformer = getPathTransformer("gitlab.com/JOSM/Mapillary/blob")
  }
}

eclipse {
  project {
    name = "JOSM-Mapillary"
    comment = josm.manifest.description
    natures("org.sonarlint.eclipse.core.sonarlintNature", "ch.acanda.eclipse.pmd.builder.PMDNature", "org.eclipse.buildship.core.gradleprojectnature")
    buildCommand("org.sonarlint.eclipse.core.sonarlintBuilder")
    buildCommand("ch.acanda.eclipse.pmd.builder.PMDBuilder")
    buildCommand("org.eclipse.buildship.core.gradleprojectbuilder")
  }
}
tasks["eclipseClasspath"].dependsOn("cleanEclipseClasspath")
tasks["eclipseProject"].dependsOn("cleanEclipseProject")
tasks["eclipse"].setDependsOn(setOf("eclipseClasspath", "eclipseProject"))

tasks.withType(JavaCompile::class) {
  // Character encoding of Java files
  options.encoding = "UTF-8"
}
tasks.withType(Javadoc::class) {
  isFailOnError = false
}
tasks.withType(SpotBugsTask::class) {
  reports {
    xml.isEnabled = false
    html.isEnabled = true
  }
}

tasks.withType(Test::class).getByName("test") {
  project.afterEvaluate {
    jvmArgs("-javaagent:${classpath.find { it.name.contains("jmockit") }!!.absolutePath}")
  }
  testLogging {
    exceptionFormat = TestExceptionFormat.FULL
    events = setOf(TestLogEvent.FAILED, TestLogEvent.SKIPPED)
    showCauses = true

    info {
      events = setOf(TestLogEvent.STARTED, TestLogEvent.PASSED, TestLogEvent.SKIPPED, TestLogEvent.FAILED, TestLogEvent.STANDARD_OUT, TestLogEvent.STANDARD_ERROR)
      showStandardStreams = true
    }
  }
}

// Configuration for publishing to Maven repository at https://gitlab.com/JOSM/Mapillary/-/packages
val PROJECT_ID = 8564565 // repository JOSM/Mapillary
val PERSONAL_ACCESS_TOKEN = System.getenv("PERSONAL_ACCESS_TOKEN")
val JOB_TOKEN = System.getenv("CI_JOB_TOKEN")

publishing {
  repositories {
    if ((PERSONAL_ACCESS_TOKEN ?: JOB_TOKEN) != null) {
      maven("https://gitlab.com/api/v4/projects/$PROJECT_ID/packages/maven") {
        name = "gitlab"
        credentials(HttpHeaderCredentials::class) {
          name = PERSONAL_ACCESS_TOKEN?.let { "Private-Token" } ?: "Job-Token"
          value = PERSONAL_ACCESS_TOKEN ?: JOB_TOKEN
        }
        authentication {
          removeAll { a -> true }
          create("auth", HttpHeaderAuthentication::class)
        }
      }
    }
    maven("$buildDir/maven") {
      name = "buildDir"
    }
  }
  publications {
    create(base.archivesBaseName, MavenPublication::class) {
      artifactId = base.archivesBaseName
      groupId = "org.openstreetmap.josm.plugins"
      version = project.version.toString()
      from(components["java"])
      pom {
        name.set("JOSM-${base.archivesBaseName}")
        description.set("The Mapillary plugin for JOSM")
        url.set("https://gitlab.com/JOSM/Mapillary")
        licenses {
          license {
            name.set("GNU General Public License Version 2")
            url.set("https://www.gnu.org/licenses/old-licenses/gpl-2.0")
          }
        }
        scm {
          connection.set("scm:git:git://gitlab.com/JOSM/Mapillary.git")
          developerConnection.set("scm:git:ssh://gitlab.com/JOSM/Mapillary.git")
          url.set("https://gitlab.com/JOSM/Mapillary")
        }
        issueManagement {
          system.set("Trac")
          url.set("https://josm.openstreetmap.de/query?component=Plugin+mapillary&status=assigned&status=needinfo&status=new&status=reopened")
        }
      }
    }
  }
}
