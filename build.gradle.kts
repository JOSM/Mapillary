import com.github.spotbugs.snom.Confidence
import com.github.spotbugs.snom.Effort
import com.github.spotbugs.snom.SpotBugsTask
import net.ltgt.gradle.errorprone.CheckSeverity
import net.ltgt.gradle.errorprone.errorprone
import org.gradle.api.tasks.testing.logging.TestExceptionFormat
import org.gradle.api.tasks.testing.logging.TestLogEvent
import org.openstreetmap.josm.gradle.plugin.task.MarkdownToHtml
import java.net.URL

plugins {
  id("org.sonarqube") version "3.0"
  id("org.openstreetmap.josm") version "0.7.1"
  id("com.github.ben-manes.versions") version "0.36.0"
  id("com.github.spotbugs") version "4.6.0"
  id("net.ltgt.errorprone") version "1.3.0"
  id("com.diffplug.spotless") version "5.8.2"

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
  errorprone("com.google.errorprone:error_prone_core:2.3.4")
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
    // TODO remove this on errorprone update
    if ((JavaVersion.current().majorVersion.toInt()) > 11) {
      check("Finally", CheckSeverity.OFF)
      check("UnnecessaryLambda", CheckSeverity.OFF)
    }
  }
}

java.sourceCompatibility = JavaVersion.VERSION_1_8
java.targetCompatibility = JavaVersion.VERSION_1_8

val versions = mapOf(
  "awaitility" to "4.0.3",
  "jdatepicker" to "1.3.4",
  "jmockit" to "1.49",
  "junit" to "5.7.0",
  "spotbugs" to "4.2.0",
  "wiremock" to "2.27.2"
)

dependencies {
  testImplementation ("org.openstreetmap.josm:josm-unittest:SNAPSHOT"){ isChanging = true }
  testImplementation("com.github.tomakehurst:wiremock:${versions["wiremock"]}")

  testImplementation("org.junit.jupiter:junit-jupiter-api:${versions["junit"]}")
  testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:${versions["junit"]}")
  // This can be removed once JOSM drops all JUnit4 support. Nothing remaining in Mapillary uses JUnit4.
  testImplementation("org.junit.vintage:junit-vintage-engine:${versions["junit"]}")
  testCompile("org.junit.jupiter:junit-jupiter-params:${versions["junit"]}")

  testImplementation("org.awaitility:awaitility:${versions["awaitility"]}")
  testImplementation("org.jmockit:jmockit:${versions["jmockit"]}")
  testImplementation("com.github.spotbugs:spotbugs-annotations:${versions["spotbugs"]}")
  implementation("org.jdatepicker:jdatepicker:${versions["jdatepicker"]}")
  packIntoJar("org.jdatepicker:jdatepicker:${versions["jdatepicker"]}")
}

sourceSets {
  test {
    java {
      setSrcDirs(listOf("test/unit"))
    }
    resources {
      setSrcDirs(listOf("test/data"))
    }
  }
}

val md2html by tasks.creating(MarkdownToHtml::class) {
  destDir = File(buildDir, "md2html")
  source(projectDir)
  include("README.md", "LICENSE.md")
}

tasks {
  processResources {
    from(project.projectDir) {
      include("LICENSE")
      include("LICENSE_*")
    }
    from(md2html)
  }
}

spotless {
  format("misc") {
    target("**/*.gradle", "**.*.md", "**/.gitignore")

    trimTrailingWhitespace()
    indentWithSpaces(2)
    endWithNewline()
  }
  java {
    // Exclude datepicker -- this should eventually become another plugin (needs to be in JOSM plugin svn or the replacement thereof)
    targetExclude("src/main/java/org/openstreetmap/josm/plugins/datepicker/**/*")
    // Avoid large formatting commits.
    ratchetFrom("origin/master")
    trimTrailingWhitespace()
    indentWithSpaces(2)
    endWithNewline()
    removeUnusedImports()
    eclipse().configFile("config/format/code_format.xml")
  }
}

josm {
  pluginName = "Mapillary"
  debugPort = 7051
  manifest {
    // See https://floscher.gitlab.io/gradle-josm-plugin/kdoc/latest/gradle-josm-plugin/org.openstreetmap.josm.gradle.plugin.config/-josm-manifest/old-version-download-link.html
    oldVersionDownloadLink(16114, "v1.5.22", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.22/Mapillary.jar"))
    oldVersionDownloadLink(15909, "v1.5.20", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.20/Mapillary.jar"))
    oldVersionDownloadLink(14149, "v1.5.16", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.16/Mapillary.jar"))
    oldVersionDownloadLink(13733, "v1.5.15", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.15/Mapillary.jar"))
    oldVersionDownloadLink(13643, "v1.5.14", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.14/Mapillary.jar"))
    oldVersionDownloadLink(13558, "v1.5.12+pre13643", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.12%2Bpre13643/Mapillary.jar"))
    oldVersionDownloadLink(12987, "v1.5.10", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.10/Mapillary.jar"))
    oldVersionDownloadLink(12675, "v1.5.7", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.7/Mapillary.jar"))
    oldVersionDownloadLink(12128, "v1.5.5", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.5/Mapillary.jar"))
    oldVersionDownloadLink(10824, "v1.5.3", URL("https://github.com/JOSM/Mapillary/releases/download/v1.5.3/Mapillary.jar"))
  }
  i18n {
    pathTransformer = getPathTransformer(project.projectDir, "gitlab.com/JOSM/plugin/Mapillary/blob")
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

tasks.withType(Test::class).getByName("test") {
  project.afterEvaluate {
    jvmArgs("-javaagent:${classpath.find { it.name.contains("jmockit") }!!.absolutePath}")
    jvmArgs("-Djunit.jupiter.extensions.autodetection.enabled=true")
    jvmArgs("-Djava.awt.headless=true")
  }
  useJUnitPlatform()
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

project.afterEvaluate {
  publishing.publications.getByName("josmPlugin", MavenPublication::class) {
    pom {
      name.set("JOSM-${base.archivesBaseName}")
      description.set("The Mapillary plugin for JOSM")
      url.set("https://gitlab.com/JOSM/plugin/Mapillary")
      licenses {
        license {
          name.set("GNU General Public License Version 2")
          url.set("https://www.gnu.org/licenses/old-licenses/gpl-2.0")
        }
      }
      scm {
        connection.set("scm:git:git://gitlab.com/JOSM/plugin/Mapillary.git")
        developerConnection.set("scm:git:ssh://gitlab.com/JOSM/plugin/Mapillary.git")
        url.set("https://gitlab.com/JOSM/plugin/Mapillary")
      }
      issueManagement {
        system.set("Trac")
        url.set("https://josm.openstreetmap.de/query?component=Plugin+mapillary&status=assigned&status=needinfo&status=new&status=reopened")
      }
    }
  }
}

// Spotbugs config
spotbugs {
  toolVersion.set("4.0.6")
  ignoreFailures.set(true)
  effort.set(Effort.MAX)
  reportLevel.set(Confidence.LOW)
  reportsDir.set(File(buildDir, "reports/spotbugs"))
}
tasks.withType(SpotBugsTask::class) {
  reports.create("html") {
    outputLocation.set(File(spotbugs.reportsDir.get().asFile, "$baseName.html"))
    setStylesheet("color.xsl")
  }
}

// JaCoCo config
jacoco {
  toolVersion = "0.8.5"
}
val jacocoTestReport by tasks.getting(JacocoReport::class) {
  reports {
    xml.isEnabled = true
    html.destination = file("$buildDir/reports/jacoco")
  }
}
tasks.build.get().dependsOn(jacocoTestReport)

// PMD config
pmd {
  toolVersion = "6.23.0"
  isIgnoreFailures = true
  ruleSetConfig = resources.text.fromFile("$projectDir/config/pmd/ruleset.xml")
  ruleSets = listOf()
  sourceSets = listOf(project.sourceSets.main.get(), project.sourceSets.test.get())
}

// SonarQube config
sonarqube {
  properties {
    property("sonar.forceAuthentication", "true")
    property("sonar.host.url", "https://sonarcloud.io")
    property("sonar.projectKey", "Mapillary")
    property("sonar.organization", "josm")
    property("sonar.projectVersion", project.version)
    property("sonar.projectDescription", properties.get("plugin.description")!!)
    property("sonar.sources", listOf("src"))
  }
}
