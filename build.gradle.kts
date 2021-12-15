import com.github.spotbugs.snom.Confidence
import com.github.spotbugs.snom.Effort
import com.github.spotbugs.snom.SpotBugsTask
import kotlinx.serialization.serializer
import net.ltgt.gradle.errorprone.CheckSeverity
import net.ltgt.gradle.errorprone.errorprone
import org.gradle.api.tasks.testing.logging.TestExceptionFormat
import org.gradle.api.tasks.testing.logging.TestLogEvent
import org.openstreetmap.josm.gradle.plugin.task.MarkdownToHtml
import java.net.URL
import kotlin.reflect.full.starProjectedType

plugins {
  id("application")
  id("com.diffplug.spotless") version "6.2.0"
  id("com.github.ben-manes.versions") version "0.41.0"
  id("com.github.spotbugs") version "5.0.5"
  id("net.ltgt.errorprone") version "2.0.2"
  id("org.openstreetmap.josm") version "0.7.1"
  id("org.sonarqube") version "3.3"

  eclipse
  jacoco
  java
  `maven-publish`
  pmd
}

repositories {
  jcenter()
  mavenCentral()
}

// Set up ErrorProne
tasks.withType(JavaCompile::class).configureEach {
  options.compilerArgs.addAll(listOf("-Xlint:all", "-Xlint:-serial"))
  options.errorprone {
    check("ClassCanBeStatic", CheckSeverity.ERROR)
    check("ConstantField", CheckSeverity.WARN)
    check("DefaultCharset", CheckSeverity.ERROR)
    check("FieldCanBeFinal", CheckSeverity.WARN)
    check("Finally", CheckSeverity.OFF)
    check("LambdaFunctionalInterface", CheckSeverity.WARN)
    check("MethodCanBeStatic", CheckSeverity.WARN)
    check("MultiVariableDeclaration", CheckSeverity.WARN)
    check("PrivateConstructorForUtilityClass", CheckSeverity.WARN)
    check("RemoveUnusedImports", CheckSeverity.WARN)
    check("StringEquality", CheckSeverity.ERROR)
    check("UngroupedOverloads", CheckSeverity.WARN)
    check("UnnecessaryLambda", CheckSeverity.OFF)
    check("WildcardImport", CheckSeverity.ERROR)
  }
}

java.sourceCompatibility = JavaVersion.VERSION_1_8
java.targetCompatibility = JavaVersion.VERSION_1_8

val versions = mapOf(
  "awaitility" to "4.1.1",
  // Errorprone 2.11 requires Java 11+
  "errorprone" to if (JavaVersion.current() >= JavaVersion.VERSION_11) "2.11.0" else "2.10.0",
  "jdatepicker" to "1.3.4",
  "jmockit" to "1.49",
  "junit" to "5.8.2",
  "pmd" to "6.20.0",
  "spotbugs" to "4.5.3",
  "wiremock" to "2.32.0"
)

dependencies {
  if (!JavaVersion.current().isJava9Compatible) {
    errorproneJavac("com.google.errorprone:javac:9+181-r4173-1")
  }
  errorprone("com.google.errorprone:error_prone_core:${versions["errorprone"]}")
  testImplementation ("org.openstreetmap.josm:josm-unittest:SNAPSHOT"){ isChanging = true }
  testImplementation("com.github.tomakehurst:wiremock-jre8:${versions["wiremock"]}")

  testImplementation("org.junit.jupiter:junit-jupiter-api:${versions["junit"]}")
  testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:${versions["junit"]}")
  // This can be removed once JOSM drops all JUnit4 support. Nothing remaining in Mapillary uses JUnit4.
  testImplementation("org.junit.jupiter:junit-jupiter-params:${versions["junit"]}")
  testImplementation("org.junit.vintage:junit-vintage-engine:${versions["junit"]}")

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

/**
 * Get a specific property, either from gradle or from the environment
 */
fun getProperty(key: String): Any? {
    if (hasProperty(key)) {
        return findProperty(key)
    }
    return System.getenv(key)
}

tasks.register("generateApiKeyFile") {
    val apiKeyFileDir = "$buildDir/resources/main"
    val apiKeyFileName = "mapillary_api_keys.json"
    doLast {
        val jsonEncoder = kotlinx.serialization.json.Json { encodeDefaults = true }
        file(apiKeyFileDir).mkdirs()
        val environmentToMap = mutableMapOf<String, kotlinx.serialization.json.JsonElement>()
        for (environment in listOf("MAPILLARY_CLIENT_TOKEN", "MAPILLARY_CLIENT_SECRET", "MAPILLARY_CLIENT_ID")) {
            if (getProperty(environment) != null) {
                val prop = getProperty(environment)
                environmentToMap[environment] = jsonEncoder.encodeToJsonElement(serializer(prop!!::class.starProjectedType), prop)
            } else {
                logger.warn("$environment not set in environment. Some functionality may not work.")
            }
        }
        //val apiJson = StringBuilder().append("{")
        //apiJson.append("}")
        file("$apiKeyFileDir/$apiKeyFileName").writeText(kotlinx.serialization.json.JsonObject(environmentToMap).toString())
    }
}
if (getProperty("MAPILLARY_CLIENT_TOKEN") != null) {
    tasks["processResources"].dependsOn("generateApiKeyFile")
} else {
    logger.warn("MAPILLARY_CLIENT_TOKEN not set in the environment. Build only usable for tests.")
}

spotless {
  format("misc") {
    target("**/*.gradle", "**.*.md", "**/.gitignore")

    trimTrailingWhitespace()
    indentWithSpaces(4)
    endWithNewline()
  }
  java {
    // Avoid large formatting commits.
    ratchetFrom("origin/master")
    trimTrailingWhitespace()
    indentWithSpaces(4)
    endWithNewline()
    removeUnusedImports()
    importOrder("java", "javax", "")
    eclipse().configFile("config/format/code_format.xml")
  }
}

josm {
  pluginName = "Mapillary"
  debugPort = 7051
  manifest {
    // See https://floscher.gitlab.io/gradle-josm-plugin/kdoc/latest/gradle-josm-plugin/org.openstreetmap.josm.gradle.plugin.config/-josm-manifest/old-version-download-link.html
    oldVersionDownloadLink(17903, "v2.0.0-alpha.36", URL("https://github.com/JOSM/Mapillary/releases/download/v2.0.0-alpha.36/Mapillary.jar"))
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
  toolVersion = "0.8.7"
}

tasks.jacocoTestReport {
  reports {
    xml.isEnabled = true
    html.destination = file("$buildDir/reports/jacoco")
  }
}

tasks.test {
  extensions.configure(JacocoTaskExtension::class) {
    // We need to excluse ObjectDetections from coverage -- it is too large for instrumentation, which means that it will always have 0% coverage, despite having tests.
    excludes = listOf("org/openstreetmap/josm/plugins/mapillary/data/mapillary/ObjectDetections.class")
  }
}

// PMD config
pmd {
  toolVersion = versions["pmd"]
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
    //property("sonar.projectDescription", "Allows the user to work with pictures hosted at mapillary.com")
    findProperty("plugin.description")?.let { property("sonar.projectDescription", it) }
    property("sonar.sources", listOf("src"))
  }
}
