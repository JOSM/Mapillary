buildscript {
  repositories {
    jcenter()
  }
  dependencies {
    val kotlinVersion: String by project.extra { "1.2.51" }
    classpath(kotlin("gradle-plugin", kotlinVersion))
  }
}
plugins {
  java
}
apply {
  plugin("org.jetbrains.kotlin.jvm")
}

repositories {
  jcenter()
}

dependencies {
  implementation("com.vladsch.flexmark:flexmark-all:0.34.8")
  val kotlinVersion: String by project.extra
  implementation(kotlin("stdlib-jdk8", kotlinVersion))
}
