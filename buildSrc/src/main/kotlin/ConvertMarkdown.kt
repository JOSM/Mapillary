package org.openstreetmap.josm.plugins.mapillary.build

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets

open class ConvertMarkdown: DefaultTask() {

  var srcDir: File? = null
  var destDir: File? = null
  var includes = arrayOf<String>()

  init {
    project.afterEvaluate {
      inputs.dir(requireNotNull(srcDir))
      outputs.dir(requireNotNull(destDir))
    }
  }

  @TaskAction
  fun convert() {
    val destDir = requireNotNull(destDir)
    project.sync {
      it.from(srcDir)
      it.into(destDir)
      it.include(*includes)
    }

    val parser = Parser.builder().build()
    val renderer = HtmlRenderer.builder().build()

    for (inFile in project.fileTree(destDir).files) {
      val outPath = inFile.absolutePath.substring(destDir.getAbsolutePath().length + 1)
      val outFile = File(
        destDir,
        (if (outPath.toLowerCase().endsWith(".md")) outPath.substring(0, outPath.length - 3) else outPath) + ".html"
      )
      outFile.parentFile.mkdirs()
      outFile.writer().use {
        it.write("<!DOCTYPE html><meta charset=\"utf-8\"><title>" + inFile.name + "</title>")
        renderer.render(parser.parseReader(InputStreamReader(FileInputStream(inFile), StandardCharsets.UTF_8)), it)
      }
      inFile.delete()
    }
  }
}

