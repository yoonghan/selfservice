package utils

import java.io.File
import java.nio.file.{Files,StandardCopyOption}

/**
 * Copied from User: Kayrnt
 */
class FlowInfo(val resumableChunkSize: Int,
             val resumableTotalSize: Long = 0L,
             val resumableIdentifier: String = null,
             val resumableFilename: String = null,
             val resumableRelativePath: String = null,
             val resumableFilePath: String = null) {

  type ResumableChunkNumber = Int

  var uploadedChunks: Set[Int] = Set[ResumableChunkNumber]()

  def valid: Boolean = {
    if (resumableChunkSize < 0 || resumableTotalSize < 0 || resumableIdentifier.isEmpty || resumableFilename.isEmpty || resumableRelativePath.isEmpty) false
    else true
  }

  def checkIfUploadFinished: Boolean = {
    //check if upload finished
    val count: Int = Math.ceil(resumableTotalSize.toDouble / resumableChunkSize.toDouble).toInt
    (1 until count).map {
      i =>
        if (!uploadedChunks.contains(i)) {
          return false
        }
    }
    //Upload finished, change filename.
    val file: File = new File(resumableFilePath)
    val newPath: String = file.getAbsolutePath.substring(0, file.getAbsolutePath.length - ".temp".length)
    
    val renamedFile:File = new File(newPath)
    Files.move(file.toPath(), renamedFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
    
    return true
  }
}