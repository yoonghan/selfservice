package utils

import java.io.File
import java.nio.file.{Files,StandardCopyOption}

/**
 * Copied from User: Kayrnt
 */
case class FlowInfo(resumableChunkSize: Int,
                         resumableTotalSize: Long = 0L,
                         resumableIdentifier: String = null,
                         resumableFilename: String = null,
                         resumableRelativePath: String = null,
                         resumableFilePath: String = null) {

  type ResumableChunckNumber = Int

  var uploadedChunks: Set[Int] = Set[ResumableChunckNumber]()

  def valid: Boolean = {
    if (resumableChunkSize < 0 || resumableTotalSize < 0 || resumableIdentifier.isEmpty || resumableFilename.isEmpty || resumableRelativePath.isEmpty) false
    else true
  }

  def checkIfUploadFinished: Boolean = {
    //check if upload finished
    val count: Int = Math.ceil(resumableTotalSize.toDouble / resumableChunkSize.toDouble).toInt
    (1 to count).map {
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