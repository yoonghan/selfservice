package utils

import java.io.File;
import java.util.HashSet;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

class ResumableInfo(
    val resumableChunkSize:Int,
    val resumableTotalSize:Long,
    val resumableIdentifier:String,
    val resumableFilename:String,
    val resumableRelativePath:String,
    val resumableFilePath:String) {

    val uploadedChunks = new HashSet[ResumableChunkNumber]()

    def valid():Boolean ={
        if (resumableChunkSize < 0 || resumableTotalSize < 0
                || HttpUtils.isEmpty(resumableIdentifier)
                || HttpUtils.isEmpty(resumableFilename)
                || HttpUtils.isEmpty(resumableRelativePath)) {
            false
        } else {
            true
        }
    }
    
    def checkIfUploadFinished():Boolean = {
        //check if upload finished
    	val count: Int = Math.ceil(resumableTotalSize.toDouble / resumableChunkSize.toDouble).toInt
    	println("COUNT:>>"+count)
        for(i <- 1 until count) {
          println("STAT:"+(!uploadedChunks.contains(ResumableChunkNumber(i))))
            if (!uploadedChunks.contains(ResumableChunkNumber(i))) {
            	println("RETURNED FALSE")
                return false
            }
        }

        println("RETURNED TRUE")
        val file = new File(resumableFilePath);
        val new_path = file.getAbsolutePath().substring(0, file.getAbsolutePath().length() - ".temp".length());
        file.renameTo(new File(new_path));
        true
    }
}

case class ResumableChunkNumber(val number:Int)