package utils

import java.io.File
import java.util.zip.ZipInputStream
import java.io.FileInputStream
import java.util.zip.ZipEntry
import scala.collection.mutable.ArrayBuffer
import java.io.ByteArrayInputStream
import java.io.BufferedInputStream
import java.util.zip.ZipOutputStream
import java.io.FileOutputStream
import scala.io.Source
import java.io.BufferedReader
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.InputStream
import scala.io.BufferedSource

class ZipReader(fileName: String) extends FileDefinition{
  
	val fileConverter = new ConvertFile(APPEND_TO_FILE, FILENAME_FORMAT)
	val BUFFER_SIZE = 1024
	val Buffer = 2 * 1024
  
	override def process(){
	  
	  def loopMe(zis:ZipInputStream){
	    val zipEntry = zis.getNextEntry()
	    if(zipEntry != null){
	      
	      if(zipEntry.isDirectory() == false){
		      fileConverter.readInputStream(zis)
	      }
	      
	      zis.closeEntry()
	      loopMe(zis)
	    }
	  }
	  val file = new File(PATH+"/"+fileName)
	  
	  val zis:ZipInputStream = new ZipInputStream(new FileInputStream(file))
	  try{
		 loopMe(zis)
	  }finally{
	  	zis.close()
	  }
	  
	  zip(PATH+"/converted_"+fileName, new File(APPEND_TO_FILE).listFiles()  , true)
	  deleteFolder()
	  file.delete
	}
	
	def zip(out: String, files: Array[File], retainPathInfo: Boolean = true) = {
	  var data = new Array[Byte](Buffer)
	  val zip = new ZipOutputStream(new FileOutputStream(out))
	  files.filter(_.isDirectory() == false)
	  		.foreach { file =>
	    val name = file.getName();
	    if (!retainPathInfo)
	      zip.putNextEntry(new ZipEntry(name.splitAt(name.lastIndexOf(File.separatorChar) + 1)._2))
	    else
	      zip.putNextEntry(new ZipEntry(name))
	    val in = new BufferedInputStream(new FileInputStream(file), Buffer)
	    var b = in.read(data, 0, Buffer)
	    while (b != -1) {
	      zip.write(data, 0, b)
	      b = in.read(data, 0, Buffer)
	    }
	    in.close()
	    zip.closeEntry()
	  }
	  zip.close()
	}
}

trait FileDefinition {
	val PATH =  "C:/temp/special"
	val APPEND_TO_FILE  = "C:/temp/special/_process/"
    val FILENAME_FORMAT = "vas_query_input_2010_"
	  
	def process()
	
	def deleteFolder(){
	  def deleteFileOrFolder(file:File){
	
        for (f <- file.listFiles()) {
            f.delete();
            if(f.isDirectory())
            	deleteFileOrFolder(f);
        }
	  }
	  
	  val removeFolder = new File(APPEND_TO_FILE)
	  deleteFileOrFolder(removeFolder)
	}
}

class ConvertFile(APPEND_TO_FILE:String, FILENAME_FORMAT:String) {

  val CSV             = ','
  val FILENAME_EXT = ".txt"
  val ORDERING_DATE_I = 0
  val ORDERING_DATE_O = 1
  val ORDERING_PHONE  = 2
  val ORDERING_TCSD   = 3
    
  def readFile(file:File){
    println("Processing file:" + file.getName())
    val fileSource = Source.fromFile(file)
    readInput(fileSource)
  }
  
  def readInputStream(is:InputStream){
    val fileSource = Source.fromInputStream(is)
    readInput(fileSource)
  }
  
  private def readInput(bs:BufferedSource){
    for( line <- bs.getLines if line != ""){
      val arrSplit = line.split(CSV)
         
      if(arrSplit.size != 4){
        System.err.println("Error splitting file length to 3. Check for seperator")
        System.err.println("Value:"+line)
      }else if(validTCSD(arrSplit(3)))
    	  writeFile(arrSplit)
    }
  }
  
  private def validTCSD(value:String):Boolean = {
    value.equals("0000") == false
  }
  
  private def writeFile(value:Array[String]){
    val filename = FILENAME_FORMAT+value(0)
    val file = new File(APPEND_TO_FILE + filename + FILENAME_EXT)
    if(file.exists() == false)
      file.createNewFile()
    val bw = new BufferedWriter(new FileWriter(file,true))
    try{
      val sb:StringBuilder = new StringBuilder(200)
      sb.append(value(1)).append(CSV).append(value(2)).append(CSV).append(value(3)).append('\n')
      bw.write(sb.toString)
      bw.flush()
    }catch{
    	case e:Exception => e.printStackTrace
    }finally{
      bw.close()
    }
  }
}