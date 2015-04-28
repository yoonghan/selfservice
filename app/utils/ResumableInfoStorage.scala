package utils


object ResumableInfoStorage {

    val mMap = collection.mutable.Map[String, ResumableInfo]()
    
    def remove(info:ResumableInfo) {
       mMap.remove(info.resumableIdentifier);
    }
    
    def get(
        resumableChunkSize:Int, 
        resumableTotalSize:Long,
        resumableIdentifier:String,
        resumableFilename:String,
        resumableRelativePath:String,
        resumableFilePath:String):ResumableInfo = {

        val info = mMap.getOrElse(resumableIdentifier, {
		            val ri = new ResumableInfo(resumableChunkSize, 
		                resumableTotalSize, 
		                resumableIdentifier, 
		                resumableFilename,
		                resumableRelativePath,
		                resumableFilePath
		            		)
		            mMap.put(resumableIdentifier, ri)
		            ri
		        })
        info
    }
}