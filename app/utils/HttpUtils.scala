package utils

object HttpUtils {

    def isEmpty(value:String):Boolean = {
        value == null || "".equals(value);
    }
    
    def toLong(value:String, definition:Long):Long = {
        if (isEmpty(value)) {
            definition;
        }else{
         
	        try {
	            value.toLong
	        } catch{
	        	case e:NumberFormatException => e.printStackTrace(); definition
	        } 
        }
    }

    def toInt(value:String, definition:Int):Int = {
        if (isEmpty(value)) {
            definition
        }else{
	        try {
	            return value.toInt;
	        }catch{
	        	case e:NumberFormatException => e.printStackTrace(); definition
	        }
        }
    }
}