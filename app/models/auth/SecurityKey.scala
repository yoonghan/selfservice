package models.auth

import java.security.MessageDigest
import javax.xml.bind.annotation.adapters.HexBinaryAdapter

object SecurityKey {
	private val keys:Array[String] = Array(
	    "secret123",
	    "secret456",
	    "secret789",
	    "secret012")
	
	private val beginIndex:Int = 5;
	    
	def getRandKey:String = {
	  keys(scala.util.Random.nextInt(keys.size))
	}
	
	def encode(userId:String, privateKey:String):String = {
	  val md:MessageDigest = MessageDigest.getInstance("MD5");
	  val strByte = md.digest((userId + privateKey).getBytes())
	  val hexed = (new HexBinaryAdapter()).marshal(strByte)
	  return (hexed).substring(beginIndex)
	}
	
}