/*
 * Copyright (C) 2014-2016 Benjamin Brieber, 2016-2017 Asil Kaan Bozcuoglu.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package org.iai.knowrob_rsg;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import java.io.*;
import java.util.UUID;
import java.util.Calendar;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;
import java.util.Date;

import zmq.*;

/**
 * A simple {@link Subscriber} {@link NodeMain}.
 */
public class RSGConnection{

  SocketBase sc;
  Ctx ctx;

  final String rsg_main_path = "/home/openease/ubx_robotscenegraph/";

  private String randomStringGenerator()
  {
	String uuid = UUID.randomUUID().toString();
	System.out.println("uuid = " + uuid);
	return uuid;
  }

  public String insertWgs84(String comment, String root_id){
         ctx = ZMQ.init(1);


      sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
      boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

	String new_id = randomStringGenerator();
	String area_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","
			+ "\"node\": " + "{"
				+ "\"@graphtype\":\"Group\", "
				+ "\"id\": \"" + new_id + "\", " 
				+ "\"attributes\": ["  
					+ "{\"key\": \"comment\", \"value\": \"" + comment +"\"},"
					+ "{\"key\": \"gis:origin\", \"value\": \"" + "wgs84" +"\"}"
 				+ "]"
			+ "},"  
			+ "\"parentId\": \"" + root_id + "\""
		+ "}";
	ZMQ.send(sc,area_query.getBytes(ZMQ.CHARSET),area_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
      
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      	return new_id;
  }

  public String insertNode(String comment, String wgs_id){
         ctx = ZMQ.init(1);


      sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
      boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

	String new_id = randomStringGenerator();
	String area_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","
			+ "\"node\": " + "{"
				+ "\"@graphtype\":\"Node\", "
				+ "\"id\": \"" + new_id + "\", " 
				+ "\"attributes\": ["  
					+ "{\"key\": \"comment\", \"value\": \"" + comment +"\"}"
 				+ "]"
			+ "},"  
			+ "\"parentId\": \"" + wgs_id + "\""
		+ "}";
	ZMQ.send(sc,area_query.getBytes(ZMQ.CHARSET),area_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
      
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      	return new_id;
  }

  public String insertTransform(String id, String ref_id, float[] pose_arr){
       ctx = ZMQ.init(1);


      sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
      boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

      DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
      df.setTimeZone(TimeZone.getTimeZone("CET"));
      Date today = Calendar.getInstance().getTime();        
      String time = df.format(today);


      String new_id = randomStringGenerator();
      String transform_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","+
      			"\"node\": " + "{"
				+ "\"@graphtype\":\"Connection\", "
				+ "\"@semanticContext\":\"Transform\", "
				+ "\"id\": \"" + new_id + "\", "
				+ "\"attributes\": ["  
					+ "{\"key\": \"tf:type\", \"value\": \"" + "wgs84" +"\"}"
 				+ "],"
				+ "\"sourceIds\": ["  
					+ "\"" + ref_id + "\""
 				+ "]," 
				+ "\"targetIds\": ["  
					+ "\"" + id + "\""
 				+ "],"
				+ "\"history\": ["  
					+ "{"	
						+ "\"stamp\":" 
						+ "{"
							+ "\"@stamptype\": \"TimeStampDate\","
							+ "\"stamp\":" + "\"" + time + "\"" //time
						+ "},"
						+ "\"transform\":" 
						+ "{"
							+ "\"type\": \"HomogeneousMatrix44\","
							+ "\"unit\": \"latlon\","
							+ "\"matrix\": [" 
								+ "[" + pose_arr[0] + ", " + pose_arr[1] + ", " + pose_arr[2] + ", " + pose_arr[3] + "],"
								+ "[" + pose_arr[4] + ", " + pose_arr[5] + ", " + pose_arr[6] + ", " + pose_arr[7] + "],"
								+ "[" + pose_arr[8] + ", " + pose_arr[9] + ", " + pose_arr[10] + ", " + pose_arr[11] + "],"
								+ "[" + pose_arr[12] + ", " + pose_arr[13] + ", " + pose_arr[14] + ", " + pose_arr[15] + "]"
							+ "]"
						+ "}"
					+ "}"
 				+ "],"
			+ "},"  
			+ "\"parentId\": \"" + ref_id + "\""
		+ "}";
      ZMQ.send(sc,transform_query.getBytes(ZMQ.CHARSET),transform_query.length(),0);
      Msg msg = ZMQ.recv(sc, 0);
      
      JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      return new_id;
  }

  public String insertARTVASignal(String artva_code, String wgs_id, String time){
	String new_id = randomStringGenerator();
	String artva_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","
			+ "\"node\": " + "{"
				+ "\"@graphtype\":\"Node\", "
				+ "\"id\": \"" + new_id + "\", " 
				+ "\"attributes\": ["  
					+ "{\"key\": \"sherpa:artva_signal\", \"value\": \"" + artva_code +"\"},"
					+ "{\"key\": \"sherpa:stamp\", \"value\": \"" + time +"\"}"
 				+ "]"
			+ "},"  
			+ "\"parentId\": \"" + wgs_id + "\""
		+ "}";
	ZMQ.send(sc,artva_query.getBytes(ZMQ.CHARSET),artva_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
      
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      	return new_id;

  }

  public String insertBusyGenius(String agent_name, String comment, String wgs_id){
	String new_id = randomStringGenerator();
	String artva_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","
			+ "\"node\": " + "{"
				+ "\"@graphtype\":\"Group\", "
				+ "\"id\": \"" + new_id + "\", " 
				+ "\"attributes\": ["  
					+ "{\"key\": \"sherpa:agent_name\", \"value\": \"" + agent_name +"\"},"
					+ "{\"key\": \"comment\", \"value\": \"" + comment +"\"},"
 				+ "]"
			+ "},"  
			+ "\"parentId\": \"" + wgs_id + "\""
		+ "}";
	ZMQ.send(sc,artva_query.getBytes(ZMQ.CHARSET),artva_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
      
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      	return new_id;
  }

  public String insertHumanCommand(String command, float[] command_parameters, String agent_name, String interpretation_success, String genius_id){
	String new_id = randomStringGenerator();
	String artva_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","
			+ "\"node\": " + "{"
				+ "\"@graphtype\":\"Node\", "
				+ "\"id\": \"" + new_id + "\", " 
				+ "\"attributes\": ["  
					+ "{\"key\": \"sherpa:command\", \"value\": \"" + command +"\"},"
					+ "{\"key\": \"sherpa:target_agent_name\", \"value\": \"" + agent_name +"\"},"
					+ "{\"key\": \"sherpa:command_pointing_gesture\", \"value\": {\"direction\":[" + command_parameters[0] + ", " 
												+ command_parameters[1] + ", " + command_parameters[2] + "]}}"
 				+ "]"
			+ "},"  
			+ "\"parentId\": \"" + genius_id + "\""
		+ "}";
	ZMQ.send(sc,artva_query.getBytes(ZMQ.CHARSET),artva_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
      
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      	return new_id;
  }

  public String insertCommandInterpretation(String agent_name, String interpretation_success, String genius_id, String action_name, float[] action_parameters){
	String new_id = randomStringGenerator();
	String artva_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","
			+ "\"node\": " + "{"
				+ "\"@graphtype\":\"Node\", "
				+ "\"id\": \"" + new_id + "\", " 
				+ "\"attributes\": ["  
					+ "{\"key\": \"sherpa:command_interpetation_success\", \"value\": \"" + interpretation_success +"\"},"
					+ "{\"key\": \"sherpa:target_agent_name\", \"value\": \"" + agent_name +"\"},"
					+ "{\"key\": \"sherpa:actions\", \"value\": {\"actions\":[{\"action\":\"" + action_name + action_parameters[0] + ", " //todo
												+ action_parameters[1] + ", " + action_parameters[2] + "]}]}}"
 				+ "]"
			+ "},"  
			+ "\"parentId\": \"" + genius_id + "\""
		+ "}";
	ZMQ.send(sc,artva_query.getBytes(ZMQ.CHARSET),artva_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
      
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      	return new_id;
  }

  public void updateTransform(String id, String time, float[] pose_arr){
	String update_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"UPDATE_TRANSFORM\","+
      			"\"node\": " + "{"
				+ "\"@graphtype\":\"Connection\", "
				+ "\"@semanticContext\":\"Transform\", "
				+ "\"id\": \"" + id + "\", "
				+ "\"history\": ["  
					+ "{"	
						+ "\"stamp\":" 
						+ "{"
							+ "\"@stamptype\": \"TimeStampDate\","
							+ "\"stamp\":" + "\"" + time + "\"" //time
						+ "},"
						+ "\"transform\":" 
						+ "{"
							+ "\"type\": \"HomogeneousMatrix44\","
							+ "\"unit\": \"latlon\","
							+ "\"matrix\": [" 
								+ "[" + pose_arr[0] + ", " + pose_arr[1] + ", " + pose_arr[2] + ", " + pose_arr[3] + "],"
								+ "[" + pose_arr[4] + ", " + pose_arr[5] + ", " + pose_arr[6] + ", " + pose_arr[7] + "],"
								+ "[" + pose_arr[8] + ", " + pose_arr[9] + ", " + pose_arr[10] + ", " + pose_arr[11] + "],"
								+ "[" + pose_arr[12] + ", " + pose_arr[13] + ", " + pose_arr[14] + ", " + pose_arr[15] + "]"
							+ "]"
						+ "}"
					+ "}"
 				+ "],"
			+ "}"
		+ "}";
	ZMQ.send(sc,update_query.getBytes(ZMQ.CHARSET),update_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
  }

  public String queryRootNode(){
        ctx = ZMQ.init(1);


  	sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
  	boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

        String returnedRootId;

        String data= "{ "+
      			"\"@worldmodeltype\": \"RSGQuery\"," +
                        "\"query\": \"GET_ROOT_NODE\""
                + "}";

        System.out.println("Sendinging Request: \n"+data);
        int result = ZMQ.send(sc,data.getBytes(ZMQ.CHARSET),data.length(),0);
        System.out.println("Sended \n"+result + " bytes");
      
        System.out.println("Waiting for Reply");
        Msg msg = ZMQ.recv(sc, 0);

        System.out.println("received reply");
        JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
	returnedRootId = jsonObject.getString("rootId");

	return returnedRootId;
  }

  public String queryWgs(){
        ctx = ZMQ.init(1);


  	sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
  	boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

        String returnedRootId;

        String data= "{ "+
      			"\"@worldmodeltype\": \"RSGQuery\"," +
                        "\"query\": \"GET_NODES\","
			+ "\"attributes\": ["  
					+ "{\"key\": \"gis:origin\", \"value\": \"" + "wgs84" +"\"}"
 				+ "]"
                + "}";

        System.out.println("Sendinging Request: \n"+data);
        int result = ZMQ.send(sc,data.getBytes(ZMQ.CHARSET),data.length(),0);
        System.out.println("Sended \n"+result + " bytes");
      
        System.out.println("Waiting for Reply");
        Msg msg = ZMQ.recv(sc, 0);

        System.out.println("received reply");
        JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
	returnedRootId = jsonObject.getJSONArray("ids").getString(0);

	return returnedRootId;
  }


  public double[] queryTransform(String id, String ref_id){
	  return queryTransform(id,ref_id,null);
  }
  public double[] queryTransform(String id, String ref_id,String time){
	  

      ctx = ZMQ.init(1);


      sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
      boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");
  	
      //FIXME do i need to reconnect???
	  
      double[] pose_arr;
	  
      //log.info("I heard noth
      String data= "{ "+
      			"\"@worldmodeltype\": \"RSGQuery\","+
      			"\"query\": \"GET_TRANSFORM\","+
      			"\"id\": \""+id+"\","+
      			"\"idReferenceNode\": \""+ref_id+"\","+
      			"\"timeStamp\": {"+
      			"	\"@stamptype\": \"TimeStampDate\","+
      		"		\"stamp\": \"2015-11-09T16:16:44Z\""+
      		"		} "+
		"}";
      System.out.println("Sendinging Request: \n"+data);
      int result = ZMQ.send(sc,data.getBytes(ZMQ.CHARSET),data.length(),0);
      System.out.println("Sended \n"+result + " bytes");
      
      System.out.println("Waiting for Reply");
      Msg msg = ZMQ.recv(sc, 0);

      System.out.println("received replyt");
      JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      JSONObject transform =  jsonObject.getJSONObject("transform");
      //List transform_list =  JSONArray 
      JSONArray array = transform.getJSONArray("matrix");
      JSONArray r0 =  array.getJSONArray(0);
      JSONArray r1 =  array.getJSONArray(1);
      JSONArray r2 =  array.getJSONArray(2);
      JSONArray r3 =  array.getJSONArray(3);

      System.out.println("Translation");
      System.out.println("("+      r0.getDouble(3)+","+      r1.getDouble(3)+","+      r2.getDouble(3)+")");
      //log.info(new String(msg.data()));java.io.BufferedReader
      //ZMQ.close(sc);
      //ZMQ.term(ctx);
      
	  //Matrix4d poseMat = new Matrix4d(pose_arr);
      
      
      pose_arr =  new double[]{
			  r0.getDouble(0),r0.getDouble(1),r0.getDouble(2),r0.getDouble(3),
			  r1.getDouble(0),r1.getDouble(1),r1.getDouble(2),r1.getDouble(3),
			  r2.getDouble(0),r2.getDouble(1),r2.getDouble(2),r2.getDouble(3),
			  r3.getDouble(0),r3.getDouble(1),r3.getDouble(2),r3.getDouble(3)};
      
      
      //FIXME remove shutown

      //ZMQ.close(sc);
      //ZMQ.term(ctx);
      
      //
      return pose_arr;
      
  }
  

  /*public String insertWgs84(String comment, String root_id){java.io.BufferedReader
         ctx = ZMQ.init(1);


      sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
      boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

	String new_id = randomStringGenerator();
	String area_query = "{ "+
      			"\"@worldmodeltype\": \"RSGUpdate\","+
      			"\"operation\": \"CREATE\","
			+ "\"node\": " + "{"
				+ "\"@graphtype\":\"Group\", "
				+ "\"id\": \"" + new_id + "\", " 
				+ "\"attributes\": ["  
					+ "{\"key\": \"comment\", \"value\": \"" + comment +"\"},"
					+ "{\"key\": \"gis:origin\", \"value\": \"" + "wgs84" +"\"}"
 				+ "]"
			+ "},"  
			+ "\"parentId\": \"" + root_id + "\""
		+ "}";
	ZMQ.send(sc,area_query.getBytes(ZMQ.CHARSET),area_query.length(),0);
	Msg msg = ZMQ.recv(sc, 0);
      
	JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      	return new_id;
  }*/


  public void applyActionDot(boolean isLoad){
      ctx = ZMQ.init(1);


      sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
      boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

      String action;

      if(isLoad) action = "LOAD"; else action = "UNLOAD";

      String data= "{ "+
      			"\"@worldmodeltype\": \"RSGFunctionBlock\","+
			"\"metamodel\": \"rsg-functionBlock-schema.json\","+
      			"\"name\": \"dotgenerator\","+
			"\"operation\": \""+ action +"\","+
      			"\"input\": {"+
      			"	\"metamodel\": \"rsg-functionBlock-path-schema.json.json\","+
		"		\"path\": \"/home/openease/brics_3d_function_blocks/lib/\","+
      		"		\"comment\": \"openease rsg pdf generator\""+
      		"		} "+
		"}";
      System.out.println("Sendinging Request: \n"+data);
      int result = ZMQ.send(sc,data.getBytes(ZMQ.CHARSET),data.length(),0);
      System.out.println("Sended \n"+result + " bytes");
     
  }

   public void printDot(String path, String root_id){
      try{
        Process p0 = Runtime.getRuntime().exec("rm -rf /home/openease/rsg_openease_dump.jpg /home/openease/rsg_openease_dump.pdf /home/openease/rsg_openease_dump.gv", null, new File(path));
        System.out.println("Removing old graph from native file system: Success");
      }
      catch (Exception e)
      {
        System.out.println("Removing old graph from native file system failed");
        e.printStackTrace();
      }


      ctx = ZMQ.init(1);


      sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
      boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");

      String data= "{ "+
      			"\"@worldmodeltype\": \"RSGFunctionBlock\","+
			"\"metamodel\": \"rsg-functionBlock-schema.json\","+
      			"\"name\": \"dotgenerator\","+
			"\"operation\": \"EXECUTE\","+
      			"\"input\": {"+
      			"	\"metamodel\": \"fbx-subgraph-and-file-schema.json\","+
		"		\"subgraphId\": \"" + root_id + "\","+
		"		\"path\": \"" + path + "\","+
      		"		\"dotFileName\": \"rsg_openease_dump\""+
      		"		} "+
		"}";
      System.out.println("Sendinging Request: \n"+data);
      int result = ZMQ.send(sc,data.getBytes(ZMQ.CHARSET),data.length(),0);
      System.out.println("Sended \n"+result + " bytes");
     
  }

  public String importPDF(String path){

    String uniqueFile = randomStringGenerator() + ".jpg";

     try{
        Process p0 = Runtime.getRuntime().exec("docker exec admin rm -rf /home/ros/user_data/rsg_openease_dump.jpg", null, new File(path));
        System.out.println("Removed existing graph in the container!");
        Process p = Runtime.getRuntime().exec("dot " + "rsg_openease_dump.gv -Tpdf -o rsg_openease_dump.pdf", null, new File(path));
        BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
        while ((reader.readLine()) != null) {}
        p.waitFor();
        System.out.println("Dot to PDF: Success!");
        Process p2 = Runtime.getRuntime().exec("convert -density 800 " + "rsg_openease_dump.pdf rsg_openease_dump.jpg", null, new File(path));
        BufferedReader reader2 = new BufferedReader(new InputStreamReader(p2.getInputStream()));
        while ((reader2.readLine()) != null) {}
        p2.waitFor();

        Process pMv = Runtime.getRuntime().exec("mv rsg_openease_dump.jpg " + uniqueFile, null, new File(path));
        BufferedReader reader0 = new BufferedReader(new InputStreamReader(pMv.getInputStream()));
        while ((reader0.readLine()) != null) {}
        pMv.waitFor();
        

        System.out.println("PDF to JPG: Success!");
        Process p3 = Runtime.getRuntime().exec("docker cp " + uniqueFile + " admin:/home/ros/user_data ", null, new File(path));
        BufferedReader reader3 = new BufferedReader(new InputStreamReader(p3.getInputStream()));
        while ((reader3.readLine()) != null) {}
        p3.waitFor();
        System.out.println("PDF to JPG: Success!");
        System.out.println("JPG Migration to user data container in docker: Success!");
     }
     catch (Exception e)
     {
        System.out.println("PDF Import and Migration to user data container in docker: Failed!");
        e.printStackTrace();
     }
     return "/home/ros/user_data/" + uniqueFile;
  }
  
  public RSGConnection() {
    	ctx = ZMQ.init(1);


    	sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
    	boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");
    	//queryTransform("92876bfd-3b6d-44a3-a9a1-a7b36c53acd1" ,"e379121f-06c6-4e21-ae9d-ae78ec1986a1" , null);
    	
    	/**
        log.info("I heard nothing");
        ctx = ZMQ.init(1);


        sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
        boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");
        String data= "{ "+
        			"\"@worldmodeltype\": \"RSGQuery\","+
        			"\"query\": \"GET_TRANSFORM\","+
        			"\"id\": \"92876bfd-3b6d-44a3-a9a1-a7b36c53acd1\","+
        			"\"idReferenceNode\": \"e379121f-06c6-4e21-ae9d-ae78ec1986a1\","+
        			"\"timeStamp\": {"+
        			"	\"@stamptype\": \"TimeStampDate\","+
        		"		\"stamp\": \"2015-11-09T16:16:44Z\","+
        		"		} "+
  		"}";
        ZMQ.send(sc,data.getBytes(ZMQ.CHARSET),data.length(),0);
        Msg msg = ZMQ.recv(sc, 0);
        log.info(msg.toString());
        log.info(new String(msg.data()));
        ZMQ.close(sc);
        ZMQ.term(ctx);
        
        log.info("I heard nothing");
    	*/
  }
}
