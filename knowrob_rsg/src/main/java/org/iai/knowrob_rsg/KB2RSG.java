/*
 * Copyright (C) 2014 Benjamin Brieber.
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

import org.apache.commons.logging.Log;
import org.ros.message.MessageListener;
import org.ros.namespace.GraphName;
import org.ros.node.AbstractNodeMain;
import org.ros.node.ConnectedNode;
import org.ros.node.NodeMain;
import org.ros.node.topic.Subscriber;

import java.util.List;

import javax.vecmath.Matrix4d;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import net.sf.json.util.JSONUtils;

import zmq.*;

/**
 * A simple {@link Subscriber} {@link NodeMain}.
 */
public class KB2RSG extends AbstractNodeMain {

  @Override
  public GraphName getDefaultNodeName() {
    return GraphName.of("rosjava/listener");
  }

  public double[] queryTransform(String id, String ref_id,String time){
	  double[] pose_arr;
	  
      //log.info("I heard nothing");
      String data= "{ "+
      			"\"@worldmodeltype\": \"RSGQuery\","+
      			"\"query\": \"GET_TRANSFORM\","+
      			"\"id\": \""+id+"\","+
      			"\"idReferenceNode\": \""+ref_id+"\","+
      			"\"timeStamp\": {"+
      			"	\"@stamptype\": \"TimeStampDate\","+
      		"		\"stamp\": \"2015-11-09T16:16:44Z\","+
      		"		} "+
		"}";
      ZMQ.send(sc,data.getBytes(ZMQ.CHARSET),data.length(),0);
      Msg msg = ZMQ.recv(sc, 0);
      
      JSONObject jsonObject = JSONObject.fromObject(new String(msg.data()));
      
      JSONObject transform =  jsonObject.getJSONObject("transform");
      //List transform_list =  JSONArray 
      JSONArray array = transform.getJSONArray("matrix");
      JSONArray r0 =  array.getJSONArray(0);
      JSONArray r1 =  array.getJSONArray(1);
      JSONArray r2 =  array.getJSONArray(2);
      JSONArray r3 =  array.getJSONArray(3);
      
      //log.info("("+      r0.getDouble(3)+","+      r1.getDouble(3)+","+      r2.getDouble(3)+")");
      //log.info(new String(msg.data()));
      //ZMQ.close(sc);
      //ZMQ.term(ctx);
      
	  //Matrix4d poseMat = new Matrix4d(pose_arr);
      
      
      pose_arr =  new double[]{
			  r0.getDouble(0),r0.getDouble(1),r0.getDouble(2),r0.getDouble(3),
			  r1.getDouble(1),r1.getDouble(1),r1.getDouble(2),r1.getDouble(3),
			  r2.getDouble(0),r2.getDouble(1),r2.getDouble(2),r2.getDouble(3),
			  r3.getDouble(0),r3.getDouble(1),r3.getDouble(2),r3.getDouble(3)};
      return pose_arr;
  }
  
  
  Log log;
  SocketBase sc;
  Ctx ctx;
  @Override
  public void onStart(ConnectedNode connectedNode) {
	  	log = connectedNode.getLog();

    	ctx = ZMQ.init(1);


    	sc = ZMQ.socket(ctx, ZMQ.ZMQ_REQ);
    	boolean rc = ZMQ.connect(sc, "tcp://127.0.0.1:22422");
    	queryTransform("92876bfd-3b6d-44a3-a9a1-a7b36c53acd1" ,"e379121f-06c6-4e21-ae9d-ae78ec1986a1" , null);
    	
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
