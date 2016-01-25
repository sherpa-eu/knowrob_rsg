# knowrob_rsg
This repository contains ontologies and prolog rules that connect knowrob with the RobotSceneGraph(RSG) Library.
It is developed in the context of the sherpa eu project and focuses on outdoor rescue mission with mixed human robot Teams.

The software provided by this package offers functionality for the following three use cases:

  1. Asserting GIS knowledge from RSG to knowrob
  2. Asserting Mission Specific Events into the knowrob knowledge-base 
  3. Querying on demand information directly from RSG





##Prolog-Rules
The prolog rules defined in this package have the purpose to:

 * Assert knowledge about Mission specific events. A Mission specific event might be the assertion of an semantic-gis-object, a status change of a agent or an detection of an object/victim. These events allways contain a GeoPose, a TimeStamp and combination of different Key-Value pairs that are used to detect the type of the event. Static knowledge(e.g. from OSM) will be asserted at TimePoint_0.
 * The other set of rules allow to query information from the database within the RSG. The communication with the RSG is realized with help of zeroMQ. Which allows to queries the database with json-queries. 
 


##Ontologies
The ontologies of this package adds SemanticDescription about GIS and rescue mission related knowledge.
This bacically means:
  * Maps (Different types of maps, different representations)
  * Geographic Points, Polylines and Polygons
  * GeoPoses and GeoRelations
  * Rescue Mission specific events
  * Agent identification
  
## Important Note
This pacakge is still empty and does not contain any usefull functionality
The intended use is to allow prolog queries which allow bidiriectional communication with the RobotSceneGraph Library

additional information can be found at the sherpa knowrob bridge
(https://github.com/blumenthal/sherpa_world_model_knowrob_bridge)


## Installation
You need a running knowrob installation to use this package.( www.knowrob.org )

The package it self can be compiled as normal catkin package
TODO


