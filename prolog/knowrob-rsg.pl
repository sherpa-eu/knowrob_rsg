/** knowrob_rsg Reasoning about robot components and capabilities


  Copyright (C) 2017 Asil Kaan Bozcuoglu, Benjamin Brieber
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the <organization> nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


  @author Asil Kaan Bozcuoglu
  @author Benjamin Brieber
  @license BSD
*/

:- module(knowrob_rsg,
    [
      rsg_interface/0,
      rsg_interface/1,
      query_root_node/1,
      query_wgs_signal/1,
      insert_object/4,
      rsg_dump_to_pdf/2,
      move_pdf_to_docker/2,

      %%%%%%%%%%%%%%%%%%%%% 
      rsg_pose/3,
      generate_rsg_perception/4
  ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('knowrob_owl')).
:- use_module(library('jpl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).


:- rdf_meta
  generate_rsg_perception(r,r,r,-).
  
  
  

:- assert(rsg_int(fail)).
rsg_interface :- rsg_interface(_).

rsg_interface(RSG) :-
    rsg_int(fail),
    jpl_new('org.iai.knowrob_rsg.RSGConnection', [], RSG),
    retract(rsg_int(fail)),
    assert(rsg_int(RSG)),!.

rsg_interface(DB) :-
    rsg_int(DB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


query_root_node(Root) :-
    rsg_interface(RSG),
    jpl_call(RSG,'queryRootNode', [], Root).


query_wgs_signal(Wgs) :-
    rsg_interface(RSG),
    jpl_call(RSG,'queryWgs', [], Wgs).

insert_object(Uri, Type, ObjectId, TransformId) :-
    current_object_pose(Uri, PoseList),
    PoseList =  [M00, M01, M02, M03, M10, M11, M12, M13, M20, M21, M22, M23, M30, M31, M32, M33], 
    jpl_list_to_array(PoseList, Array), 
    query_wgs_signal(Wgs),
    jpl_call(RSG,'insertNode', [Uri, WgsId], ObjectId), 
    jpl_call(RSG,'insertTransform', [ObjectId, WgsId, Array], TransformId).

rsg_dump_to_pdf(Root, Path) :-
    rsg_interface(RSG),
    jpl_call(RSG,'applyActionDot', [@(false)], _A), 
    jpl_call(RSG,'applyActionDot', [@(true)], _B), 
    jpl_call(RSG,'printDot', [Path, Root], _C). 

move_pdf_to_docker(Path, FileName) :-
    rsg_interface(RSG),
    jpl_call(RSG,'importPDF', [Path], FileName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%add_sherpa_object_perception(_, 'Foo').
%add_sherpa_object_perception('Bar', _ ).
%geo_reference_system_type(wgs_84,'WGS_84').

%get rsg geo information of given id  and assert it inside knowrob
add_rsg_point(Id,[Longitude,Latidute],Attr, Perception):-
  rdf_instance_from_class('http://knowrob.org/kb/knowrob.owl#GIS-Coordinate', Event),
  rdf_assert(Event, rdf:type, 'http://knowrob.org/kb/knowrob.owl#GIS-Coordinate').
  
rsg_pose(FromID,ToID,PoseMat):-
  rsg_interface(RSG),
  jpl_call(RSG,'queryTransform', [FromID,ToID], JplArr),
  jpl_array_to_list(JplArr, PoseMat).
  


  
  
%create_human_detection_perception(_,_,_,1) :- fail.
%create_sherpa_event(EventClass,GeoPose,UUID,Time,Props,EventInst) :- fail.




create_sherpa_event_instance(EventType, TimePoint,Event) :-
%  nth0(0, PerceptionTypes, PType),
  atom_concat('http://knowrob.org/kb/knowrob.owl#', EventType, EClass),
  rdf_instance_from_class(EClass, Event),
  rdf_assert(Event, rdf:type, EClass),-
  rdf_assert(Event, knowrob:startTime, TimePoint).
  
  
  
%%get height at
%% It is realy important to get the altitude of the points ithink i need a java service for that

create_geo_coord([Long,Lat], GeoPoseInst) :-
   create_geo_coord([Long,Lat,0],'WGS_84',GeoPoseInst).

create_geo_coord([Long,Lat,Alt], GeoPoseInst) :-
   create_geo_coord([Long,Lat,Alt],'WGS_84',GeoPoseInst).
   
   
create_geo_coord([Long,Lat],RefSystem, GeoPoseInst) :-
   create_geo_coord([Long,Lat,0],RefSystem,GeoPoseInst).

%add additional overrides for altitude and ReferenceSystem
generate_rsg_perception(Id,[Longitude,Latidute],Attrs,Perception):-
    create_rsg_coordinate([Longitude,Latidute], Point),
    create_rsg_point_Node(Id,Attrs, Object),
    create_rsg_perception(Point,Object,Perception).
    
create_rsg_perception(Point,Object,Perception):-
    rdf_instance_from_class(knowrob:'GIS-Perception', Perception),
    rdf_assert(Perception,knowrob:'GIS-objectActedOn',Object),
    rdf_assert(Perception,knowrob:'GIS-atCoordinate',Point),    
    rdf_assert(Perception, rdf:type, 'http://knowrob.org/kb/knowrob.owl#GIS-Perception').
    
   
create_rsg_coordinate([Longitude,Latidute], Point):-
    %rdf_instance_from_class('http://knowrob.org/kb/knowrob.owl#GIS-Perception', Perception),
    %rdf_assert(Perception, rdf:type, 'http://knowrob.org/kb/knowrob.owl#GIS-Perception'),
    %rdf_instance_from_class('http://knowrob.org/kb/knowrob.owl#GIS-Coordinate', Point),
    geo_reference_system_type(wgs_84,RefSystem),
    create_geo_coord([Longitude,Latidute,0.0],RefSystem,Point).
    
    
create_rsg_point_Node(Id,Attrs, Object):-
    rdf_instance_from_class('http://knowrob.org/kb/knowrob.owl#GIS-Object', Object),
    rdf_assert(Object,'http://knowrob.org/kb/knowrob.owl#withRSG-ID',literal(type(xsd:string,Id))),
    findall(KVPs,(member([K,V],Attrs),
                    create_gis_property(K,V, Property),
                    rdf_assert(Object,'http://knowrob.org/kb/knowrob.owl#GIS-describedBy',Property)
                    ),_).

create_rsg_point(Id,[Longitude,Latidute],Attrs, Point):-
    rdf_instance_from_class('http://knowrob.org/kb/knowrob.owl#GIS-Perception', Perception),
    %rdf_assert(Perception, rdf:type, 'http://knowrob.org/kb/knowrob.owl#GIS-Perception'),
    rdf_instance_from_class('http://knowrob.org/kb/knowrob.owl#GIS-Coordinate', Point),
    geo_reference_system_type(wgs_84,RefSystem),
    create_geo_coord([Longitude,Latidute,0.0],RefSystem,Pose),
    rdf_assert(Point,knowrob:'GIS-atCoordinate',Pose),
    
    findall(KVPs,(member([K,V],Attrs),
                    create_gis_property(K,V, Property),
                    rdf_assert(Point,'http://knowrob.org/kb/knowrob.owl#GIS-describedBy',Property)
                    ),_),
    rdf_assert(Point, rdf:type, 'http://knowrob.org/kb/knowrob.owl#GIS-Coordinate').
    
  
create_gis_property(Key,Value, Property):-
  rdf_instance_from_class(knowrob:'GIS-Property',Property),
  rdf_assert(Property,'http://knowrob.org/kb/knowrob.owl#GIS-Key', literal(type(xsd:string, Key))),
  rdf_assert(Property,'http://knowrob.org/kb/knowrob.owl#GIS-Value', literal(type(xsd:string, Value))).
  %rdf_assert(Property, rdf:type, knowrob:'GIS-Property').



create_geo_coord([Long,Lat,Alt],RefSystem, GeoPoseInst) :-

  rdf_instance_from_class(knowrob:'Coordinate', GeoPoseInst),

  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#Longitude',literal(type(xsd:float, Long))),
  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#Latitude',literal(type(xsd:float, Lat))),
  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#Altitude',literal(type(xsd:float, Alt))),
  
  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#ReferenceSystem',RefSystem).
  %rdf_assert(GeoPoseInst, rdf:type, knowrob:'Coordinate').
