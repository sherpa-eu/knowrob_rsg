/** knowrob_rsg Reasoning about robot components and capabilities


  Copyright (C) 2016 Benjamin Brieber
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

  @author Benjamin Brieber
  @license BSD
*/

:- module(knowrob_rsg,
    [
      rsg_interface/0,
      rsg_interface/1,
      create_human_detection_perception/4,
      create_sherpa_event/6,
      add_sherpa_object_perception/2 ,   
      rsg_pose/3
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
  create_human_detection_perception(r,+,+,-),
  create_sherpa_event(r,+,+,+,+,-),
  add_sherpa_object_perception(r,r).
  
  
  

:- assert(rsg_int(fail)).
rsg_interface :- rsg_interface(_).

rsg_interface(RSG) :-
    rsg_int(fail),
    jpl_new('org.iai.knowrob_rsg.RSGConnection', [], RSG),
    retract(rsg_int(fail)),
    assert(rsg_int(RSG)),!.

rsg_interface(DB) :-
    rsg_int(DB).


  
  
add_sherpa_object_perception(_, 'Foo').
add_sherpa_object_perception('Bar', _ ).
  
rsg_pose(From,To,PoseMat):-
  rsg_interface(RSG),
  jpl_call(RSG,'queryTransform', 
  [From,To], JplArr),
  jpl_array_to_list(JplArr, PoseMat).
  


  
  
create_human_detection_perception(_,_,_,1) :-
fail.


create_sherpa_event(EventClass,GeoPose,UUID,Time,Props,EventInst) :-
fail.




create_sherpa_event_instance(EventType, TimePoint,Event) :-
%  nth0(0, PerceptionTypes, PType),
  atom_concat('http://knowrob.org/kb/knowrob.owl#', EventType, EClass),
  rdf_instance_from_class(EClass, Event),
  rdf_assert(Event, rdf:type, EClass),

  rdf_assert(Event, knowrob:startTime, TimePoint).
  
  
geo_reference_system_type(wgs_84,'WGS_84').
  
  
  
create_geo_coord([Long,Lat], GeoPoseInst) :-
   create_geo_coord([Long,Lat,0],'WGS_84',GeoPoseInst).

create_geo_coord([Long,Lat,Alt], GeoPoseInst) :-
   create_geo_coord([Long,Lat,Alt],'WGS_84',GeoPoseInst).
   
   
create_geo_coord([Long,Lat],RefSystem, GeoPoseInst) :-
   create_geo_coord([Long,Lat,0],RefSystem,GeoPoseInst).

create_geo_coord([Long,Lat,Alt],RefSystem, GeoPoseInst) :-

  rdf_instance_from_class(knowrob:'GeoCoord', GeoPoseInst),

  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#Longitude',literal(type(xsd:float, Long))),
  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#Latitude',literal(type(xsd:float, Lat))),
  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#Altitude',literal(type(xsd:float, Alt))),
  
  rdf_assert(GeoPoseInst,'http://knowrob.org/kb/knowrob.owl#ReferenceSystem',RefSystem).