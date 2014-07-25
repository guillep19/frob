

central:  Task Control Server 3.7.3 (June-25-02)
central:  Expecting 1 on port 1381
central:   Received a new connection: 7
central:     modName : ServerAgent
central:     hostName: localhost.localdomain
central:   Received a new connection: 8
central:     modName : ClientAgent
central:     hostName: localhost.localdomain

 Server:  serverSide/stdout
 Server:  serverSide/stderr
 Server:  Task Control Management x.y.z (MON-DAY-YEAR)
 Server:  Attempting to connect to IPC central server on localhost... connected.
 Server:  TCM_EnableDistributedComm ( "ServerAgent", "(null)" )  SUCCEEDED.
 Server:  Number of Distributed Tasks Registered:  1

 Client:  clientSide/stdout
 Client:  clientSide/stderr
 Client:  Task Control Management x.y.z (MON-DAY-YEAR)
 Client:  Attempting to connect to IPC central server on localhost... connected.
 Client:  TCM_EnableDistributedComm ( "ClientAgent", "(null)" )  SUCCEEDED.
 Client:  Number of Distributed Tasks Registered:  1
 Client:  Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
 Client:  Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
 Client:  _TDL_OnAgent ( Constrain=foo-0, Agent-Name="ServerAgent" )
 Client:  Constraint:  _TDL_OnAgent  (0x........)
 Client:   Agent-Name = "ServerAgent"
 Client:  
 Client:  Virtual               foo {2}:        TCM {1} --> ON HOLD         (Inactive)
 Client:    Success  test-auto,wait {1}:

central:  Broadcast ServerAgent_Allocate_Task_Msg: ClientAgent --> ServerAgent     (Sent)
central:    Done    ServerAgent_Allocate_Task_Msg:
central:  Broadcast ServerAgent_Set_Instance_Name_Action_Msg: ClientAgent --> ServerAgent     (Sent)
central:    Done    ServerAgent_Set_Instance_Name_Action_Msg:
central:  Broadcast ServerAgent_Set_Action_foo: ClientAgent --> ServerAgent     (Sent)
central:    Done    ServerAgent_Set_Action_foo:
central:  Broadcast ServerAgent_Add_Parent_Msg: ClientAgent --> ServerAgent     (Sent)
central:    Done    ServerAgent_Add_Parent_Msg:
central:  Broadcast ServerAgent_Signal_Msg: ClientAgent --> ServerAgent     (Sent)
central:    Done    ServerAgent_Signal_Msg:
central:  Broadcast ServerAgent_Signal_Msg: ClientAgent --> ServerAgent     (Sent)
central:    Done    ServerAgent_Signal_Msg:
central:  Broadcast ClientAgent_Remove_Expected_Msg: ServerAgent --> ClientAgent     (Sent)
central:  Broadcast ClientAgent_Remove_Requested_Msg: ServerAgent --> Resource ClientAgent (Pending)
central:  Broadcast ClientAgent_Remove_Child_Msg: ServerAgent --> Resource ClientAgent (Pending)
central:  Broadcast ClientAgent_Signal_Msg: ServerAgent --> Resource ClientAgent (Pending)
central:  Broadcast ClientAgent_Signal_Msg: ServerAgent --> Resource ClientAgent (Pending)
central:    Done    ClientAgent_Remove_Expected_Msg:
central:  Broadcast ClientAgent_Remove_Requested_Msg: Resource ClientAgent --> ClientAgent     (Sent)
central:    Done    ClientAgent_Remove_Requested_Msg:
central:  Broadcast ClientAgent_Remove_Child_Msg: Resource ClientAgent --> ClientAgent     (Sent)
central:    Done    ClientAgent_Remove_Child_Msg:
central:  Broadcast ClientAgent_Signal_Msg: Resource ClientAgent --> ClientAgent     (Sent)
central:    Done    ClientAgent_Signal_Msg:
central:  Broadcast ClientAgent_Signal_Msg: Resource ClientAgent --> ClientAgent     (Sent)
central:    Done    ClientAgent_Signal_Msg:

 Server:  Goal                  foo {2}:  ON HOLD  --> TCM             (Sent)
 Server:  TEST foo: ()
 Server:    Success             foo {2}:

central:  Closed Connection Detected from: sd: 8: 
central:   Closing ClientAgent on localhost.localdomain
central:  close Module: Closing ClientAgent

 Client:  clientSide ending.

central:  Closed Connection Detected from: sd: 7: 
central:   Closing ServerAgent on localhost.localdomain
central:  close Module: Closing ServerAgent

 Server:  serverSide ending.

[TDL_ForkChildren]  Now sending SIGINT to all remaining children processes...

central:  Cumulative Memory Usage:
central:    Requests: 9757 (357342 bytes)
central:  Data Msg Buffer Stats:
central:    Total Alloc  : 382
central:    Total Freed  : 382
central:    Min Request  : 64
central:    Max Request  : 108
central:  Central Abort : Signal 2

