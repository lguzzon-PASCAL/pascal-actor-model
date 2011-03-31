{
	This file is part of Pascal-Actor-Model.

	Pascal-Actor-Model is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	Pascal-Actor-Model is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Pascal-Actor-Model.  If not, see <http://www.gnu.org/licenses/>.
}

{$MODE DELPHI}{$M+}{$H+}

Unit
	Actors;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	SyncObjs,
	ContNrs;

Const
	ccMainThreadName = 'main';
	ccSwitchboardName = 'switchboard';
	ccDefaultTimeout = 1000;

Type	
	TInternalMessage = Record
		MsgStr : String[255];
		Data : Pointer;
	End;
	
	TActorThread = Class;
	TActorThreadClass = Class Of TActorThread;

	TCustomMessage = Class;
	TCustomMessage = Class
	Private
		fSender,
		fReceiver : String;
	Public
		Property Sender : String Read fSender Write fSender;
		Property Receiver : String Read fReceiver Write fReceiver;
		Constructor Create(Const aSender : String = ''; Const aReceiver : String = ''); Virtual;
	End;
	TCustomMessageClass = Class Of TCustomMessage;
	
	TQuitMessage = Class(TCustomMessage);
	TTermMessage = Class(TCustomMessage);

	TRegisterInstanceMessage = Class(TCustomMessage)
	Private
		fInstance : TActorThread;
	Public
		Property Instance : TActorThread Read fInstance Write fInstance;
	End;
	
	TUnregisterInstanceMessage = Class(TRegisterInstanceMessage);

	TRegisterClassMessage = Class(TCustomMessage)
	Private
		fClass : TActorThreadClass;
	Public
		Property ActorClass : TActorThreadClass Read fClass Write fClass;
	End;

	TInitInstanceMessage = Class(TCustomMessage)
	Private
		fInstance,
		fClass : String;
	Public
		Property NameOfActor : String Read fInstance Write fInstance;
		Property NameOfClass : String Read fClass Write fClass;
	End;

	TCustomSynchronizedQueue = Class
	Private
		fQueue        : TObjectQueue;
		fSynchronizer : TMultiReadExclusiveWriteSynchronizer;
		fSignal       : TEventObject;
		fActive       : Boolean;
	Public
		// This method waits for a new object in the queue during aTimeout time, if it is sucessfull it returns wrSignalled
		// The result values are the same as for TEventObject.WaitFor
		Function WaitFor(Const aTimeout : Cardinal): TWaitResult;
		// This method returns the ammount of objects waiting at the queue
		Function Count : Integer; 
		// This method returns true if there are at least aCount objects in the queue
		Function AtLeast(Const aCount : Integer): Boolean;
		// This method pushs (inserts) a new object into the queue 
		Procedure Push(Const aObject : TCustomMessage);
		// This method pops an object from the queue
		Function Pop : TCustomMessage;
		// Standard constructors and destructors
		Constructor Create;
		Destructor Destroy; Override;
	End;

	TActorThread = Class(TThread)
	Private
		fActorName :  String;
		fMailbox : TCustomSynchronizedQueue;
		fMessage : TCustomMessage;
		fRunning : Boolean;
		fFreeToDie : Boolean;
		fTimeout : Integer;
	Public
		// This actor name
		Property ActorName : String Read fActorName;
		// This actor mailbox
		Property Mailbox : TCustomSynchronizedQueue Read fMailbox;
		// The message being currently handled
		Property Message : TCustomMessage Read fMessage;
		// True weather the actor is running or preparing to die
		Property Running : Boolean Read fRunning;
		// The timeout being used at initmessage
		Property Timeout : Integer Read fTimeout;
		// This method reads a message from the message queue, it is blocking so your thread remains stopped until a message arrives
		Procedure InitMessage; Virtual;
		// This method frees the current message, you should invoke it when the message is not needed anymore
		Procedure DoneMessage; Virtual;
		// This method sends a message to the switchboard to be routed to the correct Actor
		Procedure Send(Const aMessage : TCustomMessage); Virtual;
		// This method updates Receiver and Sender accordingly (Sender is the current actor ActorName and receiver is aAddress) and sends it to switchboard
		Procedure SendTo(Const aAddress : String; aMessage : TCustomMessage);
		// This method takes the current message addresses and swaps then, use it to reply to a message
		Procedure Reply(aMessage : TCustomMessage);
		// This method automatically registers the current thread into the actor system
		Procedure RegisterMe;
		// This method automatically unregisters the current thread into the actor system
		Procedure UnregisterMe;
		// This method dispatchs the current message via the dispatchstr system
		Procedure DispatchMessage;
		// This constructor creates the thread and registers it into the system
		// aName is the actor name to use
		// CreateSuspended says weather the thread will be created in suspended or running condition, usually is not needed (its safe to start threads in suspend mode)
		// StackSize sets the thread stack size, if you know what you are doing, set this parameter accordingly
		// aTimeout sets the time the thread will sit waiting for a new message to arrive
		Constructor Create(Const aName : String = ''; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Virtual;
		// This destructor destroys everything and sends a message to the switchboard to make it know the Actor is dead
		Destructor Destroy; Override;
		// This method runs the thread and waits for messages, if you dont need the default behaviour, override it
		Procedure Execute; Override;
		// This method gets called repeatedly while the threads waits for messages, you should use it to work assynchronously, set a small timeout (or zero) if you want
		// this method to be the sleep point of the thread (as in when you wait for a UDP packet or similar blocking task)
		Procedure Idle; Virtual; Abstract;
		// This method is usefull to unbundle a message from the aMessage parameter a dispatchstr handler will receive
		Function UnbundleMessage(Const aMessage): TCustomMessage;
		// This is a default message handler that quits the thread upon receiving a TQuitMessage message
		Procedure Quit(Var aMessage); Message 'tquitmessage';
	End;
	
	TSwitchBoardActor = Class(TActorThread)
	Private
		fClasses : TClassList;
		fInstances : TFPHashObjectList;
	Public
		Property Instances : TFPHashObjectList Read fInstances;
		Procedure Send(Const aMessage : TCustomMessage); Override;
		Constructor Create(Const aName : String = ccSwitchboardName; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Execute; Override;
		Procedure Idle; Override;
		Procedure RegisterInstance(Var aMessage); Message 'tregisterinstancemessage';
		Procedure UnregisterInstance(Var aMessage); Message 'tunregisterinstancemessage';
		Procedure RegisterClass(Var aMessage); Message 'tregisterclassmessage';
		Procedure InitInstance(Var aMessage); Message 'tinitinstancemessage';
	End;

Var
	SwitchBoard : TSwitchBoardActor;
	MainQueue : TCustomSynchronizedQueue;

Implementation

// TCustomMessage

Constructor TCustomMessage.Create(Const aSender : String = ''; Const aReceiver : String = '');
Begin
	Inherited Create;
	fSender := aSender;
	fReceiver := aReceiver;
End;

// TCustomSynchronizedQueue

Function TCustomSynchronizedQueue.WaitFor(Const aTimeout : Cardinal): TWaitResult;
Begin
	Result := fSignal.WaitFor(aTimeout);
End;

Function TCustomSynchronizedQueue.Count : Integer;
Begin
	Try
		fSynchronizer.BeginRead;
		Result := fQueue.Count;
	Finally
		fSynchronizer.EndRead;
	End;
End;

Function TCustomSynchronizedQueue.AtLeast(Const aCount : Integer): Boolean;
Begin
	Try
		fSynchronizer.BeginRead;
		Result := fQueue.AtLeast(aCount);
	Finally
		fSynchronizer.EndRead;
	End;
End;

Procedure TCustomSynchronizedQueue.Push(Const aObject : TCustomMessage);
Begin
	Try
		fSynchronizer.BeginWrite;
		fQueue.Push(aObject);
		fSignal.SetEvent;
	Finally
		fSynchronizer.EndWrite;
	End;
End;

Function TCustomSynchronizedQueue.Pop: TCustomMessage;
Begin
	Try
		fSynchronizer.BeginWrite;
		Result := (fQueue.Pop As TCustomMessage)
	Finally
		fSynchronizer.EndWrite;
	End;
End;

Constructor TCustomSynchronizedQueue.Create;
Begin
	Inherited Create;
	fQueue := TObjectQueue.Create;
	fSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
	fSignal := TEventObject.Create(Nil, False, False, '');
	fActive := True;
End;

Destructor TCustomSynchronizedQueue.Destroy;
Begin
	While fQueue.AtLeast(1) Do
		fQueue.Pop.Free;
	FreeAndNil(fQueue);
	FreeAndNil(fSynchronizer);
	FreeAndNil(fSignal);
	Inherited Destroy;
End;

// TActorThread

Procedure TActorThread.InitMessage;
Begin
	fMessage := Nil;
	Repeat
		Idle;
		If fMailbox.AtLeast(1) Then
			fMessage := fMailbox.Pop
		Else
			If fMailbox.WaitFor(fTimeout) = wrSignaled Then
				fMessage := fMailbox.Pop
			Else
				fMessage := Nil;
	Until Assigned(fMessage);
End;

Procedure TActorThread.DoneMessage;
Begin
	If Assigned(fMessage) Then
		FreeAndNil(fMessage);
End;

Procedure TActorThread.Send(Const aMessage : TCustomMessage);
Begin
	SwitchBoard.Mailbox.Push(aMessage);
End;

Procedure TActorThread.SendTo(Const aAddress : String; aMessage : TCustomMessage);
Begin
	aMessage.Sender := fActorName;
	aMessage.Receiver := aAddress;
	Send(aMessage);
End;

Procedure TActorThread.Reply(aMessage : TCustomMessage);
Begin
	aMessage.Sender := fActorName;
	aMessage.Receiver := fMessage.Sender;
	Send(aMessage);
End;

Procedure TActorThread.RegisterMe;
Var
	lMessage : TRegisterInstanceMessage;
Begin
	lMessage := TRegisterInstanceMessage.Create(fActorName, ccSwitchboardName);
	lMessage.Instance := Self;
	SendTo(ccSwitchBoardName, lMessage);
End;

Procedure TActorThread.UnregisterMe;
Var
	lMessage : TRegisterInstanceMessage;
Begin
	lMessage := TUnregisterInstanceMessage.Create(fActorName, ccSwitchboardName);
	lMessage.Instance := Self;
	SendTo(ccSwitchBoardName, lMessage);
End;

Procedure TActorThread.DispatchMessage;
Var
	lMsg : TInternalMessage;
Begin
	lMsg.MsgStr := LowerCase(Message.ClassName);
	lMsg.Data := Pointer(Message);
	DispatchStr(lMsg);
	DoneMessage;
End;

Constructor TActorThread.Create(Const aName : String = ''; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(CreateSuspended, StackSize);
	If Length(aName) > 0 Then
		fActorName := aName
	Else
		fActorName := Self.ClassName;
	fTimeout := aTimeout;
	fMailbox := TCustomSynchronizedQueue.Create;
	fMessage := Nil;
	FreeOnTerminate := True;
	fRunning := True;
	fFreeToDie  := False;
End;

Destructor TActorThread.Destroy;
Begin
	While fMailbox.AtLeast(1) Do
	Begin
		InitMessage;
		DoneMessage;
	End;
	FreeAndNil(fMailbox);
	Inherited Destroy;
End;

Procedure TActorThread.Execute;
Begin
	RegisterMe;
	Repeat
		InitMessage;
		If Assigned(fMessage) Then
			DispatchMessage;
	Until fRunning = False;
	UnregisterMe;
	Repeat
		InitMessage;
		If Assigned(fMessage) And (fMessage Is TTermMessage) Then
			fFreeToDie := True;
		DoneMessage;
	Until fFreeToDie;
End;

Function TActorThread.UnbundleMessage(Const aMessage): TCustomMessage;
Begin
	Result := TCustomMessage(TInternalMessage(aMessage).Data);
End;

Procedure TActorThread.Quit(Var aMessage);
Begin
	fRunning := False;
End;

// TSwitchBoardActor

Procedure TSwitchBoardActor.Send(Const aMessage : TCustomMessage);
Var
	lTarget : TObject;
Begin
	If Assigned(aMessage) Then
	Begin
		lTarget := fInstances.Find((aMessage As TCustomMessage).Receiver);
		If Assigned(lTarget) Then 
			(lTarget As TActorThread).MailBox.Push(aMessage)
		Else
			aMessage.Free;
	End;
End;

Constructor TSwitchBoardActor.Create(Const aName : String = ccSwitchboardName; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
	FreeOnTerminate := False;
	fInstances := TFPHashObjectList.Create;
	fInstances.OwnsObjects := False;
	fClasses := TClassList.Create;
End;

Destructor TSwitchBoardActor.Destroy;
Begin
	fClasses.Free;
	fInstances.Free;
	Inherited Destroy;
End;

Procedure TSwitchBoardActor.Execute;
Var
	lCtrl : Integer;
Begin
	While Running Do
	Begin
		InitMessage;
		If Assigned(Message) Then
			If Message.Receiver = ccSwitchboardName Then
				DispatchMessage
			Else If Message.Receiver = ccMainThreadName Then
				MainQueue.Push(Message)
			Else
				Send(Message);
	End;
	For lCtrl := 0 To fInstances.Count - 1 Do
		SendTo((fInstances.Items[lCtrl] As TActorThread).ActorName, TQuitMessage.Create(ActorName, (fInstances.Items[lCtrl] As TActorThread).ActorName));	
	While fInstances.Count > 0 Do
	Begin
		InitMessage;
		If Assigned(Message) Then
			If Message.Receiver = ccSwitchboardName Then
				DispatchMessage;
	End;
End;

Procedure TSwitchBoardActor.Idle;
Begin
End;

Procedure TSwitchBoardActor.RegisterInstance(Var aMessage);
Begin
	fInstances.Add((UnbundleMessage(aMessage) As TRegisterInstanceMessage).Instance.ActorName, (UnbundleMessage(aMessage) As TRegisterInstanceMessage).Instance);
End;

Procedure TSwitchBoardActor.UnregisterInstance(Var aMessage);
Var
	lInstance : TActorThread;
Begin
	lInstance := (fInstances.Find((UnbundleMessage(aMessage) As TRegisterInstanceMessage).Sender) As TActorThread);
	fInstances.Remove((UnbundleMessage(aMessage) As TRegisterInstanceMessage).Instance);
	lInstance.Mailbox.Push(TTermMessage.Create(ActorName, lInstance.ActorName));
End;

Procedure TSwitchBoardActor.RegisterClass(Var aMessage);
Begin
	fClasses.Add((UnbundleMessage(aMessage) As TRegisterClassMessage).ActorClass);
End;

Procedure TSwitchBoardActor.InitInstance(Var aMessage);
Var
	lCtrl : Integer;
	lMsg : TInitInstanceMessage;
	lClass : TActorThreadClass;
Begin
	lMsg := UnbundleMessage(aMessage) As TInitInstanceMessage;
	For lCtrl := 0 To fClasses.Count - 1 Do
		If LowerCase(fClasses.Items[lCtrl].ClassName) = LowerCase(lMsg.NameOfClass) Then
		Begin
			lClass := TActorThreadClass(fClasses.Items[lCtrl]);
			lClass.Create(lMsg.NameOfActor).Start;
			Break;
		End;
End;

Initialization

	MainQueue := TCustomSynchronizedQueue.Create;
	SwitchBoard := TSwitchBoardActor.Create;
	SwitchBoard.Start;

Finalization

	SwitchBoard.Terminate;
	SwitchBoard.Free;
	While MainQueue.AtLeast(1) Do
		MainQueue.Pop.Free;
	MainQueue.Free;
	
End.