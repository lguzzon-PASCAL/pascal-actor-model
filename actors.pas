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
		Function Clone : TCustomMessage; Virtual;
		Function AsString : String; Virtual;
		Procedure SetAsString(Const aString : String); Virtual;
	End;
	TCustomMessageClass = Class Of TCustomMessage;
	
	TQuitMessage = Class(TCustomMessage);
	TQuitConfirmMessage = Class(TCustomMessage);
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
		Function WaitFor(Const aTimeout : Cardinal): TWaitResult;
		Function Count : Integer; 
		Function AtLeast(Const aCount : Integer): Boolean;
		Procedure Push(Const aObject : TCustomMessage);
		Function Pop : TCustomMessage;
		Constructor Create;
		Destructor Destroy; Override;
	End;

	TActorThread = Class(TThread)
	Private
		fActorName :  String;
		fMailbox   : TCustomSynchronizedQueue;
		fMessage   : TCustomMessage;
		fRunning   : Boolean;
		fFreeToDie : Boolean;
		fTimeout   : Integer;
	Public
		Property ActorName : String Read fActorName;
		Property Mailbox : TCustomSynchronizedQueue Read fMailbox;
		Property Message : TCustomMessage Read fMessage;
		Property Running : Boolean Read fRunning;
		Property Timeout : Integer Read fTimeout;
		Procedure InitMessage; Virtual;
		Procedure DoneMessage; Virtual;
		Procedure Send(Const aMessage : TCustomMessage); Virtual;
		Procedure SendTo(Const aAddress : String; aMessage : TCustomMessage);
		Procedure Reply(aMessage : TCustomMessage);
		Procedure RegisterMe;
		Procedure UnregisterMe;
		Procedure DispatchMessage;
		Constructor Create(Const aName : String = ''; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Virtual;
		Destructor Destroy; Override;
		Procedure Execute; Override;
		Procedure Idle; Virtual; Abstract;
		Function UnbundleMessage(Const aMessage): TCustomMessage;
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

Implementation

// TCustomMessage

Constructor TCustomMessage.Create(Const aSender : String = ''; Const aReceiver : String = '');
Begin
	Inherited Create;
	fSender := aSender;
	fReceiver := aReceiver;
End;

Function TCustomMessage.Clone : TCustomMessage;
Begin
	Result := TCustomMessage.Create(fSender, Receiver);
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
			(lTarget As TActorThread).MailBox.Push(aMessage);
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
			If Message.Receiver = ActorName Then
				DispatchMessage
			Else
				Send(Message);
	End;
	For lCtrl := 0 To fInstances.Count - 1 Do
		SendTo((fInstances.Items[lCtrl] As TActorThread).ActorName, TQuitMessage.Create(ActorName, (fInstances.Items[lCtrl] As TActorThread).ActorName));	
	While fInstances.Count > 0 Do
	Begin
		InitMessage;
		If Assigned(Message) Then
			If Message.Receiver = ActorName Then
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

	SwitchBoard := TSwitchBoardActor.Create;
	SwitchBoard.Start;

Finalization

	SwitchBoard.Terminate;
	SwitchBoard.Free;

End.