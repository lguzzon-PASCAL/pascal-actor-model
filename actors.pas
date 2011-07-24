{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 - J. Aldo G. de Freitas Junior

{$MODE DELPHI}{$M+}{$H+}

Unit
	Actors;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	TypInfo,
	SyncObjs,
	Contnrs,
	SysUtils,
	Variants,
	RTTIObjects,
	SyncQueue,
	ActorMessages;

Const
	ccDefaultMainThreadName = 'main';
	ccDefaultSwitchBoardName = 'switchboard';
	ccDefaultRouter = 'router';
	ccDefaultLogger = 'logger';
	ccDefaultTimeout = 10;

Type
	TActorThread = Class;
	TActorThreadClass = Class Of TActorThread;

	// Internal dispatch messages

	TInternalMessage = Record
		MsgStr : String[255];
		Data : Pointer;
	End;

	// Basic Actor

	TActorThread = Class(TThread)
	Private
		fTypeData  : PTypeData;
		fPropList  : PPropList;
		fPropCount : Integer;
		fActorName : String;
		fMailbox   : TCustomSynchronizedQueue;
		fMessage   : TCustomActorMessage;
		fRunning   : Boolean;
		fFreeToDie : Boolean;
		fTimeout   : Integer;
	Public
		Constructor Create(
			Const aName : String = '';
			CreateSuspended : Boolean = False;
			Const StackSize : SizeUInt = DefaultStackSize;
			Const aTimeout : Integer = ccDefaultTimeout
		); Virtual;
		Destructor Destroy; Override;
		Procedure Execute; Override;
		Procedure Idle; Virtual;
		Procedure InitMessage; Virtual;
		Procedure DoneMessage; Virtual;
		Procedure Send(Const aMessage : TCustomActorMessage); Virtual;
		Procedure SendTo(Const aAddress : String; aMessage : TCustomActorMessage);
		Procedure ForwardTo(Const aAddress : String; Const aMessage : TCustomActorMessage); Overload;
		Procedure ForwardTo(Const aAddress : String); Overload;
		Procedure Reply(aMessage : TCustomActorMessage);
		Procedure RegisterMe;
		Procedure UnregisterMe;
		Procedure DispatchMessage;
		// RTTI Handling for config
		Function GetPropertyIndex(Const aName : String) : Integer;
		Function GetPropertyType(Const aIndex : Integer) : TTypeKind;
		Procedure SetPropertyValueByName(Const aName : String; Const aValue : Variant);
		Procedure InitRTTI;
		Procedure DoneRTTI;
		// Message handlers
		Procedure Quit(Var aMessage); Message 'tquitactormessage';
		Procedure ConfigInstance(Var aMessage); Message 'tconfiginstanceactormessage';
		// Properties
		Property ActorName : String Read fActorName;
		Property Running : Boolean Read fRunning Write fRunning;
		Property Mailbox : TCustomSynchronizedQueue Read fMailbox;
		Property Message : TCustomActorMessage Read fMessage Write fMessage;
	End;

	// Switchboard actor : Routes traffic between actors

	TSwitchBoardActor = Class(TActorThread)
	Private
		fClasses : TClassList;
		fInstances : TFPHashObjectList;
	Public
		Constructor Create(Const aName : String = ccDefaultSwitchBoardName; CreateSuspended : Boolean = False; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Execute; Override;
		Procedure Route(Const aMessage : TCustomActorMessage); Virtual;
		Procedure RouteTo(Const aAddress : String; Const aMessage : TCustomActorMessage); Virtual;
		// Message handlers
		Procedure RegisterInstance(Var aMessage); Message 'tregisterinstanceactormessage';
		Procedure UnregisterInstance(Var aMessage); Message 'tunregisterinstanceactormessage';
		Procedure RegisterClass(Var aMessage); Message 'tregisterclassactormessage';
		Procedure UnregisterClass(Var aMessage); Message 'tunregisterclassactormessage';
		Procedure CreateInstance(Var aMessage); Message 'tcreateinstanceactormessage';
		Procedure InjectMessage(Var aMessage); Message 'tcustomencapsulatedactormessage';
	End;

Var
	MainThreadName : String;
	MainQueue : TCustomSynchronizedQueue;
	SwitchBoard : TSwitchBoardActor;

Function UnbundleMessage(Const aMessage): TCustomActorMessage;
Procedure Init(Const aLocalName : String = ccDefaultMainThreadName; Const aLocalSwitchboardName : String = ccDefaultSwitchBoardName);
Procedure Fini;

Procedure RegisterActorClass(Const aClass : TClass);
Procedure StartActorInstance(Const aClassName, aInstanceName : String);
Procedure ConfigActor(Const aInstanceName, aVariable : String; Const aValue : Variant);

Implementation

// TActorThread

Constructor TActorThread.Create(
		Const aName : String = '';
		CreateSuspended : Boolean = False;
		Const StackSize : SizeUInt = DefaultStackSize;
		Const aTimeout : Integer = ccDefaultTimeout
	);
Begin
	Inherited Create(CreateSuspended, StackSize);
	// Debug WriteLn('Actor being created ', aName);
	If aName <> '' Then
		fActorName := aName
	Else
		fActorName := Self.ClassName + IntToStr(Random(MaxInt));
	fTimeout := aTimeout;
	fMailbox := TCustomSynchronizedQueue.Create;
	fMessage := Nil;
	FreeOnTerminate := True;
	fRunning := True;
	fFreeToDie  := False;
	InitRTTI;
End;

Destructor TActorThread.Destroy;
Begin
	While fMailbox.AtLeast(1) Do
	Begin
		InitMessage;
		DoneMessage;
	End;
	FreeAndNil(fMailbox);
	DoneRTTI;
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
		If Assigned(fMessage) And (fMessage Is TTermActorMessage) Then
			fFreeToDie := True;
		DoneMessage;
	Until fFreeToDie;
End;

Procedure TActorThread.Idle;
Begin
End;

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
	// Debug WriteLn(Message.ClassName, ' Time to live ', Message.TTL, ' From ', Message.Source, ' To ', Message.Destination, ' my name is ', ActorName);
End;

Procedure TActorThread.DoneMessage;
Begin
	If Assigned(fMessage) Then
		FreeAndNil(fMessage);
End;

Procedure TActorThread.Send(Const aMessage : TCustomActorMessage);
Begin
	SwitchBoard.Mailbox.Push(aMessage);
End;

Procedure TActorThread.SendTo(Const aAddress : String; aMessage : TCustomActorMessage);
Begin
	aMessage.Source := fActorName;
	aMessage.Destination := aAddress;
	Send(aMessage);
End;

Procedure TActorThread.ForwardTo(Const aAddress : String; Const aMessage : TCustomActorMessage);
Begin
	aMessage.Destination := aAddress;
	Send(aMessage);
End;

Procedure TActorThread.ForwardTo(Const aAddress : String);
Begin
	fMessage.Destination := aAddress;
	Send(fMessage);
	fMessage := Nil;
End;

Procedure TActorThread.Reply(aMessage : TCustomActorMessage);
Begin
	aMessage.Source := fActorName;
	aMessage.Destination := fMessage.Source;
	Send(aMessage);
End;

Procedure TActorThread.RegisterMe;
Var
	lMessage : TRegisterInstanceActorMessage;
Begin
	lMessage := TRegisterInstanceActorMessage.Create(fActorName, SwitchBoard.ActorName);
	lMessage.Name := ActorName;
	lMessage.ObjectReference := Self As TObject;
	SendTo(SwitchBoard.ActorName, lMessage);
End;

Procedure TActorThread.UnregisterMe;
Var
	lMessage : TUnregisterInstanceActorMessage;
Begin
	lMessage := TUnregisterInstanceActorMessage.Create(fActorName, SwitchBoard.ActorName);
	lMessage.Data := ActorName;
	SendTo(SwitchBoard.ActorName, lMessage);
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

Procedure TActorThread.SetPropertyValueByName(Const aName : String; Const aValue : Variant);
Begin
	Case GetPropertyType(GetPropertyIndex(aName)) Of
		tkInteger     : SetOrdProp(Self, aName, aValue);
		tkChar        : SetStrProp(Self, aName, aValue);
		tkEnumeration : SetEnumProp(Self, aName, aValue);
		tkFloat       : SetFloatProp(Self, aName, aValue);
		tkSet         : SetSetProp(Self, aName, aValue);
		tkSString     : SetStrProp(Self, aName, aValue);
		tkLString     : SetStrProp(Self, aName, aValue);
		tkAString     : SetStrProp(Self, aName, aValue);
		tkWString     : SetStrProp(Self, aName, aValue);
		tkVariant     : SetVariantProp(Self, aName, aValue);
		tkWChar       : SetWideStrProp(Self, aName, aValue);
		tkBool        : SetEnumProp(Self, aName, aValue);
		tkInt64       : SetInt64Prop(Self, aName, aValue);
		tkQWord       : SetInt64Prop(Self, aName, aValue);
	End;
End;

Function TActorThread.GetPropertyIndex(Const aName : String) : Integer;
Var
	lRow : Integer;
Begin
	Result := -1;
	For lRow := 0 To fPropCount - 1 Do
		If LowerCase(fPropList^[lRow]^.Name) = LowerCase(aName) Then
		Begin
			Result := lRow;
			Break;
		End;
End;

Function TActorThread.GetPropertyType(Const aIndex : Integer) : TTypeKind;
Begin
	Result := fPropList^[aIndex]^.PropType^.Kind;
End;

Procedure TActorThread.InitRTTI;
Begin
	fTypeData := GetTypeData(Self.ClassInfo);
	GetMem(fPropList, fTypeData^.PropCount * SizeOf(Pointer));
	fPropCount := GetPropList(Self.ClassInfo, tiStreamable, fPropList);
End;

Procedure TActorThread.DoneRTTI;
Begin
	FreeMem(fPropList, fTypeData^.PropCount * SizeOf(Pointer));
End;

Procedure TActorThread.Quit(Var aMessage);
Begin
	// Debug WriteLn(ActorName, ' is quitting.');
	fRunning := False;
End;

Procedure TActorThread.ConfigInstance(Var aMessage);
Var
	lMessage : TConfigInstanceActorMessage;
	lError : TErrorActorMessage;
Begin
	Try
		lMessage := UnbundleMessage(aMessage) As TConfigInstanceActorMessage;
		SetPropertyValueByName(lMessage.Name, lMessage.Value);
	Except
		On E: Exception Do 
		Begin
			lError := TErrorActorMessage.Create(ActorName, ccDefaultLogger);
			lError.Data := E.Message;
			Send(lError);
		End;
	End;
End;

// TSwitchBoardActor

Constructor TSwitchBoardActor.Create(
		Const aName : String = ccDefaultSwitchBoardName;
		CreateSuspended : Boolean = False;
		Const StackSize : SizeUInt = DefaultStackSize;
		Const aTimeout : Integer = ccDefaultTimeout
	);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
	FreeOnTerminate := False;
	fInstances := TFPHashObjectList.Create;
	fInstances.OwnsObjects := False;
	fClasses := TClassList.Create;
End;

Destructor TSwitchBoardActor.Destroy;
Begin
	FreeAndNil(fClasses);
	FreeAndNil(fInstances);
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
		Begin
			If Message.Destination = ActorName Then
				DispatchMessage
			Else If Message.Destination = MainThreadName Then
			Begin
				MainQueue.Push(Message);
				Message := Nil;
			End
			Else
			Begin
				Route(Message);
				Message := Nil;
			End;
		End;
		DoneMessage;
	End;
	// Debug WriteLn('Quitting.');
	For lCtrl := 0 To fInstances.Count - 1 Do
	Begin
		// Debug WriteLn('Asking ', (fInstances.Items[lCtrl] As TActorThread).ActorName, ' to quit.');
		((fInstances.Items[lCtrl] As TActorThread) As TActorThread).Mailbox.Push(TQuitActorMessage.Create(ActorName, (fInstances.Items[lCtrl] As TActorThread).ActorName));
	End;
	While fInstances.Count > 0 Do
	Begin
		InitMessage;
		If Assigned(Message) And Not(Message Is TCreateInstanceActorMessage) Then
		Begin
			If Message.Destination = ActorName Then
				DispatchMessage
			Else If Message.Destination = MainThreadName Then
			Begin
				MainQueue.Push(Message);
				Message := Nil;
			End
			Else
			Begin
				Route(Message);
				Message := Nil;
			End;
		End;
		DoneMessage;
	End;
End;

Procedure TSwitchBoardActor.Route(Const aMessage : TCustomActorMessage);
Begin
	RouteTo((aMessage As TCustomActorMessage).Destination, aMessage);
End;

Procedure TSwitchBoardActor.RouteTo(Const aAddress : String; Const aMessage : TCustomActorMessage);
Var
	lTarget : TObject;
	lError : TErrorActorMessage;
Begin
	If Assigned(aMessage) Then
	Begin
		aMessage.DecreaseTTL;
		If aMessage.IsTTLTimeout Then
		Begin
			lError := TErrorActorMessage.Create(ActorName, 'screen1');
			lError.Data := 'Message TTL expired. ' + aMessage.Source + ':' + aMessage.Destination;
			Mailbox.Push(lError);
			aMessage.Free;
		End;
		lTarget := fInstances.Find(aAddress);
		If Assigned(lTarget) Then 
			(lTarget As TActorThread).MailBox.Push(aMessage)
		Else
		Begin
			// Debug  WriteLn('Couldnt find instance ', aAddress, ', trying to route via default router.');
			lTarget := fInstances.Find(ccDefaultRouter);
			If Assigned(lTarget) Then 
				(lTarget As TActorThread).MailBox.Push(aMessage)
			Else
			Begin
				// Debug  WriteLn('No router found, trying to loopback. TTL := ', aMessage.TTL);
					Mailbox.Push(aMessage);
			End;
		End;
	End;
End;

Procedure TSwitchBoardActor.RegisterInstance(Var aMessage);
Var
	lMessage : TRegisterInstanceActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TRegisterInstanceActorMessage;
	// Debug  WriteLn('Appending ', lMessage.Name, ' to our list of active actors.');
	fInstances.Add(lMessage.Name, lMessage.ObjectReference);
End;

Procedure TSwitchBoardActor.UnregisterInstance(Var aMessage);
Var
	lInstance : TActorThread;
	lMessage : TUnregisterInstanceActorMessage;
Begin
	Try
		lMessage := UnbundleMessage(aMessage) As TUnregisterInstanceActorMessage;
		lInstance := (fInstances.Find(lMessage.Source) As TActorThread);
		fInstances.Remove(lInstance);
		lInstance.Mailbox.Push(TTermActorMessage.Create(ActorName, lInstance.ActorName));
	Except
		On E: Exception Do ;
	End;
End;

Procedure TSwitchBoardActor.RegisterClass(Var aMessage);
Var
	lMessage : TRegisterClassActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TRegisterClassActorMessage;
	fClasses.Add(lMessage.ClassReference);
End;

Procedure TSwitchBoardActor.UnregisterClass(Var aMessage);
Var
	lMessage : TUnregisterClassActorMessage;
	lCtrl : Integer;
	lClass : TActorThreadClass;
Begin
	lMessage := UnbundleMessage(aMessage) As TUnregisterClassActorMessage;
	For lCtrl := 0 To fClasses.Count - 1 Do
		If LowerCase(fClasses[lCtrl].ClassName) = LowerCase(lMessage.Data) Then
		Begin
			lClass := TActorThreadClass(fClasses[lCtrl]);
			fClasses.Remove(lClass);
			Exit;
		End;
End;

Procedure TSwitchBoardActor.CreateInstance(Var aMessage);
Var
	lCtrl : Integer;
	lMessage : TCreateInstanceActorMessage;
	lClass : TActorThreadClass;
Begin
	lMessage := UnbundleMessage(aMessage) As TCreateInstanceActorMessage;
	For lCtrl := 0 To fClasses.Count - 1 Do
		If LowerCase(fClasses.Items[lCtrl].ClassName) = LowerCase(lMessage.NameOfClass) Then
		Begin
			// Debug WriteLn('Creating a instance of ', lMessage.NameOfClass, ' named ', lMessage.NameOfInstance);
			lClass := TActorThreadClass(fClasses.Items[lCtrl]);
			lClass.Create(lMessage.NameOfInstance);
			Break;
		End;
End;

Procedure TSwitchBoardActor.InjectMessage(Var aMessage);
Var
	lMessage : TCustomEncapsulatedActorMessage;
	lTarget : TActorThread;
Begin
	lMessage := UnbundleMessage(aMessage) As TCustomEncapsulatedActorMessage;
	If (lMessage.Encapsulated.Destination = ActorName) Or (lMessage.Encapsulated.Destination = MainThreadName) Then
	Begin
		Mailbox.Push(lMessage.Encapsulated);
		lMessage.Encapsulated := Nil;
	End
	Else
	Begin
		lTarget := fInstances.Find(lMessage.Encapsulated.Destination) As TActorThread;
		If Assigned(lTarget) Then 
		Begin
			(lTarget As TActorThread).MailBox.Push(lMessage.Encapsulated);
			lMessage.Encapsulated := Nil;
		End;
	End;
End;

Function UnbundleMessage(Const aMessage): TCustomActorMessage;
Begin
	Result := TCustomActorMessage(TInternalDispatchMessage(aMessage).Data);
End;

Procedure Init(Const aLocalName : String = ccDefaultMainThreadName; Const aLocalSwitchboardName : String = ccDefaultSwitchBoardName);
Begin
	ActorMessageClassFactory.RegisterMessage(TCustomActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomEncapsulatedActorMessage);
	ActorMessageClassFactory.RegisterMessage(TForeignActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomStringActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomNameValueActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomStreamActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomObjectReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomClassReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomNamedObjectReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomNamedClassReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TSetTargetActorMessage);
	ActorMessageClassFactory.RegisterMessage(TAddTargetActorMessage);
	ActorMessageClassFactory.RegisterMessage(TDeleteTargetActorMessage);
	ActorMessageClassFactory.RegisterMessage(TRegisterInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TUnregisterInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TQuitActorMessage);
	ActorMessageClassFactory.RegisterMessage(TTermActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCreateInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TConfigInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TRegisterClassActorMessage);
	ActorMessageClassFactory.RegisterMessage(TUnregisterClassActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomLogActorMessage);
	ActorMessageClassFactory.RegisterMessage(TLogActorMessage);
	ActorMessageClassFactory.RegisterMessage(TWarningActorMessage);
	ActorMessageClassFactory.RegisterMessage(TErrorActorMessage);
	ActorMessageClassFactory.RegisterMessage(TDebugActorMessage);
	ActorMessageClassFactory.RegisterMessage(TInfoActorMessage);
	MainThreadName := aLocalName;
	MainQueue := TCustomSynchronizedQueue.Create;
	SwitchBoard := TSwitchBoardActor.Create(aLocalSwitchboardName);
	SwitchBoard.Start;
End;

Procedure Fini;
Begin
	SwitchBoard.MailBox.Push(TQuitActorMessage.Create(MainThreadName, SwitchBoard.ActorName));
	SwitchBoard.WaitFor;
	FreeAndNil(MainQueue);
	FreeAndNil(SwitchBoard);
End;

Procedure RegisterActorClass(Const aClass : TClass);
Var
	lMessage : TRegisterClassActorMessage;
Begin
	lMessage := TRegisterClassActorMessage.Create(MainThreadName, Switchboard.ActorName);
	lMessage.ClassReference := aClass;
	SwitchBoard.Mailbox.Push(lMessage);
End;

Procedure StartActorInstance(Const aClassName, aInstanceName : String);
Var
	lCreate : TCreateInstanceActorMessage;
Begin
	lCreate := TCreateInstanceActorMessage.Create(MainThreadName, Switchboard.ActorName);
	lCreate.NameOfInstance := aInstanceName;
	lCreate.NameOfClass := aClassName;
	Switchboard.Mailbox.Push(lCreate);
End;

Procedure ConfigActor(Const aInstanceName, aVariable : String; Const aValue : Variant);
Var
	lConfig : TConfigInstanceActorMessage;
Begin
	lConfig := TConfigInstanceActorMessage.Create(MainThreadName, aInstanceName);
	lConfig.Name := aVariable;
	lConfig.Value := aValue;
	Switchboard.Mailbox.Push(lConfig);
End;

End.