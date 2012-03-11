{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 2012 - J. Aldo G. de Freitas Junior

{$mode objfpc}
{$H+}{$M+}

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
	ActorMessages,
	DateUtils;

Const
	ccDefaultMainThreadName = 'main';
	ccDefaultSwitchBoardName = 'switchboard';
	ccDefaultLogger = 'logger';
	ccDefaultTimeout = 100;

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
		Procedure DispatchMessage;
		// RTTI Handling for config
		Function GetPropertyIndex(Const aName : String) : Integer;
		Function GetPropertyType(Const aIndex : Integer) : TTypeKind;
		Procedure SetPropertyValueByName(Const aName : String; Const aValue : Variant);
		Procedure InitRTTI;
		Procedure DoneRTTI;
		// Message handlers
		Procedure Quit(Var aMessage); Message 'tterminateactormessage';
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
		fClasses : TFPHashList;
		fInstances : TFPHashObjectList;
	Public
		Constructor Create(Const aName : String = ccDefaultSwitchBoardName; CreateSuspended : Boolean = False; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Execute; Override;
		Procedure Route(Const aMessage : TCustomActorMessage); Virtual;
		Procedure RouteTo(Const aAddress : String; Const aMessage : TCustomActorMessage); Virtual;
		// Message handlers
		Procedure RegisterClass(Var aMessage); Message 'tregisterclassactormessage';
		Procedure UnregisterClass(Var aMessage); Message 'tunregisterclassactormessage';
		Procedure CreateInstance(Var aMessage); Message 'tcreateinstanceactormessage';
		Procedure RemoveInstance(Var aMessage); Message 'tremoveactormessage';
	End;

Var
	MainThreadName : String;
	MainThreadQueue : TCustomSynchronizedQueue;
	SwitchBoard : TSwitchBoardActor;

Function UnbundleMessage(Const aMessage): TCustomActorMessage;
Procedure Init(Const aLocalName : String = ccDefaultMainThreadName; Const aLocalSwitchboardName : String = ccDefaultSwitchBoardName);
Procedure Fini;

Procedure RegisterActorClass(Const aClass : TClass);
Procedure StartActorInstance(Const aClassName, aInstanceName : String);
Procedure ConfigActor(Const aInstanceName, aVariable : String; Const aValue : Variant);

Function ReceiveMessage: TCustomActorMessage;

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
	InitRTTI;
End;

Destructor TActorThread.Destroy;
Begin
	FreeAndNil(fMailbox);
	DoneRTTI;
	Inherited Destroy;
End;

Procedure TActorThread.Execute;
Begin
	Repeat
		InitMessage;
		If Assigned(fMessage) Then
			DispatchMessage;
	Until fRunning = False;
	SendTo(Switchboard.ActorName, TRemoveActorMessage.Create(ActorName, Switchboard.ActorName));
End;

Procedure TActorThread.Idle;
Begin
End;

Procedure TActorThread.InitMessage;
Begin
	fMessage := Nil;
	Idle;
	If fMailbox.AtLeast(1) Then
		fMessage := fMailbox.Pop
	Else
		If fTimeout > 0 Then
			If fMailbox.WaitFor(fTimeout) = wrSignaled Then
				fMessage := fMailbox.Pop
			Else
				fMessage := Nil
		Else
			fMessage := Nil;
	// Debug If Assigned(Message) Then WriteLn(Message.ClassName, ' Time to live ', Message.TTL, ' From ', Message.Source, ' To ', Message.Destination, ' my name is ', ActorName);
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
		tkInteger	 : SetOrdProp(Self, aName, aValue);
		tkChar		: SetStrProp(Self, aName, aValue);
		tkEnumeration : SetEnumProp(Self, aName, aValue);
		tkFloat	   : SetFloatProp(Self, aName, aValue);
		tkSet		 : SetSetProp(Self, aName, aValue);
		tkSString	 : SetStrProp(Self, aName, aValue);
		tkLString	 : SetStrProp(Self, aName, aValue);
		tkAString	 : SetStrProp(Self, aName, aValue);
		tkWString	 : SetStrProp(Self, aName, aValue);
		tkVariant	 : SetVariantProp(Self, aName, aValue);
		tkWChar	   : SetWideStrProp(Self, aName, aValue);
		tkBool		: SetEnumProp(Self, aName, aValue);
		tkInt64	   : SetInt64Prop(Self, aName, aValue);
		tkQWord	   : SetInt64Prop(Self, aName, aValue);
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
	FreeOnTerminate := True;
	fInstances := TFPHashObjectList.Create;
	fInstances.OwnsObjects := False;
	fClasses := TFPHashList.Create;
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
	lStart : TDateTime;
	lTimeout : Integer;
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
				MainThreadQueue.Push(Message);
				Message := Nil;
			End
			Else
				Route(Message);
		End;
		DoneMessage;
	End;
	// Debug WriteLn('Quitting.');
	For lCtrl := 0 To fInstances.Count - 1 Do
	Begin
		// Debug WriteLn('Asking ', (fInstances.Items[lCtrl] As TActorThread).ActorName, ' to quit.');
		(fInstances.Items[lCtrl] As TActorThread).Mailbox.Push(TTerminateActorMessage.Create(ActorName, (fInstances.Items[lCtrl] As TActorThread).ActorName));
	End;
	lStart := Now;
	lTimeout := 0;
	While (fInstances.Count > 0) And (lTimeout <= ccDefaultTimeout * 10) Do
	Begin
		InitMessage;
		lTimeout := MilliSecondsBetween(Now, lStart);
		// Debug  WriteLn(Message.Source, ':', Message.Destination, ' -> ', Message.ClassName, ' at ', lTimeout);
		If Message Is TRemoveActorMessage Then
			DispatchMessage;
		DoneMessage;
	End;
	For lCtrl := 0 To fInstances.Count - 1 Do
	Begin
		// Debug WriteLn('Forcefully making ', (fInstances.Items[lCtrl] As TActorThread).ActorName, ' quit.');
		(fInstances.Items[lCtrl] As TActorThread).Terminate;
	End;
End;

Procedure TSwitchBoardActor.Route(Const aMessage : TCustomActorMessage);
Begin
	RouteTo((aMessage As TCustomActorMessage).Destination, aMessage);
End;

Procedure TSwitchBoardActor.RouteTo(Const aAddress : String; Const aMessage : TCustomActorMessage);
Var
	lTarget : TObject;
Begin
	If Assigned(aMessage) Then
	Begin
		lTarget := fInstances.Find(aAddress);
		If Assigned(lTarget) Then 
		Begin
			(lTarget As TActorThread).MailBox.Push(aMessage);
			fMessage := Nil;
		End;
	End;
End;

Procedure TSwitchBoardActor.RegisterClass(Var aMessage);
Var
	lMessage : TRegisterClassActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TRegisterClassActorMessage;
	fClasses.Add(lMessage.ClassReference.ClassName, Pointer(lMessage.ClassReference));
End;

Procedure TSwitchBoardActor.UnregisterClass(Var aMessage);
Var
	lMessage : TUnregisterClassActorMessage;
	lIndex : Integer;
Begin
	lMessage := UnbundleMessage(aMessage) As TUnregisterClassActorMessage;
	lIndex := fClasses.FindIndexOf(lMessage.Data);
	If lIndex >= 0 Then
		fClasses.Delete(lIndex);
End;

Procedure TSwitchBoardActor.CreateInstance(Var aMessage);
Var
	lMessage : TCreateInstanceActorMessage;
	lActor : TActorThread;
	lIndex : Integer;
Begin
	lMessage := UnbundleMessage(aMessage) As TCreateInstanceActorMessage;
	lIndex := fClasses.FindIndexOf(lMessage.NameOfClass);
	If lIndex >= 0 Then
	Begin
		lActor := TActorThreadClass(fClasses.Items[lIndex]).Create(lMessage.NameOfInstance, True);
		lActor.Start;
		fInstances.Add(lActor.ActorName, lActor);
	End;
End;

Procedure TSwitchBoardActor.RemoveInstance(Var aMessage);
Var
	lMessage : TRemoveActorMessage;
	lIndex : Integer;
Begin
	lMessage := UnbundleMessage(aMessage) As TRemoveActorMessage;
	lIndex := fInstances.FindIndexOf(lMessage.Source);
	If lIndex >= 0 Then
	Begin
		(fInstances.Items[lIndex] As TActorThread).Terminate;
		fInstances.Delete(lIndex);
	End;
End;

Function UnbundleMessage(Const aMessage): TCustomActorMessage;
Begin
	Result := TCustomActorMessage(TInternalDispatchMessage(aMessage).Data);
End;

Procedure Init(Const aLocalName : String = ccDefaultMainThreadName; Const aLocalSwitchboardName : String = ccDefaultSwitchBoardName);
Begin
	MainThreadName := aLocalName;
	MainThreadQueue := TCustomSynchronizedQueue.Create;
	SwitchBoard := TSwitchBoardActor.Create(aLocalSwitchboardName, True);
	SwitchBoard.Start;
End;

Procedure Fini;
Begin
	SwitchBoard.MailBox.Push(TTerminateActorMessage.Create(MainThreadName, SwitchBoard.ActorName));
	SwitchBoard.WaitFor;
	FreeAndNil(MainThreadQueue);
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

Function ReceiveMessage: TCustomActorMessage;
Begin
	Result := Nil;
	If MainThreadQueue.AtLeast(1) Then
		Result := MainThreadQueue.Pop
	Else
		If MainThreadQueue.WaitFor(ccDefaultTimeout) = wrSignaled Then
			Result := MainThreadQueue.Pop;
End;

End.
