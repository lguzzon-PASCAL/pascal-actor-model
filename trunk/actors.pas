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
		fTypeData : PTypeData;
		fPropList : PPropList;
		fPropCount : Integer;
		fActorName : String;
		fMailbox : TCustomSynchronizedQueue;
		fMessage : TCustomActorMessage;
		fRunning : Boolean;
		fTimeout : Integer;
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
		Function SaveMessage: TCustomActorMessage;
		Procedure Send(Const aMessage : TCustomActorMessage); Virtual;
		Procedure Reply(aMessage : TCustomActorMessage);
		Procedure Request(Var aMessage : TCustomActorMessage; Const aTimeout : Integer);
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
		Property Timeout : Integer Read fTimeout Write fTimeout;
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
		Procedure Route; Virtual;
		// Message handlers
		Procedure RegisterClass(Var aMessage); Message 'tregisterclassactormessage';
		Procedure UnregisterClass(Var aMessage); Message 'tunregisterclassactormessage';
		Procedure CreateInstance(Var aMessage); Message 'tcreateinstanceactormessage';
		Procedure CreateInstanceAndConfig(Var aMessage); Message 'tcreateinstanceandconfigactormessage';
		Procedure RemoveInstance(Var aMessage); Message 'tremoveactormessage';
	End;

Var
	MainThreadName : String;
	MainThreadQueue : TCustomSynchronizedQueue;
	SwitchBoard : TSwitchBoardActor;

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
	FreeOnTerminate := False;
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
	// Debug If Assigned(Message) Then WriteLn(Message.ClassName, ' From ', Message.Source, ' To ', Message.Destination, ' my name is ', ActorName);
End;

Procedure TActorThread.DoneMessage;
Begin
	If Assigned(fMessage) Then
		FreeAndNil(fMessage);
End;

Function TActorThread.SaveMessage: TCustomActorMessage;
Begin
	Result := fMessage;
	fMessage := Nil;
End;

Procedure TActorThread.Send(Const aMessage : TCustomActorMessage);
Begin
	SwitchBoard.Mailbox.Push(aMessage);
End;

Procedure TActorThread.Reply(aMessage : TCustomActorMessage);
Begin
	aMessage.Source := fActorName;
	aMessage.Destination := fMessage.Source;
	aMessage.TransactionID := fMessage.TransactionID;
	Send(aMessage);
End;

Procedure TActorThread.Request(Var aMessage : TCustomActorMessage; Const aTimeout : Integer);
Var
	lRequestID : Int64;
	lFound : Boolean;
	lStart : TDateTime;
	lTimeout : Integer;
Begin
	fMailbox.Push(SaveMessage);
	lRequestID := LastRequestID;
	Inc(LastRequestID);
	aMessage.TransactionID := lRequestID;
	Send(aMessage);
	lFound := False;
	lStart := Now;
	lTimeout := 0;
	While Not lFound And (lTimeout <= ccDefaultTimeout * 10) Do
	Begin
		lTimeout := MilliSecondsBetween(Now, lStart);
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
		If Assigned(fMessage) Then
			If fMessage.TransactionID = lRequestID Then
				lFound := True
			Else
				fMailbox.Push(SaveMessage);
	End;
	If Not lFound Then
		fMailbox.Push(SaveMessage);
End;

Procedure TActorThread.DispatchMessage;
Var
	lMsg : TInternalMessage;
Begin
	// Debug WriteLn('Dispatching message ', Message.ClassName);
	lMsg.MsgStr := LowerCase(Message.ClassName);
	lMsg.Data := Nil;
	DispatchStr(lMsg);
	DoneMessage;
End;

Procedure TActorThread.SetPropertyValueByName(Const aName : String; Const aValue : Variant);
Begin
	Case GetPropertyType(GetPropertyIndex(aName)) Of
		tkInteger	  : SetOrdProp(Self, aName, aValue);
		tkChar		  : SetStrProp(Self, aName, aValue);
		tkEnumeration : SetEnumProp(Self, aName, aValue);
		tkFloat	      : SetFloatProp(Self, aName, aValue);
		tkSet		  : SetSetProp(Self, aName, aValue);
		tkSString	  : SetStrProp(Self, aName, aValue);
		tkLString	  : SetStrProp(Self, aName, aValue);
		tkAString	  : SetStrProp(Self, aName, aValue);
		tkWString	  : SetStrProp(Self, aName, aValue);
		tkVariant	  : SetVariantProp(Self, aName, aValue);
		tkWChar	      : SetWideStrProp(Self, aName, aValue);
		tkBool		  : SetEnumProp(Self, aName, aValue);
		tkInt64	      : SetInt64Prop(Self, aName, aValue);
		tkQWord	      : SetInt64Prop(Self, aName, aValue);
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
		lMessage := fMessage As TConfigInstanceActorMessage;
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
	fInstances.OwnsObjects := True;
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
Begin
	While Running Do
	Begin
		InitMessage;
		If Assigned(Message) Then
			If Message.Destination = ActorName Then
				DispatchMessage
			Else
				If Message.Destination = MainThreadName Then
					MainThreadQueue.Push(SaveMessage)
				Else
					Route;
		DoneMessage;
	End;
	// Debug WriteLn('Quitting.');
	For lCtrl := 0 To fInstances.Count - 1 Do
	Begin
		// Debug WriteLn('Asking ', (fInstances.Items[lCtrl] As TActorThread).ActorName, ' to quit.');
		(fInstances.Items[lCtrl] As TActorThread).Mailbox.Push(TTerminateActorMessage.Create(ActorName, (fInstances.Items[lCtrl] As TActorThread).ActorName));
	End;
	Sleep(ccDefaultTimeout * 10);
	For lCtrl := 0 To fInstances.Count - 1 Do
	Begin
		// Debug WriteLn('Forcefully making ', (fInstances.Items[lCtrl] As TActorThread).ActorName, ' quit.');
		(fInstances.Items[lCtrl] As TActorThread).Terminate;
		fInstances.Delete(lCtrl);
	End;
	// Debug WriteLn(fInstances.Count);
End;

Procedure TSwitchBoardActor.Route;
Var
	lTarget : TObject;
Begin
	If Assigned(Message) Then
	Begin
		lTarget := fInstances.Find(Message.Destination);
		If Assigned(lTarget) Then 
			(lTarget As TActorThread).MailBox.Push(SaveMessage)
		Else
			DoneMessage;
	End;
End;

Procedure TSwitchBoardActor.RegisterClass(Var aMessage);
Var
	lMessage : TRegisterClassActorMessage;
Begin
	lMessage := Message As TRegisterClassActorMessage;
	fClasses.Add(lMessage.ClassReference.ClassName, Pointer(lMessage.ClassReference));
End;

Procedure TSwitchBoardActor.UnregisterClass(Var aMessage);
Var
	lMessage : TUnregisterClassActorMessage;
	lIndex : Integer;
Begin
	lMessage := Message As TUnregisterClassActorMessage;
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
	lMessage := Message As TCreateInstanceActorMessage;
	lIndex := fClasses.FindIndexOf(lMessage.NameOfClass);
	If lIndex >= 0 Then
	Begin
		lActor := TActorThreadClass(fClasses.Items[lIndex]).Create(lMessage.NameOfInstance, True);
		lActor.Start;
		fInstances.Add(lActor.ActorName, lActor);
	End;
End;

Procedure TSwitchBoardActor.CreateInstanceAndConfig(Var aMessage);
Var
	lMessage : TCreateInstanceAndConfigActorMessage;
	lActor : TActorThread;
	lIndex : Integer;
	lCtrl : Integer;
Begin
	lMessage := Message As TCreateInstanceAndConfigActorMessage;
	lIndex := fClasses.FindIndexOf(lMessage.NameOfClass);
	If lIndex >= 0 Then
	Begin
		lActor := TActorThreadClass(fClasses.Items[lIndex]).Create(lMessage.NameOfInstance, True);
		lActor.Start;
		fInstances.Add(lActor.ActorName, lActor);
		For lCtrl := 0 To lMessage.MessageCount - 1 Do
			lActor.Mailbox.Push(lMessage.Messages[lCtrl].Clone);
	End;
End;

Procedure TSwitchBoardActor.RemoveInstance(Var aMessage);
Var
	lMessage : TRemoveActorMessage;
	lIndex : Integer;
Begin
	lMessage := Message As TRemoveActorMessage;
	lIndex := fInstances.FindIndexOf(lMessage.Source);
	If lIndex >= 0 Then
	Begin
		// Debug WriteLn((fInstances.Items[lIndex] As TActorThread).ActorName, ' is quitting.');
		(fInstances.Items[lIndex] As TActorThread).Terminate;
		fInstances.Delete(lIndex);
	End;
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
	// Debug WriteLn('Asking switchboard to finish.');
	SwitchBoard.MailBox.Push(TTerminateActorMessage.Create(MainThreadName, SwitchBoard.ActorName));
	// Debug WriteLn('Waiting for switchboard to finish.');
	SwitchBoard.WaitFor;
	// Debug WriteLn('Freeing main queue.');
	MainThreadQueue.Free;
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
