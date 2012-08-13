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
	RTTIThreads,
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

	// Basic Actor

	TActorThread = Class(TRTTIThread)
	Private
		fActorName : String;
		fMailbox : TCustomSynchronizedQueue;
		fRunning : Boolean;
		fTimeout : Integer;
		Function GetTopMessage: TCustomActorMessage;
	Public
		// Message handlers
		Procedure Quit(Var aMessage); Message 'TTerminateActorMessage';
		Procedure ConfigInstance(Var aMessage); Message 'TConfigInstanceActorMessage';
		Procedure GetConfigOfInstance(Var aMessage); Message 'TGetConfigInstanceActorMessage';
		// Misc
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Virtual;
		Destructor Destroy; Override;
		Procedure Execute; Override;
		Procedure Idle; Virtual;
		Procedure Send(Const aMessage : TCustomActorMessage);
		Function Request(Const aMessage : TCustomActorMessage): Boolean;
		Procedure Reply(aMessage : TCustomActorMessage);
		Procedure DispatchTopMessage;
		// Properties
		Property ActorName : String Read fActorName;
		Property Running : Boolean Read fRunning Write fRunning;
		Property Timeout : Integer Read fTimeout Write fTimeout;
		Property Mailbox : TCustomSynchronizedQueue Read fMailbox;
		Property Message : TCustomActorMessage Read GetTopMessage;
	End;

	// Switchboard actor : Routes traffic between actors

	TSwitchBoardActor = Class(TActorThread)
	Private
		fClasses : TFPHashList;
		fInstances : TFPHashObjectList;
	Public
		Constructor Create(Const aName : String = ccDefaultSwitchBoardName; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Execute; Override;
		Procedure Route;
		Procedure AskAllActorsToQuit;
		Procedure KillAllActors;
		// Message handlers
		Procedure RegisterClass(Var aMessage); Message 'TRegisterClassActorMessage';
		Procedure UnregisterClass(Var aMessage); Message 'TUnregisterClassActorMessage';
		Procedure CreateInstance(Var aMessage); Message 'TCreateInstanceActorMessage';
		Procedure CreateInstanceAndConfig(Var aMessage); Message 'TCreateInstanceAndConfigActorMessage';
		Procedure RemoveInstance(Var aMessage); Message 'TRemoveActorMessage';
	End;

Var
	MainThreadName : String;
	MainThreadQueue : TCustomSynchronizedQueue;
	SwitchBoard : TSwitchBoardActor;

Procedure Init(Const aLocalName : String = ccDefaultMainThreadName; Const aLocalSwitchboardName : String = ccDefaultSwitchBoardName);
Procedure Fini;
Procedure RegisterMessages;

Procedure SendMessage(Const aMessage : TCustomActorMessage);
Function ReceiveMessage(Const aTimeout : Integer = ccDefaultTimeout): TCustomActorMessage;
Function Request(Const aMessage : TCustomActorMessage; Const aTimeout : Integer): Boolean;
Procedure Reply(Const aMessage : TCustomActorMessage);

Procedure RegisterActorClass(Const aClass : TClass);
Procedure StartActorInstance(Const aClassName, aInstanceName : String);
Procedure ConfigActor(Const aInstanceName, aVariable : String; Const aValue : Variant);

Implementation

Var
	gActorNameSemaphore : TMultiReadExclusiveWriteSynchronizer;
	lLastActorName : Int64;

Function GetNewActorName: String;
Begin
	gActorNameSemaphore.BeginWrite;
	Try
		Result := 'actor' + IntToStr(lLastActorName);
		Inc(lLastActorName);
	Finally
		gActorNameSemaphore.EndWrite;
	End;
End;

// TActorThread

Function TActorThread.GetTopMessage: TCustomActorMessage;
Begin
	Result := fMailbox.Top;
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
		lMessage := fMailbox.Top As TConfigInstanceActorMessage;
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

Procedure TActorThread.GetConfigOfInstance(Var aMessage);
Var
	lRequestMessage : TGetConfigInstanceActorMessage;
	lReplyMessage : TGetConfigInstanceReplyActorMessage;
	lError : TErrorActorMessage;
Begin
	Try
		lRequestMessage := fMailbox.Top As TGetConfigInstanceActorMessage;
		lReplyMessage := TGetConfigInstanceReplyActorMessage.Create('', '');
		lReplyMessage.Name := lRequestMessage.Data;
		lReplyMessage.Value := GetPropertyValueByName(lRequestMessage.Data);
		Reply(lReplyMessage);
	Except
		On E: Exception Do 
		Begin
			lError := TErrorActorMessage.Create(ActorName, ccDefaultLogger);
			lError.Data := E.Message;
			Send(lError);
		End;
	End;
End;

Constructor TActorThread.Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create;
	If aName <> '' Then
		fActorName := aName
	Else
		fActorName := GetNewActorName;
	// Debug WriteLn('Actor being created ', aName);
	fTimeout := aTimeout;
	fMailbox := TCustomSynchronizedQueue.Create;
	FreeOnTerminate := False;
	fRunning := True;
End;

Destructor TActorThread.Destroy;
Begin
	FreeAndNil(fMailbox);
	Inherited Destroy;
End;

Procedure TActorThread.Execute;
Begin
	Repeat
		If fMailbox.WaitForMessage(fTimeout) Then
			DispatchTopMessage
		Else
			Idle;
	Until Not fRunning;
	Switchboard.Mailbox.Push(TRemoveActorMessage.Create(ActorName, Switchboard.ActorName));
End;

Procedure TActorThread.Idle;
Begin
End;

Procedure TActorThread.Send(Const aMessage : TCustomActorMessage);
Begin
	SwitchBoard.Mailbox.Push(aMessage);
End;

Function TActorThread.Request(Const aMessage : TCustomActorMessage): Boolean;
Var
	lRequestID : Int64;
Begin
	lRequestID := aMessage.TransactionID;
	Send(aMessage);
	Result := fMailbox.WaitForTransaction(lRequestID, fTimeout);
End;

Procedure TActorThread.Reply(aMessage : TCustomActorMessage);
Begin
	aMessage.Source := fActorName;
	aMessage.Destination := fMailbox.Top.Source;
	aMessage.TransactionID := fMailbox.Top.TransactionID;
	Send(aMessage);
End;

Procedure TActorThread.DispatchTopMessage;
Begin
	// Debug WriteLn('Dispatching message ', Message.ClassName);
	DispatchEvent(fMailbox.Top.ClassName, Nil);
	fMailbox.Drop;
End;

// TSwitchBoardActor

Procedure TSwitchBoardActor.RegisterClass(Var aMessage);
Var
	lMessage : TRegisterClassActorMessage;
Begin
	lMessage := Mailbox.Top As TRegisterClassActorMessage;
	fClasses.Add(lMessage.ClassReference.ClassName, Pointer(lMessage.ClassReference));
End;

Procedure TSwitchBoardActor.UnregisterClass(Var aMessage);
Var
	lMessage : TUnregisterClassActorMessage;
	lIndex : Integer;
Begin
	lMessage := Mailbox.Top As TUnregisterClassActorMessage;
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
	lMessage := Mailbox.Top As TCreateInstanceActorMessage;
	lIndex := fClasses.FindIndexOf(lMessage.NameOfClass);
	If lIndex >= 0 Then
	Begin
		lActor := TActorThreadClass(fClasses.Items[lIndex]).Create(lMessage.NameOfInstance);
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
	lMessage := Mailbox.Top As TCreateInstanceAndConfigActorMessage;
	lIndex := fClasses.FindIndexOf(lMessage.NameOfClass);
	If lIndex >= 0 Then
	Begin
		lActor := TActorThreadClass(fClasses.Items[lIndex]).Create(lMessage.NameOfInstance);
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
	lMessage := Mailbox.Top As TRemoveActorMessage;
	lIndex := fInstances.FindIndexOf(lMessage.Source);
	If lIndex >= 0 Then
	Begin
		// Debug WriteLn((fInstances.Items[lIndex] As TActorThread).ActorName, ' is quitting.');
		(fInstances.Items[lIndex] As TActorThread).Terminate;
		fInstances.Delete(lIndex);
	End;
End;

Constructor TSwitchBoardActor.Create(Const aName : String = ccDefaultSwitchBoardName; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create;
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
Begin
	While Running Do
	Begin
		If Mailbox.WaitForMessage(Timeout) Then
			If Message.Destination = ActorName Then
				DispatchTopMessage
			Else
				If Message.Destination = MainThreadName Then
					MainThreadQueue.Push(Mailbox.Pop)
				Else
					Route;
	End;
	// Debug WriteLn('Running actors : ', fInstances.Count);
	AskAllActorsToQuit;
	// Debug WriteLn('Sleeping ', ccDefaultTimeout * 10, ' seconds...');
	Sleep(ccDefaultTimeout * 10);
	// Debug WriteLn('Running actors : ', fInstances.Count);
	KillAllActors;
	// Debug WriteLn('Running actors : ', fInstances.Count);
End;

Procedure TSwitchBoardActor.Route;
Var
	lTarget : TObject;
Begin
	lTarget := fInstances.Find(Mailbox.Top.Destination);
	If Assigned(lTarget) Then 
		(lTarget As TActorThread).MailBox.Push(fMailbox.Pop)
	Else
		fMailbox.Drop;
End;

Procedure TSwitchBoardActor.AskAllActorsToQuit;
Var
	lCtrl : Integer;
Begin
	// Debug WriteLn('Quitting all actors.');
	For lCtrl := 0 To fInstances.Count - 1 Do
	Begin
		// Debug WriteLn('Asking ', (fInstances.Items[lCtrl] As TActorThread).ActorName, ' to quit.');
		(fInstances.Items[lCtrl] As TActorThread).Mailbox.Push(TTerminateActorMessage.Create(ActorName, (fInstances.Items[lCtrl] As TActorThread).ActorName));
	End;
End;

Procedure TSwitchBoardActor.KillAllActors;
Var
	lCtrl : Integer;
Begin
	For lCtrl := 0 To fInstances.Count - 1 Do
	Begin
		// Debug WriteLn('Forcefully making ', (fInstances.Items[lCtrl] As TActorThread).ActorName, ' quit.');
		(fInstances.Items[lCtrl] As TActorThread).Terminate;
		fInstances.Delete(lCtrl);
	End;
End;

// Unit utils

Procedure Init(Const aLocalName : String = ccDefaultMainThreadName; Const aLocalSwitchboardName : String = ccDefaultSwitchBoardName);
Begin
	MainThreadName := aLocalName;
	MainThreadQueue := TCustomSynchronizedQueue.Create;
	SwitchBoard := TSwitchBoardActor.Create(aLocalSwitchboardName);
	SwitchBoard.Start;
	gActorNameSemaphore := TMultiReadExclusiveWriteSynchronizer.Create;
	lLastActorName := -1;
End;

Procedure Fini;
Begin
	// Debug WriteLn('Asking switchboard to finish.');
	SwitchBoard.MailBox.Push(TTerminateActorMessage.Create(MainThreadName, SwitchBoard.ActorName));
	// Debug WriteLn('Waiting for switchboard to finish.');
	SwitchBoard.WaitFor;
	// Debug WriteLn('Freeing main queue.');
	MainThreadQueue.Free;
	FreeAndNil(gActorNameSemaphore);
End;

Procedure RegisterMessages;
Begin
End;

Procedure SendMessage(Const aMessage : TCustomActorMessage);
Begin
	Switchboard.Mailbox.Push(aMessage);
End;

Function ReceiveMessage(Const aTimeout : Integer = ccDefaultTimeout): TCustomActorMessage;
Begin
	If MainThreadQueue.WaitForMessage(aTimeout) Then
		Result := MainThreadQueue.Pop
	Else
		Result := Nil;
End;

Function Request(Const aMessage : TCustomActorMessage; Const aTimeout : Integer): Boolean;
Var
	lRequestID : Int64;
Begin
	lRequestID := aMessage.TransactionID;
	SendMessage(aMessage);
	Result := MainThreadQueue.WaitForTransaction(lRequestID, aTimeout);
End;

Procedure Reply(Const aMessage : TCustomActorMessage);
Begin
	aMessage.Source := MainThreadName;
	aMessage.Destination := MainThreadQueue.Top.Source;
	aMessage.TransactionID := MainThreadQueue.Top.TransactionID;
	SendMessage(aMessage);
	MainThreadQueue.Drop;
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
