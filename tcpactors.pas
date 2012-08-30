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
	TCPActors;

Interface

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	Actors,
	ActorMessages,
	CustomActors,
	ActorLogger,
	BlckSock,
	SysUtils;

Type
	TSetTCPToListenMessage = Class(TCustomActorMessage);

	TSetSocketActorMessage = Class(TCustomActorMessage)
	Private
		fSocket : Integer;
	Public
		Function Clone: TCustomActorMessage; Override;
	Published
		Property Socket : Integer Read fSocket Write fSocket;
	End;

	TTCPMessage = Class(TCustomStringActorMessage)
	Private
		fSenderIP,
		fSenderPort,
		fReceiverIP,
		fReceiverPort : String;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property SenderIP : String Read fSenderIP Write fSenderIP;
		Property SenderPort : String Read fSenderPort Write fSenderPort;
		Property ReceiverIP : String Read fReceiverIP Write fReceiverIP;
		Property ReceiverPort : String Read fReceiverPort Write fReceiverPort;
	End;

	TTCPReceiverActor = Class(TWithTargetActor)
	Private
		fSocket : TTCPBlockSocket;
		fSocketTimeout : Integer;
		fSocketRunning : Boolean;
	Public
		Procedure SetSocket(Var aMessage); Message 'TSetSocketActorMessage';
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
	End;

	TTCPSenderActor = Class(TActorThread)
	Private
		fSocket : TTCPBlockSocket;
		fSocketRunning : Boolean;
	Public
		Procedure SetSocket(Var aMessage); Message 'TSetSocketActorMessage';
		Procedure SendString(Var aMessage); Message 'TCustomStringActorMessage';
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
	End;

	TTCPListenerActor = Class(TWithTargetActor)
	Private
		fSocket : TTCPBlockSocket;
		fSocketTimeout : Integer;
		fSocketRunning : Boolean;
		fIP,
		fPort : String;
	Public
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure StartListener(Var aMessage); Message 'TSetTCPToListenMessage';
	Published
		Property IP : String Read fIP Write fIP;
		Property Port : String Read fPort Write fPort;
	End;

Procedure Init;
Procedure Fini;
Procedure RegisterMessages;

Procedure SetTCPToListen(Const aInstanceName : String);
Procedure StartATCPListener(Const aInstanceName, aTarget, aIP, aPort : String);
Procedure SetTCPSocket(Const aInstanceName : String; Const aSocket : Integer);
Procedure StartATCPReceiver(Const aInstanceName : String; Const aSocket : Integer);
Procedure StartATCPSender(Const aInstanceName : String; Const aSocket : Integer);
Procedure ConnectTo(Const aIP, aPort, aReceiverName, aReceiverTarget, aSenderName : String);

Implementation

// TSetSocketActorMessage

Function TSetSocketActorMessage.Clone: TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TSetSocketActorMessage).Socket := fSocket;
End;

// TTCPMessage

Function TTCPMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TTCPMessage).SenderIP := fSenderIP;
	(Result As TTCPMessage).SenderPort := fSenderPort;
	(Result As TTCPMessage).ReceiverIP := fReceiverIP;
	(Result As TTCPMessage).ReceiverPort := fReceiverPort;
End;

// TTCPReceiverActor

Procedure TTCPReceiverActor.SetSocket(Var aMessage);
Var
	lSocketMsg : TSetSocketActorMessage;
Begin
	lSocketMsg := Message As TSetSocketActorMessage;
	fSocket.Socket := lSocketMsg.Socket;
	fSocket.GetSins;
	If fSocket.LastError <> 0 Then
		ThrowError(fSocket.LastErrorDesc);
	fSocketTimeout := Timeout;
	Timeout := 0;
	fSocketRunning := True;
End;

Constructor TTCPReceiverActor.Create(Const aName : String = '';
	Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TTCPBlockSocket.Create;
	fSocketRunning := False;
	fSocketTimeout := -1;
End;

Destructor TTCPReceiverActor.Destroy;
Begin
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

Procedure TTCPReceiverActor.Idle;
Var
	lMessage : TTCPMessage;
Begin
	If fSocketRunning Then
		If fSocket.CanRead(fSocketTimeout) Then
		Begin
			lMessage := TTCPMessage.Create(ActorName, Target);
			lMessage.Data := fSocket.RecvString(fSocketTimeout);
			If fSocket.LastError <> 0 Then
				ThrowError(fSocket.LastErrorDesc);
			lMessage.SenderIP := fSocket.GetRemoteSinIP;
			lMessage.SenderPort := IntToStr(fSocket.GetRemoteSinPort);
			lMessage.ReceiverIP := fSocket.GetLocalSinIP;
			lMessage.ReceiverPort := IntToStr(fSocket.GetLocalSinPort);
			Send(lMessage);
		End;
End;

// TTCPSenderActor

Procedure TTCPSenderActor.SetSocket(Var aMessage);
Var
	lSocketMsg : TSetSocketActorMessage;
Begin
	lSocketMsg := Message As TSetSocketActorMessage;
	fSocket.Socket := lSocketMsg.Socket;
	fSocket.GetSins;
	If fSocket.LastError <> 0 Then
		ThrowError(fSocket.LastErrorDesc);
	fSocketRunning := True;
End;

Procedure TTCPSenderActor.SendString(Var aMessage);
Var
	lMessage : TCustomStringActorMessage;
Begin
	lMessage := Message As TCustomStringActorMessage;
	If fSocketRunning Then
	Begin
		fSocket.SendString(lMessage.Data);
		If fSocket.LastError <> 0 Then
			ThrowError(fSocket.LastErrorDesc);
	End;
End;

Constructor TTCPSenderActor.Create(Const aName : String = '';
	Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TTCPBlockSocket.Create;
	fSocketRunning := False;
End;

Destructor TTCPSenderActor.Destroy;
Begin
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

// TTCPListenerActor

Constructor TTCPListenerActor.Create(Const aName : String = '';
	Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TTCPBlockSocket.Create;
	fSocketRunning := False;
	fSocketTimeout := -1;
	fIP := '127.0.0.1';
	fPort := '1024';
End;

Destructor TTCPListenerActor.Destroy;
Begin
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

Procedure TTCPListenerActor.Idle;
Var
	lSocket : Integer;
	lSetSocket : TSetSocketActorMessage;
Begin
	If fSocketRunning Then
		If fSocket.CanRead(fSocketTimeout) Then
		Begin
			lSocket := fSocket.Accept;
			If fSocket.LastError <> 0 Then
				ThrowError(fSocket.LastErrorDesc)
			Else
			Begin
				lSetSocket := TSetSocketActorMessage.Create(ActorName, Target);
				lSetSocket.Socket := lSocket;
				Send(lSetSocket);
			End;
		End;
End;

Procedure TTCPListenerActor.StartListener(Var aMessage);
Begin
	fSocketRunning := True;
	fSocketTimeout := Timeout;
	Timeout := 0;
	fSocket.SetLinger(True, 1000);
	If fSocket.LastError <> 0 Then
		ThrowError(fSocket.LastErrorDesc);
	fSocket.Bind(fIP, fPort);
	If fSocket.LastError <> 0 Then
		ThrowError(fSocket.LastErrorDesc);
	fSocket.Listen;
	If fSocket.LastError <> 0 Then
		ThrowError(fSocket.LastErrorDesc);
End;

Procedure Init;
Begin
	RegisterActorClass(TTCPReceiverActor);
	RegisterActorClass(TTCPSenderActor);
	RegisterActorClass(TTCPListenerActor);
End;

Procedure Fini;
Begin
End;

Procedure RegisterMessages;
Begin
	ActorMessageClassFactory.RegisterMessage(TSetTCPToListenMessage);
	ActorMessageClassFactory.RegisterMessage(TSetSocketActorMessage);
	ActorMessageClassFactory.RegisterMessage(TTCPMessage);
End;

Procedure SetTCPToListen(Const aInstanceName : String);
Var
	lMessage : TSetTCPToListenMessage;
Begin
	lMessage := TSetTCPToListenMessage.Create(MainThreadName, aInstanceName);
	SendMessage(lMessage);
End;

Procedure StartATCPListener(Const aInstanceName, aTarget, aIP, aPort : String);
Begin
	StartActorInstance('TTCPListenerActor', aInstanceName);
	SetTargetOfActor(aInstanceName, aTarget);
	ConfigActor(aInstanceName, 'ip', aIP);
	ConfigActor(aInstanceName, 'port', aPort);
	SetTCPToListen(aInstanceName);
End;

Procedure SetTCPSocket(Const aInstanceName : String; Const aSocket : Integer);
Var
	lMessage : TSetSocketActorMessage;
Begin
	lMessage := TSetSocketActorMessage.Create(ccDefaultMainThreadName, aInstanceName);
	lMessage.Socket := aSocket;
	SendMessage(lMessage);
End;

Procedure StartATCPReceiver(Const aInstanceName : String; Const aSocket : Integer);
Begin
	StartActorInstance('TTCPReceiverActor', aInstanceName);
	SetTCPSocket(aInstanceName, aSocket);
End;

Procedure StartATCPSender(Const aInstanceName : String; Const aSocket : Integer);
Begin
	StartActorInstance('TTCPSenderActor', aInstanceName);
	SetTCPSocket(aInstanceName, aSocket);
End;

Procedure ConnectTo(Const aIP, aPort, aReceiverName, aReceiverTarget, aSenderName : String);
Var
	lSocket : TTCPBlockSocket;
Begin
	lSocket := TTCPBlockSocket.Create;
	Try
		lSocket.Connect(aIP, aPort);
		StartATCPReceiver(aReceiverName, lSocket.Socket);
		SetTargetOfActor(aReceiverName, aReceiverTarget);
		StartATCPSender(aSenderName, lSocket.Socket);
	Finally
		FreeAndNil(lSocket);
	End;
End;

End.