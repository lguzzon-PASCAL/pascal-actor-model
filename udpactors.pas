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
	UDPActors;

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
	TUDPMessage = Class(TCustomStringActorMessage)
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
	
	TStartUDPListenerMessage = Class(TCustomActorMessage);

	TUDPReceiver = Class(TWithTargetActor)
	Private
		fSocketTimeout : Integer;
		fSocket : TUDPBlockSocket;
		fSocketRunning : Boolean;
		fIP,
		fPort : String;
		fPacketMaxSize : Integer;
	Public
		Constructor Create(
			Const aName : String = '';
			CreateSuspended : Boolean = False;
			Const StackSize : SizeUInt = DefaultStackSize;
			Const aTimeout : Integer = ccDefaultTimeout
		); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure StartListener(Var aMessage); Message 'tstartudplistenermessage';
	Published
		Property IP : String Read fIP Write fIP;
		Property Port : String Read fPort Write fPort;
		Property PacketMaxSize : Integer Read fPacketMaxSize Write fPacketMaxSize;
	End;
	
	TUDPSender = Class(TActorThread)
	Public
		Procedure SendString(Var aMessage); Message 'tudpmessage';
	End;
	
Procedure Init;
Procedure Fini;
Procedure RegisterMessages;

Procedure SetUDPToListen(Const aInstanceName : String);
Procedure StartAUDPReceiver(Const aInstanceName, aTarget, aIP, aPort : String; Const aMaxPacketSize : Integer);

Implementation

// TUDPMessage

Function TUDPMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TUDPMessage).SenderIP := SenderIP;
	(Result As TUDPMessage).SenderPort := SenderPort;
	(Result As TUDPMessage).ReceiverIP := ReceiverIP;
	(Result As TUDPMessage).ReceiverPort := ReceiverPort;
End;

// TUDPReceiver

Constructor TUDPReceiver.Create(Const aName : String = ''; CreateSuspended : Boolean = False;
	Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
	fSocket := TUDPBlockSocket.Create;
	fSocketRunning := False;
	fIP := '127.0.0.1';
	fPort := '1024';
	fPacketMaxSize := 4096;
End;

Destructor TUDPReceiver.Destroy;
Begin
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

Procedure TUDPReceiver.Idle;
Var
	lSourceIP,
	lSourcePort,
	lDestIP,
	lDestPort : String;
	lError : TErrorActorMessage;
	lMessage : TUDPMessage;
	lBuffer : String;
Begin
	If fSocketRunning Then
		If fSocket.CanRead(fSocketTimeout) Then
		Begin
			lSourceIP := fSocket.GetRemoteSinIP;
			lSourcePort := IntToStr(fSocket.GetRemoteSinPort);
			lDestIP := fSocket.GetLocalSinIP;
			lDestPort := IntToStr(fSocket.GetLocalSinPort);
			lBuffer := fSocket.RecvPacket(fSocketTimeout);
			If fSocket.LastError <> 0 Then
			Begin
				lError := TErrorActorMessage.Create(ActorName, ccDefaultLogger);
				lError.Data := fSocket.LastErrorDesc;
				Send(lError);
			End
			Else
			Begin
				lMessage := TUDPMessage.Create(ActorName, Target);
				lMessage.Data := lBuffer;
				lMessage.SenderIP := lSourceIP;
				lMessage.SenderPort := lSourcePort;
				lMessage.ReceiverIP := lDestIP;
				lMessage.ReceiverPort := lDestPort;
				Send(lMessage);
			End;
		End;
End;

Procedure TUDPReceiver.StartListener(Var aMessage);
Var
	lError : TErrorActorMessage;
Begin
	fSocketRunning := True;
	fSocketTimeout := Timeout;
	Timeout := 0;
	fSocket.Bind(fIP, fPort);
	If fSocket.LastError <> 0 Then
	Begin
		lError := TErrorActorMessage.Create(ActorName, ccDefaultLogger);
		lError.Data := fSocket.LastErrorDesc;
		Send(lError);
	End;
End;

// TUDPSender

Procedure TUDPSender.SendString(Var aMessage);
Var
	lSocket : TUDPBlockSocket;
	lMessage : TUDPMessage;
	lError : TErrorActorMessage;
Begin
	lMessage := Message As TUDPMessage;
	Try
		lSocket := TUDPBlockSocket.Create;
		lSocket.Bind(lMessage.SenderIP, lMessage.SenderPort);
		If lSocket.LastError <> 0 Then
		Begin
			lError := TErrorActorMessage.Create(ActorName, ccDefaultLogger);
			lError.Data := lSocket.LastErrorDesc;
			Send(lError);
		End;
		lSocket.Connect(lMessage.ReceiverIP, lMessage.ReceiverPort);
		If lSocket.LastError <> 0 Then
		Begin
			lError := TErrorActorMessage.Create(ActorName, ccDefaultLogger);
			lError.Data := lSocket.LastErrorDesc;
			Send(lError);
		End;
		lSocket.SendString(lMessage.Data);
		If lSocket.LastError <> 0 Then
		Begin
			lError := TErrorActorMessage.Create(ActorName, ccDefaultLogger);
			lError.Data := lSocket.LastErrorDesc;
			Send(lError);
		End;
	Finally
		FreeAndNil(lSocket);
	End;
End;

Procedure Init;
Begin
	RegisterActorClass(TUDPReceiver);
	RegisterActorClass(TUDPSender);
End;

Procedure Fini;
Begin
End;

Procedure RegisterMessages;
Begin
	ActorMessageClassFactory.RegisterMessage(TUDPMessage);
	ActorMessageClassFactory.RegisterMessage(TStartUDPListenerMessage);
End;

Procedure SetUDPToListen(Const aInstanceName : String);
Var
	lMessage : TStartUDPListenerMessage;
Begin
	lMessage := TStartUDPListenerMessage.Create(MainThreadName, aInstanceName);
	Switchboard.Mailbox.Push(lMessage);
End;

Procedure StartAUDPReceiver(Const aInstanceName, aTarget, aIP, aPort : String; Const aMaxPacketSize : Integer);
Begin
	StartActorInstance('TUDPReceiver', aInstanceName);
	SetTargetOfActor(aInstanceName, aTarget);
	ConfigActor(aInstanceName, 'ip', aIP);
	ConfigActor(aInstanceName, 'port', aPort);
	ConfigActor(aInstanceName, 'packetmaxsize', aMaxPacketSize);
	SetUDPToListen(aInstanceName);
End;

End.