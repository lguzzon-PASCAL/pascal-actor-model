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

	TUDPReceiver = Class(TWithTargetActor)
	Private
		fSocketInt : Integer;
		fSocketTimeout : Integer;
		fSocket : TUDPBlockSocket;
		fIP,
		fPort : String;
		fPacketMaxSize : Integer;
	Public
		Constructor Create(Const aName : String = ccDefaultSwitchBoardName; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure OnStartWork; Override;
		Procedure OnStopWork; Override;
	Published
		Property IP : String Read fIP Write fIP;
		Property Port : String Read fPort Write fPort;
		Property SocketInt : Integer Read fSocketInt Write fSocketInt;
		Property PacketMaxSize : Integer Read fPacketMaxSize Write fPacketMaxSize;
	End;

	TUDPSender = Class(TActorThread)
	Public
		Procedure SendString(Var aMessage); Message 'TUDPMessage';
	End;

	TPairedUDPSender = Class(TActorThread)
	Private
		fSocket : TUDPBlockSocket;
		fPairWith : String;
	Public
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure OnStartWork; Override;
		Procedure OnStopWork; Override;
		Procedure SendString(Var aMessage); Message 'TUDPMessage';
	Published
		Property PairWith : String Read fPairWith Write fPairWith;
	End;

Procedure Init;
Procedure Fini;
Procedure RegisterMessages;

Procedure StartAUDPReceiver(Const aInstanceName, aTarget, aIP, aPort : String; Const aMaxPacketSize : Integer = 1000);
Procedure StartAUDPSender(Const aInstanceName : String);
Procedure StartAUDPPairedSender(Const aInstanceName, aPairedReceiver : String);

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

Constructor TUDPReceiver.Create(Const aName : String = ccDefaultSwitchBoardName; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TUDPBlockSocket.Create;
	fSocketRunning := False;
	fIP := '0.0.0.0';
	fPort := '0';
	fPacketMaxSize := 1000;
	fSocketInt := -1;
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
				ThrowError(fSocket.LastErrorDesc)
			Else
			Begin
				// Debug WriteLn(ActorName, ': ', lBuffer);
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

Procedure TUDPReceiver.OnStartWork;
Begin
	fSocketTimeout := Timeout;
	Timeout := 0;
	fSocket.Bind(fIP, fPort);
	If fSocket.LastError <> 0 Then
		ThrowError(fSocket.LastErrorDesc);
	fSocketInt := fSocket.Socket;
End;

Procedure TUDPReceiver.OnStopWork;
Begin
	Timeout := fSocketTimeout;
	fSocketTimeout := 0;
	FreeAndNil(fSocket);
	fSocket := TUDPBlockSocket.Create;
End;

// TUDPSender

Procedure TUDPSender.SendString(Var aMessage);
Var
	lSocket : TUDPBlockSocket;
	lMessage : TUDPMessage;
Begin
	lMessage := Message As TUDPMessage;
	Try
		lSocket := TUDPBlockSocket.Create;
		lSocket.Bind(lMessage.SenderIP, lMessage.SenderPort);
		If lSocket.LastError <> 0 Then
			ThrowError(lSocket.LastErrorDesc);
		lSocket.Connect(lMessage.ReceiverIP, lMessage.ReceiverPort);
		If lSocket.LastError <> 0 Then
			ThrowError(lSocket.LastErrorDesc);
		lSocket.SendString(lMessage.Data);
		If lSocket.LastError <> 0 Then
			ThrowError(lSocket.LastErrorDesc);
	Finally
		FreeAndNil(lSocket);
	End;
End;

// TPairedUDPSender

Constructor TPairedUDPSender.Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName);
	// Debug WriteLn('Creating socket.');
	fSocket := TUDPBlockSocket.Create;
	fPairWith := '';
End;

Destructor TPairedUDPSender.Destroy;
Begin
	// Debug WriteLn('Destroying socket.');
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

Procedure TPairedUDPSender.SendString(Var aMessage);
Var
	lMessage : TUDPMessage;
Begin
	lMessage := Message As TUDPMessage;
	// Debug WriteLn(ActorName, ': ', lMessage.Data);
	If fSocketRunning Then
	Begin
		fSocket.Connect(lMessage.ReceiverIP, lMessage.ReceiverPort);
		If fSocket.LastError <> 0 Then
			ThrowError(fSocket.LastErrorDesc);
		fSocket.SendString(lMessage.Data);
		If fSocket.LastError <> 0 Then
			ThrowError(fSocket.LastErrorDesc);
	End;
End;

Procedure TPairedUDPSender.OnStartWork;
Var
	lMessage : TSetUDPToSendMessage;
	lSocketRequest : TGetConfigInstanceActorMessage;
	lSocketReply : TGetConfigInstanceReplyActorMessage;
Begin
	lMessage := Message As TSetUDPToSendMessage;
	lSocketReply := Nil;
	Try
		// Debug WriteLn(ActorName, ': Asking ', lMessage.Data, ' to provide the receiving socket handle.');
		lSocketRequest := TGetConfigInstanceActorMessage.Create(ActorName, fPairWith);
		lSocketRequest.Data := 'SocketInt';
		If Request(lSocketRequest) Then
		Begin
			lSocketReply := Message As TGetConfigInstanceReplyActorMessage;
			// Debug WriteLn(ActorName, ': ', lMessage.Data, ' responded with ', lSocketReply.Value);
			fSocket.Socket := lSocketReply.Value;
			fSocket.GetSins;
		End
		Else
			ThrowError('Cant find paired receiver or timedout in operation.');
	Finally
		FreeAndNil(lSocketReply);
		FreeAndNil(lMessage);
	End;
End;

Procedure TPairedUDPSender.OnStopWork;
Begin
	FreeAndNil(fSocket);
	fSocket := TUDPBlockSocket.Create;
End;

Procedure Init;
Begin
	RegisterActorClass(TUDPReceiver);
	RegisterActorClass(TUDPSender);
	RegisterActorClass(TPairedUDPSender);
End;

Procedure Fini;
Begin
End;

Procedure RegisterMessages;
Begin
	ActorMessageClassFactory.RegisterMessage(TUDPMessage);
End;

Procedure StartAUDPReceiver(Const aInstanceName, aTarget, aIP, aPort : String; Const aMaxPacketSize : Integer = 1000);
Begin
	StartActorInstance('TUDPReceiver', aInstanceName);
	SetTargetOfActor(aInstanceName, aTarget);
	ConfigActor(aInstanceName, 'ip', aIP);
	ConfigActor(aInstanceName, 'port', aPort);
	ConfigActor(aInstanceName, 'packetmaxsize', aMaxPacketSize);
	StartWork(aInstanceName);
End;

Procedure StartAUDPSender(Const aInstanceName : String);
Begin
	StartActorInstance('TUDPSender');
End;

Procedure StartAUDPPairedSender(Const aInstanceName, aPairedReceiver : String);
Begin
	StartActorInstance('TPairedUDPSender', aInstanceName);
	ConfigActor(aInstanceName, 'pairwith', aPairedReceiver);
	StartWork(aInstanceName);
End;

End.