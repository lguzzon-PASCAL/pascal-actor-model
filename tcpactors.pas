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

	TTCPListenerActor = Class(TActorThread)
	Private
		fSocket : TTCPBlockSocket;
		fSocketTimeout : Integer;
		fSocketRunning : Boolean;
		fChildClass,
		fIP,
		fPort : String;
	Public
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure StartListener(Var aMessage); Message 'TSetTCPToListenMessage';
	Published
		Property ChildClass : String Read fChildClass Write fChildClass;
		Property IP : String Read fIP Write fIP;
		Property Port : String Read fPort Write fPort;
	End;

	TTCPHandlerClass = Class(TActorThread)
	Private
		fSocket : TTCPBlockSocket;
		fSocketTimeout : Integer;
		fSocketRunning : Boolean;
	Public
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure DoRead; Virtual;
		Procedure SetSocket(Var aMessage); Message 'TSetSocketActorMessage';
	End;

Implementation

// TSetSocketActorMessage

Function TSetSocketActorMessage.Clone: TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TSetSocketActorMessage).Socket := fSocket;
End;

// TTCPListenerActor

Constructor TTCPListenerActor.Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TTCPBlockSocket.Create;
	fSocketRunning := False;
	fSocketTimeout := -1;
	fChildClass := '';
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
	lCreate : TCreateInstanceAndConfigActorMessage;
Begin
	If fSocketRunning Then
		If fSocket.CanRead(fSocketTimeout) Then
		Begin
			lSocket := fSocket.Accept;
			If fSocket.LastError <> 0 Then
				ThrowError(fSocket.LastErrorDesc)
			Else
			Begin
				lCreate := TCreateInstanceAndConfigActorMessage.Create(ActorName, Switchboard.ActorName);
				lCreate.NameOfInstance := '';
				lCreate.NameOfClass := fChildClass;
				lSetSocket := TSetSocketActorMessage.Create(ActorName, '');
				lSetSocket.Socket := lSocket;
				lCreate.AddMessage(lSetSocket);
				Send(lCreate);
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

// TTCPHandlerClass

Constructor TTCPHandlerClass.Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TTCPBlockSocket.Create;
	fSocketTimeout := -1;
	fSocketRunning := False;
End;

Destructor TTCPHandlerClass.Destroy;
Begin
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

Procedure TTCPHandlerClass.Idle;
Begin
	If fSocketRunning Then
		If fSocket.CanRead(fSocketTimeout) Then
			DoRead;
End;

Procedure TTCPHandlerClass.DoRead;
Begin
	// Implement your own handler
End;

Procedure TTCPHandlerClass.SetSocket(Var aMessage);
Var
	lMessage : TSetSocketActorMessage;
Begin
	lMessage := Mailbox.Pop As TSetSocketActorMessage;
	Try
		Timeout := 0;
		fSocketTimeout := Timeout;
		fSocketRunning := True;
		fSocket.Socket := lMessage.Socket;
		fSocket.GetSins;
	Finally
		FreeAndNil(lMessage);
	End;
End;

End.