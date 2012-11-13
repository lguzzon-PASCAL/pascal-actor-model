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
	TTCPListenerActor = Class(TWithTargetActor)
	Private
		fSocket : TTCPBlockSocket;
		fSocketTimeout : Integer;
		fIP,
		fPort : String;
		fWorkerClass : String;
	Public
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure OnStartWork; Override;
		Procedure OnStopWork; Override;
	Published
		Property IP : String Read fIP Write fIP;
		Property Port : String Read fPort Write fPort;
		Property SocketTimeout : Integer Read fSocketTimeout Write fSocketTimeout;
		Property WorkerClass : String Read fWorkerClass Write fWorkerClass;
	End;

	TTCPWorkerActor = Class(TActorThread)
	Private
		fSocket : TTCPBlockSocket;
		fSocketTimeout : Integer;
		fSocketInt : Integer;
	Public
		Constructor Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure Idle; Override;
		Procedure OnStartWork; Override;
		Procedure OnStopWork; Override;
		Procedure OnSocketCanRead; Virtual; Abstract;
	Published
		Property SocketInt : Integer Read fSocketInt Write fSocketInt;
		Property SocketTimeout : Integer Read fSocketTimeout Write fSocketTimeout;
	End;

Procedure Init;
Procedure Fini;
Procedure RegisterMessages;

Procedure StartATCPListener(Const aInstanceName, aWorkerClass, aIP, aPort : String);

Implementation

// TTCPListenerActor

Constructor TTCPListenerActor.Create(Const aName : String = '';
	Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TTCPBlockSocket.Create;
	fSocketTimeout := -1;
	fIP := '0.0.0.0';
	fPort := '0';
	fWorkerClass := '';
End;

Destructor TTCPListenerActor.Destroy;
Begin
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

Procedure TTCPListenerActor.Idle;
Var
	lSocket : Integer;
Begin
	If Working Then
		If fSocket.CanRead(fSocketTimeout) Then
		Begin
			lSocket := fSocket.Accept;
			If fSocket.LastError <> 0 Then
				ThrowError(fSocket.LastErrorDesc)
			Else
			Begin
			End;
		End;
End;

Procedure TTCPListenerActor.OnStartWork;
Begin
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

Procedure TTCPListenerActor.OnStopWork;
Begin
	Timeout := fSocketTimeout;
	fSocketTimeout := 0;
	fSocket.Close;
End;

// TTCPWorkerActor

Constructor TTCPWorkerActor.Create(Const aName : String = ''; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, aTimeout);
	fSocket := TTCPBlockSocket.Create;
	fSocketTimeout := 0;
	fSocketInt := -1;
End;

Destructor TTCPWorkerActor.Destroy;
Begin
	FreeAndNil(fSocket);
	Inherited Destroy;
End;

Procedure TTCPWorkerActor.Idle;
Begin
	If Working Then
		If fSocketInt > -1 Then
			If fSocket.CanRead(fSocketTimeout) Then
				OnSocketCanRead
			Else
		Else
			Sleep(fSocketTimeout);
End;

Procedure TTCPWorkerActor.OnStartWork;
Begin
	fSocketTimeout := Timeout;
	Timeout := 0;
	fSocket.Socket := fSocketInt;
	fSocket.GetSins;
End;

Procedure TTCPWorkerActor.OnStopWork;
Begin
	Timeout := fSocketTimeout;
	fSocketTimeout := 0;
	FreeAndNil(fSocket);
	fSocket := TTCPBlockSocket.Create;
End;

Procedure Init;
Begin
	RegisterActorClass(TTCPListenerActor);
End;

Procedure Fini;
Begin
End;

Procedure RegisterMessages;
Begin
End;

Procedure StartATCPListener(Const aInstanceName, aWorkerClass, aIP, aPort : String);
Begin
	StartActorInstance('TTCPListenerActor', aInstanceName);
	SetTargetOfActor(aInstanceName, aTarget);
	ConfigActor(aInstanceName, 'workerclass', aWorkerClass);
	ConfigActor(aInstanceName, 'ip', aIP);
	ConfigActor(aInstanceName, 'port', aPort);
	StartWork(aInstanceName);
End;

End.