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

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	Actors,
	ActorMessages,
	ActorLogger,
	CustomActors,
	DateUtils;

Type
	TPingMessage = Class(TCustomStringActorMessage)
	Private
		fPingTime : TDateTime;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Time : TDateTime Read fPingTime Write fPingTime;
	End;
	
	TPongMessage = Class(TCustomStringActorMessage)
	Private
		fPingTime : TDateTime;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Time : TDateTime Read fPingTime Write fPingTime;
	End;

	TPingActor = Class(TWithTargetActor)
	Public
		Procedure Idle; Override;
	End;
	
	TPongActor = Class(TActorThread)
	Public
		Procedure PingMessage(Var aMessage); Message 'tpingmessage';
	End;

// TPingMessage

Function TPingMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TPingMessage).Time := Self.Time;
End;
	
// TPongMessage
	
Function TPongMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TPongMessage).Time := Self.Time;
End;

// TPingActor
	
Procedure TPingActor.Idle;
Var
	lMessage : TCustomActorMessage;
Begin
	If Target <> '' Then
	Begin
		lMessage := TPingMessage.Create(ActorName, Target);
		(lMessage As TPingMessage).Time := Now;
		// Debug WriteLn('Sending ping...(', lMessage.TransactionID, ')');
		Request(lMessage, ccDefaultTimeout * 10);
		If Assigned(Message) Then
		Begin
			// Debug WriteLn('Pong received.');
			// Debug WriteLn('Time : ', (Message As TPongMessage).Time);
			// Debug WriteLn('Now : ', Now);
			// Debug WriteLn('Millisec : ', MilliSecondsBetween(Now, (Message As TPongMessage).Time));
			DoneMessage;
		End;
	End;
End;

// 	TPongActor

Procedure TPongActor.PingMessage(Var aMessage);
Var
	lMessage : TPongMessage;
Begin
	// Debug WriteLn('Received ping...(', Message.TransactionID, ')');
	lMessage := TPongMessage.Create('', '');
	lMessage.Time := (Message As TPingMessage).Time;
	// Debug WriteLn('Sending pong...');
	Reply(lMessage);
	// Debug WriteLn('Pong sent.');
End;

Var
	gBuffer : String;

Begin
	// Register messages
	ActorMessages.RegisterMessages;
	Actors.RegisterMessages;
	ActorLogger.RegisterMessages;
	CustomActors.RegisterMesssages;
	ActorMessageClassFactory.RegisterMessage(TPingMessage);
	ActorMessageClassFactory.RegisterMessage(TPongMessage);

	// Initialize systems
	Actors.Init('localhost', 'switchboard');
	ActorLogger.Init;
	CustomActors.Init;

	// Register aditional actor classes
	RegisterActorClass(TPingActor);
	RegisterActorClass(TPongActor);
	
	// Start actors and set config
	StartActorInstance('TPingActor', 'ping1');
	StartActorInstance('TPongActor', 'pong1');
	SetTargetOfActor('ping1', 'pong1');

	Repeat
		Write('Input something : '); ReadLn(gBuffer);
	Until gBuffer = 'quit';

	// Finish actors
	CustomActors.Fini;
	ActorLogger.Fini;
	Actors.Fini;
End.
