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
		Procedure PingMessage(Var aMessage); Message 'TPingMessage';
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
	lMessage : TPingMessage;
Begin
	If (Target <> '') And (Random(100) < 25) Then
	Begin
		lMessage := TPingMessage.Create(ActorName, Target);
		lMessage.Time := Now;
		WriteLn(ActorName, ' sending ping...(', lMessage.TransactionID, ')');
		If Request(lMessage) Then
		Begin
			WriteLn(ActorName, ' pong received.');
			WriteLn(ActorName, ' Time : ', TimeToStr((Message As TPongMessage).Time));
			WriteLn(ActorName, ' Now : ', TimeToStr(Now));
			WriteLn(ActorName, ' Millisec : ', MilliSecondsBetween(Now, (Message As TPongMessage).Time));
			Mailbox.Drop;
		End
		Else
			WriteLn(ActorName, ' ping timeout.');
	End;
End;

// 	TPongActor

Procedure TPongActor.PingMessage(Var aMessage);
Var
	lPingMessage : TPingMessage;
	lPongMessage : TPongMessage;
Begin
	lPingMessage := Message As TPingMessage;
	WriteLn(ActorName, ' received ping...(', lPingMessage.TransactionID, ')');
	lPongMessage := TPongMessage.Create(ActorName, lPingMessage.Source);
	lPongMessage.TransactionID := lPingMessage.TransactionID;
	lPongMessage.Time := (lPingMessage As TPingMessage).Time;
	WriteLn(ActorName, ' sending pong...');
	Sleep(100);
	Send(lPongMessage);
	WriteLn(ActorName, ' Pong sent.');
End;

Var
	gBuffer : String;

Begin
	// Register messages
	ActorMessages.RegisterMessages;
	Actors.RegisterMessages;
	ActorLogger.RegisterMessages;
	CustomActors.RegisterMessages;
	ActorMessageClassFactory.RegisterMessage(TPingMessage);
	ActorMessageClassFactory.RegisterMessage(TPongMessage);

	// Initialize systems
	ActorMessages.Init;
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
	ActorMessages.Fini;
End.
