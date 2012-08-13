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
	UDPActors;

Type
	TScreenWriterActor = Class(TActorThread)
	Public
		Procedure ScreenWrite(Var aMessage); Message 'TUDPMessage';
	End;

Procedure TScreenWriterActor.ScreenWrite(Var aMessage);
Var
	lMessage : TUDPMessage;
Begin
	lMessage := Mailbox.Pop As TUDPMessage;
	Try
		WriteLn(lMessage.SenderIP, '>', lMessage.Data);
	Finally
		FreeAndNil(lMessage);
	End;
End;

Var
	gBuffer : String;
	gUDPMessage : TUDPMessage;

Begin
	// Register messages
	ActorMessages.RegisterMessages;
	Actors.RegisterMessages;
	ActorLogger.RegisterMessages;
	CustomActors.RegisterMessages;
	UDPActors.RegisterMessages;

	// Initialize systems
	ActorMessages.Init;
	Actors.Init('localhost', 'switchboard');
	ActorLogger.Init;
	CustomActors.Init;
	UDPActors.Init;

	// Register aditional actor classes
	RegisterActorClass(TScreenWriterActor);

	// Start actors and set config
	StartActorInstance('TScreenWriterActor', 'screen1');
	StartAUDPReceiver('udpreceiver1', 'screen1', ParamStr(1), ParamStr(2), 1500);
	StartAUDPPairedSender('udpsender1', 'udpreceiver1');

	Repeat
		Write('Local>'); ReadLn(gBuffer);
		If gBuffer <> 'quit' Then
		Begin
			gUDPMessage := TUDPMessage.Create(MainThreadName, 'udpsender1');
			gUDPMessage.Data := gBuffer;
			gUDPMessage.SenderIP := '';
			gUDPMessage.SenderPort := '';
			gUDPMessage.ReceiverIP := ParamStr(3);
			gUDPMessage.ReceiverPort := ParamStr(4);
			Switchboard.Mailbox.Push(gUDPMessage);
		End;
	Until gBuffer = 'quit';

	// Finish actors
	UDPActors.Fini;
	CustomActors.Fini;
	ActorLogger.Fini;
	Actors.Fini;
	ActorMessages.Fini;
End.
