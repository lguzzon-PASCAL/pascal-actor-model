{
	This file is part of Pascal-Actor-Model.

	Pascal-Actor-Model is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	Pascal-Actor-Model is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Pascal-Actor-Model.  If not, see <http://www.gnu.org/licenses/>.
}

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	SyncObjs,
	ContNrs,
	Actors;

Type
	TStringMessage = Class(TCustomMessage)
	Private
		fBuffer : String;
	Public
		// The message is simply a string
		Property Buffer : String Read fBuffer Write fBuffer;
	End;
	
	TConsumerActor = Class(TActorThread)
	Public
		// Override this just to avoid problems with abstract virtual methods 
		Procedure Idle; Override;
		// Lets handle messages of TStringMessage type
		Procedure StringMessage(Var aMessage); Message 'tstringmessage';
	End;

Procedure TConsumerActor.Idle;
Begin
	// We have nothing to do if theres no message pending
End;

Procedure TConsumerActor.StringMessage(Var aMessage);
Begin
	// Message received, unbundle it and print it to the screen
	WriteLn((UnbundleMessage(aMessage) As TStringMessage).Buffer);
	// DoneMessage will be called automatically
End;

Var
	lMessage : TStringMessage;
	lLine : String;

Begin
	// Creates the consumer Actor (the switchboard is automatically created)
	TConsumerActor.Create('consumer').Start;
	Repeat
		// Reads a new line
		ReadLn(lLine);
		// If the line instructs us to quit, do it
		If lLine = 'quit' Then
		Begin
			WriteLn('Sending terminate message.');
			// Asking the switchboard to quit makes it ask all actors to quit too
			Switchboard.Mailbox.Push(TQuitMessage.Create(ccMainThreadName, ccSwitchboardName));
		End
		Else // any input other than 'quit' is a message to our consumer
		Begin
			// Creates the message
			lMessage := TStringMessage.Create(ccMainThreadName, 'consumer');
			// Add a payload to it
			lMessage.Buffer := lLine;
			// Send it to our consumer
			Switchboard.Mailbox.Push(lMessage);
		End;
	Until lLine = 'quit';
	// Wait for the switchboard to finish (it will take care of the actors quit sequence)
	Switchboard.Waitfor;
	// We are done.
	WriteLn('Done.');
End.