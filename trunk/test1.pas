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
		Property Buffer : String Read fBuffer Write fBuffer;
	End;
	
	TConsumerActor = Class(TActorThread)
	Public
		Constructor Create(Const aName : String = 'consumer'; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Procedure Idle; Override;
		Procedure StringMessage(Var aMessage); Message 'tstringmessage';
	End;

Constructor TConsumerActor.Create(Const aName : String = 'consumer'; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
End;

Procedure TConsumerActor.Idle;
Begin
End;

Procedure TConsumerActor.StringMessage(Var aMessage);
Begin
	WriteLn((UnbundleMessage(aMessage) As TStringMessage).Buffer);
End;

Var
	lMessage : TStringMessage;
	lLine : String;

Begin
	TConsumerActor.Create.Start;
	Repeat
		ReadLn(lLine);
		If lLine = 'quit' Then
		Begin
			WriteLn('Sending terminate message.');
			Switchboard.Mailbox.Push(TQuitMessage.Create('', ccSwitchboardName));
		End
		Else
		Begin
			lMessage := TStringMessage.Create('', 'consumer');
			lMessage.Buffer := lLine;
			Switchboard.Mailbox.Push(lMessage);
		End;
	Until lLine = 'quit';
	Switchboard.Waitfor;
	WriteLn('Done.');
End.