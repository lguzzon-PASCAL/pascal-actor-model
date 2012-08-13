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
	SyncQueue;

Interface

Uses
	Classes,
	SysUtils,
	SyncObjs,
	ActorMessages,
	ContNrs,
	DateUtils;

Type
	// Synchronized Queue

	TCustomSynchronizedQueue = Class
	Private
		fQueue	  : TObjectQueue;
		fSynchronizer : TMultiReadExclusiveWriteSynchronizer;
		fSignal	: TEventObject;
		fActive	: Boolean;
	Public
		Function WaitFor(Const aTimeout : Cardinal): TWaitResult;
		Function Count : Integer; 
		Function AtLeast(Const aCount : Integer): Boolean;
		Procedure Push(Const aObject : TCustomActorMessage);
		Function Pop : TCustomActorMessage;
		Function Top : TCustomActorMessage;
		Procedure Recycle;
		Procedure Drop;
		Function WaitForMessage(Const aTimeout : Integer): Boolean;
		Function WaitForTransaction(Const aTransactionID : Int64; Const aTimeout : Integer): Boolean;
		Constructor Create;
		Destructor Destroy; Override;
	End;

Implementation

// TCustomSynchronizedQueue

Function TCustomSynchronizedQueue.WaitFor(Const aTimeout : Cardinal): TWaitResult;
Begin
	Result := fSignal.WaitFor(aTimeout);
End;

Function TCustomSynchronizedQueue.Count : Integer;
Begin
	Try
		fSynchronizer.BeginRead;
		Result := fQueue.Count;
	Finally
		fSynchronizer.EndRead;
	End;
End;

Function TCustomSynchronizedQueue.AtLeast(Const aCount : Integer): Boolean;
Begin
	Try
		fSynchronizer.BeginRead;
		Result := fQueue.AtLeast(aCount);
	Finally
		fSynchronizer.EndRead;
	End;
End;

Procedure TCustomSynchronizedQueue.Push(Const aObject : TCustomActorMessage);
Begin
	Try
		fSynchronizer.BeginWrite;
		fQueue.Push(aObject);
		fSignal.SetEvent;
	Finally
		fSynchronizer.EndWrite;
	End;
End;

Function TCustomSynchronizedQueue.Pop: TCustomActorMessage;
Begin
	Try
		fSynchronizer.BeginWrite;
		Result := (fQueue.Pop As TCustomActorMessage)
	Finally
		fSynchronizer.EndWrite;
	End;
End;

Function TCustomSynchronizedQueue.Top : TCustomActorMessage;
Begin
	Try
		fSynchronizer.BeginRead;
		Result := (fQueue.Peek As TCustomActorMessage);
	Finally
		fSynchronizer.EndRead;
	End;
End;

Procedure TCustomSynchronizedQueue.Recycle;
Begin
	Try
		fSynchronizer.BeginWrite;
		If fQueue.AtLeast(1) Then
			fQueue.Push(fQueue.Pop);
	Finally
		fSynchronizer.EndWrite;
	End;
End;

Procedure TCustomSynchronizedQueue.Drop;
Begin
	Try
		fSynchronizer.BeginWrite;
		If Assigned(fQueue.Peek) Then
			(fQueue.Pop As TCustomActorMessage).Free;
	Finally
		fSynchronizer.EndWrite;
	End;
End;

Function TCustomSynchronizedQueue.WaitForMessage(Const aTimeout : Integer): Boolean;
Var
	lStart : TDateTime;
	lTimeout : Integer;

	Procedure CalcTimeout;
	Begin
		lTimeout := MilliSecondsBetween(Now, lStart);
	End;

	Function NotTimeout: Boolean;
	Begin
		Result := (lTimeout <= aTimeout);
	End;

	Function SmallerTimeout: Integer;
	Begin
		Result := aTimeout Div 10;
		If Result < 1 Then
			Result := 1;
	End;

Begin
	lStart := Now;
	CalcTimeout;
	While Not(AtLeast(1)) And NotTimeout Do
	Begin
		WaitFor(SmallerTimeout);
		CalcTimeout;
	End;
	Result := AtLeast(1);
End;

Function TCustomSynchronizedQueue.WaitForTransaction(Const aTransactionID : Int64; Const aTimeout : Integer): Boolean;
Var
	lStart : TDateTime;
	lTimeout : Integer;

	Procedure CalcTimeout;
	Begin
		lTimeout := MilliSecondsBetween(Now, lStart);
	End;

	Function NotTimeout: Boolean;
	Begin
		// Debug  WriteLn('lTimeout : ', lTimeout, ' aTimeout : ', aTimeout);
		Result := lTimeout <= aTimeout;
	End;

	Function NotMatch: Boolean;
	Begin
		If AtLeast(1) Then
			Result := Top.TransactionID <> aTransactionID
		Else
			Result := True;
	End;

	Function SmallerTimeout: Integer;
	Begin
		Result := aTimeout Div 10;
		If Result < 1 Then
			Result := 1;
		// Debug  WriteLn('Waiting for ', Result, ' milliseconds...');
	End;

Begin
	// Debug WriteLn('TCustomSynchronizedQueue : Waiting for transaction');
	lStart := Now;
	CalcTimeout;
	While NotMatch And NotTimeout Do
	Begin
		// Debug WriteLn('TCustomSynchronizedQueue : NotMatch ', NotMatch, ' NotTimeout ', NotTimeout);
		If AtLeast(1) Then
			// Debug WriteLn('TCustomSynchronizedQueue : ', Top.Source, '->', Top.Destination, ': ', Top.ClassName, ':', Top.TransactionID);
		Recycle;
		WaitFor(SmallerTimeout);
		CalcTimeout;
	End;
	Result := Not NotMatch;
	// Debug WriteLn('TCustomSynchronizedQueue : Found ? ', Result);
End;

Constructor TCustomSynchronizedQueue.Create;
Begin
	Inherited Create;
	fQueue := TObjectQueue.Create;
	fSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
	fSignal := TEventObject.Create(Nil, False, False, '');
	fActive := True;
End;

Destructor TCustomSynchronizedQueue.Destroy;
Begin
	Try
		While fQueue.AtLeast(1) Do
			fQueue.Pop.Free;
	Finally
		FreeAndNil(fQueue);
		FreeAndNil(fSynchronizer);
		FreeAndNil(fSignal);
	End;
	Inherited Destroy;
End;

End.
